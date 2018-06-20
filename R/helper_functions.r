##
constraint_H_rhs_b <- function(A, b) {
    constr <- list(constr = rbind(A, -A), dir = rep("<=", 2 * nrow(A)), rhs = c(b,
        -b))
    return(constr)
}

col_blank <- function(df, FUN) apply(df, 2, FUN)
colMaxes <- function(df) col_blank(df, max)
colMins <- function(df) col_blank(df, min)
colMedians <- function(df) col_blank(df, median)
untimed_lambdas <- function(length.out, cyclical_function) {
    lapply(seq(0, 2 * pi, length.out = length.out), cyclical_function) %>% dcc
}

##' @param cycles_per_second in Hz.
lambda_task_time_df <- function(n_samples, cycles_per_second, cyclical_function){
	lambdas <- untimed_lambdas(n_samples, cyclical_function)
	df <- data.frame(time=seq(0,(n_samples-1)*1/cycles_per_second, by=1/cycles_per_second),
			lambda = lambdas)
	return(df)
}

task_time_df <- function(fmax_task, n_samples, cycles_per_second, cyclical_function, muscle_names){
	time_lambda_df <- lambda_task_time_df(n_samples, cycles_per_second, cyclical_function)
	df <- apply(time_lambda_df,1, function(row){
		scaled_task <- row[['lambda']] %*% fmax_task
		concatenated_row <- c(row[['time']], row[['lambda']], scaled_task) %>% as.data.frame %>% t
		return(concatenated_row)
	}) %>% t %>% as.data.frame
	colnames(df) <- c("time", "lambda", muscle_names)
	return(df)
}

constraint_H_with_bounds <- function(A, b, bounds_tuple_of_numeric) {
    H_constraint <- constraint_H_rhs_b(A, b)
    bounds <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric)
    return(mergeConstraints(H_constraint, bounds))
}

har_sample <- function(constr, n_samples, thin) {
    state <- har.init(constr, thin = 100)
    samples <- har.run(state, n.samples = n_samples)$samples %>% as.data.frame
    return(samples)
}


bound_constraints_for_all_muscles <- function(bounds_tuple_of_numeric) {
    n_muscles <- length(bounds_tuple_of_numeric)
    lapply(1:n_muscles, function(muscle_index) {
        lb <- lowerBoundConstraint(n_muscles, muscle_index, bounds_tuple_of_numeric[[muscle_index]]$lower)
        ub <- upperBoundConstraint(n_muscles, muscle_index, bounds_tuple_of_numeric[[muscle_index]]$upper)
        return(mergeConstraints(lb, ub))
    }) %>% mergeConstraints
}