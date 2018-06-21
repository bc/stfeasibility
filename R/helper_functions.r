##
constraint_H_rhs_b <- function(A, b) {
    constr <- list(constr = rbind(A, -A), dir = rep("<=", 2 * nrow(A)), rhs = c(b,
        -b))
    return(constr)
}

set_colnames <- function(df, vector){
	df_copy <- df
	colnames(df_copy) <- vector
	return(df_copy)
}

negative_cos <- function(...) -cos(...)
force_cos_ramp <- function(...) negative_cos(...)*0.5 + 0.5


ggparcoord_har <- function(df){
	ggparcoord(df, scale="globalminmax", alpha=0.01, ...) + theme_classic()
}

col_blank <- function(df, FUN) apply(df, 2, FUN)
colMaxes <- function(df) col_blank(df, max)
colMins <- function(df) col_blank(df, min)
colMedians <- function(df) col_blank(df, median)
untimed_lambdas <- function(length.out, cyclical_function) {
    lapply(seq(0, 2 * pi, length.out = length.out), cyclical_function) %>% dcc
}

##' @param cycles_per_second in Hz.
lambda_task_time_df <- function(n_samples, cycles_per_second, cyclical_function) {
    lambdas <- untimed_lambdas(n_samples, cyclical_function)
    df <- data.frame(time = seq(0, (n_samples - 1) * 1/cycles_per_second, by = 1/cycles_per_second),
        lambda = lambdas)
    return(df)
}

task_time_df <- function(fmax_task, n_samples, cycles_per_second, cyclical_function,
    output_dimension_names) {
    time_lambda_df <- lambda_task_time_df(n_samples, cycles_per_second, cyclical_function)
    df <- apply(time_lambda_df, 1, function(row) {
        scaled_task <- row[["lambda"]] %*% fmax_task
        concatenated_row <- c(row[["time"]], row[["lambda"]], scaled_task) %>% as.data.frame %>%
            t
        return(concatenated_row)
    }) %>% t %>% as.data.frame
    colnames(df) <- c("time", "lambda", output_dimension_names)
    return(df)
}

where_muscles_have_unreasonable_values <- function(df, muscle_names){
	apply(df[,muscle_names], 2, function(muscle_entries){
		negative <- muscle_entries < 0.0
		over_one <- muscle_entries > 1.0
		return(negative | over_one)
	})
}

constraint_H_with_bounds <- function(A, b, bounds_tuple_of_numeric) {
    H_constraint <- constraint_H_rhs_b(A, b)
    bounds <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric)
    return(mergeConstraints(H_constraint, bounds))
}

constraint_H_lhs_direction <- function(A,direction, bounds_tuple_of_numeric){
	lhs <- constraint_H_rhs_b(A, rep(0, nrow(A)))
	
}

add_null_column_to_end_of_lhs <- function(constraint){
	c_copy <- constraint
	output_dimensionality <- nrow(constraint$constr)
	c_copy$constr <- cbind(constraint$constr, rep(0,output_dimensionality))
	return(c_copy)
}
a_matrix_lhs_direction <- function(A, direction, bounds_tuple_of_numeric){
	A_block <- constraint_H_rhs_b(cbind(A, direction), rep(0, nrow(A)))
	bounds_raw <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric)
	bounds <- add_null_column_to_end_of_lhs(bounds_raw)
	constraint <- mergeConstraints(A_block, bounds)
	return(constraint)
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


#to get the opposite direction, change the sign of the direction.
lpsolve_force_in_dir <- function(min_or_max, direction_constraint, n_muscles){
    #show interest in maximizing the lambda scaler parameter only.
    c_in_cTx <- c(rep(0.0, n_muscles), 1.0)
    lp_result <- lp(min_or_max, objective.in=c_in_cTx, const.mat=direction_constraint$constr,
        const.dir=direction_constraint$dir, const.rhs=direction_constraint$rhs, compute.sens=0)
    dir_of_interest <- direction_constraint$constr[1:4,ncol(direction_constraint$constr)]
	vector_out <- dir_of_interest * lp_result$objval
	vector_magnitude <- sqrt(sum(vector_out^2))
	output_structure <- list(lambda_scaler = lp_result$objval, muscle_activations = lp_result$solution[1:n_muscles],
        vector_out = vector_out, vector_magnitude = vector_magnitude)
    return(output_structure)
}