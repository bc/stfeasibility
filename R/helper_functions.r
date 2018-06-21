##
constraint_H_rhs_b <- function(A, b) {
    constr <- list(constr = rbind(A, -A), dir = rep("<=", 2 * nrow(A)), rhs = c(b,
        -b))
    return(constr)
}
   unit_cube_zoom <- function() coord_cartesian(xlim = c(0,1), ylim = c(0,1))

set_colnames <- function(df, vector){
	df_copy <- df
	colnames(df_copy) <- vector
	return(df_copy)
}

##' Ensure a folder exists, write if nonexistent
##' Derived from https://stackoverflow.com/a/29784923/2438134
##' @param mainDir main directory path character
##' @param subDir folder name character
##' @return it_had_to_be_created logical, lets you know if was just created.
ensure_folder_exists <- function(mainDir, subDir){
	ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
}

maps_within_threshold_by_dimension <- function(map0, map1, threshold) {
    if (abs(map1[1] - map0[1]) > threshold) {
        return(FALSE)
    } else {
        return(all(abs(map1 - map0) < threshold))
    }
}

split_by_time <- function(df){
	lapply(unique(df$time), function(time_t) df[df$time==time_t,])
}


evaluate_pair_feasibility <- function(pair_of_dfs, muscle_name_per_index, threshold){
    map0_map1_indices <- expand.grid(1:nrow(pair_of_dfs[[1]]), 1:nrow(pair_of_dfs[[2]]))
    colnames(map0_map1_indices) <- c("map0", "map1")
    feasible_values <- pbapply(map0_map1_indices, 1, 
    	function(index_tuple){
    		map0 <- pair_of_dfs[[1]][index_tuple[1],muscle_name_per_index]
    		map1 <- pair_of_dfs[[2]][index_tuple[2],muscle_name_per_index]
    		return(maps_within_threshold_by_dimension(map0,map1, threshold = threshold))
		})
    map0_map1_indices$feasible_transition <- feasible_values
    return(map0_map1_indices)
}

rm_solutions_with_infeasible_transitions <- function(list_of_dfs, muscle_name_per_index,
    threshold, mc.cores) {
    num_pairs <- length(list_of_dfs) - 1
    maps_per_polytope_df <- 1:nrow(list_of_dfs[[1]])
    transition_feasibility_df_per_pair <- pbmclapply(1:num_pairs, function(pair_index) {
    	t0 <- list_of_dfs[[pair_index]][1,"time"]
    	t1 <- list_of_dfs[[pair_index+1]][1,"time"]
    	print(paste0("Evaluating transitions for pair from time=(", t0,"---",t1,")"))
    	return(evaluate_pair_feasibility(list_of_dfs[pair_index:(pair_index+1)], muscle_name_per_index=muscle_name_per_index,threshold=threshold))
    }, mc.cores=mc.cores)
    # })
    unreachable_per_pair <- lapply(transition_feasibility_df_per_pair, function(feasibility_df){
    	map0_map1_feasibility_summaries <- get_map0_and_map1_summaries(feasibility_df)
    	map0_map1_unreachable_points <- lapply(map0_map1_feasibility_summaries, get_maps_with_no_links)
    })
    browser()
    return(transition_feasibility_df_per_pair)
}

##' Dataframe to list of cols
##' @description derived from https://stackoverflow.com/questions/3492379/data-frame-rows-to-a-list
##' @param df Data frame
##' @return df_list a list of elements, each of which is a representative col from the original df
df_to_list_of_cols <- function(df) {
  df_list <- setNames(split(df, seq(ncol(df))), colnames(df))
  return(df_list)
}
get_maps_with_no_links <- function(feasibility_summary_per_mapX) {
    names_vector <- colnames(feasibility_summary_per_mapX)
    if (names_vector[2] %in% c("num_reachable_map0_points", "num_reachable_map1_points")) {
        df <- data.frame(feasibility_summary_per_mapX[which(feasibility_summary_per_mapX[,
            2] == 0), 1])
        colnames(df) <- names_vector[1]
        return(df)
    } else {
        stop("the feasibility_summary_per_mapX must have map0 or map1 as the name of the second col")
    }
}
get_map0_and_map1_summaries <- function(df){
	map0_summary <- get_mapX_summary(df, "map0")
	map1_summary <- get_mapX_summary(df, "map1")
	return(list(map0_summary=map0_summary,map1_summary=map1_summary))
}

get_mapX_summary <- function(df, map_index_of_interest){
	if (map_index_of_interest=="map0"){
		num_reachable_colname <- "num_reachable_map1_points"
	} else{
		num_reachable_colname <- "num_reachable_map0_points"
	}
	df2 <- ddply(df, map_index_of_interest, summarize, num_reachable=sum(feasible_transition))
	colnames(df2) <- c(map_index_of_interest, num_reachable_colname)
	return(df2)
}

output_subfolder_path <- function(subfolder_name, filename, output_filepath="../../output"){
	tryCatch({
		ensure_folder_exists(output_filepath, subfolder_name)
		}, warning = function(w) {
		    print(w)
		}, error = function(e) {
		    stop("Could not ensure the folder exists",e)
		})
	output_local_filepath <- paste0(subfolder_name, "/", filename)
	return(output_filepath(output_local_filepath))
}

##' Output Filepath
##' by default takes the starting working directory from the tests/testthat directory.
##' @param out_path by default ../../output/
##' @param filename filename of interest
##' @return output_filepath stringpath
output_filepath <- function(filename, out_path="../../output") file.path(out_path, filename)

negative_cos <- function(...) -cos(...)
force_cos_ramp <- function(...) negative_cos(...)*0.5 + 0.5


generate_task_trajectory_and_har <- function(H_matrix, vector_out, n_task_values,
    cycles_per_second, cyclical_function, output_dimension_names, muscle_name_per_index,
    bounds_tuple_of_numeric, num_har_samples, har_thin, ...) {
    tasks <- task_time_df(fmax_task = vector_out, n_samples = n_task_values, cycles_per_second = cycles_per_second,
        cyclical_function = cyclical_function, output_dimension_names = output_dimension_names)
    list_of_constraints_per_task <- apply(tasks, 1, function(x) {
        constraint_H_with_bounds(H_matrix, x[output_dimension_names], bounds_tuple_of_numeric)
    })
    bigL <- list_of_constraints_per_task %>% pbmclapply(. %>% har_sample(num_har_samples,
        thin = har_thin), ...)
    bigL_labeled <- lapply(1:length(bigL), function(list_index) {
        cbind(tasks[list_index, ], bigL[[list_index]], row.names = NULL)
    })
    bigL_column_labeled <- lapply(bigL_labeled, set_colnames, c(colnames(tasks),
        muscle_name_per_index))
    har_per_task_df <- bigL_column_labeled %>% dcrb
    return(har_per_task_df)
}

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
	A_block <- constraint_H_rhs_b(cbind(A, -direction), rep(0, nrow(A)))
	bounds_raw <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric)
	bounds <- add_null_column_to_end_of_lhs(bounds_raw)
	constraint <- mergeConstraints(A_block, bounds)
	return(constraint)
}

merge_diagonal_constraints <- function(constr_A,constr_B){
	A_dimensions <- nrow(constr_A$constr)
	B_dimensions <- nrow(constr_B$constr)

}

har_sample <- function(constr, n_samples, thin) {
    state <- har.init(constr, thin = thin)
    samples <- har.run(state, n.samples = n_samples)$samples %>% as.data.frame
    return(samples)
}

pb_har_sample <- function(constr, n_samples, thin, shards=10){
	samples_per_core <- n_samples/shards
	samples <- pbmclapply(1:shards, function(i) {
		har_sample(constr, n_samples=samples_per_core, thin=thin)
}, mc.cores=6)
	message('dcrb\'ing')
	return(dcrb(samples))
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
	vector_out <- -1 * dir_of_interest * lp_result$objval
	vector_magnitude <- sqrt(sum(vector_out^2))
	output_structure <- list(lambda_scaler = -lp_result$objval, muscle_activations = lp_result$solution[1:n_muscles],
        vector_out = vector_out, vector_magnitude = vector_magnitude)
    return(output_structure)
}