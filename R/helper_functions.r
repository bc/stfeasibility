##
create_equality_constraint <- function(A, b) {
    stop_if_dimensionality_names_are_missing(A)
    constr <- list(constr = rbind(A, -A), dir = rep("<=", 2 * nrow(A)), rhs = c(b,
        -b))
    rownames(constr$constr) <- c(rownames(A), negative_string(rownames(A)))
    return(constr)
}
unit_cube_zoom <- function() coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

set_colnames <- function(df, vector) {
    df_copy <- df
    colnames(df_copy) <- vector
    return(df_copy)
}

##' Ensure a folder exists, write if nonexistent
##' Derived from https://stackoverflow.com/a/29784923/2438134
##' @param mainDir main directory path character
##' @param subDir folder name character
##' @return it_had_to_be_created logical, lets you know if was just created.
ensure_folder_exists <- function(mainDir, subDir) {
    ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir,
        subDir)), FALSE)
}

maps_within_threshold_by_dimension <- function(map0, map1, threshold) {
    if (abs(map1[1] - map0[1]) > threshold) {
        return(FALSE)
    } else {
        return(all(abs(map1 - map0) < threshold))
    }
}

split_by_time <- function(df) {
    lapply(unique(df$time), function(time_t) df[df$time == time_t, ])
}


evaluate_pair_feasibility <- function(pair_of_dfs, muscle_name_per_index, threshold) {
    map0_map1_indices <- expand.grid(1:nrow(pair_of_dfs[[1]]), 1:nrow(pair_of_dfs[[2]]))
    colnames(map0_map1_indices) <- c("map0", "map1")
    feasible_values <- apply(map0_map1_indices, 1, function(index_tuple) {
        map0 <- pair_of_dfs[[1]][index_tuple[1], muscle_name_per_index]
        map1 <- pair_of_dfs[[2]][index_tuple[2], muscle_name_per_index]
        return(maps_within_threshold_by_dimension(map0, map1, threshold = threshold))
    })
    map0_map1_indices$feasible_transition <- feasible_values
    return(map0_map1_indices)
}

##' @return transition_feasibility_df_per_pair a list of df, each a 'data.frame': N obs. of  3 variables:
##' $ map0               : int
##' $ map1               : int
##' $ feasible_transition: logi
##' where N is the number of hit and run samples. length of the list is equal to length(list_of_dfs)-1.
get_transition_feasibility_per_pair <- function(list_of_dfs, muscle_name_per_index,
    threshold, ...) {
    num_pairs <- length(list_of_dfs) - 1
    maps_per_polytope_df <- 1:nrow(list_of_dfs[[1]])
    transition_feasibility_df_per_pair <- pbmclapply(1:num_pairs, function(pair_index) {
        t0 <- list_of_dfs[[pair_index]][1, "time"]
        t1 <- list_of_dfs[[pair_index + 1]][1, "time"]
        print(paste0("Evaluating transitions for pair from time=(", t0, "---", t1,
            ")"))
        return(evaluate_pair_feasibility(list_of_dfs[pair_index:(pair_index + 1)],
            muscle_name_per_index = muscle_name_per_index, threshold = threshold))
    }, ...)
}



pass_df_if_index_vector_is_empty_else_rm_rows <- function(df, vector_of_row_indices_to_rm) {
    if (length(vector_of_row_indices_to_rm) == 0) {
        return(df)
    } else {
        return(df[-vector_of_row_indices_to_rm, ])
    }
}


rm_solutions_with_infeasible_transitions <- function(list_of_dfs, muscle_name_per_index,
    threshold, ...) {

    transition_feasibility_df_per_pair <- get_transition_feasibility_per_pair(list_of_dfs = list_of_dfs,
        muscle_name_per_index = muscle_name_per_index, threshold = threshold, ...)
    unreachable_points_per_pair <- lapply(transition_feasibility_df_per_pair, function(feasibility_df) {
        map0_map1_feasibility_summaries <- get_map0_and_map1_summaries(feasibility_df)
        map0_map1_unreachable_points <- lapply(map0_map1_feasibility_summaries, get_maps_with_no_links)
    })
    time_per_polytope <- sapply(list_of_dfs, function(x) x$time[1])
    unreasonable_points_by_time <- merge_unreasonable_points_by_time(unreachable_points_per_pair = unreachable_points_per_pair,
        time_value_for_each_polytope = time_per_polytope)
    list_of_dfs_with_no_unreasonable_points <- lapply(1:length(list_of_dfs), function(time_index) {
        df_of_interest <- list_of_dfs[[time_index]]
        to_destroy <- unreasonable_points_by_time[[time_index]]$to_rm
        df_result <- df_of_interest %>% pass_df_if_index_vector_is_empty_else_rm_rows(to_destroy)
        return(df_result)
    })
    return(list_of_dfs_with_no_unreasonable_points)
}

merge_unreasonable_points_by_time <- function(unreachable_points_per_pair, time_value_for_each_polytope) {
    time_and_unreasonable_points <- lapply(1:length(time_value_for_each_polytope),
        function(time_index) {
            # sole points if first or last, else, merge the two.
            if (time_index == 1) {
                res <- list(time_value = time_value_for_each_polytope[time_index],
                  to_rm = as.numeric(unlist(unreachable_points_per_pair[[1]]$map0_unreasonable_points)))
                return(res)
            } else if (time_index == length(time_value_for_each_polytope)) {
                res <- list(time_value = time_value_for_each_polytope[time_index],
                  to_rm = as.numeric(unlist(unreachable_points_per_pair[[length(unreachable_points_per_pair)]]$map1_unreasonable_points)))
                return(res)
            } else {
                forward_direction <- unreachable_points_per_pair[[time_index - 1]]$map1
                backward_direction <- unreachable_points_per_pair[[time_index]]$map0
                res <- list(time_value = time_value_for_each_polytope[time_index],
                  to_rm = as.numeric(unlist(c(forward_direction, backward_direction))))
                return(res)
            }
        })
    return(time_and_unreasonable_points)
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
get_map0_and_map1_summaries <- function(df) {
    map0_summary <- get_unreasonable_points(df, "map0")
    map1_summary <- get_unreasonable_points(df, "map1")
    return(list(map0_unreasonable_points = map0_summary, map1_unreasonable_points = map1_summary))
}

get_unreasonable_points <- function(df, map_index_of_interest) {
    if (map_index_of_interest == "map0") {
        num_reachable_colname <- "num_reachable_map1_points"
    } else {
        num_reachable_colname <- "num_reachable_map0_points"
    }
    df2 <- ddply(df, map_index_of_interest, summarize, num_reachable = sum(feasible_transition))
    colnames(df2) <- c(map_index_of_interest, num_reachable_colname)
    return(df2)
}

output_subfolder_path <- function(subfolder_name, filename, output_filepath = "../../output") {
    tryCatch({
        ensure_folder_exists(output_filepath, subfolder_name)
    }, warning = function(w) {
        print(w)
    }, error = function(e) {
        stop("Could not ensure the folder exists", e)
    })
    output_local_filepath <- paste0(subfolder_name, "/", filename)
    return(output_filepath(output_local_filepath))
}

##' Output Filepath
##' by default takes the starting working directory from the tests/testthat directory.
##' @param out_path by default ../../output/
##' @param filename filename of interest
##' @return output_filepath stringpath
output_filepath <- function(filename, out_path = "../../output") file.path(out_path,
    filename)

negative_cos <- function(...) -cos(...)
force_cos_ramp <- function(...) negative_cos(...) * 0.5 + 0.5


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

ggparcoord_har <- function(df) {
    ggparcoord(df, scale = "globalminmax", alpha = 0.01, ...) + theme_classic()
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


append_with_underscore <- function(s,appendix) paste0(s,"_",appendix)

where_muscles_have_unreasonable_values <- function(df, muscle_names) {
    apply(df[, muscle_names], 2, function(muscle_entries) {
        negative <- muscle_entries < 0
        over_one <- muscle_entries > 1
        return(negative | over_one)
    })
}

diagonal_merge_constraints <- function(first_constraint, second_constraint, string_to_append_to_second_constraint){
    first_constraint_copy <- first_constraint
    second_constraint_copy <- second_constraint

    padding_for_constraint1 <- zeros_df(nrow(first_constraint$constr), ncol(second_constraint$constr))
    appended_second_constraint_colnames <- paste(colnames(second_constraint$constr),string_to_append_to_second_constraint, sep="_")
    dimnames(padding_for_constraint1) <- list(rownames(first_constraint$constr),appended_second_constraint_colnames)
    first_constraint_copy$constr <- cbind(first_constraint$constr, padding_for_constraint1)

    padding_for_constraint2 <- zeros_df(nrow(second_constraint$constr), ncol(second_constraint$constr))
    dimnames(padding_for_constraint2) <- list(rownames(second_constraint$constr),colnames(first_constraint$constr))
    second_constraint_copy$constr <- cbind(padding_for_constraint2, second_constraint$constr)
    rownames(second_constraint_copy$constr) <- paste(rownames(second_constraint$constr),string_to_append_to_second_constraint, sep="_")
    merged_constraint <- merge_constraints(first_constraint_copy, second_constraint_copy)
    return(merged_constraint)
}



constraint_H_with_bounds <- function(A, b, bounds_tuple_of_numeric) {
    H_constraint <- create_equality_constraint(A, b)
    bounds <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric, muscle_names=colnames(A))
    return(merge_constraints(H_constraint, bounds))
}

constraint_H_lhs_direction <- function(A, direction, bounds_tuple_of_numeric) {
    lhs <- create_equality_constraint(A, rep(0, nrow(A)))

}

add_null_column_to_end_of_lhs <- function(constraint, column_name) {
    c_copy <- constraint
    output_dimensionality <- nrow(constraint$constr)
    c_copy$constr <- cbind(constraint$constr, rep(0, output_dimensionality))
    colnames(c_copy$constr)[ncol(c_copy$constr)] <- column_name
    return(c_copy)
}

stop_if_dimensionality_names_are_missing <- function(df){
    if (is.null(rownames(df)) | is.null(colnames(df)))
    {
        stop("A matrix passed to a_matrix_lhs_direction must have colnames and rownames for the dimensions of input and output")
    }
}


zeros_df <- function(nrow,ncol) data.frame(matrix(0, ncol = ncol, nrow = nrow))

##' @param A equality constraints—must have named columns and rows for input and output dimensions
##' @param direction the direction to generate constraints on. Must have an attr(direction, "output_dimension_names") with a string name for each dimension. same len as direction
a_matrix_lhs_direction <- function(H_matrix, direction, bounds_tuple_of_numeric) {
    task_lambda_colname <- "task_lambda"
    stop_if_dimensionality_names_are_missing(H_matrix)
    muscle_names <- colnames(H_matrix)
    output_dimension_names <- rownames(H_matrix)
    A_block <- create_equality_constraint(cbind(H_matrix, -direction), rep(0, nrow(H_matrix)))
    colnames(A_block$constr)[ncol(A_block$constr)] <- task_lambda_colname
    bounds_raw <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric, muscle_names)
    bounds <- add_null_column_to_end_of_lhs(bounds_raw, column_name=task_lambda_colname)
    constraint <- merge_constraints(A_block, bounds)
    return(constraint)
}
##' @see a_matrix_lhs_direction
negative_string <- function(s) paste0("-",s)

##' wrapped merge_constraints that respsects the rhs_dimnames and constr_dimnames
merge_constraints <- function(a,b){
    constr <- mergeConstraints(a,b)
    constr$rhs_dimnames <- c(a$dimnames, b$dimnames)
    constr$constr_dimnames <- a$constr_dimnames
    return(constr)
}

har_sample <- function(constr, n_samples, thin) {
    state <- har.init(constr, thin = thin)
    samples <- har.run(state, n.samples = n_samples)$samples %>% as.data.frame
    return(samples)
}

pb_har_sample <- function(constr, n_samples, thin, shards = 10) {
    samples_per_core <- n_samples/shards
    samples <- pbmclapply(1:shards, function(i) {
        har_sample(constr, n_samples = samples_per_core, thin = thin)
    }, mc.cores = 6)
    message("dcrb'ing")
    return(dcrb(samples))
}

lb_ub_strings <- function(name) paste0(c("lb_", "ub_"), name)

bound_constraints_for_all_muscles <- function(bounds_tuple_of_numeric, muscle_names) {
    n_muscles <- length(bounds_tuple_of_numeric)
    res <- lapply(1:n_muscles, function(muscle_index) {
        lb <- lowerBoundConstraint(n_muscles, muscle_index, bounds_tuple_of_numeric[[muscle_index]]$lower)
        ub <- upperBoundConstraint(n_muscles, muscle_index, bounds_tuple_of_numeric[[muscle_index]]$upper)
        return(merge_constraints(lb, ub))
    }) %>% mergeConstraints
    rownames(res$constr) <- dcclapply(muscle_names, lb_ub_strings)
    colnames(res$constr) <- muscle_names
    return(res)
}

##' do call concatenate upon all list elements returned by lapply
dcclapply <- function(...){
    dcc(lapply(...) )
}

# to get the opposite direction, change the sign of the direction.
lpsolve_force_in_dir <- function(min_or_max, direction_constraint, n_muscles) {
    # show interest in maximizing the lambda scaler parameter only.
    c_in_cTx <- c(rep(0, n_muscles), 1)
    lp_result <- lp(min_or_max, objective.in = c_in_cTx, const.mat = direction_constraint$constr,
        const.dir = direction_constraint$dir, const.rhs = direction_constraint$rhs,
        compute.sens = 0)
    dir_of_interest <- direction_constraint$constr[1:4, ncol(direction_constraint$constr)]
    vector_out <- -1 * dir_of_interest * lp_result$objval
    vector_magnitude <- sqrt(sum(vector_out^2))
    output_structure <- list(lambda_scaler = -lp_result$objval, muscle_activations = lp_result$solution[1:n_muscles],
        vector_out = vector_out, vector_magnitude = vector_magnitude)
    return(output_structure)
}
