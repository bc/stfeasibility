
maps_within_threshold_by_dimension <- function(map0, map1, threshold) {
    if (abs(map1[1] - map0[1]) > threshold) {
        return(FALSE)
    } else {
        return(all(abs(map1 - map0) < threshold))
    }
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

