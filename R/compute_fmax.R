##' to get the opposite direction, change the sign of the direction.
lpsolve_force_in_dir <- function(min_or_max, direction_constraint, indices_for_muscles,
    output_wrench_dimensionality = 4) {
    # show interest in maximizing the lambda scaler parameter only.
    c_in_cTx <- indices_to_control(direction_constraint, indices_for_muscles)

    lp_result <- lp(min_or_max, objective.in = c_in_cTx, const.mat = direction_constraint$constr,
        const.dir = direction_constraint$dir, const.rhs = direction_constraint$rhs,
        compute.sens = 0)

    # TODO make a function to format the output and return the list
    lambda_constraint_columns <- direction_constraint$constr[, which(c_in_cTx ==
        1)] %>% as.data.frame

    # TODO rm hardcoding to len-22 lambda indices
    lambda_start_indices <- head(which(0:nrow(lambda_constraint_columns)%%22 == 0),
        sum(c_in_cTx))
    lambda_indices <- sapply(lambda_start_indices, function(x) return(x:(x + (output_wrench_dimensionality -
        1))))
    lambda_values <- lp_result$solution[c_in_cTx == 1]
    num_tasks <- sum(c_in_cTx)
    task_directions_of_interest <- lapply(1:ncol(lambda_indices), function(i) {
        # multiplied by -1 because we need to move the output direction back to the rhs
        -lambda_constraint_columns[lambda_indices[, i], i]
    })
    output_vector_per_task <- compose_output_vector_per_task(num_tasks, task_directions_of_interest,
        lambda_values)
    vector_magnitude_per_task <- lapply(output_vector_per_task, function(vector_out) sqrt(sum(vector_out^2)))
    num_muscles <- get_num_muscles_via_indices_for_muscles(indices_for_muscles)
    muscle_activation_pattern_per_task <- compose_muscle_activation_per_task(lp_result,
        num_muscles)
    raw_x_concatenated <- compose_raw_x_concatenated(muscle_activation_pattern_per_task,
        lambda_values)
    output_structure <- list(total_cost = lp_result$objval, direction_per_task = task_directions_of_interest,
        muscle_activation_pattern_per_task = muscle_activation_pattern_per_task,
        lambda_values = lambda_values, output_vector_per_task = output_vector_per_task,
        vector_magnitude_per_task = vector_magnitude_per_task, raw_x_concatenated = raw_x_concatenated)
    return(output_structure)
}
compose_muscle_activation_per_task <- function(lp_result, num_muscles) {
    muscle_activation_pattern_per_task <- t(matrix(lp_result$solution, nrow = num_muscles +
        1)[1:(num_muscles), ]) %>% df_to_list_of_rows
}
compose_output_vector_per_task <- function(num_tasks, task_directions_of_interest,
    lambda_values) {
    output_vector_per_task <- lapply(1:num_tasks, function(i) {
        task_directions_of_interest[[i]] * lambda_values[i]
    })
}
compose_raw_x_concatenated <- function(muscle_activation_pattern_per_task, lambda_values) {
    num_tasks <- length(muscle_activation_pattern_per_task)
    if (num_tasks != length(lambda_values)) {
        stop("length(muscle_activation_pattern_per_task must be the same as length(lambda_values))")
    }
    x <- lapply(1:num_tasks, function(task_index) {
        c(muscle_activation_pattern_per_task[[task_index]], lambda_values[[task_index]])
    }) %>% dcc
    return(x)
}