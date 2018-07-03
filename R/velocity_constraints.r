##' @param eliminate logical, `TRUE` by default.
force_cos_ramp_constraint <- function(H_matrix, bounds_tuple_of_numeric, vector_out, max_allowable_increasing_tension_speed,
    max_allowable_decreasing_tension_speed, n_task_values=100, cycles_per_second=60, cyclical_function=force_cos_ramp, eliminate=TRUE){
        if (n_task_values <2) stop("must be at least 3 task values for spatiotemporal constraints")
        near_maximal_task <- vector_out*(1-1e-05)
        tasks_and_constraints <- generate_tasks_and_corresponding_constraints(H_matrix=H_matrix, vector_out = near_maximal_task, n_task_values = n_task_values, cycles_per_second = cycles_per_second, cyclical_function = cyclical_function,
        bounds_tuple_of_numeric=bounds_tuple_of_numeric)
        num_muscles <- ncol(H_matrix)
        inequality_constraints <- diagonal_merge_constraint_list(tasks_and_constraints$constraint)
        st_constr <- generate_and_add_velocity_constraint(inequality_constraints, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed, num_muscles)
        if(eliminate){
            print('eliminate_redundant')
            #overwrite original constraint with nonredundant constraint
            st_constr <- st_constr %>% eliminate_redundant
        }
        res <- list(nonredundant_constr=st_constr, tasks_and_constraints=tasks_and_constraints)
        return(res)
        }

mbm_median <- function(mbm_object) summary(mbm)$median
generate_and_add_velocity_constraint <- function(constraint_object, max_allowable_increasing_tension_speed,
    max_allowable_decreasing_tension_speed, num_muscles) {
    indices_for_muscles <- muscle_and_lambda_indices(constraint_object, num_muscles)$indices_for_muscles
    velocity_constraint <- generate_full_velocity_constraint(constraint_object, max_allowable_increasing_tension_speed,
        max_allowable_decreasing_tension_speed, num_muscles)
    constraint_with_velocity_requirements <- merge_constraints(constraint_object,
        velocity_constraint)
    expect_equal(sum(nrow(constraint_object$constr), nrow(velocity_constraint$constr)),
        nrow(constraint_with_velocity_requirements$constr))
    return(constraint_with_velocity_requirements)
}

transition_inequalities_for_velocity <- function(muscle_names) {
    num_muscles <- length(muscle_names)/2
    time_point_1 <- rbind(-diag(num_muscles), diag(num_muscles))
    dimnames(time_point_1) <- NULL
    df <- cbind(time_point_1, -time_point_1) %>% set_colnames(muscle_names)
    colnames(df) <- muscle_names
    rownames_pos <- transition_names(muscle_names) %>% paste0("_positive_velocitylimit")
    rownames_neg <- transition_names(muscle_names) %>% paste0("_negative_velocitylimit")
    rownames(df) <- c(rownames_pos, rownames_neg)
    return(df)
}


zeros <- function(rows, cols) matrix(0, nrow = rows, ncol = cols)

##' Transition Names for velocity/acceleration constraints
##' x_to_xprime. note that the first and second half of the input colnames should represent the same muscle, but after deltaT. 
##' @param colnames_for_transition list of strings, each a muscle.
##' @return transition_names vector of strings like "LUM_0_to_LUM1" "EIP_0_to_EIP1", where the input was c("LUM_0", "EIP_0", "LUM_1", "EIP_1")
transition_names <- function(colnames_for_transition){
    num_muscles <- length(colnames_for_transition)/2
    rowname_part1 <- colnames_for_transition[1:num_muscles]
    rowname_part2 <- colnames_for_transition[(1+num_muscles):(num_muscles*2)]
    apply(cbind(rowname_part1, rowname_part2), 1, paste, collapse = "_to_")
}

compose_velocity_constraint_per_transition <- function(transition_blocks,
    num_muscles) {
    velocity_constraint <- lapply(1:length(transition_blocks), function(transition_index) {
        left_pad_blocks <- transition_index - 1
        right_pad_blocks <- -transition_index + length(transition_blocks)
        L <- zeros(num_muscles * 2, num_muscles * left_pad_blocks) %>% as.data.frame
        R <- zeros(num_muscles * 2, num_muscles * right_pad_blocks) %>% as.data.frame
        constraint_row <- cbind(L, transition_blocks[[transition_index]], R)
        return(constraint_row)
    })
    return(velocity_constraint)
}

combine_velocity_constraints <- function(velocity_constraint_list) {
    velocity_constraint_constr <- dcrb(lapply(velocity_constraint_list, function(x) {
        colnames(x) <- NULL
        return(x %>% as.matrix)
    }))
    return(velocity_constraint_constr)
}

set_dimnames <- function(input_df_or_mat, colnames_to_use, rownames_to_use) {
    # Make a copy
    df_or_mat <- input_df_or_mat
    colnames(df_or_mat) <- colnames_to_use
    rownames(df_or_mat) <- rownames_to_use
    return(df_or_mat)
}


generate_full_velocity_constraint <- function(constraint_object, max_allowable_increasing_tension_speed,
    max_allowable_decreasing_tension_speed, num_muscles) {
    num_tasks <- ncol(constraint_object$constr)/num_muscles
    num_pairs <- num_tasks - 1
    velocity_constraint <- generate_velocity_constraint_matrix(constraint_object, num_muscles)
    expect_equal(ncol(velocity_constraint), num_tasks * (num_muscles))
    expect_equal(nrow(velocity_constraint), num_pairs * num_muscles * 2)
    max_inc_speed <- rep(max_allowable_increasing_tension_speed, num_muscles)
    max_dec_speed <- rep(max_allowable_decreasing_tension_speed, num_muscles)
    rhs_velocity <- rep(c(max_inc_speed, max_dec_speed), num_pairs)
    dir_velocity <- all_less_than(velocity_constraint)
    constraint_result <- list(constr = velocity_constraint, dir = dir_velocity, rhs = rhs_velocity)
    plot_constraint_matrix(constraint_result) + ggtitle("Velocity constraint")
    return(constraint_result)
}

generate_velocity_constraint_matrix <- function(constraint_object, num_muscles) {
    num_tasks <- ncol(constraint_object$constr)/num_muscles
    all_colnames <- colnames(constraint_object$constr)
    num_pairs <- num_tasks-1
    muscle_names_per_transition <- lapply(1:num_pairs, function(pair_i){
        pair_start <- (pair_i-1)*num_muscles + 1
        pair_end <- pair_start + num_muscles*2 - 1
        
        tryCatch({
            names_of_interest <- all_colnames[pair_start:pair_end]
    }, warning = function(w) {
        browser()
    }, error = function(e) {
        browser()
    })


        return(names_of_interest)
    })
    transition_blocks <- lapply(muscle_names_per_transition, transition_inequalities_for_velocity)
    velocity_constraint <- compose_velocity_constraint_per_transition(transition_blocks,num_muscles) %>% combine_velocity_constraints
    expect_equal(num_pairs * num_muscles * 2, nrow(velocity_constraint))
    velocity_constraint <- velocity_constraint %>% set_colnames(colnames(constraint_object$constr))
    return(velocity_constraint)
}


