##' Merge Constraints and respect rhs_dimnames and constr_dimnames
##' wraps hitandrun::mergeConstraints to preserve component dimnames
##' @param a,b a constraint object as in hitandrun
##' @return ab merged constraint object
merge_constraints <- function(a, b) {
    constr <- mergeConstraints(a, b)
    constr$rhs_dimnames <- c(a$dimnames, b$dimnames)
    constr$constr_dimnames <- a$constr_dimnames
    return(constr)
}

##' Merge Constraints Recursively
##' wraps merge_constraints and thereby hitandrun::mergeConstraints
##' @param a,b a constraint object as in hitandrun
##' @return ab merged constraint object
merge_constraints_list <- function(constraints_list) {
    # Reduce(merge_constraints, constraints_list)
    len <- length(constraints_list)
    if (len == 1){
        return(constraints_list[[1]])
    } else if (len == 2){
        return(merge_constraints(constraints_list[[1]], constraints_list[[2]]))
    } else {
        return(merge_constraints(constraints_list[[1]], merge_constraints_list(constraints_list[2:len])))
    }
}

##' create vector of inequalities ('<=' to match shape of constraint
##' useful when creating an inequality from scratch.
##' @param constr constraint matrix as in hitandrun
##' @return dir vector of '<=' of len == nrow(constr)
all_less_than <- function(constr) rep("<=", nrow(constr))

##' Print constraint to terminal
##' Useful for inspection. see plot_constraint_matrix for a visual representation
##' @param constr constraint matrix as in hitandrun
print_constraint <- function(constraint_object) {
    print(paste(nrow(constraint_object$constr), "rows (constraints)", ncol(constraint_object$constr),
        "cols (variables)"))
    cbind(constraint_object$constr, dir = constraint_object$dir, rhs = constraint_object$rhs) %>%
        as.data.frame %>% print
}

##' Diagonally merge two constraints
##' combines two constraints, and renames the colnames of the second constraint
##' to make sure they do not stack atop one another.
##' @param first_constraint,second_constraint constraint objects with $constr, $dir, $rhs
##' @param string_to_append_to_second_constraint string to append. i.e. a number. it will be appended after an underscore.
##' @return constr combined constraint going down the diagonal.
diagonal_merge_constraints <- function(first_constraint, second_constraint, string_to_append_to_second_constraint) {
    first_constraint_copy <- first_constraint
    second_constraint_copy <- second_constraint
    padding_for_constraint1 <- zeros_df(nrow(first_constraint$constr), ncol(second_constraint$constr))
    appended_second_constraint_colnames <- paste(colnames(second_constraint$constr),
        string_to_append_to_second_constraint, sep = "_")
    dimnames(padding_for_constraint1) <- list(rownames(first_constraint$constr),
        appended_second_constraint_colnames)
    first_constraint_copy$constr <- cbind(first_constraint$constr, padding_for_constraint1)
    padding_for_constraint2 <- zeros_df(nrow(second_constraint$constr), ncol(first_constraint$constr))
    dimnames(padding_for_constraint2) <- list(rownames(second_constraint$constr),
        colnames(first_constraint$constr))
    lapply(list(rownames(second_constraint$constr), colnames(first_constraint$constr)),
        length)
    second_constraint_copy$constr <- cbind(padding_for_constraint2, second_constraint$constr)
    rownames(second_constraint_copy$constr) <- paste(rownames(second_constraint$constr),
        string_to_append_to_second_constraint, sep = "_")
    merged_constraint <- merge_constraints(first_constraint_copy, second_constraint_copy)
    merged_constraint$constr <- merged_constraint$constr %>% as.matrix
    return(merged_constraint)
}

##' Diagonally merge constraint list
##' Accepts inputs of length 1, 2, to n. Respects colnames and rownames
##' Adds iteravely to the diagonal. Performance fo current implementation is acceptable.
##' @param list_of_constraints each a constraint, see `?hitandrun`
##' @return constr combined constraint going down the diagonal.
diagonal_merge_constraint_list <- function(list_of_constraints) {
    list_len <- length(list_of_constraints)
    if (list_len == 1) {
        return(list_of_constraints[[1]])
    } else if (list_len == 2) {
        return(diagonal_merge_constraints(list_of_constraints[[1]], list_of_constraints[[2]],
            1))
    }
    browser()
    var_result <- diagonal_merge_constraints(list_of_constraints[[1]], list_of_constraints[[2]],
        1)
    for (pair_i in seq(2, list_len - 1)) {
        var_result <- diagonal_merge_constraints(var_result, list_of_constraints[[pair_i +
            1]], pair_i)
    }
    var_result$constr <- var_result$constr %>% as.matrix
    var_result$dir <- var_result$dir %>% as.vector
    var_result$rhs <- var_result$rhs %>% as.vector
    return(var_result)
}

##' remove labels
##' useful for ggplot objects you want to clean up
##' @return p ggplot object function that can be added to a ggplot object.
remove_labels <- function() {
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
}

##' Rotate Matrix
##' Useful when you want to visualize a matrix with image().
##' via https://www.r-bloggers.com/creating-an-image-of-a-matrix-in-r-using-image/
##' @param x matrix
##' @return x_prime rotated matrix
rotate <- function(x) t(apply(x, 2, rev))


##' Plot constraint matrix as colorful xy plot
##' useful for visualizing constraint matrices before and after changes
##' @param constraint constraint object, with constr, dir, and rhs elements.
##' return p ggplot object
plot_constraint_matrix <- function(constraint) {
    if (is.null(rownames(constraint$constr))) {
        message("Plotting redundant-free constraint without constraint names")
        rownames(constraint$constr) <- paste("c", 1:nrow(constraint$constr), sep = "_")
    }
    mat <- cbind(constraint$constr, constraint$rhs) %>% melt %>% as.data.frame
    mat$Var1 <- factor(mat$Var1, levels = rev(levels(mat$Var1)))
    p <- ggplot(mat, aes(x = Var2, y = Var1)) + geom_raster(aes(fill = value)) +
        scale_fill_gradientn(colors = brewer.pal(11, "PRGn")) + labs(x = "input variable per timestep",
        y = "constraint", title = "Matrix") + theme_classic() + theme(axis.text.x = element_text(size = 9,
        angle = 0, vjust = 0.3), axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))
    return(p + remove_labels())
}

##' compose velocity constraint
##' require a given muscle's activation to change no more than some delta between two timepoints.
##' @param constraint constraint object as in ?hitandrun
##' @param max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed value between min-max of muscle activation capability that defines maximal muscle activation change.
##' @return  velocity_constraint constraint object
compose_velocity_constraint <- function(constraint, max_allowable_increasing_tension_speed,
    max_allowable_decreasing_tension_speed) {
    indices_for_muscles <- muscle_and_lambda_indices(constraint, 7)$indices_for_muscles
    num_muscles <- get_num_muscles_via_indices_for_muscles(indices_for_muscles)
    velocity_constraint <- generate_and_add_velocity_constraint(constraint, max_allowable_increasing_tension_speed,
        max_allowable_decreasing_tension_speed, num_muscles)
    return(velocity_constraint)
}



########### DECOMPOSITION


##' useful for split_lhs_har_df_by_constraint
stop_if_tasks_not_wholenumber <- function(num_tasks) {
    if (num_tasks != floor(num_tasks)) {
        stop("The number of muscles does not match up with the number of tasks in the multiconstraint")
    }
}

##' derived from https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r
matrix_to_list_of_cols <- function(x) split(x, rep(1:ncol(x), each = nrow(x)))

split_lhs_har_df_by_constraint <- function(har_df, multiconstraint, num_muscles) {
    # if left hand side then we can extract task num
    num_tasks <- ncol(multiconstraint$constr)/num_muscles
    stop_if_tasks_not_wholenumber(num_tasks)
    constraint_index_matrix <- matrix(1:(num_tasks * num_muscles), nrow = num_muscles)
    hardf_list <- lapply(constraint_index_matrix %>% matrix_to_list_of_cols, function(muscle_indices) {
        print(muscle_indices)
        solutions_subset <- har_df[, muscle_indices]
        return(solutions_subset)
    })
    return(hardf_list)
}


pair_har_solutions_df_with_constraint <- function(list_of_har_solution_dfs, list_of_constraints) {
    constraint_solution_pairs <- lapply(1:length(list_of_constraints), function(i) {
        return(list(constraint_object = list_of_constraints[[i]], har_solutions = list_of_har_solution_dfs[[i]]))
    })
    return(constraint_solution_pairs)
}


##' @param list_of_constraints the list of constraints, where the task was set (independent A matrix equality) but no velocity constraints were added.
##' @param trajectory_constraint constraint, which *must* be result of eliminate_redundant.
har_and_split_trajectory_constraint <- function(trajectory_constraint, list_of_constraints,
    num_muscles, har_samples, ...) {
    muscle_solutions <- trajectory_constraint %>% eliminate_redundant %>% pb_har_sample(har_samples, mc.cores=mc.cores, eliminate=FALSE)
    har_df_list <- split_lhs_har_df_by_constraint(muscle_solutions, trajectory_constraint, num_muscles)
    constraint_solution_pairs <- pair_har_solutions_df_with_constraint(har_df_list,
        list_of_constraints)
    expect_true(evaluate_constraint_solution_pairs(constraint_solution_pairs, ...) %>%
        dcc %>% all)
    return(constraint_solution_pairs)
}