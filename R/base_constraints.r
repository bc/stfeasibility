create_equality_constraint <- function(A, b) {
    stop_if_dimensionality_names_are_missing(A)
    constr <- list(constr = rbind(A, -A), dir = rep("<=", 2 * nrow(A)), rhs = c(b,
        -b))
    rownames(constr$constr) <- c(rownames(A), negative_string(rownames(A)))
    return(constr)
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

##' @param A equality constraints—must have named columns and rows for input and output dimensions
##' @param direction the direction to generate constraints on. Must have an attr(direction, "output_dimension_names") with a string name for each dimension. same len as direction
a_matrix_lhs_direction <- function(H_matrix, direction, bounds_tuple_of_numeric) {
    wrench_and_H_dims_match(H_matrix, direction)
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

a_matrix_rhs_task <- function(H_matrix, task_wrench, bounds_tuple_of_numeric){
    wrench_and_H_dims_match(H_matrix,task_wrench)
    stop_if_dimensionality_names_are_missing(H_matrix)
    muscle_names <- colnames(H_matrix)
    bounds_raw <- bound_constraints_for_all_muscles(bounds_tuple_of_numeric, muscle_names)
    H_equality <- create_equality_constraint(H_matrix, task_wrench)
    constraint <- merge_constraints(H_equality, bounds_raw)
    return(constraint)
}

##' Negate a Constraint
##' useful for creating equality constraints. then stack this result with the original to make an equality constr.
##' @param constraint_object constraint object as in hitandrun
##' @param neg_constr same constraints, but all negated. The direction remains the same, as less than.
negate_constraint <- function(constraint_object){
    list(constr = -constraint_object$constr, dir = constraint_object$dir, rhs=-constraint_object$rhs)
}

write_constraint_to_csv <- function(constraint_object, output_filepath){
    write.csv(cbind(constraint_object$constr, dir=constraint_object$dir,rhs=constraint_object$rhs), output_filepath)
}

#bounds 
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


split_constraints <- function(constraint, n_shards){
    n_constraints <- nrow(constraint$constr)
    idxs <- 1:n_constraints
    indices_per_shard <- split(idxs, cut_number(idxs, n_shards)) %>% unname
    miniconstraints <- lapply(indices_per_shard, function(indices_for_shard){
        res <- list(constr=constraint$constr[indices_for_shard,],
            dir=constraint$dir[indices_for_shard],
            rhs=constraint$rhs[indices_for_shard])
        return(res)
    })
    return(miniconstraints)
}
