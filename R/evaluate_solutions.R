
evaluate_solution <- function(solution_vector, constraint, tol=1e-14){
    if(ncol(constraint$constr) != length(solution_vector)){
        stop("constraint length must match solution vector length.")
    }
    b_hat <- constraint$constr %*% solution_vector
    b <- constraint$rhs
    # we check if it is smaller than 1e-14 because if we set it to 0, 
    # we expect cases where floating point precision does not match
    constraints_were_met <- (b_hat - b) <= tol
    rownames(b)[which(constraints_were_met==FALSE)]
    valid_for_all_tasks <- all(constraints_were_met)
    return(valid_for_all_tasks)
}

evaluate_solutions <- function(solution_per_row_df, constraint, ...){
    apply(solution_per_row_df,1, evaluate_solution, constraint, ...)
}

evaluate_constraint_solution_pairs <- function(constraint_solution_pairs, ...){
    lapply(constraint_solution_pairs, function(x){
        evaluate_solutions(x$har_solutions, x$constraint_object, ...)
    })
}