
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

every_solution_is_ind_valid <- function(soln_df_list, constraints_list, tol=1e-4){

ress <- all(lapply(1:length(soln_df_list), function (i){
    return(
        all(evaluate_solutions(soln_df_list[[i]], constraints_list[[i]], tol=tol))
        )
    })%>%dcc)
return(ress)
}