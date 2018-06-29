##' @param min_or_max string, either 'max' or 'min'
##' @param taskRHSconstraint constraint object as in hitandrun where task is on the RHS. No lamdbas.
##' @param num_muscles integer
lpsolve_muscles_for_task <- function(min_or_max, taskRHSconstraint, num_muscles) {
    c_in_cTx <- rep(1, ncol(taskRHSconstraint$constr))
    lp_result <- lp(min_or_max, objective.in = c_in_cTx, const.mat = taskRHSconstraint$constr,
        const.dir = taskRHSconstraint$dir, const.rhs = taskRHSconstraint$rhs, compute.sens = 0)
    return(lp_result)
}