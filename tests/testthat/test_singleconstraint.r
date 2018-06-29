context('testing singleconstraint')
test_that("H matrix with defined task on rhs has 0 sd for lambda",{
	lhs <- constraint_H_with_bounds(H_matrix_mini, c(1), bounds_tuple_of_numeric_mini) 
	muscle_solutions <- lhs %>% eliminate_redundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE) %>% as.matrix 
	wrench_outputs <- H_matrix_mini %*% t(muscle_solutions) %>% as.numeric
	expect_equal(wrench_outputs, rep(1, nrow(muscle_solutions)))
	expect_equal(sd(wrench_outputs), 0)
})

test_that("H matrix mini can be combined and har'd successfully",{
	lhszero <- constraint_H_with_bounds(H_matrix_mini, c(0), bounds_tuple_of_numeric_mini) 
	lhshalf <- constraint_H_with_bounds(H_matrix_mini, c(0.5), bounds_tuple_of_numeric_mini) 
	lhsone <- constraint_H_with_bounds(H_matrix_mini, c(1), bounds_tuple_of_numeric_mini) 
	list_of_constraints <- list(lhszero,lhshalf,lhsone)
	trajectory_miniH_constr <- diagonal_merge_constraint_list(list_of_constraints)
	sum_rows <- sapply(list_of_constraints, function(x) nrow(x$constr)) %>% sum
	expect_equal(nrow(trajectory_miniH_constr$constr), sum_rows)
	muscle_solutions <- trajectory_miniH_constr %>% eliminate_redundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE) %>% as.matrix 
	expect_true(all(evaluate_solutions(muscle_solutions, trajectory_miniH_constr)))
	
##' useful for split_lhs_har_df_by_constraint
stop_if_tasks_not_wholenumber <- function(num_tasks) {
    if (num_tasks != floor(num_tasks)) {
        stop("The number of muscles does not match up with the number of tasks in the multiconstraint")
    }
}

	har_df_list <- split_lhs_har_df_by_constraint(muscle_solutions, trajectory_miniH_constr, 3)
	wrench_outputs <- H_matrix_mini %*% t(muscle_solutions) %>% as.numeric
	expect_equal(wrench_outputs, rep(1, nrow(muscle_solutions)))
	expect_equal(sd(wrench_outputs), 0)
})