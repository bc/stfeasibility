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

	pair_har_solutions_df_with_constraint <- function(list_of_har_solution_dfs, list_of_constraints){
		constraint_solution_pairs <- lapply(1:length(list_of_constraints), function(i){
			return(list(constraint_object = list_of_constraints[[i]],
				har_solutions = list_of_har_solution_dfs[[i]]))
		})
		return(constraint_solution_pairs)
	}
	har_df_list <- split_lhs_har_df_by_constraint(muscle_solutions, trajectory_miniH_constr, 3)
	constraint_solution_pairs <- pair_har_solutions_df_with_constraint(har_df_list, list_of_constraints)
	expect_true(evaluate_constraint_solution_pairs(constraint_solution_pairs) %>% dcc %>% all)
	evaluate_constraint_solution_pairs <- function(constraint_solution_pairs){
		lapply(constraint_solution_pairs, function(x){
			evaluate_solutions(x$har_solutions, x$constraint_object)
		})
	}
	wrench_outputs <- H_matrix_mini %*% t(muscle_solutions) %>% as.numeric
	expect_equal(wrench_outputs, rep(1, nrow(muscle_solutions)))
	expect_equal(sd(wrench_outputs), 0)
})