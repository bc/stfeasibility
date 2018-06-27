context("multiconstraint redundancy")
test_that("we can make a mini-H multiconstraint", {
	mini_H_task_trajectory <- cos(seq(0,(pi*2), length.out=7))
	num_muscles <- 3
	constraint_list <- lapply(mini_H_task_trajectory, function(task_x){
		a_matrix_lhs_direction(H_matrix_mini, direction = task_x, bounds_tuple_of_numeric_mini) 
	})
	mini_trajectory_constr <- diagonal_merge_constraint_list(constraint_list)
	expect_equal(ncol(mini_trajectory_constr$constr),length(mini_H_task_trajectory)*4)
	expect_equal(nrow(mini_trajectory_constr$constr), 56)

	mini_trajectory_constr_nonredundant <- mini_trajectory_constr %>% eliminate_redundant

	expect_equal(ncol(mini_trajectory_constr_nonredundant$constr),length(mini_H_task_trajectory)*4)
	expect_equal(nrow(mini_trajectory_constr_nonredundant$constr), 49)
	a <- plot_constraint_matrix(mini_trajectory_constr) + ggtitle("H_matrix_mini trajectory over cosine task in Fx")
	b <- plot_constraint_matrix(mini_trajectory_constr_nonredundant) + ggtitle("redundant constraints removed")
	redundancy_comparisons_H_mini <- arrangeGrob(grobs=list(a,b), ncol=2)
	plot(redundancy_comparisons_H_mini)
	# mini_trajectory_constr %>% har_sample(1e4)	

})
