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
	# add the lambda=1 constraint:
	mini_trajectory_lambda_constr <- mini_trajectory_constr %>% add_lambda_equality_constraint(3)
	mini_trajectory_lambda_velocity_consr <- mini_trajectory_lambda_constr %>% generate_and_add_velocity_constraint(0.5,0.5,3)
	full_constr_mini <- mini_trajectory_lambda_velocity_consr %>% eliminate_redundant

	expect_equal(ncol(mini_trajectory_constr_nonredundant$constr),length(mini_H_task_trajectory)*4)
	expect_equal(nrow(mini_trajectory_constr_nonredundant$constr), 49)



	a <- plot_constraint_matrix(mini_trajectory_constr) + ggtitle("H_matrix_mini trajectory over cosine task in Fx")
	b <- plot_constraint_matrix(mini_trajectory_lambda_constr) + ggtitle("task lambda fixed to 1")
	c <- plot_constraint_matrix(mini_trajectory_lambda_velocity_consr) + ggtitle("velocity_added")
	d <- plot_constraint_matrix(mini_trajectory_constr_nonredundant) + ggtitle("redundant constraints removed")
	redundancy_comparisons_H_mini <- arrangeGrob(grobs=list(a,b,c,d), ncol=4)
	plot(redundancy_comparisons_H_mini)
	
	points <- mini_trajectory_lambda_constr%>% eliminate_redundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE)	
	points <- mini_trajectory_constr_nonredundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE)	

	is_feasible(mini_trajectory_constr)
	res <- lpsolve_muscles_for_task("min", mini_trajectory_constr, 3)

})
