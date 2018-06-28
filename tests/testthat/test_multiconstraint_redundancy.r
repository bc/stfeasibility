context("multiconstraint redundancy")
test_that("H matrix of two moments with defined task on rhs has 0 sd in wrench_outputs",{
	lhs_0 <- constraint_H_with_bounds(H_matrix_mini, c(0.25), bounds_tuple_of_numeric_mini) 
	lhs_1 <- constraint_H_with_bounds(H_matrix_mini, c(0.75), bounds_tuple_of_numeric_mini) 
	constr_tuple <- diagonal_merge_constraint_list(list(lhs_0, lhs_1))
	print_constraint(constr_tuple)
	context('type requirements')
	expect_equal(class(constr_tuple$constr), "matrix")

	context('must be a feasible constraint set')
	expect_true(constraint_is_feasible(constr_tuple,3))
	solutions <- constr_tuple %>% eliminate_redundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE) %>% as.matrix
	
	context('bound requirements')
	expect_true(all(as.numeric(unlist(solutions))<=1.0))
	expect_true(all(as.numeric(unlist(solutions))>=0.0))

	context('output matches desired wrench')
	lhs_0_output <- lhs_0$constr %*% t(solutions[,1:3])
	expect_equal(sd(lhs_0_output["Fx",]),0)
	expect_equal(mean(lhs_0_output["Fx",]),0.25)

	lhs_1_output <- lhs_1$constr %*% t(solutions[,4:6])
	expect_equal(sd(lhs_1_output["Fx",]),0)
	expect_equal(mean(lhs_1_output["Fx",]),0.75)
})

test_that("we can make a mini-H multiconstraint with RHS", {
	mini_H_task_trajectory <- cos(seq(0,(pi*2), length.out=11))
	num_muscles <- 3
	constraint_list <- lapply(mini_H_task_trajectory, function(task_x){
		a_matrix_rhs_task(H_matrix_mini, task_x, bounds_tuple_of_numeric_mini) 
	})
	mini_trajectory_constr <- diagonal_merge_constraint_list(constraint_list)
	expect_true(constraint_is_feasible(mini_trajectory_constr,3))
	expect_equal(ncol(mini_trajectory_constr$constr),3*length(mini_H_task_trajectory))
	expect_equal(nrow(mini_trajectory_constr$constr), 88)
	# add the lambda=1 constraint:
	# devtools::use_data(mini_trajectory_constr, overwrite=TRUE)
})

test_that("we can make a mini-H multiconstraint", {
	mini_H_task_trajectory <- cos(seq(0,(pi*2), length.out=11))
	num_muscles <- 3
	constraint_list <- lapply(mini_H_task_trajectory, function(task_x){
		a_matrix_lhs_direction(H_matrix_mini, direction = task_x, bounds_tuple_of_numeric_mini) 
	})
	mini_trajectory_constr <- diagonal_merge_constraint_list(constraint_list)
	expect_equal(ncol(mini_trajectory_constr$constr),length(mini_H_task_trajectory)*4)
	expect_equal(nrow(mini_trajectory_constr$constr), 56)
	# add the lambda=1 constraint:
	# devtools::use_data(mini_trajectory_constr)
})

test_that("zero variance occurs in lambda when we have fixed tasks per moment of a trajectory", {
	har_mini_task <- mini_trajectory_constr %>% eliminate_redundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE)
	lambda_indices <- colnames(har_mini_task)[4*(1:7)]
	variances_for_lambda_parameters <- apply(har_mini_task[,lambda_indices],2,sd)%>% as.numeric
	expect_equal(variances_for_lambda_parameters, rep(0.0,7), 1e-10)
})

test_that("adding lambda_q", {
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
