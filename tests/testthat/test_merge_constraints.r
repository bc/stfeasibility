context('Functions for merging constraints')

test_that("two constraints can be combined without affecting one another's individual outputs", {
	# fx_constraint
	first_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
	# fy_constraint
	second_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(0,1,0,0), bounds_tuple_of_numeric) 	
	merged_constraint <- diagonal_merge_constraints(first_constraint, second_constraint, "2")
	p <- plot_constraint_matrix(merged_constraint)
	show(p)
})

test_that("we can merge constraints in bulk", {
	# fx_constraint line
	first_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
	# fy_constraint line
	second_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(0,1,0,0), bounds_tuple_of_numeric) 	
	# go back to fx line
	third_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 	
	list_of_constraints <- list(first_constraint, second_constraint,third_constraint,third_constraint,third_constraint,third_constraint)
	context('basecase')
	expect_equal(list_of_constraints[[1]],diagonal_merge_constraint_list(list_of_constraints[1]))
	expect_equal(list_of_constraints[[2]],diagonal_merge_constraint_list(list_of_constraints[2]))
	expect_equal(list_of_constraints[[3]],diagonal_merge_constraint_list(list_of_constraints[3]))
	diagonal_merge_constraint_list(list_of_constraints[1:2])
	diagonal_merge_constraint_list(list_of_constraints[1:3])

	big <- diagonal_merge_constraint_list(list_of_constraints)
	p2 <- plot_constraint_matrix(big)
	show(p2)

	context("conglomerate and standalone constraints")
	num_muscles <- 7
	indices <- muscle_and_lambda_indices(big$constr, num_muscles)
	conglomerate_constraint <- lpsolve_force_in_dir("max", big, indices_for_muscles=indices$indices_for_muscles)
	standalone_constraint   <- lpsolve_force_in_dir("max", first_constraint, indices_for_muscles=1:7)
	standalone_constraint2  <- lpsolve_force_in_dir("max", second_constraint, indices_for_muscles=1:7)
	expect_equal(conglomerate_constraint$output_vector_per_task[[1]], standalone_constraint$output_vector_per_task[[1]])
	expect_equal(conglomerate_constraint$output_vector_per_task[[2]], standalone_constraint2$output_vector_per_task[[1]])
	colnames(big_sample) <- colnames(big$constr)
	devtools::use_data(multiconstraint, overwrite=TRUE)

})
test_that("mbm har multiconstraint", {
		mbm <- microbenchmark(
		"har_1e3" = {multiconstraint %>% har_sample(1e3); print("1/7" %>% paste(Sys.time()))},
		"har_1e4" = {multiconstraint %>% har_sample(1e4); print("2/7" %>% paste(Sys.time()))},
		"har_1e5" = {multiconstraint %>% har_sample(1e5); print("5/7" %>% paste(Sys.time()))},
		"har_5e5" = {multiconstraint %>% har_sample(5e5); print("7/7" %>% paste(Sys.time()))},
		times=1
	)
})

test_that("mbm har multiconstraint velocity", {
	r <- generate_and_add_velocity_constraint(multiconstraint, 1.0, 1.0, muscle_and_lambda_indices(multiconstraint, 7)$indices_for_muscles)
		mbm <- microbenchmark(
		"2e4_eliminate_default" = {a <- r %>% har_sample(1e3); print("1/4" %>% paste(Sys.time()))},
		"2e4_eliminate_false" 	= {b <- r %>% har_sample(1e4); print("2/4" %>% paste(Sys.time()))},
		"2e4_eliminate_false" 	= {c <- r %>% har_sample(1e5); print("2/4" %>% paste(Sys.time()))},
		times=1
	)
})

test_that("velocity_constraint works on multiconstraint", {

	trajectories <- r $>$ har_sample(200)
})