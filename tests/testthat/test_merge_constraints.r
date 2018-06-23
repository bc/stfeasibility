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
	first_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
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
		"har_1e2" = {multiconstraint %>% har_sample(1e2); print("1/7" %>% paste(Sys.time()))},
		"har_1e3" = {multiconstraint %>% har_sample(1e3); print("1/7" %>% paste(Sys.time()))},
		# "har_1e4" = {multiconstraint %>% har_sample(1e4); print("2/7" %>% paste(Sys.time()))},
		# "har_1e5" = {multiconstraint %>% har_sample(1e5); print("5/7" %>% paste(Sys.time()))},
		# "har_5e5" = {multiconstraint %>% har_sample(5e5); print("7/7" %>% paste(Sys.time()))},
		times=1
	)
})

test_that("mbm har multiconstraint velocity", {
	r <- generate_and_add_velocity_constraint(multiconstraint, 1.0, 1.0, muscle_and_lambda_indices(multiconstraint, 7)$indices_for_muscles)
		mbm <- microbenchmark(
		"1e3" = {a <- r %>% har_sample(1e3); print("1/4" %>% paste(Sys.time()))},
		# "1e4" 	= {b <- r %>% har_sample(1e4); print("2/4" %>% paste(Sys.time()))},
		# "1e5" 	= {c <- r %>% har_sample(1e5); print("2/4" %>% paste(Sys.time()))},
		times=2
	)
})

test_that("we can generate and har upon each task polytope independently", {
    positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,
        0, 0, 0), bounds_tuple_of_numeric)
    indices_for_muscles <- 1:7
    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles)
    tasks_and_constraints <- generate_tasks_and_corresponding_constraints(vector_out = fmax_info$output_vector_per_task[[1]] *
        (1 - 1e-05), n_task_values = 60, cycles_per_second = 2, cyclical_function = force_cos_ramp,
        output_dimension_names = force_dimnames,
        bounds_tuple_of_numeric)
    spatiotemporal_inequality_constraints <- diagonal_merge_constraint_list(tasks_and_constraints$constraint)
    spatiotemporal_inequality_constraints$constr <- as.matrix(spatiotemporal_inequality_constraints$constr)
    st_task_and_constraint <- list(tasks=tasks_and_constraints$tasks, constraint=spatiotemporal_inequality_constraints)
    plot_constraint_matrix(spatiotemporal_inequality_constraints)
    devtools::use_data(st_task_and_constraint)
   })
test_that("spatiotemporal tunnel har is computationally tractable", {
	r <- st_task_and_constraint$constraint
    	mbm <- microbenchmark(
		"60 tasks, 1 point" = {a <- diagonal_merge_constraint_list(r) %>% har_sample(1e1); print("1/5" %>% paste(Sys.time()))},
		"60 tasks, 1e2 points" = {b <- diagonal_merge_constraint_list(r) %>% har_sample(1e2); print("2/5" %>% paste(Sys.time()))},
		# "60 tasks, 1e3 points" = {c <- diagonal_merge_constraint_list(r) %>% har_sample(1e3); print("3/5" %>% paste(Sys.time()))},
		# "60 tasks, 1e4 points" 	= {d <- diagonal_merge_constraint_list(r) %>% har_sample(1e4); print("4/5" %>% paste(Sys.time()))},
		# "60 tasks, 1e5 points" 	= {e <- diagonal_merge_constraint_list(r) %>% har_sample(1e5); print("5/5" %>% paste(Sys.time()))},
		times=1
	)
})
