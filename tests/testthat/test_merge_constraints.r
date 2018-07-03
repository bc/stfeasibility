context('Functions for merging constraints')

test_that("spatiotemporal tunnel har is computationally tractable", {
	r <- generate_and_add_velocity_constraint(multiconstraint, 1.0, 1.0, 7)
	res <- lpsolve_force_in_dir("max", r, indices_for_muscles=muscle_and_lambda_indices(r, 7)$indices_for_muscles)	
	global_solution <- res$raw_x_concatenated
	valid <- evaluate_solution(global_solution, r)
	point_list <- r %>% har_sample(1e1, eliminate=FALSE)
})


test_that("two constraints can be combined without affecting one another's individual outputs", {
	# fx_constraint
	first_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
	# fy_constraint
	second_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(0,1,0,0), bounds_tuple_of_numeric) 	
	merged_constraint <- diagonal_merge_constraints(first_constraint, second_constraint, "2")
})


test_that("velocity helper functions work", {
	expect_equal(transition_names(c("a","b","c","d","e","f")), c("a_to_d", "b_to_e" ,"c_to_f"))
	expect_equal(transition_names(c("a","b","c","d")), c("a_to_c", "b_to_d"))
})

context("compare independent constraints to merged constraints")
test_that("merging constraints yields the same result as independent constraints", {
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
	num_muscles <- 7
	indices <- muscle_and_lambda_indices(big, num_muscles)
	conglomerate_constraint <- lpsolve_force_in_dir("max", big, indices_for_muscles=indices$indices_for_muscles)
	standalone_constraint   <- lpsolve_force_in_dir("max", first_constraint, indices_for_muscles=1:7)
	standalone_constraint2  <- lpsolve_force_in_dir("max", second_constraint, indices_for_muscles=1:7)
	expect_equal(conglomerate_constraint$output_vector_per_task[[1]], standalone_constraint$output_vector_per_task[[1]])
	expect_equal(conglomerate_constraint$output_vector_per_task[[2]], standalone_constraint2$output_vector_per_task[[1]])
	colnames(big$constr) <- colnames(big$constr)
	multiconstraint <- big
	devtools::use_data(multiconstraint, overwrite=TRUE)
})


context("har on independent constraints w/o velocity")
test_that("we can generate and har upon each task polytope independently", {
    positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,
        0, 0, 0), bounds_tuple_of_numeric)
    indices_for_muscles <- 1:7
    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles)
    tasks_and_constraints <- generate_tasks_and_corresponding_constraints(H_matrix=H_matrix, vector_out = fmax_info$output_vector_per_task[[1]] *
        (1 - 1e-05), n_task_values = 5, cycles_per_second = 60, cyclical_function = force_cos_ramp,
        bounds_tuple_of_numeric=bounds_tuple_of_numeric)

	inequality_constraints <- diagonal_merge_constraint_list(tasks_and_constraints$constraint)
	st_constr <- generate_and_add_velocity_constraint(inequality_constraints, 0.042, 0.040, 7)
    a <- plot_constraint_matrix(st_constr)
    st_task_and_constraint <- list(st_constr = st_constr, tasks_and_independent_constraints = tasks_and_constraints)
    devtools::use_data(st_task_and_constraint, overwrite = TRUE)

        tasks_and_constraints <- generate_tasks_and_corresponding_constraints(H_matrix=H_matrix, vector_out = fmax_info$output_vector_per_task[[1]] *
        (1 - 1e-05), n_task_values = 10, cycles_per_second = 60, cyclical_function = force_cos_ramp,
        bounds_tuple_of_numeric=bounds_tuple_of_numeric)

	inequality_constraints <- diagonal_merge_constraint_list(tasks_and_constraints$constraint)
	st_constr <- generate_and_add_velocity_constraint(inequality_constraints, 0.042, 0.040, 7)
    a <- plot_constraint_matrix(st_constr)
    st_task_and_constraint <- list(st_constr = st_constr, tasks_and_independent_constraints = tasks_and_constraints)
    devtools::use_data(st_task_and_constraint, overwrite = TRUE)
    print(microbenchmark("50"={ b <- st_task_and_constraint[[1]] %>% eliminate_redundant}, times=1))
    fmax_info$output_vector_per_task[[1]]

	inequality_constraints <- diagonal_merge_constraint_list(tasks_and_constraints$constraint)
	st_constr <- generate_and_add_velocity_constraint(inequality_constraints, 0.042, 0.040, 7)
    a <- plot_constraint_matrix(st_constr)
    st_task_and_constraint <- list(st_constr = st_constr, tasks_and_independent_constraints = tasks_and_constraints)
    devtools::use_data(st_task_and_constraint, overwrite = TRUE)
    mbm <- microbenchmark("100"={ 
    	c <- st_task_and_constraint[[1]] %>% eliminate_redundant
    },
     times=1)
   })

test_that("force_cos_ramp_constraint", {

	fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction=c(1,0,0,0))
	st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 1.0,1.0, n_task_values=10)
	list_of_constraints <- st_constr_str[[2]]$constraints
	 har_constraint_pairs_velocity_constrained <- har_and_split_trajectory_constraint(st_constr_str$nonredundant_constr, list_of_constraints, list_of_tasks,
    num_muscles=ncol(H_matrix), har_samples=1e4, mc.cores=8, tol=1e-11)
	mbm <- microbenchmark(	
		"8 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=3);print('Ø')},
		"9 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=4);print('Ø')},
		"10 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=4);print('Ø')},
		"11 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=5);print('Ø')},
		times=1
	)
	print(mbm)
	print(summary(mbm))
	plot(mbm)
	print('NEXT')

	mbm <- microbenchmark(	
		"12 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=12);print('Ø')},
		"13 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=13);print('Ø')},
		"14 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=14);print('Ø')},
		"15 task_v_constr" = {force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, fmax_info$output_vector_per_task[[1]], 0.8,0.8, n_task_values=15);print('Ø')},
		times=1
	)
	print(mbm)
	print(summary(mbm))
	plot(mbm)
})