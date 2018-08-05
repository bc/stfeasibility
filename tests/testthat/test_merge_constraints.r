context('Functions for merging constraints')

test_that("spatiotemporal tunnel har is computationally tractable", {
	r <- generate_and_add_velocity_constraint(multiconstraint, 1.0, 1.0, muscle_and_lambda_indices(multiconstraint, 7)$indices_for_muscles, 7)
	res <- lpsolve_force_in_dir("max", r, indices_for_muscles=muscle_and_lambda_indices(r, 7)$indices_for_muscles)	
	global_solution <- res$raw_x_concatenated
	evaluate_solution(r, global_solution)
	point_list <- r %>% har_sample(1e1, eliminate=FALSE)
})

`

test_that("two constraints can be combined without affecting one another's individual outputs", {
	# fx_constraint
	first_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
	# fy_constraint
	second_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(0,1,0,0), bounds_tuple_of_numeric) 	
	merged_constraint <- diagonal_merge_constraints(first_constraint, second_constraint, "2")
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

test_that("we can make a mini-H multiconstraint", {

	mini_H_task_trajectory <- cos(seq(0,(pi*2), length.out=7))
	constraint_list <- lapply(mini_H_task_trajectory, function(task_x){
		a_matrix_lhs_direction(H_matrix_mini, direction = task_x, bounds_tuple_of_numeric_mini) 
	})
	second_constraint <- a_matrix_lhs_direction(H_matrix_mini, direction = c(0), bounds_tuple_of_numeric_mini) 	
	third_constraint <- a_matrix_lhs_direction(H_matrix_mini, direction = c(1), bounds_tuple_of_numeric_mini) 
})

context("har on velocity constraint")
test_that("mbm har multiconstraint velocity", {
		constraint_velocity <- compose_velocity_constraint(multiconstraint, 0.042,0.042)
		plot_constraint_matrix(constraint_velocity)
		a <- constraint_velocity %>% har_sample(1e2); print("1/4" %>% paste(Sys.time()))
		browser()
})

context("har on independent constraints w/o velocity")
test_that("we can generate and har upon each task polytope independently", {
    positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,
        0, 0, 0), bounds_tuple_of_numeric)
    indices_for_muscles <- 1:7
    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles)
    tasks_and_constraints <- generate_tasks_and_corresponding_constraints(H_matrix=H_matrix, vector_out = fmax_info$output_vector_per_task[[1]] *
        (1 - 1e-05), n_task_values = 6, cycles_per_second = 2, cyclical_function = force_cos_ramp,
        output_dimension_names = force_dimnames,
        bounds_tuple_of_numeric=bounds_tuple_of_numeric)

	inequality_constraints <- diagonal_merge_constraint_list(tasks_and_constraints$constraint)
	st_constr <- generate_and_add_velocity_constraint(inequality_constraints, 0.042, 0.040, muscle_and_lambda_indices(inequality_constraints, 7)$indices_for_muscles, 7)
    a <- plot_constraint_matrix(st_constr)
    ggplotly(a)
    st_task_and_constraint <- list(st_constr = st_constr, tasks_and_independent_constraints = tasks_and_constraints)
    devtools::use_data(st_task_and_constraint, overwrite = TRUE)
   })
