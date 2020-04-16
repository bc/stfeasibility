	maximize_wrench <- function(H_matrix_input, bounds_tuple_of_numeric, direction) {
		positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix_input, direction = direction, bounds_tuple_of_numeric)
	    indices_for_muscles <- 1:ncol(H_matrix_input)
	    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles, nrow(H_matrix_input))
	}