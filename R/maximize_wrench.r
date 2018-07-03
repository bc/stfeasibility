	maximize_wrench <- function(H_matrix, bounds_tuple_of_numeric, direction) {
		positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = direction, bounds_tuple_of_numeric)
	    indices_for_muscles <- 1:7
	    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles)
	}