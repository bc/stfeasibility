context('Functions for merging constraints')

skip("fs")
test_that("two constraints can be combined without affecting one another's individual outputs", {
	# first_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric)
	second_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric)
	pad_first_constraint_A(first_constraint, second_constraint_dimensionality)
	pad_first_constraint_A <- function(first_constraint, second_constraint_dimensionality){
		cbind()
	}
	browser()

	# expect_equal(minimizing on left constraint result, minimizing on combined constraint[left part])
})