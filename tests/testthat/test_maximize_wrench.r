test_that("works with smaller matrix",{
  fmax_info <- maximize_wrench(H_matrix_mini, bounds_tuple_of_numeric_mini, direction = c(1.0))
  expect_equal(fmax_info$muscle_activation_pattern_per_task[[1]],c(1,0,1))
  expect_equal(fmax_info$output_vector_per_task[[1]],5.333333333, 1e-5)
  })  

test_that("f",{
  fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
  })  