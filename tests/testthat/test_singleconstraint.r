context('testing singleconstraint')
test_that("H matrix with defined task on rhs has 0 sd for lambda",{
	lhs <- constraint_H_with_bounds(H_matrix_mini, c(1), bounds_tuple_of_numeric_mini) 
	muscle_solutions <- lhs %>% eliminate_redundant %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE) %>% as.matrix 
	wrench_outputs <- H_matrix_mini %*% t(muscle_solutions) %>% as.numeric
	expect_equal(wrench_outputs, rep(1, nrow(muscle_solutions)))
	expect_equal(sd(wrench_outputs), 0)
})

