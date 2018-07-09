context("test eliminate constraint")
test_that("with degenerate velocity constraint --> no velocity constraint", {
    fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
        fmax_info$output_vector_per_task[[1]], 1, 1, n_task_values = 3, eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    removed_rows <- attr(res, "redundancy_info")$redundant
    degenerate_case_redundant_rows <- c(9, 10, 11, 12, 20, 21, 22, 31, 32, 33, 34,
        35, 36, 37, 39, 41, 44, 53, 54, 55, 56, 64, 65, 66, 67, 68, 69, 70, 71, 72,
        73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
        92, 93, 94)
    expect_equal(removed_rows, degenerate_case_redundant_rows)

})

test_that("with 0.5 velocity constraint", {
    fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
        fmax_info$output_vector_per_task[[1]], 0.5, 0.5, n_task_values = 3, eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant

    expected_n3_medium_constr_redundant_rows <- c(5, 6, 7, 8, 12, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 27, 28, 29, 30, 32, 34, 36, 37, 38, 39, 40, 41, 42, 43,
        44, 49, 50, 51, 52, 54, 56, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
        70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,
        89, 90, 91, 92, 93, 94)

    expect_equal(removed_rows, expected_n3_medium_constr_redundant_rows)
})


test_that("with very aggressive velocity constraint", {
    fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
        fmax_info$output_vector_per_task[[1]], 0.001, 0.001, n_task_values = 3, eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant2
    removed_rows <- extract_redundant_rows(res)

    expected_n3_no_space_redundant_rows <- c(5, 6, 7, 8, 12, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 27, 28, 29, 30, 32, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44,
        49, 50, 51, 52, 54, 56, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
        71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
        90, 91, 92, 93, 94)
    expect_equal(removed_rows, expected_n3_no_space_redundant_rows)
})

test_that("same result as original hitandrun::eliminateRedundant", {
    fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
        fmax_info$output_vector_per_task[[1]], 0.001, 0.001, n_task_values = 3, eliminate = FALSE)
    res_2 <- st_constr_str$nonredundant_constr %>% eliminate_redundant2
    res_original <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    removed_rows <- extract_redundant_rows(res)

    expect_equal(res_2$constr%>%unname, res_original$constr%>%unname)
    expect_equal(res_2$rhs%>%unname, res_original$rhs%>%unname)

    expected_n3_no_space_redundant_rows <- c(5, 6, 7, 8, 12, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 27, 28, 29, 30, 32, 34, 36, 37, 38, 39, 40, 41, 42, 43, 44,
        49, 50, 51, 52, 54, 56, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
        71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
        90, 91, 92, 93, 94)
    expect_equal(removed_rows, expected_n3_no_space_redundant_rows)
})

test_that("performance of one linear program", {
	mbm<- microbenchmark("hi" = {maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))}, times=10000)
})

test_that("generate large test set for st tunneling", {
		n_task_values <- 10
    	num_muscles<- ncol(H_matrix)
    	fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
	    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
	    	#assuming that it takes 200ms to go from 0 to 100 percent.
        fmax_info$output_vector_per_task[[1]], 0.5, 0.5, cycles_per_second=4, n_task_values = n_task_values,
        eliminate = FALSE)
        st10_half <- st_constr_str$nonredundant_constr %>% eliminate_redundant2(8)
        st10_half_r0 <- st_constr_str$nonredundant_constr %>% eliminate_redundant
        devtools::use_data(st10_half, overwrite = TRUE)
        har_st10_half <- st10_half %>% pb_har_sample(1e4, mc.cores=8, eliminate=FALSE)
		use_data(har_st10_half, overwrite = TRUE)
})


test_that("performance", {
    n_task_values <- 6
    num_muscles <- ncol(H_matrix)
    fmax_info <- maximize_wrench(H_matrix, bounds_tuple_of_numeric, direction = c(1,
        0, 0, 0))
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
        fmax_info$output_vector_per_task[[1]], 0.5, 0.5, n_task_values = n_task_values,
        eliminate = FALSE)

    expected_n5_rm_redundancy <- c(5, 6, 7, 8, 9, 10, 11, 12, 20, 21, 22, 27, 28,
        29, 30, 31, 32, 33, 34, 36, 37, 39, 41, 42, 44, 49, 50, 51, 52, 53, 54,
        55, 56, 57, 58, 59, 61, 63, 66, 71, 72, 73, 74, 75, 76, 77, 78, 80, 81, 83,
        85, 86, 88, 93, 94, 95, 96, 97, 98, 99, 100, 108, 109, 110, 111, 112, 117,
        118, 119, 123, 124, 125, 126, 127, 131, 132, 133, 135, 136, 137, 138, 139,
        140, 142, 143, 144, 145, 146, 147, 148, 152, 153, 154, 158, 159, 160, 161,
        166)
    expect_equal(sort(rownames(res1$constr)),sort(rownames(res2$constr)))
    res1 <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    res4 <- st_constr_str$nonredundant_constr %>% eliminate_redundant2(4)
	points_res1 <- res1 %>% pb_har_sample(1e5, mc.cores=8, eliminate=FALSE)
	points_res4 <- res4 %>% pb_har_sample(1e5, mc.cores=8, eliminate=FALSE)
	expect_true(evaluate_solutions(points_res1, res1, 1e-10) %>% all)
	expect_true(evaluate_solutions(points_res1, res4, 1e-10) %>% all)
	expect_true(evaluate_solutions(points_res4, res1, 1e-10) %>% all)
	expect_true(evaluate_solutions(points_res4, res4, 1e-10) %>% all)

	p_values_distributions <- sapply(1:num_muscles, function(i) {
		ks.test(points_res1[,i],points_res4[,i])$p.value
	})
	expect_true(all(p_values_distributions < 0.01))

	p_values <- sapply(1:num_muscles, function(i) {
		t.test(points_res1[,i],points_res4[,i])$p.value
	})
	expect_true(all(p_values_means < 0.01))

	plot(points_res1[,7],points_res4[,7])
})