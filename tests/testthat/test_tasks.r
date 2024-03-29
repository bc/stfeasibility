context("generate_feasible patterns")

test_that("har datasets can be empirically culled under delta constraints", {
    positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,
        0, 0, 0), bounds_tuple_of_numeric)
    indices_for_muscles <- 1:7
    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles)
    context("computing polytope samples for tasks")
    har_per_task_df <- generate_task_trajectory_and_har(H_matrix = H_matrix, vector_out = fmax_info$output_vector_per_task[[1]] *
        (1 - 1e-05), n_task_values = 5, cycles_per_second = 2, cyclical_function = force_cos_ramp,
        output_dimension_names = force_dimnames, muscle_name_per_index = muscle_name_per_index,
        bounds_tuple_of_numeric = bounds_tuple_of_numeric, num_har_samples = 100,
        har_thin = 100)
    context("splitting by task")
    list_of_polytope_dfs <- split_by_time(har_per_task_df)
    # expect_equal(length(list_of_polytope_dfs),10)
    # expect_equal(nrow(list_of_polytope_dfs[[1]]),100)
    # expect_equal(nrow(list_of_polytope_dfs[[9]]),100)
    context("attempting tunneling")
    list_of_culled_polytope_dfs <- rm_solutions_with_infeasible_transitions(list_of_polytope_dfs,
        muscle_name_per_index, threshold = 1, mc.cores = 1)
    expect_equal(length(list_of_culled_polytope_dfs), length(list_of_polytope_dfs))
    threshold_disabled_polytope_dfs <- rm_solutions_with_infeasible_transitions(list_of_polytope_dfs,
        muscle_name_per_index, threshold = 1, mc.cores = 1)
    expect_equal(sapply(threshold_disabled_polytope_dfs, nrow), sapply(list_of_polytope_dfs,
        nrow))
    threshold_infinitely_strict <- rm_solutions_with_infeasible_transitions(list_of_polytope_dfs,
        muscle_name_per_index, threshold = 0, mc.cores = 1)
    expect_equal(sapply(threshold_infinitely_strict, nrow), rep(0, length(list_of_polytope_dfs)))
    medium_threshold <- rm_solutions_with_infeasible_transitions(list_of_polytope_dfs,
        muscle_name_per_index, threshold = 0.5, mc.cores = 1)
    browser()
    # TODO ANIMATION etc/overlay <- dcrb(list_of_culled_polytope_dfs)
})

test_that("feasible task for H yields valid points", {
    # the set of lambdas by which to scale the maximum feasible force for a given
    # task direction. P=
    constr <- constraint_H_with_bounds(H_matrix, c(5, 0, 0, 0), bounds_tuple_of_numeric)
    points <- har_sample(constr, 10000)
    colnames(points) <- muscle_name_per_index
    # Generate points without delta constraints
    expect_true(all(colMins(points) > 0))
    expect_true(all(colMaxes(points) < 1))
})

test_that("feasible task for mini H yields valid points", {
    # the set of lambdas by which to scale the maximum feasible force for a given
    # task direction. P=
    constr <- constraint_H_with_bounds(H_matrix_mini, c(1), bounds_tuple_of_numeric_mini)
    points <- har_sample(constr, 10000)
    # Generate points without delta constraints
    expect_true(all(colMins(points) > 0))
    expect_true(all(colMaxes(points) < 1))
    points_are_valid <- evaluate_solutions(points, constr)
    expect_true(all(points_are_valid))
})

test_that("har performance with fixed dimensionality", {
    constr <- constraint_H_with_bounds(H_matrix, c(5, 0, 0, 0), bounds_tuple_of_numeric) %>%
        eliminate_redundant
    mbm <- microbenchmark(pb1e3 = {
        a <- constr %>% pb_har_sample(1e+07, mc.cores = 8, eliminate = FALSE)
    }, pb1e4 = {
        a <- constr %>% pb_har_sample(1e+07, mc.cores = 8, eliminate = FALSE)
    }, pb1e5 = {
        a <- constr %>% pb_har_sample(1e+07, mc.cores = 8, eliminate = FALSE)
    }, pb1e6 = {
        a <- constr %>% pb_har_sample(1e+07, mc.cores = 8, eliminate = FALSE)
    }, pb1e7 = {
        a <- constr %>% pb_har_sample(1e+07, mc.cores = 8, eliminate = FALSE)
    }, times = 1)
})

test_that("we can generate force trajectory", {
    trajectory_task <- task_time_df(c(100, 0, 0, 0), 20, 1, force_cos_ramp, force_dimnames)
    expect_equal(nrow(trajectory_task), 20)
    expect_equal(trajectory_task$Fx[9], 93.973688, tol = 1e-04)
})

test_that("maps_within_threshold_by_dimension", {
    expect_true(maps_within_threshold_by_dimension(c(1, 2, 3), c(1, 2, 3), threshold = 0.01))
    expect_true(maps_within_threshold_by_dimension(rep(0.5, 7), rep(0.5, 7) * 0.99999,
        threshold = 0.001))
    expect_false(maps_within_threshold_by_dimension(rep(0.5, 7), rep(0.5, 7) * 0.99999,
        threshold = 1e-08))
    expect_false(maps_within_threshold_by_dimension(c(rep(0.5, 7), 0.5), c(rep(0.5,
        7), 0.9) * 0.99999, threshold = 1e-08))
    expect_true(maps_within_threshold_by_dimension(c(1), c(1), threshold = 0.01))
    expect_false(maps_within_threshold_by_dimension(c(0), c(1), threshold = 0.01))
    expect_true(maps_within_threshold_by_dimension(c(1, 2, 3), c(1, 2, 3), threshold = 0.01))
    map0 <- structure(list(FDP = 0.281445228858277, FDS = 0.13815374660091, EIP = 0.481078359065577,
        EDC = 0.94192914712619, LUM = 0.640311108736637, DI = 0.346844080864468,
        PI = 0.225458337356392), row.names = 101L, class = "data.frame")
    map1 <- structure(list(FDP = 0.208347120776137, FDS = 0.0984276508753465, EIP = 0.381784882330989,
        EDC = 0.659188727382571, LUM = 0.654171949911292, DI = 0.119571917524553,
        PI = 0.144473748280277), row.names = 1L, class = "data.frame")
    expect_false(maps_within_threshold_by_dimension(map0, map1, threshold = 0.2))
    expect_true(maps_within_threshold_by_dimension(map0, map1, threshold = 0.3))
    expect_false(maps_within_threshold_by_dimension(map0, map1, threshold = 0.03))
})

context("fmax for a direction")

skip_if_not(run_full_long_tests, message = deparse(substitute(condition)))
test_that("(har vs lpSolve) estimations of fmax in a given dir are expected to be different.",
    {

        negative_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(-1,
            0, 0, 0), bounds_tuple_of_numeric)
        positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,
            0, 0, 0), bounds_tuple_of_numeric)
        res_neg <- pb_har_sample(negative_fx_direction_constraint, n_samples = 10000)
        res_pos <- pb_har_sample(positive_fx_direction_constraint, n_samples = 10000)

        expect_equal(colMaxes(res_pos[, 1:7]), colMaxes(res_neg[1:7]), tol = 0.01)
        expect_equal(colMins(res_pos[, 1:7]), colMins(res_neg[, 1:7]), tol = 0.01)
        indices_for_muscles <- 1:7
        max_lp_negative <- lpsolve_force_in_dir("max", negative_fx_direction_constraint,
            indices_for_muscles)
        max_lp_positive <- lpsolve_force_in_dir("max", positive_fx_direction_constraint,
            indices_for_muscles)

        # Hit and run does not get all the way to the vertex/edge unless you give it
        # infinite time.
        expect_false(abs(max(res_pos[, 8]) - max_lp_positive$vector_magnitude_per_task[[1]]) <
            0.001)
        most_negative_mvc_fx <- H_matrix %*% max_lp_negative$muscle_activation_pattern_per_task[[1]]
        most_positive_mvc_fx <- H_matrix %*% max_lp_positive$muscle_activation_pattern_per_task[[1]]
        expect_equal(as.numeric(most_negative_mvc_fx), c(-17.5927374630042, -4.44089209850063e-16,
            3.5527136788005e-15, -6.93889390390723e-18))
        expect_equal(as.numeric(most_positive_mvc_fx), c(28.8115546379602, -9.99200722162641e-16,
            0, -2.42861286636753e-17))
        expect_false(abs(max(res_neg[, 8]) - max_lp_negative$vector_magnitude_per_task[[1]]) <
            0.001)
    })

skip_if_not(run_full_long_tests, message = deparse(substitute(condition)))
test_that("output_folder paths work", {
    expect_equal(output_filepath("hexbin/hexbin.html"), "../../output/hexbin/hexbin.html")
    expect_equal(output_subfolder_path("hexbin", "hexbin.html"), "../../output/hexbin/hexbin.html")
})

skip_if_not(run_full_long_tests, message = deparse(substitute(condition)))

test_that("we can generate and har upon each task polytope independently", {
    positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1,
        0, 0, 0), bounds_tuple_of_numeric)
    indices_for_muscles <- 1:7
    fmax_info <- lpsolve_force_in_dir("max", positive_fx_direction_constraint, indices_for_muscles)
    har_per_task_df <- generate_task_trajectory_and_har(H_matrix = H_matrix, vector_out = fmax_info$output_vector_per_task[[1]] *
        (1 - 1e-05), n_task_values = 60, cycles_per_second = 2, cyclical_function = force_cos_ramp,
        output_dimension_names = force_dimnames, muscle_name_per_index = muscle_name_per_index,
        bounds_tuple_of_numeric = bounds_tuple_of_numeric, num_har_samples = 1000,
        har_thin = 100)
    expect_true(all(!where_muscles_have_unreasonable_values(har_per_task_df, muscle_name_per_index)))
    p <- ggplot(har_per_task_df, aes(DI, PI, frame = time)) + geom_hex(aes(group = time),
        binwidth = c(0.05, 0.05)) + coord_fixed() + unit_cube_zoom() + theme_classic()
    gganimate::gganimate(p, output_subfolder_path("hexbin", "hexbin.html"))
    p2 <- ggplot(har_per_task_df, aes(DI, PI, frame = time)) + geom_point(aes(group = time),
        size = 0.01, alpha = 0.1) + stat_chull(aes(group = time), alpha = 0.1, geom = "polygon") +
        coord_fixed() + unit_cube_zoom() + theme_classic()
    gganimate::gganimate(p2, output_subfolder_path("points", "points.html"))
})

context('full_for_task_st')
test_that('full_ st histograms', {
    har_n <- 1e3
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, c(28,0,0,0), 0.5, 0.5, n_task_values = 10, cycles_per_second=10, eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    points_st <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    p_st <- ggplot(har_per_task_df, aes(DI, PI, frame = time)) + geom_hex(aes(group = time),
        binwidth = c(0.05, 0.05)) + coord_fixed() + unit_cube_zoom() + theme_classic()
    points_st$st <- factor(1, levels=c(0,1))
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, c(28,0,0,0), 1.0, 1.0, n_task_values = 10, cycles_per_second=10, eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    points_no_st <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    points_no_st$st <- factor(0, levels(c(0,1)))

    gganimate::gganimate(p_st, output_subfolder_path("st_vs_degenerate", "st.html"))

})
