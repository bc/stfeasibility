context("generate_feasible patterns")
# Test finger in flexed posture, from frontiers2018 feasibility theory paper
JR <- rbind(c(-0.08941, -0.0447, -0.009249, 0.03669, 0.1421, 0.2087, -0.2138), c(-0.04689,
    -0.1496, 0.052, 0.052, 0.0248, 0, 0.0248), c(0.06472, 0.001953, -0.1518, -0.1518,
    0.2919, 0.0568, 0.2067), c(0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483,
    0.0001579, -0.000685))
maximal_tension_per_muscle <- c(123, 219, 23.52, 91.74, 21.6, 124.8, 129.6)
muscle_name_per_index <- c("FDP", "FDS", "EIP", "EDC", "LUM", "DI", "PI")
force_dimnames <- c("Fx", "Fy", "Fz", "tx")

H_matrix <- JR %*% diag(maximal_tension_per_muscle)
bounds_tuple_of_numeric <- rep(list(list(lower = 0, upper = 1)), length(maximal_tension_per_muscle))

test_that("make feasible task trajectory", {
    # the set of lambdas by which to scale the maximum feasible force for a given
    # task direction.

    constr <- constraint_H_with_bounds(H_matrix, c(5, 0, 0, 0), bounds_tuple_of_numeric)
    points <- har_sample(constr, 10000, 100)
    colnames(points) <- muscle_name_per_index
    # Generate points without delta constraints
    expect_true(all(colMins(points) > 0))
    expect_true(all(colMaxes(points) < 1))
})

test_that("we can generate force trajectory", {
    trajectory_task <- task_time_df(c(100, 0, 0, 0), 20, 1, force_cos_ramp, force_dimnames)
    expect_equal(nrow(trajectory_task), 20)
    expect_equal(trajectory_task$Fx[9], 93.973688, tol = 1e-04)
})

context("fmax for a direction")
test_that("(har vs lpSolve) estimations of fmax in a given dir are expected to be different.", {
    positive_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(-1.0,0.0,0.0,0.0), bounds_tuple_of_numeric)
    negative_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1.0,0.0,0.0,0.0), bounds_tuple_of_numeric)
    res_pos <- pb_har_sample(positive_direction_constraint, n_samples=1e4, thin=100)
    res_neg <- pb_har_sample(negative_direction_constraint, n_samples=1e4, thin=100)
    
    expect_equal(colMaxes(res_pos[,1:7]),colMaxes(res_neg[1:7]), tol=1e-2)
    expect_equal(colMins(res_pos[,1:7]),colMins(res_neg[,1:7]), tol=1e-2)
    max_lp_positive <- lpsolve_force_in_dir("max",positive_direction_constraint, 7)
    max_lp_negative <- lpsolve_force_in_dir("max",negative_direction_constraint, 7)

    # Hit and run does not get all the way to the vertex/edge unless you give it infinite time.
    expect_false(abs(max(res_pos[,8]) - max_lp_positive$vector_magnitude) < 1e-3)
    expect_false(abs(max(res_neg[,8]) - max_lp_negative$vector_magnitude) < 1e-3)
})




test_that("we can test each task independently", {
    positive_fx_direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1.0,0.0,0.0,0.0), bounds_tuple_of_numeric)
    fmax_info <- lpsolve_force_in_dir("max",positive_fx_direction_constraint, 7)
    har_per_task_df <- generate_task_trajectory_and_har(H_matrix = H_matrix, vector_out = fmax_info$vector_out *
        (1 - 1e-05), n_task_values = 60, cycles_per_second = 2, cyclical_function = force_cos_ramp,
        output_dimension_names = force_dimnames, muscle_name_per_index = muscle_name_per_index,
        bounds_tuple_of_numeric = bounds_tuple_of_numeric, num_har_samples = 1000,
        har_thin = 100)
    expect_true(all(!where_muscles_have_unreasonable_values(har_per_task_df, muscle_name_per_index)))
 
    p <- ggplot(har_per_task_df, aes(DI, PI, frame=time)) + geom_hex(aes(group=time), binwidth = c(.05, .05)) + coord_fixed() + unit_cube_zoom() + theme_classic()
    gganimate::gganimate(p, "hexbin/hexbin.html")
    p2 <- ggplot(har_per_task_df, aes(DI, PI, frame=time)) + geom_point(aes(group=time), size = 0.01, alpha=0.1) + stat_chull(aes(group = time), alpha = 0.1, geom = "polygon") + coord_fixed() + unit_cube_zoom() + theme_classic()
    gganimate::gganimate(p2, "points/points.html")


})