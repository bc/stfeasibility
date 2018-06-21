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
f_out = c(28, 0, 0, 0)

test_that("make feasible task trajectory", {
    # the set of lambdas by which to scale the maximum feasible force for a given
    # task direction.

    constr <- constraint_H_with_bounds(H_matrix, f_out, bounds_tuple_of_numeric)
    points <- har_sample(constr, 10000, 100)
    colnames(points) <- muscle_name_per_index
    # Generate points without delta constraints
    expect_true(all(colMins(points) > 0))
    expect_true(all(colMaxes(points) < 1))
})

test_that("we can generate force trajectory", {
    trajectory_task <- task_time_df(c(100, 0, 0, 0), 20, 1, force_cos_ramp, force_dimnames)
    expect_equal(nrow(trajectory_task), 20)
    expect_equal(trajectory_task$Fx[9], 87.947375, tol = 1e-04)
})

test_that("compute fmax task in given direction", {
    direction_constraint <- a_matrix_lhs_direction(H_matrix, direction = c(1.0,0.0,0.0,0.0), bounds_tuple_of_numeric)
    res <- direction_constraint %>% har_sample(1e5,thin=100)
    res$V8 <- -res$V8
    max(res$V8)
    c_in_cTx <- c(0,0,0,0,0,0,0,1)
    lpsolve_force_in_dir("min",direction_constraint, c_in_cTx, 7)
    lpsolve_force_in_dir("max",direction_constraint, c_in_cTx, 7)
    summary(res)
    browser()
})




test_that("we can test each task independently", {
    tasks <- task_time_df(fmax_task=c(0, -28.8, 0, 0), n_samples=20, cycles_per_second=1, cyclical_function=force_cos_ramp, output_dimension_names=force_dimnames)
    list_of_constraints_per_task <- apply(tasks, 1, function(x){
        constraint_H_with_bounds(H_matrix, x[force_dimnames], bounds_tuple_of_numeric)
    })
    bigL <- list_of_constraints_per_task %>% pbmclapply(. %>% har_sample(10,thin=100))
    bigL_labeled <- lapply(1:length(bigL), function(list_index){
        cbind(tasks[list_index,], bigL[[list_index]])
    })
    bigL_column_labeled <- lapply(bigL_labeled, set_colnames, c(colnames(tasks), muscle_name_per_index))
    har_per_task_df <- bigL_column_labeled %>% dcrb
    plot(har_per_task_df$time, har_per_task_df$Fx)
    expect_true(all(where_muscles_have_unreasonable_values(har_per_task_df, muscle_name_per_index)))

    # p <- ggplot(har_per_task_df, aes(DI, PI, frame=time)) + geom_point()
    # gganimate::gganimate(p, "output.html")

})