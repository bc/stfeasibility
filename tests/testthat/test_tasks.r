context("generate_feasible patterns")





# Test finger in flexed posture, from frontiers2018 feasibility theory paper
JR <- rbind(c(-0.08941, -0.0447, -0.009249, 0.03669, 0.1421, 0.2087, -0.2138), c(-0.04689,
    -0.1496, 0.052, 0.052, 0.0248, 0, 0.0248), c(0.06472, 0.001953, -0.1518, -0.1518,
    0.2919, 0.0568, 0.2067), c(0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483,
    0.0001579, -0.000685))
maximal_tension_per_muscle <- c(123, 219, 23.52, 91.74, 21.6, 124.8, 129.6)
muscle_name_per_index <- c("FDP", "FDS", "EIP", "EDC", "LUM", "DI", "PI")

H_matrix <- JR %*% diag(maximal_tension_per_muscle)
bounds_tuple_of_numeric <- rep(list(list(lower = 0, upper = 1)), length(maximal_tension_per_muscle))
f_out = c(28, 0, 0, 0)

test_that("make feasible task trajectory", {
    # the set of lambdas by which to scale the maximum feasible force for a given
    # task direction.
    task_lambdas <- lapply(seq(0, 2 * pi, length.out = 100), sin) %>% dcc
    constr <- constraint_H_with_bounds(H_matrix, f_out, bounds_tuple_of_numeric)
    points <- har_sample(constr, 10000, 100)
    colnames(points) <- muscle_name_per_index
    # Generate points without delta constraints
    expect_true(all(colMins(points) > 0))
    expect_true(all(colMaxes(points) < 1))
    browser()
})