context("testing singleconstraint")
test_that("H matrix with defined task on rhs has 0 sd for lambda", {
    lhs <- constraint_H_with_bounds(H_matrix_mini, c(1), bounds_tuple_of_numeric_mini)
    muscle_solutions <- lhs %>% eliminate_redundant %>% pb_har_sample(10000, mc.cores = 8,
        eliminate = FALSE) %>% as.matrix
    wrench_outputs <- H_matrix_mini %*% t(muscle_solutions) %>% as.numeric
    expect_equal(wrench_outputs, rep(1, nrow(muscle_solutions)))
    expect_equal(sd(wrench_outputs), 0)
})

test_that("H matrix mini can be combined and har'd successfully", {
    lhszero <- constraint_H_with_bounds(H_matrix_mini, c(0), bounds_tuple_of_numeric_mini)
    lhshalf <- constraint_H_with_bounds(H_matrix_mini, c(0.5), bounds_tuple_of_numeric_mini)
    lhsone <- constraint_H_with_bounds(H_matrix_mini, c(1), bounds_tuple_of_numeric_mini)
    list_of_constraints <- list(lhszero, lhshalf, lhsone)
    trajectory_miniH_constr <- diagonal_merge_constraint_list(list_of_constraints)
    context('make sure row amount matches')
    sum_rows <- sapply(list_of_constraints, function(x) nrow(x$constr)) %>% sum
    expect_equal(nrow(trajectory_miniH_constr$constr), sum_rows)
    
    context('make sure solutions are valid within the given jumboconstraint')
    muscle_solutions <- trajectory_miniH_constr %>% eliminate_redundant %>% pb_har_sample(10000,
        mc.cores = 8, eliminate = FALSE) %>% as.matrix
    expect_true(all(evaluate_solutions(muscle_solutions, trajectory_miniH_constr)))

    context('run har and split on independent constraints')
    har_constraint_pairs_independent <- har_and_split_trajectory_constraint(trajectory_miniH_constr, list_of_constraints,
    num_muscles=3, har_samples=1e5)

    context("now try with velocity constraints")
    st_constraint <- generate_and_add_velocity_constraint(trajectory_miniH_constr, 0.5,
    0.5, num_muscles=3)
    expect_true(constraint_is_feasible(st_constraint))
    plot_constraint_matrix(st_constraint)
    har_constraint_pairs_velocity_constrained <- har_and_split_trajectory_constraint(trajectory_miniH_constr, list_of_constraints,
    num_muscles=3, har_samples=1e5)
})