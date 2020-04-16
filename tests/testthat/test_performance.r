skip("times aren't significantly different when velocity requirements differ")
test_that("mbm har multiconstraint velocity", {
		mbm <- microbenchmark(
			`no velocity` = {
		    a <- compose_velocity_constraint(multiconstraint, 0.042, 0.042) %>% har_sample(1e2)
		    print(paste(Sys.time()))}, 
			`half` = {
		    b <- compose_velocity_constraint(multiconstraint, 0.5, 0.5) %>% har_sample(1e2)
		    print(paste(Sys.time()))}, 
		    `0.25` = {
		    c <- compose_velocity_constraint(multiconstraint, 0.25, 0.25) %>% har_sample(1e2)
		    print(paste(Sys.time()))}, 
		    `0.125` = {
		    d <- compose_velocity_constraint(multiconstraint, 0.125, 0.125) %>% har_sample(1e2)
		    print(paste(Sys.time()))}, 
		    `0.0625` = {
		    e <- compose_velocity_constraint(multiconstraint, 0.0625, 0.0625) %>% har_sample(1e2)
		    print(paste(Sys.time()))}, 
			`too small bound` = {
		    f <- compose_velocity_constraint(multiconstraint, 1e-5, 1e-5) %>% har_sample(1e2)
		    print(paste(Sys.time()))}, 
		times = 1)
		browser()
})


test_that("mbm har multiconstraint", {
		mbm <- microbenchmark(
		"har_1e2" = {multiconstraint %>% har_sample(1e2); print("1/7" %>% paste(Sys.time()))},
		"har_1e3" = {multiconstraint %>% har_sample(1e3); print("1/7" %>% paste(Sys.time()))},
		times=1
	)
})

test_that("is feasible", {

})

test_that("mbm har multiconstraint velocity", {
 		expect_false(is_feasible(compose_velocity_constraint(multiconstraint, 0,0)))
 		expect_false(is_feasible(compose_velocity_constraint(multiconstraint, 0.042,0.042)))
 		expect_true(is_feasible(compose_velocity_constraint(multiconstraint, 0.5,0.5)))
 		expect_true(is_feasible(compose_velocity_constraint(multiconstraint, 1,1)))

 		expect_true(constraint_is_feasible(compose_velocity_constraint(multiconstraint, 1,1),7))
 		expect_false(constraint_is_feasible(compose_velocity_constraint(multiconstraint, 0,0),7))
 		res <- lpsolve_muscles_for_task("min", multiconstraint, 7)
 		constraint_velocity <- compose_velocity_constraint(multiconstraint, 0,0)
		plot_constraint_matrix(constraint_velocity)
		mbm <- microbenchmark(
		"1e3" = {a <- constraint_velocity %>% har_sample(1e2)},
		times=2
	)
})


skip("Already_produced as r_non_redundant in gloabl environment. Re-run to overwrite.")
test_that("evaluate performance gains with eliminating redundancy prior to har sampling",{
	r_non_redundant <- eliminateRedundant(st_task_and_constraint[[1]])
	devtools::use_data(r_non_redundant, overwrite = TRUE)
})

skip("Yes, we should set eliminate to FALSE to prevent re-removing redundancies")
test_that("should we use eliminate if we have pre-eliminated redundant constraints?", {
    	mbm <- microbenchmark(
		"6 tasks, 1e2 point pre_eliminated" 		 = {a <- r_non_redundant %>% har_sample(1e2); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e2 point pre_eliminated_override" = {a <- r_non_redundant %>% har_sample(1e2, eliminate=FALSE); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e3 point pre_eliminated" 		 = {a <- r_non_redundant %>% har_sample(1e3); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e3 point pre_eliminated_override" = {a <- r_non_redundant %>% har_sample(1e3, eliminate=FALSE); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e4 point pre_eliminated" 		 = {a <- r_non_redundant %>% har_sample(1e4); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e4 point pre_eliminated_override" = {a <- r_non_redundant %>% har_sample(1e4, eliminate=FALSE); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e5 point pre_eliminated" 		 = {a <- r_non_redundant %>% har_sample(1e5); print("1/5" %>% paste(Sys.time()))},
		"6 tasks, 1e5 point pre_eliminated_override" = {a <- r_non_redundant %>% har_sample(1e5, eliminate=FALSE); print("1/5" %>% paste(Sys.time()))},
		times=1
	)
})