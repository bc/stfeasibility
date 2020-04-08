
wideScreen()
task_definitions <- generate_task_csvs_for_cat(9,5)
const_A <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, task_definitions$task_A, cat1$bounds_tuple_of_numeric)
const_B <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, task_definitions$task_B, cat1$bounds_tuple_of_numeric)
const_C <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, task_definitions$redirection_tasks, cat1$bounds_tuple_of_numeric)


p_per_C <- pbmclapply(const_C$constraints,har_sample,10000, mc.cores=8)
p_per_A <- pbmclapply(const_A$constraints,har_sample,10000, mc.cores=8)
p_per_B <- pbmclapply(const_B$constraints,har_sample,10000, mc.cores=8)

to_midpt <- diagonal_merge_constraint_list(const_C$constraints[c(1,9)])

    velocity_constraint <- generate_full_velocity_constraint(to_midpt, 0.5,
        0.5, 31)
constraint_with_velocity_requirements <- merge_constraints(to_midpt,
        velocity_constraint)

ress <- constraint_with_velocity_requirements %>% har_sample(1000)
if(all(c(
	every_solution_is_ind_valid(p_per_A, const_A$constraints),
	every_solution_is_ind_valid(p_per_B, const_B$constraints),
	every_solution_is_ind_valid(p_per_C, const_C$constraints)))){
	print("All sampled solutions meet set requirements")
} else{
	print("Error: sample does not match constraints")
}

Q
culled <- function(B, A, max_delta){
	a_connections_per_soln_in_b <- pbapply(B, 1, function(b_soln){
		max_delta_vals <- apply(A, 1, function(a_soln){
				return(max(abs(b_soln[1:31] - a_soln[1:31])))
				})
		print(summary(max_delta_vals))
		return(max_delta_vals < max_delta)
		})
	return(B[a_connections_per_soln_in_b,])
}
L <- lapply(p_per_C, function(x){
	x$solution_id <- 1:nrow(x)
	rr <- data.table(x)
	# rr %>% setorder("adf")
	return(rr[1:10000])
	})

L_out <- list()
#initialization
L_prime_i <- L[[1]]
for (i in seq(1,9)) {
	L_prime_i <- culled(L[[i + 1]], L_prime_i, 0.125)
	L_out <- c(L_prime, L_prime_i)
}




muscle_bounds <- get_muscle_bounds_for_rhs_task(nulltask)

nulltask <- const1$constraints[[1]]
get_muscle_bounds_for_rhs_task(nulltask)


nored <- pbmclapply(const1$constraints, eliminate_redundant)
nonred_then_diag <- diagonal_merge_constraint_list(nored)

bigg <- diagonal_merge_constraint_list(const1$constraints)

d <- fread("data/nine_task_equivalence_matrix.csv",skip=1)[,2:10]
d[is.na(d)] <- 0
c_in_cTx <- rep(1,ncol(bigg$constr))
#minimize l1 sum activation
lp_result <- lp("min", objective.in = c_in_cTx, const.mat = bigg$constr,
    const.dir = bigg$dir, const.rhs = bigg$rhs,
    compute.sens = 0)

pbapply(a,1,function(myrow){
	# matrix_subtraction where myrow is the nth element of the first task matrix
	# where b is the entire second task matrix
	starting_point_bigmat <- as.matrix(t(replicate(nrow(b), a[1,])))
	difference_matrix <- as.matrix(b) - starting_point_bigmat
	viable_row_indices <- sapply(difference_matrix, 1, function(x) which(max(abs(x)) < 0.01))
	return(viable_row_indices)
	})




##########

merged_const <- diagonal_merge_constraint_list(const1$constraints)
generate_and_add_velocity_constraint(merged_const, 1,
    1, 31)


directions <- cbind(rbind(diag(3),diag(3)*-1),zeros(6,3))
colnames(directions) <- rownames(cat1$H_matrix)
rownames(directions) <- c("dir_+x",
							"dir_+y",
							"dir_+z",
							"dir_-x",
							"dir_-y",
							"dir_-z")
directional_mvcs <- apply(directions,1,function(direction){
	solution <- maximize_wrench(cat1$H_matrix, cat1$bounds_tuple_of_numeric, direction = direction)
	return(solution$lambda_values)
	})
print(directional_mvcs)