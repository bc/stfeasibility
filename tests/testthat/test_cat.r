
wideScreen()
task_definitions <- generate_task_csvs_for_cat(9,5)
const_A <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, task_definitions$task_A, cat1$bounds_tuple_of_numeric)
const_B <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, task_definitions$task_B, cat1$bounds_tuple_of_numeric)
const_C <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, task_definitions$redirection_tasks, cat1$bounds_tuple_of_numeric)


p_per_A <- pbmclapply(const_A$constraints,har_sample,10000, mc.cores=8)
p_per_B <- pbmclapply(const_B$constraints,har_sample,10000, mc.cores=8)
p_per_C <- pbmclapply(const_C$constraints,har_sample,10000, mc.cores=8)

every_solution_is_ind_valid(p_per_A, const_A$constraints)
# see if the solutions actually meet the constraints:
every_solution_is_ind_valid <- function(soln_df_list, constraints_list){

ress <- all(lapply(1:length(soln_df_list), function (i){
	return(
		all(evaluate_solutions(soln_df_list[[i]], constraints_list[[i]], tol=1e-4))
		)
	})%>%dcc)
return(ress)
}
# they do!



nulltask <- const1$constraints[[1]]
muscle_bounds <- get_muscle_bounds_for_rhs_task(nulltask)
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


 har_list <- pbmclapply(seq(1, length(const1$constraints)), function(x) {
	tic = Sys.time()
	points <- const1$constraints[[x]] %>% har_sample(1e5)
	toc = Sys.time()
	print(paste("per:", as.numeric(toc-tic)))

	# Sort by first row
	return(points[order(points[,1]),])
}, mc.cores=8)


bigdt <- pblapply(get_sorted_csv_filenames_for_taskA(), fread) %>% rbindlist(id=TRUE)
wtask <- merge(bigdt, taskdf, ".id")

taskdf$.id <- 1:nrow(taskdf)
pointclouds <- split(wtask, wtask$.id)

a <- pointclouds[[1]] #all assumed to be viable
b <- pointclouds[[2]]

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