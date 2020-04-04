
wideScreen()
generate_task_csvs_for_cat(100,1)
source('R/base_constraints.r')
source('R/tasks.r')

mat_path <- "~/Documents/GitHub/bc/stfeasibility/data/Sohn2013_hinlimb_models.mat"
cat_mat <- readMat(mat_path)
cat1 <- get_cat_H_matrix(cat_mat, 1)


taskdf <- fread('output/task_A.csv')
# TODO debug issue of Error in -b_vec : invalid argument to unary operator
taskdf$y  <- as.numeric(taskdf$y)
taskdf$z  <- as.numeric(taskdf$z)
taskdf$tx <- as.numeric(taskdf$tx)
taskdf$ty <- as.numeric(taskdf$ty)
taskdf$tz <- as.numeric(taskdf$tz)
const1 <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, taskdf, cat1$bounds_tuple_of_numeric)





nulltask <- const1$constraints[[1]]
muscle_bounds <- get_muscle_bounds_for_rhs_task(nulltask)


pbmclapply(seq(1, length(const1$constraints)), function(x) {
	tic = Sys.time()
	points <- const1$constraints[[x]] %>% har_sample(1e5)
	fwrite(points,paste0("A/",Sys.time(),"_index_",as.character(x),"_.csv"))
	toc = Sys.time()
	print(paste("per:", as.numeric(toc-tic)))
}, mc.cores=8)

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