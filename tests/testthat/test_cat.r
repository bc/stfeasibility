
wideScreen()
# generate_task_csvs_for_cat(100,1)
source('R/base_constraints.r')
source('R/tasks.r')

mat_path <- "~/Documents/GitHub/bc/stfeasibility/data/Sohn2013_hinlimb_models.mat"
cat_mat <- readMat(mat_path)
cat1 <- get_cat_H_matrix(cat_mat, 1)


taskdf <- fread('output/task_A.csv')
# TODO debug issue of Error in -b_vec : invalid argument to unary operator
# taskdf$y  <- as.numeric(taskdf$y)
# taskdf$z  <- as.numeric(taskdf$z)
# taskdf$tx <- as.numeric(taskdf$tx)
# taskdf$ty <- as.numeric(taskdf$ty)
# taskdf$tz <- as.numeric(taskdf$tz)
const1 <- generate_tasks_and_corresponding_constraints_via_df(cat1$H_matrix, taskdf, cat1$bounds_tuple_of_numeric)



nulltask <- const1$constraints[[1]]
muscle_bounds <- get_muscle_bounds_for_rhs_task(nulltask)
bigg <- diagonal_merge_constraint_list(const1$constraints[1:2])

bigg_e <- eliminate_redundant(bigg)
rr1 <- bigg %>% har_sample(100)

bigg <- diagonal_merge_constraint_list(const1$constraints[1])
rr2 <- bigg%>% har_sample(100)


diagonal_merge_constraints(seg1,const1$constraints[[3]],3)

# pbmclapply(seq(1, length(const1$constraints)), function(x) {
# 	tic = Sys.time()
# 	points <- const1$constraints[[x]] %>% har_sample(1e5)
# 	fwrite(points,paste0("A/",Sys.time(),"_index_",as.character(x),"_.csv"))
# 	toc = Sys.time()
# 	print(paste("per:", as.numeric(toc-tic)))
# }, mc.cores=8)


get_sorted_csv_filenames_for_taskA <- function(){
	base <- "/Volumes/GoogleDrive/My\ Drive/st/task_A"
	raw_filenames <- dir(base)
	first_underscores <- sub("^[^_]*_", "", raw_filenames)
	second_underscores <- sub("^[^_]*_", "", first_underscores)
	numbers <- as.integer(gsub("_.*","",second_underscores))
	sorted_indices <- order(numbers)
	return(file.path(base,raw_filenames[sorted_indices]))
}

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