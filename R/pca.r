generate_pca_projection_plots <- function(seed_vs_noseed_trajectories_rda_str, suffix=""){
		
	
	message("Assembling nonseed trajectories")
	noseed_points_per_task<- lapply(0:6, function(task_i){
		rr <- dcast(seed_vs_noseed_trajectories_rda_str[task_index==task_i & seed_id=="Not Seeded"], muscle_trajectory~muscle,value.var="activation")
		ggg<- rr[,c("FDP","FDS","EIP","EDC","LUM","DI","PI")]
		return(ggg)
		})
	
	message("-Getting PCA Models for each of the 7 tasks")
	message("-Projecting non-seeded trajectories onto 7 sets of PC vectors")
	pca_results <- lapply(0:6, function(i){
		# trim muscle_trajectory number off of the trajectory.
		pca_res_nonseed <- prcomp(noseed_points_per_task[[i+1]], center=FALSE, scale=FALSE)
		projected_points_from_nonseed <- scale(noseed_points_per_task[[i+1]], pca_res_nonseed$center, pca_res_nonseed$scale) %*% pca_res_nonseed$rotation
		return(list(pca_res_nonseed = pca_res_nonseed, projected_points_from_nonseed=projected_points_from_nonseed, task_index=i))
		})

	message("-Visualizing loadings over each task")
	normalize_to_max_abs_value <- TRUE
	normalized_loadings <- lapply(0:6,function(x){
		loadings <- t(pca_results[[x+1]]$pca_res_nonseed$rotation)
		if (normalize_to_max_abs_value){
			normalized_res <- apply(loadings,1, function(x) divide_vector_by_max_of_vectors_abs_value(x))
			dff <- melt(normalized_res)
			dff$task_index <- x
		return(dff)
		} else {
			return(res)
		}
	}) %>% rbindlist

	message("--Plotting (1/2)")
	p1 <- ggplot(normalized_loadings[Var2 %in%c("PC1","PC2","PC3")], aes(task_index,value, col=Var2)) + geom_path() + facet_grid(Var2~Var1) + theme_classic() + ylab("loading value")
	var_exp_per_task <- lapply(0:6,function(x){
		my_model <- pca_results[[x+1]]
		res <- summary(my_model$pca_res_nonseed)$importance
		var_exp <- melt(res[2,]) %>% data.table
		var_exp$task_index <- x
		var_exp$PC <- paste0("PC",1:7)
		return(var_exp)
		}) %>% rbindlist

	p2 <- ggplot(var_exp_per_task, aes(task_index,value, col=PC)) + geom_line(size=2) + geom_point(size=3) + ylab("Variance Explained for PC at a given task index") + theme_classic()
	message("--Plotting (2/2)")
	ggsave("figures/pca_loadings_for_nonseed_%s.pdf" %--% suffix %>% time_dot("pdf"), grid.arrange(grobs=list(p1,p2),cols=1))

	message("-Projecting seed trajectories onto the 7 PC models. Performed for each of 10 seed points")
	nonseeded_proj_points_per_task <- pbmclapply(0:6, function(i){
		dfa <-as.data.frame(pca_results[[i+1]]$projected_points_from_nonseed)
		dfa$task_index = i
		dfa$seed_id <- "Not Seeded"
		return(dfa)
		}) %>% rbindlist
	ids <- unique(seed_vs_noseed_trajectories_rda_str$seed_id)[1:10]
	seeded_points_per_multiple_seedpoints <- lapply(ids, function(input_seed_id){
	seeded_points_per_task <- lapply(0:6, function(task_i){
		ddf <- dcast(seed_vs_noseed_trajectories_rda_str[task_index==task_i & seed_id==input_seed_id], muscle_trajectory~muscle,value.var="activation")
		ddf2 <- ddf[,c("FDP","FDS","EIP","EDC","LUM","DI","PI")]
		pca_model_for_task <- pca_results[[task_i+1]]$pca_res_nonseed # extract the model
		projected_points_from_nonseed <- scale(ddf2, pca_model_for_task$center, pca_model_for_task$scale) %*% pca_model_for_task$rotation %>% as.data.frame
		projected_points_from_nonseed$task_index <- task_i
		projected_points_from_nonseed$seed_id <- input_seed_id
		return(projected_points_from_nonseed%>%data.table)
		}) %>% rbindlist
	return(seeded_points_per_task)}) %>% rbindlist
	
	nonseed_and_seed_projected <- rbind(nonseeded_proj_points_per_task, seeded_points_per_multiple_seedpoints)	
	nonseed_and_seed_projected$seed_id <- factor(nonseed_and_seed_projected$seed_id)
	message('--Plotting')
	# color the projected coordinates by the distance that point is from the 2D plane?
	p <- ggplot(nonseed_and_seed_projected, aes(PC1,PC2,col=seed_id)) 
	p <- p + geom_point(size=0.05, alpha=0.5,shape=1, col="grey", data=nonseed_and_seed_projected[seed_id=="Not Seeded"]) 
	p <- p + geom_point(size=0.1, alpha=0.5,shape=1, data=nonseed_and_seed_projected[seed_id!="Not Seeded"]) 
	p <- p + facet_grid(~task_index)
	p <- p + coord_fixed() 
	p <- p + theme_classic()
	p <- p + stat_chull(fill=NA, col="black")
	p <- p + theme(panel.grid = element_blank(), panel.border = element_blank())
	ggsave("figures/PC_view_of_st_tunnel_%s"%--% suffix %>%time_dot("pdf"), p)


	# TODO a .50 task
}