########################################
test_that('we can extract 100 seeds for a given speed multiconstraint #23', {
	library(data.table)
    n_seeds <- 100
    #speed is fixed across this entire run below; is dependent on the rda used
    st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda")
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    activation_per_seed <- extract_n_seeds_from_rda_ste(st_res, 10)
    multiconstraint_per_seed <- lapply(seq(1,ncol(activation_per_seed)), function(task_0_seed_activation){
    	# trim top which has task0 wrench requirements
    	seed_a <- activation_per_seed[,task_0_seed_activation]
    	seed_id <- colnames(activation_per_seed)[task_0_seed_activation]
    	res <- merge_constraints(trim_top_of_constraint(H_multiconstraint,22),assemble_equality_with_seed_point(seed_id, seed_a))
    	attr(res, "seed_activation") <- seed_a
    	attr(res, "seed_id") <- seed_id
    	return(res)
    	})


    result_filepaths <- pbmclapply(multiconstraint_per_seed, seed_sample_and_save)

       
seeded_points <- ex%>% har_sample(1000)
	parcoords(seeded_points, reorderable = TRUE, brushMode = "1D-axes-multi", autoresize=TRUE, width=1900, height=500, alpha=0.1)
    })



test_that('pca for dimensionality of different slices', {
    trajectories <- data.table(readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda"))
    velocity_limit_fixed <- trajectories$velocity_limit[1]
	sampled_task0_points <- dcast(trajectories[task_index==0],muscle_trajectory~muscle, value.var="activation")
	sampled_task1_points <- dcast(trajectories[task_index==1],muscle_trajectory~muscle, value.var="activation")
	sampled_task2_points <- dcast(trajectories[task_index==2],muscle_trajectory~muscle, value.var="activation")
	sampled_task3_points <- dcast(trajectories[task_index==3],muscle_trajectory~muscle, value.var="activation")
	sampled_task4_points <- dcast(trajectories[task_index==4],muscle_trajectory~muscle, value.var="activation")
	sampled_task5_points <- dcast(trajectories[task_index==5],muscle_trajectory~muscle, value.var="activation")
	sampled_task6_points <- dcast(trajectories[task_index==6],muscle_trajectory~muscle, value.var="activation")
	all_combined <- dcast(trajectories,muscle_trajectory~muscle+task_index, value.var="activation")
	big_pca_res <- prcomp(all_combined[,2:ncol(all_combined)],scale=FALSE,center=FALSE)
	trajectories_per_task_index <- list(sampled_task0_points,
									sampled_task1_points,
									sampled_task2_points,
									sampled_task3_points,
									sampled_task4_points,
									sampled_task5_points,
									sampled_task6_points)
	pca_results <- lapply(1:length(trajectories_per_task_index), function(i){
		# trim muscle_trajectory number off of the trajectory.
		pca_res_nonseed <- prcomp(trajectories_per_task_index[[i]][,2:8], center=FALSE, scale=FALSE)
		projected_points_from_nonseed <- scale(sampled_task0_points[,2:8], pca_res_nonseed$center, pca_res_nonseed$scale) %*% pca_res_nonseed$rotation
		return(list(pca_res_nonseed = pca_res_nonseed, projected_points_from_nonseed=projected_points_from_nonseed, task_index=i-1))
		})
	proj_points_per_task <- pbmclapply(1:length(pca_results), function(i){
		df <-as.data.frame(pca_results[[i]]$projected_points_from_nonseed)
		df$task_index = i-1
		return(df)
		}) %>% rbindlist

	trajectories_per_seed <- lapply(dir("/Volumes/GoogleDrive/My\ Drive/outputs/seed_evals/", full.names=TRUE), readRDS)
	splitup <- pblapply(trajectories_per_seed[1:10], function(x){
		ddf <- trajectory_har_df_melt(x,7) %>% data.table
		# add a column called seed_id so we can keep track of where these 10k trajectories came from
		seed_id <- attr(attr(x, "constraint_with_seed"),"seed_id")
		ddf[, ("seed_id") := seed_id]
		return(ddf)
	})

	seed_based_trajectories <- rbindlist(splitup)
	trajectories[,("seed_id") := "Not Seeded"]
	trajectories[,velocity_limit:=NULL]
	seed_vs_noseed_trajectories <- rbind(seed_based_trajectories,trajectories[muscle_trajectory%in%seq(1,10000)])
	seed_vs_noseed_trajectories[,("is_constrained_by_seed"):= seed_id == "Not Seeded"]
	attr(seed_vs_noseed_trajectories,"velocity_limit") <- velocity_limit_fixed
	saveRDS(seed_vs_noseed_trajectories, sprintf("/Volumes/GoogleDrive/My\ Drive/outputs/seed_vs_noseed_trajectories_at_speedlimit_%s.rds",velocity_limit_fixed))

	p <- ggplot(seed_vs_noseed_trajectories ) 
	p <- p + geom_histogram(aes(activation, fill=seed_id),alpha=0.5, bins=50, position="identity", data = seed_vs_noseed_trajectories[seed_id!="Not Seeded"]) 
	p <- p + geom_histogram(aes(activation),alpha=0.5, bins=50, fill="black", position="identity", data = seed_vs_noseed_trajectories[seed_id=="Not Seeded"]) 
	p <- p + facet_grid(task_index~factor(muscle, levels=muscle_name_per_index), scales="free_y", space="free_y") 
	p <- p + theme_classic()
	ggsave("seed_vs_noseed_trajectories"%>%time_dot("png"),p, width=20,height=20)

	p <- ggplot(seed_vs_noseed_trajectories) 
	p <- p + geom_bar(aes(sd(activation), fill=seed_id),alpha=0.5, bins=50, position="identity", data = seed_vs_noseed_trajectories[seed_id!="Not Seeded"]) 
	p <- p + geom_histogram(aes(activation),alpha=0.5, bins=50, fill="black", position="identity", data = seed_vs_noseed_trajectories[seed_id=="Not Seeded"]) 
	p <- p + facet_grid(task_index~factor(muscle, levels=muscle_name_per_index), scales="free_y", space="free_y") 
	p <- p + theme_classic()
	ggsave("seed_vs_noseed_trajectories"%>%time_dot("png"),p, width=20,height=20)


	# show an example of some trajectories, and their derivatives
	desired_trajectories <- seq(1,10)
	trajectories[,("muscle_and_trajectory"):=paste(factor(muscle, levels=muscle_name_per_index),muscle_trajectory)]
	selected_trajectories <- trajectories[muscle_trajectory%in%desired_trajectories & seed_id=="Not Seeded"]
	p_example_trajectory <- ggplot(selected_trajectories, aes(task_index,activation, group=as.factor(muscle_and_trajectory), col=as.factor(muscle_trajectory)))
	p_example_trajectory <- p_example_trajectory + geom_line() + facet_grid(~factor(muscle, levels=muscle_name_per_index))
	p_example_trajectory <- p_example_trajectory + theme_classic() + theme(legend.title = element_blank())
	ggsave("example_trajectories_for_vel_point12676"%>%time_dot("png"),p_example_trajectory, width=10,height=5)

	differentiated <- selected_trajectories[, .(task_index, dot=c(diff(activation),0)),by=.(muscle,muscle_trajectory)]
	differentiated[,("muscle_and_trajectory"):=paste(factor(muscle, levels=muscle_name_per_index),muscle_trajectory)]
	
	dot_plot <- ggplot(differentiated[task_index < 6], aes(task_index, dot, group=as.factor(muscle_and_trajectory), col=as.factor(muscle_trajectory))) +  geom_line() + facet_grid(~factor(muscle, levels=muscle_name_per_index))
	dot_plot <- dot_plot + theme_classic() + theme(legend.title = element_blank()) + ylab("delta activation between tasks i and i+1") + geom_hline(yintercept=as.numeric(velocity_limit_fixed),col="#990000", linetype="dashed") + geom_hline(yintercept=-as.numeric(velocity_limit_fixed),col="#990000", linetype="dashed")

	p_activation_and_dot <- grid.arrange(p_example_trajectory,dot_plot,ncol=1)
	ggsave("sample_trajectories_for_vel_point1267"%>%time_dot("pdf"), p_activation_and_dot, width=10, height=4, dpi=600)

	# show range instead:


	summary(pca_res)
	print(pca_res)
	projected <- scale(sampled_task0_points[,2:8], pca_res$center, pca_res$scale) %*% pca_res$rotation
	ggplot(projected%>% as.data.frame, aes(PC1,PC2))+ geom_point(size=0.1,alpha=0.2)
	})