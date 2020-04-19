########################################
test_that('we can extract 100 seeds for a given speed multiconstraint #23', {
	library(data.table)
    n_seeds <- 100
    #speed is fixed across this entire run below; is dependent on the rda used
    st_res <- st_with_vel(my_H_matrix, 0.126767676, har_n=1e5)
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


    result_filepaths <- pbmclapply(multiconstraint_per_seed, seed_sample_and_save, har_samples_per_seed = 1e4, mc.cores=detectCores(all.tests = FALSE, logical = TRUE))

       
	seeded_points <- ex%>% har_sample(1000)
	parcoords(seeded_points, reorderable = TRUE, brushMode = "1D-axes-multi", autoresize=TRUE, width=1900, height=500, alpha=0.1)
    })


test_that("pca projections for each task-poltope projected", {
	seed_vs_noseed_trajectories <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/seed_vs_noseed_trajectories_at_speedlimit_0.126767676767677.rds")
	generate_pca_projection_plots(seed_vs_noseed_trajectories, "speedlimit_pt1267")
	})


test_that('we can combine the unseeded and seeded trajectories', {

    trajectories <- data.table(readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda"))
    velocity_limit_fixed <- trajectories$velocity_limit[1]
    
	trajectories_per_seed <- lapply(dir("/Volumes/GoogleDrive/My\ Drive/outputs/seed_evals/", full.names=TRUE), readRDS)
	seed_vs_noseed_trajectories <- combine_unseeded_and_seeded_data_into_id_tall_df(trajectories_unseeded=trajectories, trajectories_per_seed=trajectories_per_seed)
	saveRDS(seed_vs_noseed_trajectories, sprintf("/Volumes/GoogleDrive/My\ Drive/outputs/seed_vs_noseed_trajectories_at_speedlimit_%s.rds",velocity_limit_fixed))

})

test_that("seed vs noseed from scratch", {
	library(data.table)
    #speed is fixed across this entire run below; is dependent on the rda used
    fixed_velocity_constraint_speed <- .126767
    my_H_matrix <- read.csv("data/fvc_hentz_2002.csv", row.names=1) %>% as.matrix
    message('expect 10m on mac')
    st_res <- st_with_vel(my_H_matrix, fixed_velocity_constraint_speed, har_n=1e5)
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    activation_per_seed <- extract_n_seeds_from_rda_ste(st_res, 10)
    multiconstraint_per_seed <- lapply(seq(1,ncol(activation_per_seed)), function(task_0_seed_activation){
    	# trim top which has task0 wrench requirements
    	seed_a <- activation_per_seed[,task_0_seed_activation]
    	seed_id <- colnames(activation_per_seed)[task_0_seed_activation]
    	res <- merge_constraints(trim_top_of_constraint(H_multiconstraint,22), assemble_equality_with_seed_point(seed_id, seed_a))
    	attr(res, "seed_activation") <- seed_a
    	attr(res, "seed_id") <- seed_id
    	return(res)
    	})
    result_filepaths <- pblapply(multiconstraint_per_seed, seed_sample_and_save,
     target_string = "/Volumes/GoogleDrive/My\ Drive/outputs/seed_evals_medium_speed/%s.rda",
     har_samples_per_seed = 1e4),
     mc.cores=detectCores(all.tests = FALSE, logical = TRUE))

	trajectories_per_seed <- lapply(result_filepaths, readRDS)
	seed_vs_noseed_trajectories <- combine_unseeded_and_seeded_data_into_id_tall_df(trajectories_unseeded=st_res, trajectories_per_seed=trajectories_per_seed)
	saveRDS(seed_vs_noseed_trajectories, "/Volumes/GoogleDrive/My\ Drive/outputs/medium_speedlimit_seed_vs_noseed.rds")

	})
test_that('effect of a seed on downstream polytope projections onto each muscle', {
	seed_vs_noseed_trajectories <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/seed_vs_noseed_trajectories_at_speedlimit_0.126767676767677.rds")
    velocity_limit_fixed <- attr(seed_vs_noseed_trajectories,"velocity_limit")

	p <- ggplot(seed_vs_noseed_trajectories,aes(activation))
	p <- p + geom_freqpoly(aes(y=..ncount.., fill=seed_id,col=seed_id, frame=seed_id),
		alpha=0.5, bins=30, position="identity", data = seed_vs_noseed_trajectories[seed_id!="Not Seeded"])
	p <- p + geom_freqpoly(aes(y=..ncount..), alpha=0.5, bins=30, col="black", position="identity", data = seed_vs_noseed_trajectories[seed_id=="Not Seeded"]) 
	p <- p + facet_grid(task_index~factor(muscle, levels=muscle_name_per_index), scales="free_y", space="free_y") 
	p <- p + theme_classic() + ylab("Within-bin Volume wrt to mode") + xlab("Muscle activation (0 to 1 is 0 to 100%)")
	ggsave("seed_vs_noseed_trajectories"%>%time_dot("pdf"),p, width=20,height=20)
	htmlwidgets::saveWidget(ggplotly(p), "index.html")

	# just show 3
	p <- ggplot(seed_vs_noseed_trajectories,aes(activation))
	seed_id_interesting <- unique(seed_vs_noseed_trajectories$seed_id)[1:3]
	noseed_selection <- seed_vs_noseed_trajectories[seed_id%in%seed_id_interesting & seed_id!="Not Seeded"]
	p <- p + geom_freqpoly(aes(y=..ncount.., col=seed_id), alpha=0.5, bins=30, position="identity", data = noseed_selection)
	p <- p + geom_freqpoly(aes(y=..ncount..), alpha=0.5, bins=30, col="black", position="identity", data = seed_vs_noseed_trajectories[seed_id=="Not Seeded"]) 
	p <- p + facet_grid(task_index~factor(muscle, levels=muscle_name_per_index)) + coord_fixed()
	p <- p + theme_classic() + ylab("Within-bin Volume wrt to mode") + xlab("Muscle activation (0 to 1 is 0 to 100%)")
	ggsave("unseeded_and_seed"%>%time_dot("pdf"), p, width=8,height=10)
})

test_that('Show 10 unseeded trajectories', {
	trajectories <- data.table(readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda"))
    velocity_limit_fixed <- attr(seed_vs_noseed_trajectories,"velocity_limit")

	# show an example of some trajectories, and their derivatives
	desired_trajectories <- seq(1,10)
	trajectories[,("muscle_and_trajectory"):=paste(factor(muscle, levels=muscle_name_per_index), muscle_trajectory)]
	selected_trajectories <- trajectories[muscle_trajectory%in%desired_trajectories]
	p_example_trajectory <- ggplot(selected_trajectories, aes(task_index,activation, group=as.factor(muscle_and_trajectory), col=as.factor(muscle_trajectory)))
	p_example_trajectory <- p_example_trajectory + geom_line() + facet_grid(~factor(muscle, levels=muscle_name_per_index))
	p_example_trajectory <- p_example_trajectory + theme_classic() + theme(legend.title = element_blank())

	differentiated <- selected_trajectories[, .(task_index, dot=c(diff(activation),0)),by=.(muscle,muscle_trajectory)]
	differentiated[,("muscle_and_trajectory"):=paste(factor(muscle, levels=muscle_name_per_index),muscle_trajectory)]
	
	dot_plot <- ggplot(differentiated[task_index < 6], aes(task_index, dot, group=as.factor(muscle_and_trajectory), col=as.factor(muscle_trajectory))) +  geom_line() + facet_grid(~factor(muscle, levels=muscle_name_per_index))
	dot_plot <- dot_plot + theme_classic() + theme(legend.title = element_blank()) + ylab("delta activation between tasks i and i+1") + geom_hline(yintercept=as.numeric(velocity_limit_fixed),col="#990000", linetype="dashed") + geom_hline(yintercept=-as.numeric(velocity_limit_fixed),col="#990000", linetype="dashed")

	p_activation_and_dot <- grid.arrange(p_example_trajectory,dot_plot,ncol=1)
	ggsave("sample_trajectories_for_vel_point1267"%>%time_dot("pdf"), p_activation_and_dot, width=10, height=4, dpi=600)
	})