require(data.table)

# 
# st_res_input <- readRDS("tmp.rds")

# aa <- extract_n_seeds_from_rds_ste(st_res_input,2)
extract_n_seeds_from_rds_ste <- function(st_res, n_seeds){
	require(data.table)
	# saveRDS(st_res, "tmp.rds")
    seeds <- sample(1:max(st_res$muscle_trajectory), n_seeds, replace=FALSE)
    st_res_dt <- data.table(st_res)
    H_multiconstraint <- attr(st_res,"constraints_and_tasks")$nonredundant_constr
    only_seeds <- st_res_dt[st_res_dt$task_index == 0 & st_res_dt$muscle_trajectory%in%seeds,]    
    setorder(only_seeds, "muscle_trajectory")
    only_seeds[,muscle:=factor(only_seeds$muscle, levels = colnames(H_multiconstraint$constr)[1:7])]
    seeds <- data.frame(dcast(only_seeds, muscle~muscle_trajectory, value.var="activation"))
    activation_per_seed <- seeds[,-1]
	rownames(activation_per_seed) <- seeds[,1]
	return(activation_per_seed)
}

#puts a diag and -diag equality with the activations in task_0 for the input activations7
assemble_equality_with_seed_point <- function(seed_id, activations7,H_multiconstraint){
	library(data.table)
	equalityconst <- cbind(diag(7), matrix(0,7,42))
	rownames(equalityconst) <- rep(paste0(seed_id,"_task0_equality_w_seed_const"), nrow(equalityconst))
	print('names are')
	print(equalityconst)
	colnames(equalityconst) <- colnames(H_multiconstraint$const)
	equalit_constr_formatted <- create_equality_constraint(equalityconst, activations7)
	return(list(constr = equalit_constr_formatted$constr[1:7,], dir= rep("=",7),rhs = equalit_constr_formatted$rhs[1:7]))
}

trim_top_of_constraint <- function(constraint, nrows_to_rm){
	newversion <- constraint # mk copy
	newversion$constr <- constraint$constr[(nrows_to_rm+1):nrow(constraint$constr),]
	newversion$dir <- constraint$dir[(nrows_to_rm+1):length(constraint$dir)]
	newversion$rhs <- constraint$rhs[(nrows_to_rm+1):length(constraint$rhs)]
	return(newversion)
}
#target string must have a %s so it can put the ID in there
seed_sample_and_save <- function(constraint_with_seed_fixation, har_samples_per_seed, target_string){
    	points <- constraint_with_seed_fixation %>% har_sample(har_samples_per_seed, eliminate=TRUE)
    	attr(points, "constraint_with_seed") <- constraint_with_seed_fixation
    	seed_id <- attr(constraint_with_seed_fixation, "seed_id")
    	target_filepath <- sprintf(target_string,seed_id)
    	saveRDS(points, target_filepath)
    	return(target_filepath)
}

combine_unseeded_and_seeded_data_into_id_tall_df <- function(trajectories_unseeded, trajectories_per_seed){
	#prepare unseeded portion
	velocity_limit <- attr(trajectories_unseeded,"speed_limit")
	unseeded_dt <- data.table(trajectories_unseeded)
	rm(trajectories_unseeded) # reduce memory usage
	unseeded_dt[,seed_id := "Not Seeded"]
	unseeded_dt[,velocity_limit:=NULL]

	seed_based_trajectories <- pblapply(trajectories_per_seed, function(x){
		ddf <- trajectory_har_df_melt(x,7) %>% data.table
		# add a column called seed_id so we can keep track of where these 10k trajectories came from
		seed_id <- attr(attr(x, "constraint_with_seed"),"seed_id")
		ddf[,seed_id := seed_id]
		return(ddf)
	}) %>% rbindlist

	seed_vs_noseed_trajectories <- rbind(seed_based_trajectories,unseeded_dt[muscle_trajectory%in%seq(1,10000)])
	attr(seed_vs_noseed_trajectories,"velocity_limit") <- velocity_limit
	return(seed_vs_noseed_trajectories)
}

gen_freqpoly_seed_vs_unseeded <- function(seed_vs_noseed_trajectories, seed_id_interesting_idx = c(1,2,3)){
	    seed_id_interesting <- unique(seed_vs_noseed_trajectories$seed_id)[seed_id_interesting_idx]
	    seed_collection <- seed_vs_noseed_trajectories[seed_id%in%seed_id_interesting & seed_id!="Not Seeded"]
		p <- ggplot(seed_vs_noseed_trajectories,aes(activation))
		p <- p + geom_freqpoly(aes(y=..ncount.., fill=seed_id, col=seed_id, frame=seed_id),
			alpha=0.5, bins=30, position="identity", data = seed_collection)
		p <- p + geom_freqpoly(aes(y=..ncount..), alpha=0.5, bins=30, col="black", position="identity", data = seed_vs_noseed_trajectories[seed_id=="Not Seeded"]) 
		p <- p + facet_grid(task_index~factor(muscle, levels=muscle_name_per_index), scales="free_y", space="free_y") 
		p <- p + theme_classic() + ylab("Within-bin Volume wrt to mode") + xlab("Muscle activation (0 to 1 is 0 to 100%)")
		return(p)
    }

seed_vs_noseed_diff_speeds <- function(vel, n_seeds = 10, n_samples_per_seed=1e4){
	message('lets begin')
    fixed_velocity_constraint_speed <- vel
    my_H_matrix <- read.csv("data/fvc_hentz_2002.csv", row.names=1) %>% as.matrix
    tic <- Sys.time()
    st_res <- st_with_vel(my_H_matrix, fixed_velocity_constraint_speed, har_n=1e5)
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    activation_per_seed <- extract_n_seeds_from_rds_ste(st_res, n_seeds)
    multiconstraint_per_seed <- lapply(seq(1,ncol(activation_per_seed)), function(task_0_seed_activation){
    	# trim top which has task0 wrench requirements
    	seed_a <- activation_per_seed[,task_0_seed_activation]
    	seed_id <- colnames(activation_per_seed)[task_0_seed_activation]
    	res <- merge_constraints(trim_top_of_constraint(H_multiconstraint,22), assemble_equality_with_seed_point(seed_id, seed_a, H_multiconstraint))
    	attr(res, "seed_activation") <- seed_a
    	attr(res, "seed_id") <- seed_id
    	return(res)
    	})
    result_filepaths <- lapply(multiconstraint_per_seed, seed_sample_and_save,
     					target_string = paste0("outputs/seed_speed_%s_id_"%--%vel, "%s.rda"),
     					har_samples_per_seed = n_samples_per_seed)

	trajectories_per_seed <- lapply(result_filepaths, readRDS)
	seed_vs_noseed_trajectories <- combine_unseeded_and_seeded_data_into_id_tall_df(trajectories_unseeded=st_res, trajectories_per_seed=trajectories_per_seed)
	saveRDS(seed_vs_noseed_trajectories, "outputs/seed_vs_nospeed_talldf_speed_%s.rda"%--%vel)
    p <- gen_freqpoly_seed_vs_unseeded(seed_vs_noseed_trajectories, 1:3)
    p <- p + ggtitle("Velocity lim: %s"%--%fixed_velocity_constraint_speed)
	ggsave("outputs/seed_vs_noseed_trajectories_speed_%s_"%--%fixed_velocity_constraint_speed%>%time_dot("pdf"),p, width=10,height=10)
    message("DONE WITH SPEED OF %s"%--%vel)
    message(print(Sys.time() - tic))

	projection_str <- generate_pca_projection_plots(seed_vs_noseed_trajectories, suffix="_VEL_%s"%--%vel)
	saveRDS("outputs/projectionstr_%s.rds"%--%vel,projection_str)


	system("rclone copy outputs remote:outputs", wait=TRUE)
    }