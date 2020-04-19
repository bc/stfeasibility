require(data.table)
extract_n_seeds_from_rda_ste <- function(st_res, n_seeds){
	require(data.table)
    seeds <- sample(1:max(st_res$muscle_trajectory),n_seeds,replace=FALSE)
    st_res_dt <- data.table(st_res)
    only_seeds <- st_res_dt[muscle_trajectory%in%seeds & task_index == 0,activation,by=.(muscle_trajectory, muscle)]    
    setorder(only_seeds, "muscle_trajectory")
    only_seeds[,muscle:=factor(only_seeds$muscle, levels = colnames(H_multiconstraint$constr)[1:7])]
    seeds <- data.frame(dcast(only_seeds, muscle~muscle_trajectory, value.var="activation"))
    activation_per_seed <- seeds[,-1]
	rownames(activation_per_seed) <- seeds[,1]
	return(activation_per_seed)
}

#puts a diag and -diag equality with the activations in task_0 for the input activations7
assemble_equality_with_seed_point <- function(seed_id, activations7){
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
	#TODO rm
	seed_vs_noseed_trajectories[,("is_constrained_by_seed"):= seed_id == "Not Seeded"]
	attr(seed_vs_noseed_trajectories,"velocity_limit") <- velocity_limit_fixed
	return(seed_vs_noseed_trajectories)
}