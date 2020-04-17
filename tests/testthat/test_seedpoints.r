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



