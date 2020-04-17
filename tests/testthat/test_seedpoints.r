########################################
test_that('we can extract 100 seeds for a given speed multiconstraint #23', {
	library(data.table)
    n_seeds <- 100
    #speed is fixed across this entire run below; is dependent on the rda used
    st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda")
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    activation_per_seed <- extract_n_seeds_from_rda_ste(st_res, 100)
    multiconstraint_per_seed <- pbapply(activation_per_seed, 2, function(task_0_seed_activation){
    	# trim top which has task0 wrench requirements
    	res <- merge_constraints(trim_top_of_constraint(H_multiconstraint,22),assemble_equality_with_seed_point(task_0_seed_activation))
    	attr(res, "seed_activation") <- task_0_seed_activation
    	return(res)
    	})


    pbmclapply(multiconstraint_per_seed, function(constraint_with_seed_fixation){
    	points <- har_sample(constraint_with_seed_fixation, n_samples=1e5, eliminate=TRUE)
    	attr(points, "constraint_with_seed") <- constraint_with_seed_fixation
    	seed_id <- attr(constraint_with_seed_fixation, "seed_activation")
    	attr(points, "seed_activation") <- seed_id
    	target_filepath <- sprintf("/Volumes/GoogleDrive/My\ Drive/outputs/seed_evals/%s.rda",colnames(seed_id)[1])
    	saveRDS(points, target_filepath)
    	}, mc.cores=8)


        ex <- multiconstraint_per_seed[[44]]
    data(mtcars)
seeded_points <- ex%>% har_sample(1000)
parcoords(seeded_points, reorderable = TRUE, brushMode = "1D-axes-multi", autoresize=TRUE, width=1900, height=500,alpha=0.1)



    })


