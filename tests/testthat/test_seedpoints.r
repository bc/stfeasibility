########################################
test_that('we can extract 100 seeds for a given speed multiconstraint #23', {
	library(data.table)
    n_seeds <- 100
    #speed is fixed across this entire run below; is dependent on the rda used
    st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda")
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    activation_per_seed <- extract_n_seeds_from_rda_ste(st_res, 100)
    multiconstraint_per_seed <- pbapply(activation_per_seed, 2, function(task_0_seed_activation){
    	res <- merge_constraints(H_multiconstraint,assemble_equality_with_seed_point(task_0_seed_activation))
    	attr(res, "seed_activation") <- task_0_seed_activation
    	return(res)
    	})
    ex <- multiconstraint_per_seed[[44]]
    ex$constr <- ex$constr[23:nrow(ex$constr),]
    ex$dir <- ex$dir[23:length(ex$dir)]
    ex$rhs <- ex$rhs[23:length(ex$rhs)]


    lapply(multiconstraint_per_seed, function(constraint_with_seed_fixation){
    	points <- har_sample(constraint_with_seed_fixation, n_samples=1e5, eliminate=FALSE)
    	attr(points, "constraint_with_seed") <- constraint_with_seed_fixation
    	seed_id <- attr(constraint_with_seed_fixation, "seed_activation")
    	attr(points, "seed_activation") <- seed_id
    	target_filepath <- sprintf("/Volumes/GoogleDrive/My\ Drive/outputs/seed_evals/%s.rda",colnames(seed_id)[1])
    	saveRDS(points,target_filepath)
    	})
    })

