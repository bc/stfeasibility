########################################
test_that('we can extract 100 seeds for a given speed multiconstraint #23', {
    n_seeds <- 100
    st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda")
    seeds <- sample(1:max(st_res$muscle_trajectory),n_seeds,replace=FALSE)
    st_res_dt <- data.table(st_res)
    only_seeds <- st_res_dt[muscle_trajectory%in%seeds & task_index == 0,activation,by=.(muscle_trajectory, muscle)]    
    setorder(only_seeds, "muscle_trajectory")
    only_seeds$muscle <- factor(only_seeds$muscle, levels = colnames(H_multiconstraint$constr)[1:7])
    seeds <- data.frame(dcast(only_seeds, muscle~muscle_trajectory, value.var="activation"))
    samp2 <- seeds[,-1]
	rownames(samp2) <- seeds[,1]
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    
    apply(samp2, 2, function(task_0_seed_activation){

    	})

    assemble_equality_with_seed_point <- function(activations7){
    	equalityconst <- cbind(diag(7), matrix(0,7,42))
		rownames(equalityconst) <- paste0(seeds[,1],"_task0_equality_w_seed_const")
		colnames(equalityconst) <- colnames(H_multiconstraint$const)
		equalit_constr_formatted <- create_equality_constraint(equalityconst, activations7)
		return(equalit_constr_formatted)
    })