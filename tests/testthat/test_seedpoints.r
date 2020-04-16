########################################
test_that('we can extract 100 seeds for a given speed multiconstraint #23', {
    n_seeds <- 100
    st_res <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/ste_1e5_speed_13_timefin_09:04:05.556.rda")
    seeds <- sample(1:max(st_res$muscle_trajectory),n_seeds,replace=FALSE)
    st_res_dt <- data.table(st_res)
    only_seeds <- st_res_dt[muscle_trajectory%in%seeds & task_index == 0,activation,by=.(muscle_trajectory, muscle)]    
    setorder(only_seeds, "muscle_trajectory")
    only_seeds$muscle <- factor(only_seeds$muscle, levels = colnames(H_multiconstraint$constr)[1:7])
    seeds <- data.table(dcast(only_seeds, muscle~muscle_trajectory, value.var="activation"))
    row.names(seeds) <- seeds$muscle
    seeds$muscle <- NULL
    H_multiconstraint <- attr(st_res, "constraints_and_tasks")$nonredundant_constr
    })