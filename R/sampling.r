har_time_estimate <- function(constr, n_samples, thin) {
    if(abs(thin - 480) < 100){

    dat <- structure(list(x = c(1000, 10000, 20000, 50000, 1e+05, 2e+05, 5e+05),
        y = c(32, 46, 64, 114, 200, 370, 870)), class = "data.frame", row.names = c(NA,
        -7L))
    lm_fit <- lm(y ~ x, data = dat)
    interval <- predict(lm_fit, data.frame(x = n_samples), interval = "predict")
    plus_or_minus <- floor((interval[[3]] - interval[[2]])/2)
    message(paste0(as.character(floor(interval[[1]])), "+/-", as.character(plus_or_minus), " seconds expected for ", n_samples," har points"))
    }
}

har_sample <- function(constr, n_samples, ...) {
    x_dimensionality <- ncol(constr$constr)
    thin <- emiris_and_fisikopoulos_suggested_thin_steps(x_dimensionality)
    message(paste("har thin steps:",thin,"for dimensionality_x=",x_dimensionality, ""))
    har_time_estimate(constr, n_samples, thin)
    samples <- hitandrun(constr, n.samples=n_samples, thin=thin, ...)
    colnames(samples) <- colnames(constr$constr)
    return(samples %>% as.data.frame)
}

pb_har_sample <- function(constr, n_samples, shards = 10) {
    samples_per_core <- n_samples/shards
    samples <- pbmclapply(1:shards, function(i) {
        har_sample(constr, n_samples = samples_per_core)
    }, mc.cores = 6)
    message("dcrb'ing")
    return(dcrb(samples))
}

generate_task_trajectory_and_har <- function(H_matrix, vector_out, n_task_values,
    cycles_per_second, cyclical_function, output_dimension_names, muscle_name_per_index,
    bounds_tuple_of_numeric, num_har_samples, har_thin, ...) {
    tasks_and_constraints <- generate_tasks_and_corresponding_constraints(H_matrix=H_matrix, vector_out=vector_out,
        n_task_values=n_task_values, cycles_per_second=cycles_per_second, cyclical_function=cyclical_function, output_dimension_names=output_dimension_names,
        bounds_tuple_of_numeric=bounds_tuple_of_numeric)
    bigL <- tasks_and_constraints$constraints %>% pbmclapply(. %>% har_sample(num_har_samples), ...)
    bigL_labeled <- lapply(1:length(bigL), function(list_index) {
        cbind(tasks_and_constraints$tasks[list_index, ], bigL[[list_index]], row.names = NULL)
    })
    bigL_column_labeled <- lapply(bigL_labeled, set_colnames, c(colnames(tasks_and_constraints$tasks),
        muscle_name_per_index))
    har_per_task_df <- bigL_column_labeled %>% dcrb
    return(har_per_task_df)
}