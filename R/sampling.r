har_time_estimate <- function(constr, n_samples, thin) {
    if(abs(thin - 480) < 100){

    dat <- structure(list(x = c(1000, 10000, 20000, 50000, 1e+05, 2e+05, 5e+05),
        y = c(32, 46, 64, 114, 200, 370, 870)), class = "data.frame", row.names = c(NA,
        -7L))
    lm_fit <- lm(y ~ x, data = dat)
    interval <- predict(lm_fit, data.frame(x = n_samples), interval = "predict")
    plus_or_minus <- floor((interval[[3]] - interval[[2]])/2)
    message(paste0(as.character(floor(interval[[1]])), "+/-", as.character(plus_or_minus), " seconds expected for ", n_samples," har points"))
    return(interval[[1]])
    } else{
        return(0.0)
    }
}

har_sample <- function(constr, n_samples, ...) {
    x_dimensionality <- ncol(constr$constr)
    thin <- emiris_and_fisikopoulos_suggested_thin_steps(x_dimensionality)
    message(paste("har thin steps:",thin,"for dimensionality_x=",x_dimensionality, ""))
    serial_time <- har_time_estimate(constr, n_samples, thin)    
    samples <- hitandrun(constr, n.samples=n_samples, thin=thin, ...) %>% as.data.frame
    colnames(samples) <- colnames(constr$constr)
    return(samples)
}

pb_har_sample <- function(constr, n_samples, mc.cores=1, ...) {
    samples_per_core <- shard_a_total(total=n_samples, n_shards=mc.cores)
    samples <- pbmclapply(samples_per_core, function(har_n) {
        har_sample(constr, n_samples = har_n, ...)
    }, mc.cores = mc.cores)
    return(rbindlist(samples)%>%as.data.frame)
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
    har_per_task_df <- bigL_column_labeled %>% rbindlist
    return(har_per_task_df)
}


##' Does a feasible space exist?
##' useful so we don't har_sample on infeasible spaces.
##' @param constraint_object constraint object as in hitandrun
##' @return isFeasible logical, TRUE if there is a point or points to be collected.
is_feasible <- function(constraint_object){
    point <- tryCatch(point <- findInteriorPoint(constraint_object, homogeneous=FALSE), error=function(e) return(FALSE))
    if (point==FALSE){
        return(FALSE)
    } else if (evaluate_solution(point, constraint_object)){
        return(TRUE)
    } else {
        message('hitandrun package said it was feasible but it returned a solution that did not meet the constraints')
        return(FALSE)
    }
}

constraint_is_feasible <- function(constraint_object, num_muscles){
        lp_res <- lpsolve_muscles_for_task("min", constraint_object, num_muscles)
        if (lp_res$status == 0){
            return(TRUE)
        } else if (lp_res$status ==2){
            return(FALSE)
        } else{
            stop("The lpSolve result status was neither 0 or 2, where 0 is feasible and 2 is infeasible.")
        }
    }



# after points have been picked

split_by_time <- function(df) {
    lapply(unique(df$time), function(time_t) df[df$time == time_t, ])
}

##' emiris_and_fisikopoulos estimated mixing time
##' useful for hit and run.  
##' Ioannis Z Emiris and Vissarion Fisikopoulos. Efficient randomwalk
##' methods for approximating polytope volume. arXiv preprint
##' arXiv:1312.2873, 2013.
##' @see har_sample
##' @param n integer, dimensionality of the x variable in Ax=b for given system of inequalities
##" @return p number of points to mix with during hit and run.
emiris_and_fisikopoulos_suggested_thin_steps <- function(n) (10 + (10/n))*n


