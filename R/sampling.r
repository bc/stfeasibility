har_time_estimate <- function(constr, n_samples, thin) {
    if (abs(thin - 480) < 100) {

        dat <- structure(list(x = c(1000, 10000, 20000, 50000, 1e+05, 2e+05, 5e+05),
            y = c(32, 46, 64, 114, 200, 370, 870)), class = "data.frame", row.names = c(NA,
            -7L))
        lm_fit <- lm(y ~ x, data = dat)
        interval <- predict(lm_fit, data.frame(x = n_samples), interval = "predict")
        plus_or_minus <- floor((interval[[3]] - interval[[2]])/2)
        message(paste0(as.character(floor(interval[[1]])), "+/-", as.character(plus_or_minus),
            " seconds expected for ", n_samples, " har points"))
        return(interval[[1]])
    } else {
        return(0)
    }
}

har_sample <- function(constr, n_samples, ...) {
    x_dimensionality <- ncol(constr$constr)
    thin <- emiris_and_fisikopoulos_suggested_thin_steps(x_dimensionality)
    message(paste("har thin steps:", thin, "for dimensionality_x=", x_dimensionality,
        ""))
    serial_time <- har_time_estimate(constr, n_samples, thin)
    samples <- brian_hitandrun(constr, n.samples = n_samples, thin = thin, ...) %>% as.data.frame
    colnames(samples) <- colnames(constr$constr)
    return(samples)
}
brian_hitandrun <- function (constr, n.samples = 10000, thin.fn = function(n) {
    ceiling(log(n + 1)/4 * n^3)
}, thin = NULL, x0.randomize = FALSE, x0.method = "slacklp",
    x0 = NULL, eliminate = TRUE)
{
    message(sprintf('Initiating HAR at %s', Sys.time()))
    state <- brian_har_init(constr, thin.fn, thin, x0.randomize, x0.method,
        x0, eliminate)
    message(sprintf('Initiation complete. MCMC begins now at %s', Sys.time()))
    result <- har.run(state, n.samples)
    result$samples
}

brian_har_init <- function (constr, thin.fn = function(n) {
    ceiling(log(n + 1)/4 * n^3)
}, thin = NULL, x0.randomize = FALSE, x0.method = "slacklp",
    x0 = NULL, eliminate = TRUE)
{
    stopifnot(length(constr[["rhs"]]) == length(constr[["dir"]]))
    stopifnot(length(constr[["rhs"]]) == nrow(constr[["constr"]]))
    stopifnot(length(constr[["rhs"]]) > 0)
    message(sprintf('---eliminating redundant constraints at %s',Sys.time()))
    constr <- if (eliminate)
        eliminateRedundant(constr)
    else constr
    message(sprintf('---operating on nonredundant constraints at %s',Sys.time()))
    eq <- hitandrun:::eq.constr(constr)
    iq <- hitandrun:::iq.constr(constr)
    basis <- if (length(eq$dir) > 0) {
        hitandrun:::solution.basis(eq)
    }
    else {
        n <- ncol(constr$constr)
        list(translate = rep(0, n), basis = diag(n))
    }
    message(sprintf('---Extracting basis transform %s',Sys.time()))
    transform <- createTransform(basis)
    constr <- transformConstraints(transform, iq)
    
    message(sprintf('---Generating seed point at %s',Sys.time()))
    if (is.null(x0)) {
        x0 <- createSeedPoint(constr, homogeneous = TRUE, randomize = x0.randomize,
            method = x0.method)
    }
    else {
        x0 <- createTransform(basis, inverse = TRUE) %*% c(x0,
            1)
    }
    n <- length(x0) - 1
    if (is.null(thin)) {
        thin <- if (n == 0)
            1
        else thin.fn(n)
    }
    list(basis = basis, transform = transform, constr = constr,
        x0 = x0, thin = thin)
}



pb_har_sample <- function(constr, n_samples, mc.cores = 1, ...) {
    if (!constraint_is_feasible(constr)){
        stop("constraint_infeasible")
    }
    samples_per_core <- shard_a_total(total = n_samples, n_shards = mc.cores)
    browser("sampling shards")
    samples <- pbmclapply(samples_per_core, function(har_n) {
        har_sample(constr, n_samples = har_n, ...)
    }, mc.cores = mc.cores)
    tryCatch(res <- rbindlist(samples) , error=function(e){
        browser()
        })
    
    return(res %>% as.data.frame)
}

generate_task_trajectory_and_har <- function(H_matrix, vector_out, n_task_values,
    cycles_per_second, cyclical_function, output_dimension_names, muscle_name_per_index,
    bounds_tuple_of_numeric, num_har_samples, har_thin, ...) {
    tasks_and_constraints <- generate_tasks_and_corresponding_constraints(H_matrix = H_matrix,
        vector_out = vector_out, n_task_values = n_task_values, cycles_per_second = cycles_per_second,
        cyclical_function = cyclical_function, output_dimension_names = output_dimension_names,
        bounds_tuple_of_numeric = bounds_tuple_of_numeric)
    bigL <- tasks_and_constraints$constraints %>% pbmclapply(. %>% har_sample(num_har_samples),
        ...)
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
is_feasible <- function(constraint_object) {


    point <- tryCatch(point <- findInteriorPoint(constraint_object, homogeneous = FALSE),
        error = function(e) {print(e);return(FALSE)})
    if (point == FALSE) {
        return(FALSE)
    } else if (evaluate_solution(point, constraint_object,0.01)) {
        return(TRUE)
    } else {
        message("hitandrun package said it was feasible but it returned a solution that did not meet the constraints")
        return(FALSE)
    }
}

constraint_is_feasible <- function(constraint_object, num_muscles) {
    lp_res <- lpsolve_muscles_for_task("min", constraint_object, num_muscles)
    if (lp_res$status == 0) {
        return(TRUE)
    } else if (lp_res$status == 2) {
        return(FALSE)
    } else {
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
##' @return p number of points to mix with during hit and run.
emiris_and_fisikopoulos_suggested_thin_steps <- function(n) (10 + (10/n)) * n

trajectory_har_df_melt <- function(har_samples_df, num_muscles){
    slice_muscle_name <- function(str, delimiter = "_") strsplit(str, delimiter)[[1]][1]
    slice_muscle_names <- function(str_vector) sapply(str_vector%>%as.character, slice_muscle_name) %>% unname
    slice_task_index <- function(str, delimiter = "_") strsplit(str, delimiter)[[1]][2]
    slice_task_indices <- function(str_vector) sapply(str_vector%>%as.character, slice_task_index) %>% unname %>% as.integer
    muscle_names <- colnames(har_samples_df)[1:num_muscles]
    har_samples_df$muscle_trajectory <- seq(1, nrow(har_samples_df))
    melted_st <- melt(har_samples_df, id.vars="muscle_trajectory", variable.factor = FALSE)
    melted_st$muscle <- slice_muscle_names(melted_st$variable)
    melted_st$task_index[melted_st$variable %in% muscle_names == FALSE] <- slice_task_indices(melted_st$variable)[melted_st$variable %in% muscle_names == FALSE]
    melted_st$task_index[melted_st$variable %in% muscle_names == TRUE] <- 0
    #successfully split the single variable name into two variables. Rm now-redundant column.
    melted_st$variable <- NULL
    colnames(melted_st) <- c("muscle_trajectory", "activation","muscle","task_index")
    return(melted_st)
}

plot_har_trajectory <- function(melted_st){
    p <- ggplot(melted_st, aes(task_index, activation, group=muscle_trajectory)) + geom_line() + facet_grid(~muscle)
    p <- p + theme_classic()
    return(p)
}