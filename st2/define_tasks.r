library(magrittr)
library(ggplot2)
library(data.table)

source('btools.r')
negative_cos <- function(...) -cos(...)
force_cos_ramp <- function(...) negative_cos(...) * 0.5 + 0.5

generate_tasks_and_corresponding_constraints <- function(H_matrix, vector_out, n_task_values,
    cycles_per_second, cyclical_function, bounds_tuple_of_numeric) {
    output_dimension_names <- rownames(H_matrix)
    tasks <- task_time_df(fmax_task = vector_out, n_samples = n_task_values, cycles_per_second = cycles_per_second,
        cyclical_function = cyclical_function, output_dimension_names = output_dimension_names)
    list_of_constraints_per_task <- apply(tasks, 1, function(x) {
        a_matrix_rhs_task(H_matrix, task_wrench=x[output_dimension_names], bounds_tuple_of_numeric)
    })
    browser()
    return(list(tasks = tasks, constraints = list_of_constraints_per_task))
}

untimed_lambdas <- function(length.out, cyclical_function) {
    lapply(seq(0, 2 * pi, length.out = length.out), cyclical_function) %>% dcc
}

##' @param cycles_per_second in Hz.
lambda_task_time_df <- function(n_samples, cycles_per_second, cyclical_function) {
    lambdas <- untimed_lambdas(n_samples, cyclical_function)
    df <- data.frame(time = seq(0, (n_samples - 1) * 1/cycles_per_second, by = 1/cycles_per_second),
        lambda = lambdas)
    return(df)
}

task_time_df <- function(fmax_task, n_samples, cycles_per_second, cyclical_function,
    output_dimension_names) {
    time_lambda_df <- lambda_task_time_df(n_samples, cycles_per_second, cyclical_function)
    df <- apply(time_lambda_df, 1, function(row) {
        scaled_task <- row[["lambda"]] %*% fmax_task
        concatenated_row <- c(row[["time"]], row[["lambda"]], scaled_task) %>% as.data.frame %>%
            t
        return(concatenated_row)
    }) %>% t %>% as.data.frame
    colnames(df) <- c("time", "lambda", output_dimension_names)
    return(df)
}

##@param theta numeric: angle between distal and palmar: should be between 0 and pi/2
# distal <- c(0.0,0.0,-10.0,0.0,0.0,0.0)
# palmar <- c(-10.0,0.0,0.0,0.0,0.0,0.0)
distal_to_palmar_transition_coord_normalized <- function(theta) {
    if(theta < 0){
        stop("Bad input theta, was negative")
    }
    if(theta > pi/2){
        stop("Bad input theta, was too big; above pi/2")
    }
    output <- c(-sin(theta),0.0,-cos(theta),0.0,0.0,0.0)
    return(output)
}

##@param lambda between 0 and 1 where 0 is distal and 1 is palmar along the arc of equal force magnitude
distal_to_palmar_transition_lambda <- function(lambda){
    return(distal_to_palmar_transition_coord_normalized(lambda * pi/2))
}

get_wrench_names <- function() c("dorsal_fx","medial_fy","proximal_fz","JR3_MX","JR3_MY","JR3_MZ")
get_muscle_name_per_index <- function() c("FDP","FDS","EIP","EDC","LUM","DI","PI")

normalized_transition_forces <- function(lenout){
    m <- lapply(untimed_lambdas(lenout,force_cos_ramp), function(a){distal_to_palmar_transition_lambda(a)})%>%dcrb
    colnames(m) <- get_wrench_names()
    m <- data.table(m)
    return(m)
}
force_redirection_tasks <- normalized_transition_forces(100)

distal_scaling_tasks <- 


