negative_cos <- function(...) -cos(...)
force_cos_ramp <- function(...) negative_cos(...) * 0.5 + 0.5

generate_tasks_and_corresponding_constraints <- function(H_matrix, vector_out, n_task_values,
    cycles_per_second, cyclical_function, bounds_tuple_of_numeric) {
    output_dimension_names <- rownames(H_matrix)
    tasks <- task_time_df(fmax_task = vector_out, n_samples = n_task_values, cycles_per_second = cycles_per_second,
        cyclical_function = cyclical_function, output_dimension_names = output_dimension_names)
    list_of_constraints_per_task <- apply(tasks, 1, function(x) {
        a_matrix_rhs_task(H_matrix, task_wrench=as.numeric(x[output_dimension_names]), bounds_tuple_of_numeric)
    })
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
        stop("Bad input theta, was lower than pi")
    }
    if(theta > pi/2){
        stop("Bad input theta, was too big; above 3pi/2")
    }
    output <- c(-sin(theta),-cos(theta),0.0,0.0,0.0,0.0)
    return(output)
}

##@param lambda between 0 and 1 where 0 is distal and 1 is palmar along the arc of equal force magnitude
distal_to_palmar_transition_lambda <- function(lambda){
    return(distal_to_palmar_transition_coord_normalized(lambda * pi/2))
}
get_wrench_names <- function(){
    return(c(
"x",
"y",
"z",
"tx",
"ty",
"tz"
        ))
}


normalized_transition_forces <- function(lenout){
    lambdas <- untimed_lambdas(lenout,force_cos_ramp)
    
    m <- lapply(lambdas, function(a){distal_to_palmar_transition_lambda(1-a)})%>%dcrb
    colnames(m) <- get_wrench_names()
    m <- data.table(m)
    m$lambda <- lambdas
    m$time <- seq(0,0.199,length.out = nrow(m))
    setcolorder(m, c("time","lambda",get_wrench_names()))
    return(m)
}


generate_task_csvs_for_cat <- function(steps=200, task_magnitude=1){
    redirection_tasks <- normalized_transition_forces(steps)
    redirection_tasks$x <-  redirection_tasks$x * task_magnitude
    redirection_tasks$y <- redirection_tasks$y * task_magnitude
    
    # currently negative x which is backwards
    task_A <- task_time_df(c(-1.0 * task_magnitude,0.0,0.0,0.0,0.0,0.0), steps, 2, force_cos_ramp, get_wrench_names())
    # currently positive y which is up
    task_B <- task_time_df(c(0.0,-1.0 * task_magnitude,0.0,0.0,0.0,0.0), steps, 2, force_cos_ramp, get_wrench_names())

    # Match time manually
    task_A$time <- redirection_tasks$time
    task_B$time <- redirection_tasks$time

    task_A$id <-"taskA"
    task_B$id <- "taskB"
    redirection_tasks$id <- "redirection_A_to_B"

    write.csv(task_A,'output/task_A.csv', row.names=FALSE)
    write.csv(task_B,'output/task_B.csv', row.names=FALSE)
    write.csv(redirection_tasks,'output/redirection_tasks.csv', row.names=FALSE)

    if(steps >50){
    task_palette <- rbindlist(list(task_A,task_B,redirection_tasks),id=FALSE)
    task_plot <- ggplot(task_palette, aes(x,y,frame=time,col=as.factor(id))) + geom_point(size=8,alpha=0.4) + coord_fixed() + theme_classic()
    fig <- ggplotly(task_plot)
    fig <- fig %>% 
      animation_opts(
        1, redraw = FALSE
      )
    fig
    }
}

generate_tasks_and_corresponding_constraints_via_df <- function(H_matrix_input, tasks, bounds_tuple_of_numeric) {
    output_dimension_names <- rownames(H_matrix_input)
    list_of_constraints_per_task <- apply(tasks, 1, function(x) {
        a_matrix_rhs_task(H_matrix_input, task_wrench=x[output_dimension_names], bounds_tuple_of_numeric)
    })
    return(list(tasks = tasks, constraints = list_of_constraints_per_task))
}

