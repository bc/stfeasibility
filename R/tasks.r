negative_cos <- function(...) -cos(...)
force_cos_ramp <- function(...) negative_cos(...) * 0.5 + 0.5

generate_tasks_and_corresponding_constraints <- function(H_matrix, vector_out, n_task_values,
    cycles_per_second, cyclical_function, bounds_tuple_of_numeric) {
    output_dimension_names <- rownames(H_matrix)
    tasks <- task_time_df(fmax_task = vector_out, n_samples = n_task_values, cycles_per_second = cycles_per_second,
        cyclical_function = cyclical_function, output_dimension_names = output_dimension_names)
    tasks$Fx <- c(
0,
0.130526192220052,
0.38268343236509,
0.5,
0.38268343236509,
0.130526192220052,
0
        )
tasks$Fy <- rep(0.0, 7)
        tasks$Fz<- c(
-1,
-0.99144486137381,
-0.923879532511287,
-0.866025403784439,
-0.923879532511287,
-0.99144486137381,
-1
        )


    time <- c(0.000,0.050,0.100,0.150,0.200,0.250,0.300)
    list_of_constraints_per_task <- apply(tasks, 1, function(x) {
        a_matrix_rhs_task(H_matrix, task_wrench=as.numeric(x[output_dimension_names]), bounds_tuple_of_numeric)
    })
    return(list(tasks = tasks, constraints = list_of_constraints_per_task))
}



untimed_lambdas <- function(length.out, cyclical_function) {
    lapply(seq(0, 2 * pi, length.out = length.out), cyclical_function) %>% dcc
}
super_la <- function(){
    rm(list = ls()); load_all()
}

library(boot)
library(R.matlab)
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
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
    return(distal_to_palmar_transition_coord_normalized(lambda * pi/6))
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

make_nice_task_animation <- function(task_definitions){ 
p <- ggplot(task_definitions$redirection_tasks%>%data.table,aes(x,y,frame=time))  + geom_point() + coord_fixed() +xlim(-1,1) + ylim(-1,1)
ggplotly(p)
}
normalized_transition_forces <- function(lenout){
    lambdas <- untimed_lambdas(lenout,force_cos_ramp)
    
    m <- lapply(lambdas, function(a){distal_to_palmar_transition_lambda(a)})%>%dcrb
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
    write.csv(redirection_tasks,'output/task_A_to_B.csv', row.names=FALSE)

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
    LL <- list(task_A=task_A,
task_B=task_B,
redirection_tasks=redirection_tasks)
    return(LL)
}

generate_tasks_and_corresponding_constraints_via_df <- function(H_matrix_input, tasks, bounds_tuple_of_numeric) {
    output_dimension_names <- rownames(H_matrix_input)
    list_of_constraints_per_task <- apply(tasks, 1, function(x) {
        relevant_wrench <- as.numeric(x[output_dimension_names])
        a_matrix_rhs_task(H_matrix_input, task_wrench=relevant_wrench, bounds_tuple_of_numeric)
    })
    return(list(tasks = tasks, constraints = list_of_constraints_per_task))
}

gen_7d_ffs <- function(H_matrix_input){
    all_combinations <- expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))
    vals <- as.matrix(H_matrix_input) %*% t(all_combinations) %>% t
    colnames(vals) <- rownames(H_matrix_input)
    return(vals%>%data.table)
}


plot_ffs <- function(ffs_points, tasks_df){
    p <- ggplot(ffs_points, aes(Fx,Fz,col=Fy)) 
    p <- p + geom_point()
    p <- p + coord_fixed() 
    p <- p + theme_bw() 
    p <- p + geom_hline(yintercept = 0) 
    p <- p + geom_vline(xintercept = 0)
    p <- p + stat_chull(alpha = 0.01, geom = "polygon")
    p <- p + geom_path(data = tasks_df, aes(x=Fx, y = Fz), col="green")
    ggsave("figures/ffs.pdf")
}
    st_with_vel <- function(discrete_speed_limit,har_n) {
        loop_update(discrete_speed_limit)
        my_H_matrix <- read.csv("data/fvc_hentz_2002.csv", row.names=1) %>% as.matrix
        my_H_matrix <- my_H_matrix
        ffs_points <- gen_7d_ffs(my_H_matrix)
        plot_ffs(ffs_points, st_constr_str$tasks_and_constraints$tasks * 10)
        # H_matrix
        st_constr_str <- force_cos_ramp_constraint(my_H_matrix, bounds_tuple_of_numeric, c(10,0,0,0), discrete_speed_limit, discrete_speed_limit, n_task_values = 7, cycles_per_second=10, eliminate = FALSE)
        extremes <- lapply(st_constr_str$tasks_and_constraints$constraints, findExtremePoints)
        res <- st_constr_str$nonredundant_constr %>% eliminate_redundant(7)
        points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
        tall_df_st <- points %>% trajectory_har_df_melt(7)
        tall_df_st$velocity_limit <- discrete_speed_limit
        tall_df_st$velocity_limit <- as.character(tall_df_st$velocity_limit)
        print(extremes)
        attr(tall_df_st, 'extremes') <- extremes
        attr(tall_df_st, 'speed_limit') <- discrete_speed_limit
        attr(tall_df_st, 'constraints_and_tasks') <- st_constr_str
        return(tall_df_st)
    }

loop_update <- function(fraction){
    system(sprintf("curl --location --request POST 'http://34.66.244.118/update/?token=f8992e21-a350-40a5-986f-5221412bdad8&obs=%s'",fraction), wait=FALSE)
}
