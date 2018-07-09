context('hi')

force_cos_ramp_constraint_prefilled <- function(speed_limit, vector_out = c(28.8,0,0,0)) {
    return(force_cos_ramp_constraint(H_matrix = H_matrix, bounds_tuple_of_numeric = bounds_tuple_of_numeric,
    vector_out = vector_out, max_allowable_increasing_tension_speed=speed_limit,max_allowable_decreasing_tension_speed=speed_limit,
    n_task_values = 100, cycles_per_second = 60, cyclical_function = force_cos_ramp,
    eliminate = FALSE))
}
force_cos_ramp_is_feasible <- function(speed_limit, ...){
    tryCatch({
        big_constraint <- force_cos_ramp_constraint_prefilled(speed_limit=speed_limit, ...)
        c_is_feasible <- big_constraint$nonredundant_constr %>% constraint_is_feasible
        return(c_is_feasible)
    }, error = function(cond){
        print(cond)
        return(FALSE)
    })
}

har_dataframe_force_cos <- function(H_matrix,bounds_tuple_of_numeric,increasing,decreasing,n_task_values, har_n, vector_out){
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, vector_out, increasing,decreasing, n_task_values = n_task_values, cycles_per_second=10, eliminate = FALSE)
    param_string <- paste0("n_task_values:",n_task_values,"st increasing:", increasing, "decreasing:", decreasing)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant_single
    points <- res %>% har_sample(har_n, eliminate=FALSE)
    tall_df_st <- points %>% trajectory_har_df_melt(length(bounds_tuple_of_numeric))
    tall_df_st$st <- increasing
    return(tall_df_st)
}

test_that('minitest', { 
    vector_out <- c(10,0,0,0)
    smallest_feasible_speedlimit <- bisection_method(1e-8, 1.0, 1e-5, f = force_cos_ramp_is_feasible, vector_out=vector_out)
    velocity_constraint_options <- seq(smallest_feasible_speedlimit, 1.0, length.out = 1)
    har_n <- 1e3
    n_task_values <- 3
    pbmclapply(velocity_constraint_options, function(speed_limit){
        my_filename <- paste0("max_force_is_submaximal_10.0N_in_fx__n_task_values_", n_task_values, "_speed_limit_",speed_limit, "har_n_",har_n, ".csv")
        print('starting')
        print(my_filename)
        tall_segment <- har_dataframe_force_cos(H_matrix, bounds_tuple_of_numeric, speed_limit,speed_limit,n_task_values, har_n, vector_out)
        print('wroteCSV')
        write.csv(tall_segment, my_filename)
        print('wrote to csv:')
        print(my_filename)
    }, mc.cores=8)
})


skip('already done')
test_that('very submaximal forces g789sgd/', { 
    vector_out <- c(10,0,0,0)
    smallest_feasible_speedlimit <- bisection_method(1e-8, 1.0, 1e-5, f = force_cos_ramp_is_feasible, vector_out=vector_out)
    velocity_constraint_options <- seq(smallest_feasible_speedlimit, 1.0, length.out = 8)
    har_n <- 1e5
    n_task_values <- 20
    pbmclapply(velocity_constraint_options, function(speed_limit){
        my_filename <- paste0("max_force_is_submaximal_10.0N_in_fx__n_task_values_", n_task_values, "_speed_limit_",speed_limit, "har_n_",har_n, ".csv")
        print('starting')
        print(my_filename)
        tall_segment <- har_dataframe_force_cos(H_matrix, bounds_tuple_of_numeric, speed_limit,speed_limit,n_task_values, har_n, vector_out)
        print('wroteCSV')
        write.csv(tall_segment, my_filename)
        print('wrote to csv:')
        print(my_filename)
    }, mc.cores=8)
})
skip('already done')
test_that('full_ st histograms_20 sd7f89/', { 
    vector_out <- c(28.8,0,0,0)
    smallest_feasible_speedlimit <- bisection_method(1e-8, 1.0, 1e-5, f = force_cos_ramp_is_feasible, vector_out=vector_out)
    velocity_constraint_options <- seq(smallest_feasible_speedlimit, 1.0, length.out = 8)

    har_n <- 1e5
    n_task_values <- 20
    pbmclapply(velocity_constraint_options, function(speed_limit){
        my_filename <- paste0("ramp_to_full_mvc_fx_28.8N_n_task_values_", n_task_values, "_speed_limit_",speed_limit, "har_n_",har_n, ".csv")
        print(my_filename)
        tall_segment <- har_dataframe_force_cos(H_matrix, bounds_tuple_of_numeric, speed_limit,speed_limit,n_task_values, har_n, vector_out)
        print('wroteCSV')
        write.csv(tall_segment, my_filename)
    }, mc.cores=8)
})

skip('not today ')
test_that('full_ st histograms', {
    har_n <- 1e3
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, c(28,0,0,0), 0.5, 0.5, n_task_values = 10, cycles_per_second=10, eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant(6)
    points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(points))
    tall_df_st <- points %>% trajectory_har_df_melt(7)
    tall_df_st$st <- 1

    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, c(28,0,0,0), 1.0, 1.0, n_task_values = 10, cycles_per_second=10,eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant(6)
    points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(points))
    tall_df_no_st <- points %>% trajectory_har_df_melt(7)
    tall_df_no_st$st <- 0

    tall_df_st_and_no_st<- rbind(tall_df_no_st, tall_df_st)
    tall_df_st_and_no_st$st <- as.factor(tall_df_st_and_no_st$st)
})

skip('not today ')
test_that('tailor visualization', {

    p <- ggplot(tall_df_st_and_no_st, aes(fill=st,frame=as.factor(task_index))) +  geom_density(aes(activation), alpha=0.25) + facet_grid(~muscle, space="free")
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of 0.5 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    gganimate::gganimate(p, output_subfolder_path("st_animation2", "st_animation2.html"))

    site <- ggplotly(p)
    htmlwidgets::saveWidget(site, output_subfolder_path("st_histograms", "st_histograms.html"))
})

skip('not today ')
test_that('tailor animation', {  
p <- ggplot(tall_df_st_and_no_st, aes(fill=st, frame=as.factor(task_index))) +  geom_histogram(aes(activation)) + facet_grid(~muscle, space="free")
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of 0.5 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    p <- p + geom_segment(aes(x =max(activation), y = 0, xend = max(activation), yend = 1, group=muscle), linetype="dashed",  color="red")
    gganimate::gganimate(p, output_subfolder_path("st_animation", "st_animation.html"))
    show(p)
})


skip('not today ')
test_that('logical binary newton bisection_method works', {
    x_is_at_least_67 <- function(x) x>=67.98095181
    bisection_method(1.0,100.0,tol=1e-9,x_is_at_least_67)
})

skip('not today ')
test_that('boxplot significance testing', {  
p <- ggplot(tall_df_st_and_no_st, aes(task_index,activation,fill=st, group=task_index)) +  geom_boxplot() + facet_grid(st~muscle, space="free")
skip('not today ')
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of 0.5 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    p <- p + geom_segment(aes(x =max(activation), y = 0, xend = max(activation), yend = 1, group=muscle), linetype="dashed",  color="red")
    gganimate::gganimate(p, output_subfolder_path("st_animation", "st_animation.html"))
    show(p)
})

skip('not today ')
test_that('animate sample trajectories', {  

p <- ggplot(tall_df_st_and_no_st, aes(task_index,activation,fill=st, group=task_index)) +  geom_boxplot() + facet_grid(st~muscle, space="free")
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of 0.5 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    p <- p + geom_segment(aes(x =max(activation), y = 0, xend = max(activation), yend = 1, group=muscle), linetype="dashed",  color="red")
    gganimate::gganimate(p, output_subfolder_path("st_animation", "st_animation.html"))
    show(p)
})

