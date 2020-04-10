context('hi')

test_that('minitest', { 
    vector_out <- c(10,0,0,0)
    profvis({smallest_feasible_speedlimit <- bisection_method(1e-8, 0.25, 1e-5, f = force_cos_ramp_is_feasible, vector_out=vector_out)})
    velocity_constraint_options <- seq(smallest_feasible_speedlimit, 1.0, length.out = 1)
    har_n <- 1e3
    n_task_values <- 3
    pbmclapply(velocity_constraint_options, function(speed_limit){
        my_filename <- paste0("max_force_is_submaximal_10.0N_in_fx__n_task_values_", n_task_values, "_speed_limit_",speed_limit, "har_n_",har_n, ".csv")
        tall_segment <- har_dataframe_force_cos(H_matrix, bounds_tuple_of_numeric, speed_limit,speed_limit,n_task_values, har_n, vector_out)
        write.csv(tall_segment, my_filename)
        print('wrote to csv:')
        print(my_filename)
    }, mc.cores=8)  
})

library(pracma)
test_that('very submaximal forces', { 
    profvis({vector_out <- c(10,0,0,0)
    smallest_feasible_speedlimit <- bisection_method(1e-9, 0.10, 1e-5, f = force_cos_ramp_is_feasible, vector_out=vector_out)
    velocity_constraint_options <- c(logseq(smallest_feasible_speedlimit, 1.0, length.out = 8),1)
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
    }, mc.cores=8)}, interval=300)
})

context('fullst 20')
test_that('full_ st histograms_20', { 
    profvis({vector_out <- c(28.8,0,0,0)
    smallest_feasible_speedlimit <- bisection_method(1e-9, 10, 1e-5, f = force_cos_ramp_is_feasible, vector_out=vector_out)
    velocity_constraint_options <- c(logseq(smallest_feasible_speedlimit, 1.0, length.out = 8),1.0)
    har_n <- 1e5
    n_task_values <- 20
    pbmclapply(velocity_constraint_options, function(speed_limit){
        my_filename <- paste0("ramp_to_full_mvc_fx_28.8N_n_task_values_", n_task_values, "_speed_limit_",speed_limit, "har_n_",har_n, ".csv")
        print(my_filename)
        tall_segment <- har_dataframe_force_cos(H_matrix, bounds_tuple_of_numeric, speed_limit,speed_limit,n_task_values, har_n, vector_out)
        print('wroteCSV')
        write.csv(tall_segment, my_filename)
    }, mc.cores=8)}, interval=300)
})

skip('not today ')
test_that('full_st histograms', {

    # ~.05 every 10ms, so 50ms = 25% change in activation


    st_with_vel <- function(discrete_speed_limit,har_n) {
        st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, c(5,0,0,0), discrete_speed_limit, discrete_speed_limit, n_task_values = 7, cycles_per_second=10, eliminate = FALSE)
        extremes <- lapply(st_constr_str$tasks_and_constraints$constraints, findExtremePoints)
        res <- st_constr_str$nonredundant_constr %>% eliminate_redundant(7)
        points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
        print(summary(points))
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

    speeds <- c(0.05,0.1,.25,0.5,.75,1.0)
    loop_update(0.0)
    spatiotemporal_evaluations <- lapply(speeds, st_with_vel, har_n=1e7)
    loop_update(1.0)

    # boxplots figure
    points <- rbindlist(spatiotemporal_evaluations)

    ggplot(points[,var(activation),by=.(muscle,task_index,velocity_limit)],aes(task_index,V1,col=velocity_limit)) + geom_point() + facet_grid(~muscle) + theme_classic()
    ggsave("variance_per_task_per_vel.pdf")


    boxplots_plot <- ggplot(points,aes(task_index,activation,col=velocity_limit, group=task_index)) + geom_boxplot() + facet_grid(velocity_limit~muscle) + theme_classic()
    ggsave("boxplots_by_vel.pdf", boxplots_plot)


    diffs <- points[,max(abs(diff(activation))),by=.(muscle_trajectory, muscle,velocity_limit)]
    speed_dists <- ggplot(diffs, aes(V1, col=velocity_limit)) + geom_histogram(bins=100) + facet_grid(velocity_limit~muscle, scales="free_y")
    ggsave("speed_distributions_by_vel_const.pdf", speed_dists)

    summ_stats <- diffs[,.(min=min(V1),median=median(V1),mean=mean(V1),max=max(V1),span=max(V1) - min(V1)), by=.(muscle,velocity_limit)]
    setorder(summ_stats,"muscle", "velocity_limit")
    summ_stats$velocity_limit <- factor(summ_stats$velocity_limit,levels=rev(as.character(speeds)))
    summary_stats_p <- ggplot(melt(summ_stats), aes(velocity_limit, value, group=muscle, col=muscle)) + geom_path() + facet_grid(~variable) + theme_classic()
    ggsave("per_muscle_changes_in_fas_by_vel_constraint.pdf", summary_stats_p)

    

    lp("min", objective.in = rep(1,ncol(res)), const.mat = res$constr,
              const.dir = res$dir, const.rhs = res$rhs,
              compute.sens = 0)
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


# skip('not today ')
test_that('logical binary newton bisection_method works', {
    x_is_at_least_67 <- function(x) x>=67.98095181
    bisection_method(1.0,100.0,tol=1e-9,x_is_at_least_67)
})

# skip('not today ')
test_that('boxplot significance testing', {  
p <- ggplot(tall_df_st_and_no_st, aes(task_index,activation,fill=st, group=task_index)) +  geom_boxplot() + facet_grid(st~muscle, space="free")
skip('not today ')
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of 0.5 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    p <- p + geom_segment(aes(x =max(activation), y = 0, xend = max(activation), yend = 1, group=muscle), linetype="dashed",  color="red")
    gganimate::gganimate(p, output_subfolder_path("st_animation", "st_animation.html"))
    show(p)
})

# skip('not today ')
test_that('animate sample trajectories', {  

p <- ggplot(tall_df_st_and_no_st, aes(task_index,activation,fill=st, group=task_index)) +  geom_boxplot() + facet_grid(st~muscle, space="free")
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of 0.5 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    p <- p + geom_segment(aes(x =max(activation), y = 0, xend = max(activation), yend = 1, group=muscle), linetype="dashed",  color="red")
    gganimate::gganimate(p, output_subfolder_path("st_animation", "st_animation.html"))
    show(p)
})

