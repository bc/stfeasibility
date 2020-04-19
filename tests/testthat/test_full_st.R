context('hi')

test_that('full_st histograms', {

    # ~.05 every 10ms, so 50ms = 25% change in activation
    setwd('/Users/Olive/Documents/GitHub/bc/stfeasibility')

    speeds <- sample(seq(0.05,1,length.out=100)) # this is the one that produced 100 speeds, unseeded with lipschitz constraints.
    speeds <- sample(seq(0.05,1,length.out=3))
    loop_update(1.0)
    destination <- "/Volumes/GoogleDrive/My\ Drive/outputs/apr18_outputs" #don't include trailing slash
    destinations <- pblapply(seq(1,100),function(i){
        message(sprintf('CURRENT I: %s',1))
        out_filepath <- "%s/ste_1e5_speed_%s_timefin_%s.rda"%--%c(destination, i,format(Sys.time(), "%H:%M:%OS3"))
        message('Saving to %s' %--% out_filepath)
        my_H_matrix <- read.csv("data/fvc_hentz_2002.csv", row.names=1) %>% as.matrix
        saveRDS(st_with_vel(my_H_matrix, speeds[i], har_n=1e5),out_filepath)
        # on MSI
        # system("rclone copy outputs remote:outputs", wait=TRUE)
        system(sprintf("rm %s",out_filepath), wait=TRUE)
        gc()
        return(out_filepath)
    })
    
test_that("we can plot things about the many tasks", {
    library(data.table)
    six_speed_spatiotemporal_evaluations <- readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/100kvals_task_A_10N_mat_A.rda")
    runplots(six_speed_spatiotemporal_evaluations)
    run_step_speed_distributions_plot(six_speed_spatiotemporal_evaluations)
})


skip('not today ')
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
    }, mc.cores=detectCores(all.tests = FALSE, logical = TRUE))  
})

library(pracma)
skip('not today ')
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
    }, mc.cores=detectCores(all.tests = FALSE, logical = TRUE))}, interval=300)
})

context('fullst 20')
skip('not today ')
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
    }, mc.cores=detectCores(all.tests = FALSE, logical = TRUE))}, interval=300)
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
skip('not today')
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

