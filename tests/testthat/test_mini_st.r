context('mini via har; st tunneling boxplots')
test_that("mini_constr", {
	har_n <- 1e3
    
    st_constr_str <- force_cos_ramp_constraint(H_matrix_mini, bounds_tuple_of_numeric_mini, 5.33, .2187, .2187, n_task_values = 10, cycles_per_second=10,eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    mini_points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(mini_points))
    profvis(expr={tall_df <- mini_points %>% trajectory_har_df_melt(3)})
    p_st1 <- ggplot(tall_df, aes(task_index, col=muscle)) + geom_boxplot(aes(x="", activation, group=task_index)) + facet_wrap(~muscle) + ggtitle("ST2") + theme_classic()


    st_constr_str <- force_cos_ramp_constraint(H_matrix_mini, bounds_tuple_of_numeric_mini, 5.33, .25, .25, n_task_values = 10, cycles_per_second=10,eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    mini_points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(mini_points))
    profvis(expr={tall_df <- mini_points %>% trajectory_har_df_melt(3)})
    p_st2 <- ggplot(tall_df, aes(task_index, col=muscle)) + geom_boxplot(aes(x="", activation, group=task_index)) + facet_wrap(~muscle) + ggtitle("ST2") + theme_classic()

    st_constr_str <- force_cos_ramp_constraint(H_matrix_mini, bounds_tuple_of_numeric_mini, 5.33, 1.0, 1.0, n_task_values = 10, cycles_per_second=10,eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    mini_points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(mini_points))
    profvis(expr={tall_df <- mini_points %>% trajectory_har_df_melt(3)})
    p_degenerate <- ggplot(tall_df, aes(task_index, col=muscle)) + geom_boxplot(aes(x="", activation, group=task_index)) + facet_wrap(~muscle) + ggtitle("p_degenerate") + theme_classic()

    p <- arrangeGrob(grobs=list(p_degenerate, p_st2,p_st1), ncol=1)
    ggsave("mini_constr.jpg",p, width=20,height=30)
    browser()
    print(st_constr_str$tasks_and_constraints$tasks$time)

})


test_that('histograms', {
    har_n <- 1e3
    st_constr_str <- force_cos_ramp_constraint(H_matrix_mini, bounds_tuple_of_numeric_mini, 5.33, .2187, .2187, n_task_values = 10, cycles_per_second=10,eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    mini_points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(mini_points))
    tall_df_st <- mini_points %>% trajectory_har_df_melt(3)
    tall_df_st$st <- 1

    st_constr_str <- force_cos_ramp_constraint(H_matrix_mini, bounds_tuple_of_numeric_mini, 5.33, 1.0, 1.0, n_task_values = 10, cycles_per_second=10,eliminate = FALSE)
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant
    mini_points <- res %>% pb_har_sample(har_n, mc.cores=8, eliminate=FALSE)
    print(summary(mini_points))
    tall_df_no_st <- mini_points %>% trajectory_har_df_melt(3)
    tall_df_no_st$st <- 0

    tall_df<- rbind(tall_df_no_st, tall_df_st)
    tall_df$st <- as.factor(tall_df$st)


    p <- ggplot(tall_df, aes(fill=st)) +  geom_histogram(aes(activation)) + facet_grid(task_index~muscle, space="free")
    p <- p + theme_classic() + xlab("Task Index") + ylab("Number of Trajectories") + ggtitle("ST of .2187 max delta activation per 10ms vs degenerate case")
    p <- p + scale_y_continuous(labels = scales::percent)
    ggplotly(p)

})

