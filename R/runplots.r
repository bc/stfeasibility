runplots <- function(spatiotemporal_evaluations){
    # boxplots figure
    points <- rbindlist(spatiotemporal_evaluations)
    points$velocity_limit <- factor(points$velocity_limit,levels=rev(as.character(speeds)))
    summary_stats_p<- ggplot(points[,var(activation),by=.(muscle,task_index,velocity_limit)],aes(task_index,V1,col=velocity_limit,group=velocity_limit)) + geom_path() + facet_grid(~muscle) + theme_classic()
    ggsave("redirection_figures/variance_per_task_per_vel.pdf",summary_stats_p)


    boxplots_plot <- ggplot(points,aes(task_index,activation,col=velocity_limit, group=task_index)) + geom_boxplot() + facet_grid(velocity_limit~muscle) + theme_classic()
    ggsave("redirection_figures/boxplots_by_vel.pdf", boxplots_plot)

    distributions_per_task_per_muscle_per_st <- ggplot(points,aes(activation, col=velocity_limit)) + geom_histogram(bins=50) + facet_grid(velocity_limit~muscle+task_index) + theme_classic()
    ggsave("redirection_figures/distributions_per_task_per_muscle_per_st.pdf", distributions_per_task_per_muscle_per_st)


    diffs <- points[,max(abs(diff(activation))),by=.(muscle_trajectory, muscle,velocity_limit)]
    diffs$velocity_limit <- factor(diffs$velocity_limit,levels=rev(as.character(speeds)))
    speed_dists <- ggplot(diffs, aes(V1, col=velocity_limit)) + geom_histogram(bins=100) + facet_grid(velocity_limit~muscle, scales="free_y") + theme_classic()
    ggsave("redirection_figures/speed_distributions_by_vel_const.pdf", speed_dists)

    dotdot <- points[,abs(diff(diff(activation))),by=.(muscle_trajectory, muscle,velocity_limit)]
    dotdot$velocity_limit <- factor(dotdot$velocity_limit,levels=rev(as.character(speeds)))
    dot_dot_p <- ggplot(dotdot, aes(V1, col=velocity_limit)) + geom_histogram(bins=100) + facet_grid(velocity_limit~muscle, scales="free_y") + theme_classic()
    ggsave("redirection_figures/dot_dot_dist.pdf", dot_dot_p)

    summ_stats <- diffs[,.(min=min(V1),median=median(V1),mean=mean(V1),max=max(V1)), by=.(muscle,velocity_limit)]
    setorder(summ_stats,"muscle", "velocity_limit")
    summ_stats$velocity_limit <- factor(summ_stats$velocity_limit,levels=rev(as.character(speeds)))
    summary_stats_p <- ggplot(melt(summ_stats), aes(velocity_limit, value, group=muscle, col=muscle)) + geom_path() + facet_grid(~variable) + theme_classic()
    ggsave("redirection_figures/per_muscle_changes_in_fas_by_vel_constraint.pdf", summary_stats_p)
}