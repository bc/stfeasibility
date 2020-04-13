get_deltas_dt <- function(seed_0, dataset){
        merged_activation_trajectory <- merge(seed_0, dataset, by=c("muscle", "task_index"))
        #split by muscle so we can get a_{i+1} - a_{i}
        each_muscle <- split(merged_activation_trajectory, merged_activation_trajectory$muscle)
        diffsets <- lapply(each_muscle, function(sa){
                seed_activations <- sa$activation.x[1:6]
                other_traj_activations <- sa$activation.y[2:7]
                the_diffs <- other_traj_activations - seed_activations
                return(the_diffs)
            }) %>% dcrb

        add_trailing_0 <- function(v) c(v,0)
        norms_per_txn <- apply(diffsets,2, pracma::Norm,2) %>% add_trailing_0 %>% as.numeric
        abs_per_txn <- apply(diffsets,2, function(v) max(abs(v))) %>% add_trailing_0 %>% as.numeric
        res <- data.table(normval = norms_per_txn, max_abs_dot = abs_per_txn, transition_index=task_transition_idx())
        return(res)
}
task_transition_idx <- function() {c("t0_t1",
                            "t1_t2",
                            "t2_t3",
                            "t3_t4",
                            "t4_t5",
                            "t5_t6", "end_padding")}

run_step_speed_distributions_plot <- function(spatiotemporal_evaluations){
    library(data.table)
    points <- rbindlist(spatiotemporal_evaluations)
    points$velocity_limit <- factor(points$velocity_limit, levels=rev(as.character(speeds)))
    points <- data.table(points)
    

    # points_with_dot <- points[,.( task_index, activation, transition_index=task_transition_idx(), dot = c(diff(activation),0)), by=.(muscle_trajectory, muscle, velocity_limit)]
    points_with_dot <- points[,.( task_index, activation, transition_index=task_transition_idx(), dot = c(diff(activation),0)), by=.(muscle_trajectory, muscle, velocity_limit)]

    # within a given muscle trajectory, describe the norm of the transition from moment to moment
    within <- points_with_dot[transition_index!="end_padding",.(normval = pracma::Norm(dot,2), max_abs_dot = max(abs(dot))), by=.(muscle_trajectory,velocity_limit,transition_index)]
    message('computing within_trajectory_delta_distributions.pdf')

    #across analyses from the seed trajectory (a view from the 'transect', which is the predefined trajectory of the first har point sampled)
    seed_0 <- points[velocity_limit==1.0 & muscle_trajectory==1, .(muscle,task_index,activation)]


    message('computing for "across" plot')


    message('--calculating across distance metrics')
    across <- points[muscle_trajectory!=1, get_deltas_dt(seed_0, .SD), by=.(velocity_limit, muscle_trajectory)]
    setcolorder(across, colnames(within))
    within_and_across_within <- rbindlist(list(within, across), id=TRUE)
    p3 <- ggplot(within_and_across_within[transition_index!="end_padding"], aes(normval, fill=as.factor(.id), group=as.factor(.id))) + geom_histogram(bins=100, alpha=0.7,position="identity") +facet_grid(velocity_limit~transition_index, scales = "free", space="free")
    p3 <- p3 + theme_classic() + xlab("norm of the delta within a given transition, \n where all operations are performed from id=1 (red) within each single activation trajectory and \n id=2(blue) across, with a single seed point across all of the other trajectories.")
    p3 <- p3 + ylab("number of transitions") + theme_bw()
    p3 <- p3 + labs(caption = "degenerate case is where the velocity constraint is set to 1. \n Rows as you go down are increasingly stringent velocity constraints, \n down to 0.05, indicating no change can be greater than 5% activation change per 50ms transition.")
    p3 <- p3 + ggtitle("Norm(deltas,2))")
    message('--did norm deltas plot')

    
    p4 <- ggplot(within_and_across_within[transition_index!="end_padding"], aes(max_abs_dot, fill=as.factor(.id), group=as.factor(.id))) + geom_histogram(bins=100, alpha=0.7,position="identity") +facet_grid(velocity_limit~transition_index, scales = "free", space="free")
    p4 <- p4 + theme_classic() + xlab("maximum absolute delta observed within a given transition, \n where all operations are performed from id=1 (red) within each single activation trajectory and \n id=2(blue) across, with a single seed point across all of the other trajectories.")
    p4 <- p4 + ylab("number of transitions") + theme_bw()
    p4 <- p4 + labs(caption = "degenerate case is where the velocity constraint is set to 1. \n Rows as you go down are increasingly stringent velocity constraints, \n down to 0.05, indicating no change can be greater than 5% activation change per 50ms transition.")
    p4 <- p4 + ggtitle("Max(abs(deltas))")
    message('did max abs deltas plot')

    p_comb_conglomerate <- grid.arrange(p3,p4,ncol=1)
    ggsave("redirection_figures/within_vs_across_transition_distributions.pdf", p_comb_conglomerate, height=20)
    message('done with redirection_figures/within_vs_across_transition_distributions.pdf')
 }
runplots <- function(spatiotemporal_evaluations){
    # boxplots figure
    points <- rbindlist(spatiotemporal_evaluations)
    points$velocity_limit <- factor(points$velocity_limit,levels=rev(as.character(speeds)))
    summary_stats_p<- ggplot(points[,var(activation),by=.(muscle,task_index,velocity_limit)],aes(task_index,V1,col=velocity_limit,group=velocity_limit)) + geom_path() + facet_grid(~muscle) + theme_classic()
    ggsave("redirection_figures/variance_per_task_per_vel.pdf",summary_stats_p)


    boxplots_plot <- ggplot(points,aes(task_index,activation,col=velocity_limit, group=task_index)) + geom_boxplot() + facet_grid(velocity_limit~muscle) + theme_classic()
    ggsave("redirection_figures/boxplots_by_vel.pdf", boxplots_plot)

    distributions_per_task_per_muscle_per_st <- ggplot(points,aes(y=activation, col=velocity_limit)) + geom_histogram(bins=50) + facet_grid(velocity_limit~muscle+task_index, scales="free", space="free") + theme_classic()
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
    summary_stats_p <- ggplot(melt(summ_stats, id.vars=c("muscle","velocity_limit")), aes(velocity_limit, value, group=muscle, col=muscle)) + geom_path() + facet_grid(~variable) + theme_classic()
    ggsave("redirection_figures/per_muscle_changes_in_fas_by_vel_constraint.pdf", summary_stats_p)
}