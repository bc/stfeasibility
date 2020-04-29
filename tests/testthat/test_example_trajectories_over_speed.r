
test_that('Show 10 unseeded trajectories', {


   produce_sample_trajectories_plot <- function(tall_input_dt, velocity_limit){
	# show an example of some trajectories, and their derivatives
	desired_trajectories <- seq(1,10)
	tall_input_dt[,("muscle_and_trajectory"):=paste(factor(muscle, levels=muscle_name_per_index), muscle_trajectory)]
	selected_tall_input_dt <- tall_input_dt[muscle_trajectory%in%desired_trajectories]
	p_example_trajectory <- ggplot(selected_tall_input_dt, aes(task_index,activation, group=as.factor(muscle_and_trajectory), col=as.factor(muscle_trajectory)))
	p_example_trajectory <- p_example_trajectory + geom_line() 
	p_example_trajectory <- p_example_trajectory + facet_grid(~factor(muscle, levels=muscle_name_per_index))
	p_example_trajectory <- p_example_trajectory + theme_classic() 
	p_example_trajectory <- p_example_trajectory + theme(legend.title = element_blank()) 
	p_example_trajectory <- p_example_trajectory + ggtitle(label=as.character(velocity_limit)) + xlab(velocity_limit) + theme(axis.title = element_blank())

	differentiated <- selected_tall_input_dt[, .(task_index, dot=c(diff(activation),0)),by=.(muscle,muscle_trajectory)]
	differentiated[,("muscle_and_trajectory"):=paste(factor(muscle, levels=muscle_name_per_index),muscle_trajectory)]
	
	dot_plot <- ggplot(differentiated[task_index < 6], aes(task_index, dot, group=as.factor(muscle_and_trajectory), col=as.factor(muscle_trajectory))) 
	dot_plot <- dot_plot +  geom_line() 
	dot_plot <- dot_plot + facet_grid(~factor(muscle, levels=muscle_name_per_index))
	dot_plot <- dot_plot + theme_classic() 
	dot_plot <- dot_plot + theme(legend.title = element_blank()) 
	dot_plot <- dot_plot + ylab("delta activation between tasks i and i+1") 
	dot_plot <- dot_plot + geom_hline(yintercept=as.numeric(velocity_limit),col="#990000", linetype="dashed") 
	dot_plot <- dot_plot + geom_hline(yintercept=-as.numeric(velocity_limit),col="#990000", linetype="dashed")
	dot_plot <- dot_plot + ylim(-1,1) + theme(axis.title = element_blank())


	p <- ggplot(differentiated[task_index < 6], aes(dot, col=factor(muscle,levels=muscle_name_per_index)))
	p <- p + geom_boxplot( alpha=0.5) + coord_flip() + xlim(-1,1) + ggtitle("Trajectory overall activation_dot distribution")
	p <- p + geom_vline(xintercept=as.numeric(velocity_limit),col="#990000", linetype="dashed") 
	p <- p + geom_vline(xintercept=-as.numeric(velocity_limit),col="#990000", linetype="dashed")
	p <- p +  guides(fill=guide_legend(title="Muscle")) + theme(axis.title = element_blank())

	p_activation_and_dot <- grid.arrange(p_example_trajectory,dot_plot, ncol=1)
	p_activation_and_dot_w_boxplots <- grid.arrange(p_activation_and_dot,p, ncol=2)
	return(p_activation_and_dot_w_boxplots)
}
	trajectories <- data.table(readRDS("/Volumes/GoogleDrive/My\ Drive/outputs/unseeded_har_1e5_100speeds/ste_1e5_speed_13_timefin_09:04:05.556.rds"))
    velocity_limit_input <- as.numeric(unique(trajectories$velocity_limit)[1])

	p <- produce_sample_trajectories_plot(trajectories,velocity_limit_input)
	ggsave("sample_trajectories_for_vel_point_%s"%--%velocity_limit_input %>%time_dot("pdf"), p, width=10, height=4, dpi=600)
})

test_that("animate squeeze on trajectories",{
	unseeded_filepaths <- dir("/Volumes/GoogleDrive/My\ Drive/outputs/unseeded_har_1e5_100speeds", full.names=TRUE)
	multiconstraint_structures <- lapply(unseeded_filepaths, function(x){
		trajectories <- data.table(readRDS(x))
    	velocity_limit_input <- as.numeric(unique(trajectories$velocity_limit)[1])

		p <- produce_sample_trajectories_plot(trajectories,velocity_limit_input)
		ggsave(paste0("sample_trajectories_vel_",velocity_limit_input,".pdf"), p, width=10, height=4, dpi=600)
		})


	})