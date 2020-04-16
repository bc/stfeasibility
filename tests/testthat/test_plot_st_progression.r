context('A')

sub_maximal_fx<- c("~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_0.156053824008658har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_0.296711520007215har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_0.437369216005772har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_0.578026912004329har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_0.718684608002886har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_0.859342304001443har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/g789sgd/max_force_is_submaximal_10.0N_in_fx__n_task_values_20_speed_limit_1har_n_1e+05.csv")

near_maximal_fx <- c("~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_0.156053824008658har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_0.296711520007215har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_0.437369216005772har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_0.578026912004329har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_0.718684608002886har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_0.859342304001443har_n_1e+05.csv",
"~/Resilio\ Sync/stfeasibility/sd7f89/n_task_values_20_speed_limit_1har_n_1e+05.csv")

point_sets <- pblapply(near_maximal_fx, fread) %>% rbindlist
point_sets$muscle <- factor(point_sets$muscle, levels = muscle_name_per_index)
max_point_sets <- pblapply(sub_maximal_fx, fread) %>% rbindlist
max_point_sets$muscle <- factor(max_point_sets$muscle, levels = muscle_name_per_index)
st_vec <- c(0.156053824008658, 0.296711520007215, 0.437369216005772, 0.578026912004329,
0.718684608002886, 0.859342304001443, 1)

min_max_per_config <- max_point_sets[st %in% st_vec[c(1,length(st_vec))],.(min_a=min(activation), max_a=max(activation),median_a=median(activation)),by=.(muscle,st,task_index)]
p <- ggplot(min_max_per_config, aes(task_index, min_a)) + geom_line() + facet_grid(st~muscle) + geom_line(aes(task_index, max_a)) + geom_line(aes(task_index,median_a))
p <- p + geom_ribbon(aes(ymin = max_a, ymax = pmin(min_a, max_a) , fill=factor(st)))
p <- p + geom_line(aes(task_index, median_a), col="white")
p <- p + theme_classic() + xlab('time')
p

p <- ggplot(min_max_per_config, aes(task_index, min_a, frame=st)) + geom_line() + facet_grid(~muscle) + geom_line(aes(task_index, max_a)) + geom_line(aes(task_index,median_a))
p <- p + geom_ribbon(aes(ymin = max_a, ymax = pmin(min_a, max_a) ),fill="black")
p <- p + geom_line(aes(task_index, median_a), col="white")
p <- p + theme_classic()
gganimate::gganimate(p, output_subfolder_path("st", "st.html"), ani.width=1200, ani.height=400)

test_that('A',{
	p <- ggplot(point_sets, aes(task_index, activation, group=task_index)) + geom_boxplot(outlier.size=0.25) + facet_grid(st~muscle)
	p <- p + theme_classic()
	ggsave('submaximal_set.png', p, width=8)

	p1 <- ggplot(max_point_sets, aes(task_index, activation, group=task_index)) + geom_boxplot(outlier.size=0.25) + facet_grid(st~muscle)
	p1 <- p1 + theme_classic()
	ggsave('maximal_set.png', p1, width=8)
})

a <- point_sets%>% as.data.frame
tasks <- sort(unique(a$task_index))
bounds_per_task_no_st <- lapply(tasks, function(vector_out){
	taskRHSconstraint <- a_matrix_rhs_task(H_matrix, c(vector_out,0,0,0), bounds_tuple_of_numeric)
	extreme_points <- findExtremePoints(taskRHSconstraint)
	mins <- colMins(extreme_points)
	maxes <- colMaxes(extreme_points)
	bound_info <- cbind(muscle=muscle_name_per_index,activation_min=mins,activation_max=maxes,task_index = rep(vector_out, length(maxes)))
	return(bound_info %>% as.data.frame)
}) %>% dcrb

bounds_per_task_no_st$activation_min <- as.numeric(as.character(bounds_per_task_no_st$activation_min))
bounds_per_task_no_st$activation_max <- as.numeric(as.character(bounds_per_task_no_st$activation_max))
bounds_per_task_no_st$task_index <- as.numeric(as.character(bounds_per_task_no_st$task_index))


ggplot(bounds_per_task_no_st, aes(task_index,activation_min, fill=muscle)) + geom_point(col="blue") + facet_grid(~muscle) + geom_point(aes(task_index, activation_max))

