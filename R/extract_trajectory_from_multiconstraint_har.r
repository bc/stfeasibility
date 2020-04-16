library(data.table)
five_concat <- as.data.frame(fread("Aconcat_output.csv"))
idx <- 1:(31*5)

cat_indices_rolling <- function(i) (1:31)+31*(i-1)

degenerate_trajectories <- lapply(1:5, function(i) {
	task_i <- five_concat[,cat_indices_rolling(i)]
	task_i$point_id <- 1:nrow(task_i)
	melted_task <- melt(task_i,id.vars="point_id")
	if (i>1){
		melted_task$task_i <- as.integer(sub("^[^_]*_", "", melted_task$variable))
		melted_task$muscle <- as.factor(gsub("_.*","",melted_task$variable))
	} else {
		#when there is no _# at end of muscle name
		melted_task$task_i <- 0
		melted_task$muscle <- melted_task$variable
	}
	return(melted_task)
	}) %>% rbindlist


# per_muscle_per_task_hists <- ggplot(degenerate_trajectories, aes(y=value)) + geom_histogram() + facet_grid(muscle~task_i)

# animate_hists <- ggplot(degenerate_trajectories%>%as.data.frame, aes(y=value, frame=task_i, col=muscle)) + geom_histogram()
# ggplotly(animate_hists)
# gganimate::gganimate(animate_hists, "animation_bytask.html", ani.width=1200, ani.height=400)