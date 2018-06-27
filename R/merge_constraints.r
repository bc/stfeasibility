
diagonal_merge_constraints <- function(first_constraint, second_constraint, string_to_append_to_second_constraint){
    first_constraint_copy <- first_constraint
    second_constraint_copy <- second_constraint
    padding_for_constraint1 <- zeros_df(nrow(first_constraint$constr), ncol(second_constraint$constr))
    appended_second_constraint_colnames <- paste(colnames(second_constraint$constr),string_to_append_to_second_constraint, sep="_")
    dimnames(padding_for_constraint1) <- list(rownames(first_constraint$constr),appended_second_constraint_colnames)
    first_constraint_copy$constr <- cbind(first_constraint$constr, padding_for_constraint1)

    padding_for_constraint2 <- zeros_df(nrow(second_constraint$constr), ncol(first_constraint$constr))
    dimnames(padding_for_constraint2) <- list(rownames(second_constraint$constr),colnames(first_constraint$constr))
    lapply(list(rownames(second_constraint$constr),colnames(first_constraint$constr)),length)
    second_constraint_copy$constr <- cbind(padding_for_constraint2, second_constraint$constr)
    rownames(second_constraint_copy$constr) <- paste(rownames(second_constraint$constr),string_to_append_to_second_constraint, sep="_")
    merged_constraint <- merge_constraints(first_constraint_copy, second_constraint_copy)
    return(merged_constraint)
}


##' @return constr combined constraint.
diagonal_merge_constraint_list <- function(list_of_constraints){
	list_len <- length(list_of_constraints)
	if (list_len == 1){
		return(list_of_constraints[[1]])
	}
	else if(list_len==2){
		return(diagonal_merge_constraints(list_of_constraints[[1]], list_of_constraints[[2]],1))
	}
	var_result <- diagonal_merge_constraints(list_of_constraints[[1]], list_of_constraints[[2]], 1)
	for (pair_i in seq(2,list_len-1)) {
		var_result <- diagonal_merge_constraints(var_result, list_of_constraints[[pair_i+1]], pair_i)
	}
    var_result$constr <- var_result$constr %>% as.matrix
    var_result$dir <- var_result$dir %>% as.vector
    var_result$rhs <- var_result$rhs %>% as.vector
	return(var_result)
}



remove_labels <- function(){
    theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )
}
#via https://www.r-bloggers.com/creating-an-image-of-a-matrix-in-r-using-image/
rotate <- function(x) t(apply(x, 2, rev))
plot_constraint_matrix <- function(constraint) {
    mat <- cbind(constraint$constr, constraint$rhs) %>% as.matrix %>% melt
    tall_constraints <- mat[mat$value != 0, ]
    tall_constraints$Var1 <- factor(tall_constraints$Var1, levels = rev(levels(tall_constraints$Var1)))
    p <- ggplot(tall_constraints, aes(x = Var2, y = Var1)) + geom_raster(aes(fill = value)) +
        scale_fill_gradientn(colors = brewer.pal(11, "PRGn")) + labs(x = "input variable per timestep",
        y = "constraint", title = "Matrix") + theme_classic() + theme(axis.text.x = element_text(size = 9,
        angle = 0, vjust = 0.3), axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))
    return(p + remove_labels())
}
	

        compose_velocity_constraint <- function(constraint, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed){
            indices_for_muscles <- muscle_and_lambda_indices(constraint, 7)$indices_for_muscles
            num_muscles <- get_num_muscles_via_indices_for_muscles(indices_for_muscles)
            velocity_constraint <- generate_and_add_velocity_constraint(constraint, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed, indices_for_muscles, num_muscles)
            return(velocity_constraint)
        }