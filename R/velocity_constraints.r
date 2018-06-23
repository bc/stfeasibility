
transition_inequalities_for_velocity <- function(transition_specification){
	n <- transition_specification$num_muscles
	zeros_col <- data.frame(rep(0, n*2))
    time_point_1 <- rbind(-diag(n), diag(n))
    dimnames(time_point_1) <- NULL
    df <- cbind(time_point_1, zeros_col, -time_point_1, zeros_col)
    colnames(df) <- transition_specification$colnames
    return(df)
}

transition_specifications <- function(constraint_object, num_pairs, cols_per_task, num_muscles){
	info_per_transition <- lapply(1:num_pairs, function(pair_index){
		from_here <- (cols_per_task*pair_index-num_muscles)
		to_here <- (pair_index*cols_per_task)
		transition_indices <- from_here:(to_here+cols_per_task)
		print(transition_indices)
		transition_colnames <- colnames(constraint_object$constr)[transition_indices]
		return(list(indices=transition_indices, colnames=transition_colnames, num_muscles=num_muscles))
	})
	return(info_per_transition)
}

zeros <- function(rows,cols) matrix(0.0, nrow=rows, ncol=cols)
add_velocity_constraint <- function(constraint_object, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed, indices_for_muscles, num_muscles){
	    num_muscles <- get_num_muscles_via_indices_for_muscles(indices_for_muscles)
	    num_tasks <- sum(indices_to_control(constraint_object, indices_for_muscles))
	    num_pairs <- num_tasks - 1
	    cols_per_task <- num_muscles + 1
	    info_per_transition <- transition_specifications(constraint_object, num_pairs, cols_per_task, num_muscles)
	    transition_blocks <- lapply(info_per_transition, transition_inequalities_for_velocity)
	    velocity_constraint <- lapply(1:num_pairs, function(transition_index){
	    	left_pad_blocks <- transition_index - 1
	    	right_pad_blocks <- -transition_index + 5
	    	L <- zeros(num_muscles*2, cols_per_task*left_pad_blocks) %>% as.data.frame
	    	R <- zeros(num_muscles*2, cols_per_task*right_pad_blocks) %>% as.data.frame
	    	constraint_row <- cbind(L, transition_blocks[[transition_index]], R)
	    })
	    velocity_constraint_constr <-dcrb(lapply(velocity_constraint, function(x) {
	    	colnames(x) <- NULL
	    	return(x%>%as.matrix)
	    }))
	    colnames(velocity_constraint_constr) <- colnames(constraint_object$constr)
	    combinations <- which(velocity_constraint_constr != 0, arr.ind=TRUE)
	    browser()
    	return(velocity_constraint)
	}