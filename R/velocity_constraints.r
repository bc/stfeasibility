
transition_inequalities_for_velocity <- function(task_i, transition_colnames){
    time_point_1 <- cbind(rbind(-diag(num_muscles), diag(num_muscles)), rep(0, num_muscles*2))
    df <- rbind(time_point_1, -time_point_1)
    colnames(df) <- transition_colnames
    return(df)
})

add_velocity_constraint <- function(constraint_object, indices_for_muscles, num_muscles){
	    num_muscles <- get_num_muscles_via_indices_for_muscles(indices_for_muscles)
	    num_tasks <- sum(indices_to_control(constraint_object, indices_for_muscles))
	    num_pairs <- num_tasks - 1
	    cols_per_task <- num_muscles + 1
	    colnames_per_pair <- lapply(1:num_pairs, function(pair_index){
	    	from_here <- (cols_per_task*pair_index-num_muscles)
	    	to_here <- (pair_index*cols_per_task)
	    	colnames(constraint_object$constr)[from_here:to_here]
	    })

    	browser()
    	return(0)
	}
]