
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
		transition_colnames <- colnames(constraint_object$constr)[transition_indices]
		return(list(indices=transition_indices, colnames=transition_colnames, num_muscles=num_muscles))
	})
	return(info_per_transition)
}

zeros <- function(rows,cols) matrix(0.0, nrow=rows, ncol=cols)

create_increasing_and_decreasing_speedlimit_rownames <- function(info_per_transition, cols_per_task, num_muscles){
	    constraint_rownames <- lapply(info_per_transition, function(e) {
	    	all_colnames <- e$colnames
	    	rowname_part1 <- all_colnames[1:num_muscles]
	    	rowname_part2 <- all_colnames[(cols_per_task+1):(length(all_colnames)-1)]
	    	combos <- apply(cbind(rowname_part1, rowname_part2),1,paste, collapse="_to_")
	    	return(combos)
		})
		return(constraint_rownames)
	    }

velocity_constraint_rownames <- function(info_per_transition, cols_per_task, num_muscles){
	constraint_rownames <- create_increasing_and_decreasing_speedlimit_rownames(info_per_transition, cols_per_task, num_muscles)
	extended_rownames <- dcclapply(constraint_rownames, function(x){
			pos <- paste0(x, "_positive_speedlimit")
			neg <- paste0(x, "_negative_speedlimit")
			return(c(pos,neg))
		})
	return(extended_rownames)
	}


compose_velocity_constraint_per_transition <- function(transition_blocks, cols_per_task, num_muscles){
velocity_constraint <- lapply(1:length(transition_blocks), function(transition_index){
	left_pad_blocks <- transition_index - 1
	right_pad_blocks <- -transition_index + length(transition_blocks)
	L <- zeros(num_muscles*2, cols_per_task*left_pad_blocks) %>% as.data.frame
	R <- zeros(num_muscles*2, cols_per_task*right_pad_blocks) %>% as.data.frame
	constraint_row <- cbind(L, transition_blocks[[transition_index]], R)
	return(constraint_row)
})
return(velocity_constraint)
}

combine_velocity_constraints <- function(velocity_constraint_list) {
    velocity_constraint_constr <- dcrb(lapply(velocity_constraint_list, function(x) {
        colnames(x) <- NULL
        return(x %>% as.matrix)
    }))
    return(velocity_constraint_constr)
}

set_dimnames <- function(input_df_or_mat, colnames_to_use, rownames_to_use) {
    # Make copy
    df_or_mat <- input_df_or_mat
    colnames(df_or_mat) <- colnames_to_use
    rownames(df_or_mat) <- rownames_to_use
    return(df_or_mat)
}

generate_and_add_velocity_constraint <- function(constraint_object, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed, indices_for_muscles, num_muscles){
	velocity_constraint <- generate_full_velocity_constraint(constraint_object, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed, indices_for_muscles, num_muscles)
	constraint_with_velocity_requirements <- merge_constraints(constraint_object, velocity_constraint)
	expect_equal(sum(nrow(constraint_object$constr),nrow(velocity_constraint$constr)),nrow(constraint_with_velocity_requirements$constr))
	return(constraint_with_velocity_requirements)
}

generate_full_velocity_constraint <- function(constraint_object, max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed, indices_for_muscles, num_muscles){
	num_tasks <- sum(indices_to_control(constraint_object, indices_for_muscles))
	num_pairs <- num_tasks - 1
	velocity_constraint <- generate_velocity_constraint_matrix(constraint_object, indices_for_muscles, num_tasks, num_pairs, num_muscles)
	image(velocity_constraint%>%rotate)
	expect_equal(ncol(velocity_constraint), num_tasks*(num_muscles + 1))
	expect_equal(nrow(velocity_constraint), num_pairs*num_muscles*2)
	max_inc_speed <- rep(max_allowable_increasing_tension_speed, num_muscles)
	max_dec_speed <- rep(max_allowable_decreasing_tension_speed, num_muscles)
	rhs_velocity <- rep(c(max_inc_speed, max_dec_speed),num_pairs)
	dir_velocity <- rep("<=", nrow(velocity_constraint))
	return(list(constr=velocity_constraint, dir=dir_velocity, rhs=rhs_velocity))
}

generate_velocity_constraint_matrix <- function(constraint_object, indices_for_muscles, num_tasks, num_pairs, num_muscles){
	    cols_per_task <- num_muscles + 1
	    info_per_transition <- transition_specifications(constraint_object, num_pairs, cols_per_task, num_muscles)
	    constraint_rownames <- velocity_constraint_rownames(info_per_transition, cols_per_task, num_muscles)
	    transition_blocks <- lapply(info_per_transition, transition_inequalities_for_velocity)
	    velocity_constraint <- compose_velocity_constraint_per_transition(transition_blocks,
	        cols_per_task, num_muscles) %>% combine_velocity_constraints
	    expect_equal(num_pairs*num_muscles*2, nrow(velocity_constraint))
	    velocity_constraint <- set_dimnames(velocity_constraint, colnames(constraint_object$constr),
	        constraint_rownames)
    	return(velocity_constraint)
	}