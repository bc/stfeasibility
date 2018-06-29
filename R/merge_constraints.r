##' Merge Constraints and respect rhs_dimnames and constr_dimnames
##' wraps hitandrun::mergeConstraints to preserve component dimnames
##' @param a,b a constraint object as in hitandrun
##' @return ab merged constraint object
merge_constraints <- function(a,b){
    constr <- mergeConstraints(a,b)
    constr$rhs_dimnames <- c(a$dimnames, b$dimnames)
    constr$constr_dimnames <- a$constr_dimnames
    return(constr)
}

##' create vector of inequalities ("<=" to match shape of constraint
##' useful when creating an inequality from scratch.
##' @param constr constraint matrix as in hitandrun
##' @return dir vector of "<=" of len == nrow(constr)
all_less_than <- function(constr) rep("<=", nrow(constr))

##' Print constraint to terminal
##' Useful for inspection. see plot_constraint_matrix for a visual representation
##' @param constr constraint matrix as in hitandrun
print_constraint <- function(constraint_object){
    print(paste(nrow(constraint_object$constr),"rows (constraints)",ncol(constraint_object$constr),"cols (variables)"))
    cbind(constraint_object$constr, dir=constraint_object$dir, rhs=constraint_object$rhs)%>% as.data.frame %>% print
}

##' Diagonally merge two constraints
##' combines two constraints, and renames the colnames of the second constraint 
##' to make sure they do not stack atop one another.
##' @param first_constraint,second_constraint constraint objects with $constr, $dir, $rhs
##' @param string_to_append_to_second_constraint string to append. i.e. a number. it will be appended after an underscore.
##' @return constr combined constraint going down the diagonal.
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
    merged_constraint$constr <- merged_constraint$constr %>% as.matrix
    return(merged_constraint)
}

##' Diagonally merge constraint list
##' Accepts inputs of length 1, 2, to n. Respects colnames and rownames
##' Adds iteravely to the diagonal. Performance fo current implementation is acceptable.
##' @param list_of_constraints each a constraint, see `?hitandrun`
##' @return constr combined constraint going down the diagonal.
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

##' Wrapped function for eliminateRedundant that preserves colnames
##' @see eliminateRedundant
##' @inheritParams plot_constraint_matrix
##' @return constraint_object constraint object, with colnames but no rownames
eliminate_redundant <- function(constraint_object){
    res <- eliminateRedundant(constraint_object)
    colnames(res$constr) <- colnames(constraint_object$constr)
    return(res)
}

##' remove labels
##' useful for ggplot objects you want to clean up
##' @return p ggplot object function that can be added to a ggplot object.
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

##' Rotate Matrix
##' Useful when you want to visualize a matrix with image().
##' via https://www.r-bloggers.com/creating-an-image-of-a-matrix-in-r-using-image/
##' @param x matrix
##' @return x_prime rotated matrix
rotate <- function(x) t(apply(x, 2, rev))


##' Plot constraint matrix as colorful xy plot
##' useful for visualizing constraint matrices before and after changes
##' @param constraint constraint object, with constr, dir, and rhs elements.
##' return p ggplot object
plot_constraint_matrix <- function(constraint) {
    if(is.null(rownames(constraint$constr))){
        message('Plotting redundant-free constraint without constraint names')
        rownames(constraint$constr) <- paste("c", 1:nrow(constraint$constr), sep="_")
    }
    mat <- cbind(constraint$constr, constraint$rhs) %>% melt %>% as.data.frame
    mat$Var1 <- factor(mat$Var1, levels = rev(levels(mat$Var1)))
    p <- ggplot(mat, aes(x = Var2, y = Var1)) + geom_raster(aes(fill = value)) +
        scale_fill_gradientn(colors = brewer.pal(11, "PRGn")) + labs(x = "input variable per timestep",
        y = "constraint", title = "Matrix") + theme_classic() + theme(axis.text.x = element_text(size = 9,
        angle = 0, vjust = 0.3), axis.text.y = element_text(size = 9), plot.title = element_text(size = 11))
    return(p + remove_labels())
}

##' compose velocity constraint
##' require a given muscle's activation to change no more than some delta between two timepoints.
##' @param constraint constraint object as in ?hitandrun
##' @param max_allowable_increasing_tension_speed, max_allowable_decreasing_tension_speed value between min-max of muscle activation capability that defines maximal muscle activation change.
##' @return  velocity_constraint constraint object
compose_velocity_constraint <- function(constraint, max_allowable_increasing_tension_speed,
    max_allowable_decreasing_tension_speed) {
    indices_for_muscles <- muscle_and_lambda_indices(constraint, 7)$indices_for_muscles
    num_muscles <- get_num_muscles_via_indices_for_muscles(indices_for_muscles)
    velocity_constraint <- generate_and_add_velocity_constraint(constraint, max_allowable_increasing_tension_speed,
        max_allowable_decreasing_tension_speed, num_muscles)
    return(velocity_constraint)
}