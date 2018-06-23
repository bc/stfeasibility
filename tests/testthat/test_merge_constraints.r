context('Functions for merging constraints')


diagonal_merge_constraints <- function(first_constraint, second_constraint, string_to_append_to_second_constraint){
	first_constraint_copy <- first_constraint
	second_constraint_copy <- second_constraint

	padding_for_constraint1 <- zeros_df(nrow(first_constraint$constr), ncol(second_constraint$constr))
	appended_second_constraint_colnames <- paste(colnames(second_constraint$constr),string_to_append_to_second_constraint, sep="_")
	dimnames(padding_for_constraint1) <- list(rownames(first_constraint$constr),appended_second_constraint_colnames)
	first_constraint_copy$constr <- cbind(first_constraint$constr, padding_for_constraint1)

	padding_for_constraint2 <- zeros_df(nrow(second_constraint$constr), ncol(second_constraint$constr))
	dimnames(padding_for_constraint2) <- list(rownames(second_constraint$constr),colnames(first_constraint$constr))
	second_constraint_copy$constr <- cbind(padding_for_constraint2, second_constraint$constr)
	rownames(second_constraint_copy$constr) <- paste(rownames(second_constraint$constr),string_to_append_to_second_constraint, sep="_")
	merged_constraint <- merge_constraints(first_constraint_copy, second_constraint_copy)
	browser()
	return(merged_constraint)
}


test_that("two constraints can be combined without affecting one another's individual outputs", {
	# fx_constraint
	first_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(1,0,0,0), bounds_tuple_of_numeric) 
	# fy_constraint
	second_constraint<- a_matrix_lhs_direction(H_matrix, direction = c(0,1,0,0), bounds_tuple_of_numeric) 	
	merged_constraint <- diagonal_merge_constraints(first_constraint, second_constraint, "2")
	#via https://www.r-bloggers.com/creating-an-image-of-a-matrix-in-r-using-image/
	rotate <- function(x) t(apply(x, 2, rev))

	image(merged_constraint$constr%>%as.matrix%>%rotate%>%sqrt, col=brewer.pal(12, "PRGn"))

	# expect_equal(minimizing on left constraint result, minimizing on combined constraint[left part])
})


append_with_underscore <- function(s,appendix) paste0(s,"_",appendix)