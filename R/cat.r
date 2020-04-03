
get_cat_H_matrix <- function(cat_mat, cat_number){
	get_R <- function(cat_mat, cat_number) cat_mat$Cats[[cat_number]][[1]][[1]]
	get_J <- function(cat_mat, cat_number) cat_mat$Cats[[cat_number]][[1]][[2]]
	get_J_pinv <- function(cat_mat, cat_number) pracma::pinv(get_J(cat_mat,cat_number))
	scalings_for_muscles <- as.numeric(cat_mat$afl95) * (cat_mat$fmax * cat_mat$cosa95)
	muscle_scaling_diag <- diag(as.numeric(scalings_for_muscles))
	RFm <- get_R(cat_mat, 1) %*% muscle_scaling_diag
	H_matrix_cat <- pracma::pinv(get_J(cat_mat,cat_number))%>%t %*% RFm

	cat_muscle_names <- as.character(unlist(cat_mat$muscles))

	cat_wrench_names <- c("x","y","z","tx","ty","tz")
	colnames(H_matrix_cat) <- cat_muscle_names
	rownames(H_matrix_cat) <- cat_wrench_names
	bounds_tuple_of_numeric_cat <- rep(list(list(lower = 0, upper = 1)), 31)
return(list(H_matrix = H_matrix_cat%>%as.matrix, bounds_tuple_of_numeric = bounds_tuple_of_numeric_cat)
	)}

get_muscle_bounds_for_rhs_task <- function(constraint){
	n_muscles <- ncol(constraint$constr)
	muscle_identities <- diag(n_muscles)
	bound_solutions <- apply(muscle_identities, 1, function(muscle_focus){
	lb_solution <- simplex(muscle_focus, nulltask$constr,nulltask$rhs)
	ub_solution <- simplex(-1*muscle_focus, nulltask$constr,nulltask$rhs)
	return(data.table(lb=lb_solution$value,ub=ub_solution$value*-1.0))
	})
	return(rbindlist(bound_solutions))
}