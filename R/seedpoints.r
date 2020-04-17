extract_n_seeds_from_rda_ste <- function(st_res, n){
	library(data.table)
    seeds <- sample(1:max(st_res$muscle_trajectory),n_seeds,replace=FALSE)
    st_res_dt <- data.table(st_res)
    only_seeds <- st_res_dt[muscle_trajectory%in%seeds & task_index == 0,activation,by=.(muscle_trajectory, muscle)]    
    setorder(only_seeds, "muscle_trajectory")
    only_seeds$muscle <- factor(only_seeds$muscle, levels = colnames(H_multiconstraint$constr)[1:7])
    seeds <- data.frame(dcast(only_seeds, muscle~muscle_trajectory, value.var="activation"))
    activation_per_seed <- seeds[,-1]
	rownames(activation_per_seed) <- seeds[,1]
	return(activation_per_seed)
}

#puts a diag and -diag equality with the activations in task_0 for the input activations7
assemble_equality_with_seed_point <- function(activations7){
	library(data.table)
	equalityconst <- cbind(diag(7), matrix(0,7,42))
	rownames(equalityconst) <- paste0(seeds[,1],"_task0_equality_w_seed_const")
	colnames(equalityconst) <- colnames(H_multiconstraint$const)
	equalit_constr_formatted <- create_equality_constraint(equalityconst, activations7)
	return(list(constr = equalit_constr_formatted$constr[1:7,], dir= rep("=",7),rhs = equalit_constr_formatted$rhs[1:7]))
}

trim_top_of_constraint <- function(constraint, nrows_to_rm){
	newversion <- constraint # mk copy
	newversion$constr <- constraint$constr[(nrows_to_rm+1):nrow(ex$constr),]
	newversion$dir <- constraint$dir[(nrows_to_rm+1):length(ex$dir)]
	newversion$rhs <- constraint$rhs[(nrows_to_rm+1):length(ex$rhs)]
	return(newversion)
}