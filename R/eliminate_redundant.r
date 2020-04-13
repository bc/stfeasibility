##' Wrapped function for eliminateRedundant that preserves colnames
##' @see eliminateRedundant
##' @inheritParams plot_constraint_matrix
##' @return constraint_object constraint object, with colnames and rownames
eliminate_redundant_single <- function(constraint_object) {
    nonredundant <- eliminate_redundant_hitandrun(constraint_object)
    colnames(nonredundant$constr) <- colnames(constraint_object$constr)
    nonredundant_res <- set_nr_rownames(constraint_object, nonredundant)
    return(nonredundant_res)
}

set_nr_rownames <- function(constraint_object, nr_constraint_object){
    positions <- attr(nr_constraint_object, "redundancy_info")$new.position
    nr_rownames<- rownames(constraint_object$constr)[which(positions != 0)]
    indices_for_nr_rownames <- positions[positions != 0]
    num_before <- nrow(constraint_object$constr)
    num_after <- length(indices_for_nr_rownames)
    message("n_constr:%s-->%s; %s removed" %--% c(num_before,num_after,num_before - num_after))
    rownames(nr_constraint_object$constr)[indices_for_nr_rownames] <- nr_rownames
    return(nr_constraint_object)
}

##' Wrapped function for eliminateRedundant that preserves colnames
##' @see eliminateRedundant
##' @inheritParams plot_constraint_matrix
##' @return constraint_object constraint object, with colnames but no rownames
eliminate_redundant <- function(constraint_object, mc.cores=NULL) {
    if (is.null(mc.cores)){
      return(eliminate_redundant_single(constraint_object))
    }
    num_constraints <- nrow(constraint_object$constr)
    message("Attempting reduction elimination on %s constraints"%--%num_constraints)
    constraint_list <- split_constraints(constraint_object, mc.cores)
    nonredundant_shards <- pbmclapply(constraint_list, eliminate_redundant_single, mc.cores=mc.cores)
    redundant_constraints_removed_per_shard <- sapply(nonredundant_shards, function(e){
      length(extract_redundant_rows(e))
    })
    message(paste("Removed per shard",paste(redundant_constraints_removed_per_shard, collapse=",")) )
    c2 <- merge_constraints_list(nonredundant_shards)
    res <- c2 %>% eliminate_redundant_single
    constraints_per_step <- c(nrow(constraint_object$constr),nrow(c2$constr), nrow(res$constr))
    "Number of constraints:  %s -->(shard cull)--> %s -->(final cull)--> %s"  %--% constraints_per_step %>% print
    return(res)
}

eliminate_redundant_hitandrun <- function(constr) {
  n <- ncol(constr$constr)

  eq <- hitandrun:::eq.constr(constr)
  iq <- hitandrun:::iq.constr(constr)

  h <- 
    if (length(eq[['dir']]) > 0) {
      rcdd::makeH(iq$constr, iq$rhs, eq$constr, eq$rhs)
    } else {
      rcdd::makeH(iq$constr, iq$rhs)
    }

  d2q.res <- rcdd::d2q(h)

  if (nrow(d2q.res) < 2) {
      print('rownum was under 2')
      return(constr)
  }
  redundancy_result <- rcdd::redundant(d2q.res)
  h.nr <- rcdd::q2d(redundancy_result$output)
  rows <- h.nr[, 1] == 0
  nvar <- ncol(constr$constr)
  nr_constr <- list(
    constr = -h.nr[ , -c(1, 2), drop=FALSE],
    rhs    = h.nr[ , 2, drop=TRUE],
    dir    = c("<=", "=")[h.nr[, 1] + 1])
  attr(nr_constr, "redundancy_info") <- redundancy_result
  return(nr_constr)
}

##' extract redundant rows
##' useful for eliminate_redundant
##' @param er_res output from eliminate_redundant, where an attr called "redundancy_info" exists
##' @return vector of indices that were deleted through redundancy elimination.
extract_redundant_rows <- function(err_res_constr) {
  which(attr(err_res_constr,"redundancy_info")$new.position == 0)
}


#testing functions
force_cos_H_matrix_7_har <- function(increasing,decreasing,num_tasks, num_har, er2, n_task_values){
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric,
        c(28.8,0,0,0), increasing, decreasing, n_task_values = n_task_values,
        eliminate = FALSE)
    if(is.null(er2)){
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant_single
    } else {
        res <- st_constr_str$nonredundant_constr %>% eliminate_redundant(er2)
    }
    # completed culling of redundant constraints
    tryCatch({points <- res %>% pb_har_sample(1e5, mc.cores=8, eliminate=FALSE)}, error=function(cond) {
      message('The sampler is reporting and error. The space might be infeasible.\nEntering browser in force_cos_ramp_constraint in eliminate_redundant.r')
      browser()
      return("INFEASIBLE")
      })
    result <- list(nr_constr = res, points = points, st_constr_str = st_constr_str)
    return(result)
}

create_nonredundant_structures <- function(n_task_values){

    mbm <- microbenchmark("a" = {
        a <- force_cos_H_matrix_7_har(0.5, 0.5, 10, 1e4, 8, n_task_values); print("a")
    }, "b" = {
        b <- force_cos_H_matrix_7_har(1, 1, 10, 1e4, 8, n_task_values); print("b")
    }, "c" = {
        c <- force_cos_H_matrix_7_har(0.4, 0.4, 10, 1e4, 8, n_task_values); print("c")
    }, "d" = {
        d <- force_cos_H_matrix_7_har(0.3, 0.3, 10, 1e4, 8, n_task_values); print("d")
    }, "e" = {
        e <- force_cos_H_matrix_7_har(0.2, 0.2, 10, 1e4, 8, n_task_values); print("e")
    }, "f" = {
        f <- force_cos_H_matrix_7_har(0.1, 0.1, 10, 1e4, 8, n_task_values); print("f")
    }, "g" = {
        g <- force_cos_H_matrix_7_har(0.05, 0.05, 10, 1e4, 8, n_task_values); print("g")
    }, "h" = {
        h <- force_cos_H_matrix_7_har(0.03, 0.03, 10, 1e4, 8, n_task_values); print("h")
    }, "i" = {
        i <- force_cos_H_matrix_7_har(0.01, 0.01, 10, 1e4, 8, n_task_values); print("i")
    }, "a_orig_redundancy" = {
        a_orig_redundancy <- force_cos_H_matrix_7_har(0.5, 0.5, 10, 1e4, NULL, n_task_values); print("a_orig_redundancy")
    }, "b_orig_redundancy" = {
        b_orig_redundancy <- force_cos_H_matrix_7_har(1, 1, 10, 1e4, NULL, n_task_values); print("b_orig_redundancy")
    }, "c_orig_redundancy" = {
        c_orig_redundancy <- force_cos_H_matrix_7_har(0.4, 0.4, 10, 1e4, NULL, n_task_values); print("c_orig_redundancy")
    }, "d_orig_redundancy" = {
        d_orig_redundancy <- force_cos_H_matrix_7_har(0.3, 0.3, 10, 1e4, NULL, n_task_values); print("d_orig_redundancy")
    }, "e_orig_redundancy" = {
        e_orig_redundancy <- force_cos_H_matrix_7_har(0.2, 0.2, 10, 1e4, NULL, n_task_values); print("e_orig_redundancy")
    }, "f_orig_redundancy" = {
        f_orig_redundancy <- force_cos_H_matrix_7_har(0.1, 0.1, 10, 1e4, NULL, n_task_values); print("f_orig_redundancy")
    }, "g_orig_redundancy" = {
        g_orig_redundancy <- force_cos_H_matrix_7_har(0.05, 0.05, 10, 1e4, NULL, n_task_values); print("g_orig_redundancy")
    }, "h_orig_redundancy" = {
        h_orig_redundancy <- force_cos_H_matrix_7_har(0.03, 0.03, 10, 1e4, NULL, n_task_values); print("h_orig_redundancy")
    }, "i_orig_redundancy" = {
        i_orig_redundancy <- force_cos_H_matrix_7_har(0.01, 0.01, 10, 1e4, NULL, n_task_values); print("h_orig_redundancy")
    }, times = 1)
    print('%%%%%%%%%%%%%%%')
    print('task values:')
    print(n_task_values)
    print(mbm)
    print('%%%%%%%%%%%%%%%')
    nonredundant_structures <- list(a = a, b = b, c = c, d = d, e = e, f = f, g = g,
        h = h, i = i, a_orig_redundancy = a_orig_redundancy, b_orig_redundancy = b_orig_redundancy,
        c_orig_redundancy = c_orig_redundancy, d_orig_redundancy = d_orig_redundancy,
        e_orig_redundancy = e_orig_redundancy, f_orig_redundancy = f_orig_redundancy,
        g_orig_redundancy = g_orig_redundancy, h_orig_redundancy = h_orig_redundancy,
        i_orig_redundancy = i_orig_redundancy)
    return(nonredundant_structures)
}