##' Wrapped function for eliminateRedundant that preserves colnames
##' @see eliminateRedundant
##' @inheritParams plot_constraint_matrix
##' @return constraint_object constraint object, with colnames but no rownames
eliminate_redundant <- function(constraint_object) {
    res <- eliminate_redundant_hitandrun(constraint_object)
    colnames(res$constr) <- colnames(constraint_object$constr)
    return(res)
}

##' Wrapped function for eliminateRedundant that preserves colnames
##' @see eliminateRedundant
##' @inheritParams plot_constraint_matrix
##' @return constraint_object constraint object, with colnames but no rownames
eliminate_redundant2 <- function(constraint_object) {
    browser()

    res <- eliminate_redundant_hitandrun(constraint_object)
    colnames(res$constr) <- colnames(constraint_object$constr)
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
extract_redundant_rows <- function(er_res) {
  which(attr(er_res,"redundancy_info")$new.position == 0)
}
