muscles_in_order <- function() c("FDP","FDS","EIP","EDC","LUM","DI","PI")

unit_cube_zoom <- function() coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

##' @see a_matrix_lhs_direction
negative_string <- function(s) paste0("-", s)

str1 <- function(anything) str(anything,1)


##' Ensure a folder exists, write if nonexistent
##' Derived from https://stackoverflow.com/a/29784923/2438134
##' @param mainDir main directory path character
##' @param subDir folder name character
##' @return it_had_to_be_created logical, lets you know if was just created.
ensure_folder_exists <- function(mainDir, subDir) {
    ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir,
        subDir)), FALSE)
}

## via https://stackoverflow.com/questions/46085274/is-there-a-string-formatting-operator-in-r-similar-to-pythons
`%--%` <- function(x, y) {
  do.call(sprintf, c(list(x), y))
}

#useful when you want to make a bunch of files with different names, where the time is the difference.
# example: "trajectories" %>% time_dot("png")
# will yield trajectories_2020-04-17 10/06/11.png
time_dot <- function(mystring, ending){
    paste0(mystring,"_", Sys.time(), ".",ending)
}

divide_vector_by_max_of_vectors_abs_value <- function(vector) vector/max(abs(vector))

##' Dataframe to list of cols
##' @description derived from https://stackoverflow.com/questions/3492379/data-frame-rows-to-a-list
##' @param df Data frame
##' @return df_list a list of elements, each of which is a representative col from the original df
df_to_list_of_cols <- function(df) {
    df_list <- setNames(split(df, seq(ncol(df))), colnames(df))
    return(df_list)
}


output_subfolder_path <- function(subfolder_name, filename, output_filepath = "../../output") {
    tryCatch({
        ensure_folder_exists(output_filepath, subfolder_name)
    }, warning = function(w) {
        print(w)
    }, error = function(e) {
        stop("Could not ensure the folder exists", e)
    })
    output_local_filepath <- paste0(subfolder_name, "/", filename)
    return(output_filepath(output_local_filepath))
}


ggparcoord_har <- function(df) {
    ggparcoord(df, scale = "globalminmax", alpha = 0.01, ...) + theme_classic()
}

col_blank <- function(df, FUN) apply(df, 2, FUN)
colMaxes <- function(df) col_blank(df, max)
colMins <- function(df) col_blank(df, min)
colMedians <- function(df) col_blank(df, median)
set_colnames <- function(df, vector) {
    df_copy <- df
    colnames(df_copy) <- vector
    return(df_copy)
}

append_with_underscore <- function(s, appendix) paste0(s, "_", appendix)

where_muscles_have_unreasonable_values <- function(df, muscle_names) {
    apply(df[, muscle_names], 2, function(muscle_entries) {
        negative <- muscle_entries < 0
        over_one <- muscle_entries > 1
        return(negative | over_one)
    })
}

stop_if_dimensionality_names_are_missing <- function(df) {
    if (is.null(rownames(df)) | is.null(colnames(df))) {
        stop("A matrix passed to a_matrix_lhs_direction must have colnames and rownames for the dimensions of input and output")
    }
}


zeros_df <- function(nrow, ncol) data.frame(matrix(0, ncol = ncol, nrow = nrow))
wrench_and_H_dims_match <- function(H_matrix, wrench) {
    if (nrow(H_matrix) != length(wrench)) {
        stop("nrow(H matrix) must match the length of direction.")
    }
}

##' do call concatenate upon all list elements returned by lapply
dcclapply <- function(...) {
    dcc(lapply(...))
}


##' @see lpsolve_force_in_dir
get_num_muscles_via_indices_for_muscles <- function(indices_for_muscles) {
    muscles_are_true <- 1:max(indices_for_muscles) %in% indices_for_muscles
    if (all(muscles_are_true)) {
        return(length(indices_for_muscles))
    } else {
        return(which(!(muscles_are_true))[1] - 1)
    }

}

muscle_and_lambda_indices <- function(constr, num_muscles) {
    indices_for_muscles <- 1:ncol(constr$constr)
    indices_for_lambdas <- (indices_for_muscles%%(num_muscles + 1)) != 0
    indices_for_muscles <- indices_for_muscles[indices_for_lambdas]
    return(list(indices_for_muscles = indices_for_muscles, indices_for_lambdas = indices_for_lambdas))
}


indices_to_control <- function(constr_object, indices_for_muscles) {
    indices_to_control <- rep(1, ncol(constr_object$constr))
    # only optimizing over the output force
    indices_to_control[indices_for_muscles] <- 0
    c_in_cTx <- indices_to_control
    return(c_in_cTx)
}

gen_c_in_cTx_for_lambdas <- function(constraint, num_muscles) {
    indices_ml <- muscle_and_lambda_indices(constraint, num_muscles)
    c_in_cTx <- as.integer(indices_ml$indices_for_lambdas)
    return(c_in_cTx)
}

##' Output Filepath
##' by default takes the starting working directory from the tests/testthat directory.
##' @param out_path by default ../../output/
##' @param filename filename of interest
##' @return output_filepath stringpath
output_filepath <- function(filename, out_path = "../../output") file.path(out_path,
    filename)

##' Shard a total
##' useful for splitting hit and run desired samples into groups of samples for parallelization
##' @see har_sample
##' @param total total sum of elements
##' @param n_shards length of the output vector
##' @return shard_nums a vector of length n_shards, which adds up to the total
shard_a_total <- function(total, n_shards) {
    if (n_shards > total) {
        stop("too many shards requested. the total should be at least the size of the n_shards")
    }
    sequence <- rep(floor(total/n_shards), n_shards)
    # add remainder to first element
    sequence[1] <- sequence[1] + (total - sum(sequence))
    return(sequence)
}

split_into_pieces <- function(vector, slice_size) {
    if (length(vector)%%slice_size != 0) {
        stop("input was not cleanly split into the desired slice size")
    }
    split(vector, seq_along(vector)/slice_size)
}

##' Bisection Method for logical '
##'  inspiration: http://dkmathstats.com/the-bisection-method-in-r/
##' a = lowest acceptable value
##' b = highest acceptable value
##' @param tol lowest allowable absolute different between lower and upper bound. will return upper bound of the two.
##' @param f function with logical output, 1 input that a or b is plugged in.

bisection_method <- function(a, b, tol, f, ...) {
    midpt <- function(a,b) sum(b,a)*0.5
    print_bounds <- function(a,b) print(paste("[",a,"——",b,"]"))
    print_bounds(a,b)
    prospective_bound <- midpt(a,b)
    if (f(b, ...) == FALSE) {
        stop("Upper limit is supposed to be TRUE, but was FALSE")
    } else if(abs(b-a) < tol){
        return(b)
    } else if (f(prospective_bound, ...)==TRUE){
        return(bisection_method(a, prospective_bound, tol, f))
    } else if (f(prospective_bound, ...)==FALSE){
        return(bisection_method(prospective_bound, b, tol, f))
    } else if (f(a, ...) == FALSE && f(b, ...) == FALSE){
        stop("limits are both false")
        # we have to go deeper
    } else {
        print("a")
        print(a)
        print(f(a, ...))
        print("b")
        print(b)
        print(f(b, ...))
        stop("unknown err")
    }

}

    


    #helper functions from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
    StatChull <- ggproto("StatChull", Stat,
      compute_group = function(data, scales) {
        data[chull(data$x, data$y), , drop = FALSE]
      },
      
      required_aes = c("x", "y")
    )
    stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
        layer(
            stat = StatChull, data = data, mapping = mapping, geom = geom, 
            position = position, show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        )
    }

    #/end helper functions from https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

