la <- load_all

##' hyphens_to_underscores
##' @param str string or list of string to do replacement upon
##' @return str_prime string with underscores
hyphens_to_underscores <- function(str) {
  gsub("-", "_", str)
}

##' underscores to spaces
##' @param str string or list of string to do replacement upon underscores
##' @return str_prime string with spaces
underscores_to_spaces <- function(str) {
  gsub("_", " ", str)
}

##' hyphens_to_dots
##' @param str string or list of strings to do replacement upon
##' @return str_prime string with dot
hyphens_to_dots <- function(str) {
  gsub("-", ".", str)
}
##' dots_to_underscores
##' @param str string or list of strings to do replacement upon
##' @return str2 dots replaced with underscores
dots_to_underscores <- function(str) {
  gsub(".", "_", str, fixed = TRUE)
}


##' All values of a vector are within a desired range
##' TODO Test example:
##' > experimental_range
##' [1]  0.05845569 19.80210853
##' > range_tension
##' [1]  0 20
##' should return true'
##' @param vector vector of elements
##' @param bounds 2 element vector
is_within_range <- function(vector, bounds) {
  truth_vector <- dcc(lapply(vector, function(x) x < bounds[2] || x > bounds[1]))
  return(all(truth_vector))
}

##' downsampled_df'
##' TODO test
##' @param df dataframe
##' @param by Starting from 1, every 'by' datapoint will be kept
##' @param df2 dataframe that has been downsampled
downsampled_df <- function(df, by) df[seq(1, nrow(df), by = by), ]

##' Maximum absolute residual
##' TODO test
##' Of all of the absolute residuals from a desired value, this function returns the maximums
##' This is a good measure of the maximum variance.
##' @param vector vector of numeric values
##' @param desired_value numeric, the desired value that all values of the vector should match closely
##' @return max_abs_diff maximum absolute residual
maximum_absolute_residual <- function(vector, desired_val) max(abs(vector - desired_val))

##' Read RDS from package extdata folder
##' @param filename string, for the file within the extdata folder of the frontiers2017 package.
##' @return object the object yielded from the filepath rds
read_rds_from_package_extdata <- function(filename) {
  path <- system.file("extdata", filename, package = "frontiers2017")
  readRDS(path)
}

##' @title Save RDS to package extdata folder
##' @param filename string, for the file to place within the extdata folder of the frontiers2017 package.
##' @return object the object yielded from the filepath rds
save_rds_to_package_extdata <- function(object, filename) {
  path <- system.file('extdata', filename, package='frontiers2017')
  saveRDS(object, path)
}

##' @title Save RDS to resilio sync folder
##' TODO test
##' @param filename string, for the file to place within the extdata folder of the frontiers2017 package.
##' @return object the object yielded from the filepath rds
save_rds_to_Resilio <- function(object, filename) {
  path <- paste0("~/Resilio Sync/data/", filename)
  # path <- system.file('extdata', filename, package='frontiers2017')
  saveRDS(object, filename)
}

##' fread df from resilio
##' Get a dataframe from a file in Resilio's data folder
##' @param filename string of the object of interest, i.e. 'realTimeData2017_08_16_13_23_42.txt'
##' @return df data.frame of the file of interest, via fread with data.table=FALSE
##' @export
fread_df_from_Resilio <- function(filename){
  fread(get_Resilio_filepath(filename), data.table=FALSE)
}

get_Resilio_filepath <- function(filename) {
  paste0("~/Resilio Sync/data/", filename)
}

##' Do call concatenate
##' when output of lapply is a list of elements, where each element is a string or number or integer, this will create a simpified list
##' @param input_list input vector or list that will be concatenated
##' @return output concatenated vector
dcc <- function(input_list) {
  do.call("c", input_list)
}

##' Get Mode from vector
##' if there is no element that is the mode, it returns the first element of the list of equal-occurrence elements.
##' @param v vector of elements
##' @return mode most common element
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##' @title first_rowname
##' @param df data frame with rownames
##' @param name string for the first rowname
first_rowname <- function(df) head(rownames(df), 1)

##' @title last_rowname
##' @param df data frame with rownames
##' @param name string for the last rowname
last_rowname <- function(df) tail(rownames(df), 1)

##' Get full paths of all files within a directory
##' useful when you want to map across items within an entire directory.
##' @param pwd_of_directory the full path to get to the file
##' @return filepaths vector of strings of all filepaths within the provided dir
all_file_paths <- function(pwd_of_directory) {
  simplify2array(lapply(dir(pwd_of_directory), prepend_string, pwd_of_directory))
}

##' Row-Wise Shuffle a Dataframe
##' @param df dataframe
##' @return df2 dataframe with rows shuffled
shuffle_row_wise <- function(df) df[sample(nrow(df)), ]


##' Do Call Rbind on list of dataframes
##' @param list of dataframes
##' @return df row-bound concatenated dataframe
dcrb <- function(list_of_dataframes) {
  do.call("rbind", list_of_dataframes)
}

##' Do Call cbind on list of dataframes
##' @param list of dataframes
##' @return df column-bound concatenated dataframe
dccb <- function(list_of_dataframes) {
  do.call("cbind", list_of_dataframes)
}

##' Prepend String
##' This is like paste0, but the arguments are reversed. This way you can use it with lapply.
##' @param b string to put in back
##' @param a string to put in front
##' @return a_and_b string concatenated
prepend_string <- function(b, a) {
  paste0(a, b)
}

##' Add a pad values to the left side of the Matrix
##' @param mat matrix that you want to add the pad to
##' @param val_to_repeat int the value you want to repeat
##' @return mat_prime matrix of the updated matrix with the pad
left_pad <- function(mat, val_to_repeat) {
  mat_prime <- cbind(rep(1, nrow(mat)), mat)
  return(mat_prime)
}

##' Applies left_pad of 1's
##' @param mat matrix that you want to add the pad to
##' @param 1 int value of the pad to be added
##' @return mat_prime matrix with a left pad of 1's added
left_pad_ones <- function(mat) left_pad(mat, 1)

##' Applies left_pad of 0's
##' @param mat matrix that you want to add the pad to
##' @param 0 int value of the pad to be added
##' @return mat_prime matrix with a left pad of 0's added
left_pad_zeros <- function(mat) left_pad(mat, 0)

##' Create Output folder
##' Creates an output folder right next to frontiers2017
create_output_folder <- function() dir.create(file.path("../../", "output"), showWarnings = FALSE)

##
spin_around_rgl_plot <- function(n_frames=120) {
  start <- proc.time()[3]
  while ((i <- n_frames * (proc.time()[3] - start)) < 360) {
    rgl.viewpoint(i, i/4)
  }
}

# https://stackoverflow.com/questions/17549762/is-there-such-colsd-in-r
##' todo test
colSdApply <- function(x, ...) apply(X=x, MARGIN=2, FUN=sd, ...)
