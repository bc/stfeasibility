library(boot)
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}
wideScreen()
generate_task_csvs_for_cat(3,.1)

taskdf <- fread('output/distal_scaling_tasks.csv')
const1 <- generate_tasks_and_corresponding_constraints_via_df(cadaver_5k_dataset_train4k_LRM()$H_matrix%>%as.matrix, taskdf,cadaver_5k_dataset_train4k_LRM()$bounds_tuple_of_numeric)
