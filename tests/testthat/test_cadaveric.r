library(boot)
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}
wideScreen()
generate_task_csvs(3,.1)

taskdf <- fread('output/distal_scaling_tasks.csv')
const1 <- generate_tasks_and_corresponding_constraints_via_df(cadaver_5k_dataset_train4k_LRM()$H_matrix%>%as.matrix, taskdf,cadaver_5k_dataset_train4k_LRM()$bounds_tuple_of_numeric)

num_muscles <- ncol(cadaver_5k_dataset_train4k_LRM()$H_matrix)
bounds_tuple_of_numeric <- rep(list(list(lower = 0, upper = 10)), length(muscle_name_per_index))
bounds_tuple_of_numeric_01 <- rep(list(list(lower = 0, upper = 1)), length(muscle_name_per_index))

zero_wrench_task <- constraint_H_with_bounds(cadaver_5k_dataset_train4k_LRM()$H_matrix[c(1:6),]%>%as.matrix, rep(0,6), bounds_tuple_of_numeric)
# identify max capability along distal_fz direction
# direction <- cadaver_5k_dataset_train4k_LRM()$H_matrix[1:2,1]
direction <- c(-1,0,0,0,0,0)
# note that direction is flipped to be negative because it was brought from the B side to the A side.

starter_mat <- zero_wrench_task
lhs_with_target_dir <- starter_mat$constr %>% as.matrix
plus_or_minus_margin <- c(0,0,0,.5,.5,.5)
upperbnd <- direction + plus_or_minus_margin
lowerbnd <- direction - plus_or_minus_margin
new <- cbind(lhs_with_target_dir, c(-lowerbnd,upperbnd, rep(0,num_muscles*2)))
colnames(new)[num_muscles+1] <- "wrench"
constr_task <- list(constr=new,rhs=starter_mat$rhs,dir=starter_mat$dir)
print_constraint(constr_task)
simplex(c(rep(0,7),0), constr_task$constr,constr_task$rhs)

poin <- constr_task %>% har_sample(1e5)
print(summary(poin))


max_task_in_dir <- simplex(c(rep(0,7),-1), constr_task$constr,constr_task$rhs)


simplex(rep(-1,21), inequality_constraints$constr,inequality_constraints$rhs)
is_feasible(zero_wrench_task)

inequality_constraints <- diagonal_merge_constraint_list(const1$constraints)

eliminate = TRUE
max_allowable_increasing_tension_speed=200 #activations per millisecond
max_allowable_decreasing_tension_speed=200 #activations per millisecond

st_constr <- generate_and_add_velocity_constraint(inequality_constraints,
                                                 max_allowable_increasing_tension_speed,
                                                 max_allowable_decreasing_tension_speed,
                                                 num_muscles)
is_feasible(st_constr)
res <- list(nonredundant_constr=eee, tasks_and_constraints=const1)

eee %>% har_sample(10, eliminate=TRUE)
return(res)

1124$7jbcaoqsdgad

$7jbcaoq

