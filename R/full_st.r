force_cos_ramp_constraint_prefilled <- function(speed_limit, vector_out = c(28.8,0,0,0)) {
    return(force_cos_ramp_constraint(H_matrix = H_matrix, bounds_tuple_of_numeric = bounds_tuple_of_numeric,
    vector_out = vector_out, max_allowable_increasing_tension_speed=speed_limit,max_allowable_decreasing_tension_speed=speed_limit,
    n_task_values = 100, cycles_per_second = 60, cyclical_function = force_cos_ramp,
    eliminate = FALSE))
}
force_cos_ramp_is_feasible <- function(speed_limit, ...){
    tryCatch({
        big_constraint <- force_cos_ramp_constraint_prefilled(speed_limit=speed_limit, ...)
        c_is_feasible <- big_constraint$nonredundant_constr %>% constraint_is_feasible
        return(c_is_feasible)
    }, error = function(cond){
        print(cond)
        return(FALSE)
    })
}

har_dataframe_force_cos <- function(H_matrix,bounds_tuple_of_numeric,increasing,decreasing,n_task_values, har_n, vector_out){
    st_constr_str <- force_cos_ramp_constraint(H_matrix, bounds_tuple_of_numeric, vector_out, increasing,decreasing, n_task_values = n_task_values, cycles_per_second=10, eliminate = FALSE)
    param_string <- paste0("n_task_values:",n_task_values,"st increasing:", increasing, "decreasing:", decreasing)
    print('redundancies')
    res <- st_constr_str$nonredundant_constr %>% eliminate_redundant_single
    print('har')
    points <- res %>% har_sample(har_n, eliminate=FALSE)
    tall_df_st <- points %>% trajectory_har_df_melt(length(bounds_tuple_of_numeric))
    tall_df_st$st <- increasing
    return(tall_df_st)
}
