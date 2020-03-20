load_all("~/Documents/GitHub/bc/frontiers2017")
source('R/helper_functions.r')
la <- load_all
te <- test
atp <- auto_test_package

run_full_long_tests <- FALSE

# Sample Data for use in testing:
# Test finger in flexed posture, from frontiers2018 feasibility theory paper
JR <- rbind(c(-0.08941, -0.0447, -0.009249, 0.03669, 0.1421, 0.2087, -0.2138), c(-0.04689,
    -0.1496, 0.052, 0.052, 0.0248, 0, 0.0248), c(0.06472, 0.001953, -0.1518, -0.1518,
    0.2919, 0.0568, 0.2067), c(0.003081, -0.002352, -0.0001649, -0.0001649, -0.0004483,
    0.0001579, -0.000685))
maximal_tension_per_muscle <- c(123, 219, 23.52, 91.74, 21.6, 124.8, 129.6)
muscle_name_per_index <- c("FDP", "FDS", "EIP", "EDC", "LUM", "DI", "PI")
force_dimnames <- c("Fx", "Fy", "Fz", "tx")
H_matrix <- JR %*% diag(maximal_tension_per_muscle)
colnames(H_matrix) <- muscle_name_per_index
rownames(H_matrix) <- force_dimnames
bounds_tuple_of_numeric <- rep(list(list(lower = 0, upper = 1)), length(maximal_tension_per_muscle))

# Toy example for testing and illustration
H_matrix_mini <- cbind(m0=c(3.333333333),
      m1=c(-3.533333333),
      m2=c(2.0))
rownames(H_matrix_mini) <- "Fx"
bounds_tuple_of_numeric_mini <- rep(list(list(lower = 0, upper = 1)), 3)


##' medial is towards center of the hand
##' derived from culled_hand4_flex_uar_5k_clean_static_response_from_tail_100ms_mean.csv
cadaver_5k_dataset_train4k_LRM <- function(){

muscle_name_per_index <- c("FDP","FDS","EIP","EDC","LUM","DI","PI")
wrench_names <- c("dorsal_fx","medial_fy","proximal_fz","JR3_MX","JR3_MY","JR3_MZ")

	lrm_model_of_tension_transduction <- rbind(c(-0.055472,0.0085059,-0.04143,0.040855,0.060872,0.0083857),
										   c(-0.051982,0.0056465,-0.088819,0.030892,0.08346,0.003642),
										   c(0.0052534,-0.0002142,0.025817,-0.002114,-0.0083006,-0.00026338),
										   c(0.0070741,-0.0036766,0.021978,-0.019363,-0.0087962,-0.0016665),
										   c(0.036154,0.0041829,-0.039141,0.0022464,-0.030165,0.003371),
										   c(-0.002553,-0.014332,-0.0088733,-0.088249,0.0106,-0.0037038),
										   c(0.039546,0.0079125,-0.038078,0.045004,-0.036593,0.0019535)) %>% t %>% as.data.frame
	colnames(lrm_model_of_tension_transduction) <- muscle_name_per_index
	rownames(lrm_model_of_tension_transduction) <- wrench_names
	bias_on_wrench <- c(0.30017,-0.034335,0.17062,-0.26764,-0.40899,-0.014565)
	names(bias_on_wrench) <- wrench_names
	bounds_tuple_of_numeric <- rep(list(list(lower = 0, upper = 10)), length(muscle_name_per_index))
	return(list(H_matrix=lrm_model_of_tension_transduction, wrench_bias=bias_on_wrench, bounds_tuple_of_numeric=bounds_tuple_of_numeric))
}


get_cat_H_matrix <- function(cat_mat, cat_number){
	get_R <- function(cat_mat, cat_number) cat_mat$Cats[[cat_number]][[1]][[1]]
	get_J <- function(cat_mat, cat_number) cat_mat$Cats[[cat_number]][[1]][[2]]
	RFm <- get_R(cat_mat, 1) * diag(as.numeric(cat_mat$afl95) * (cat_mat$fmax * cat_mat$cosa95))
	H_matrix_cat <- get_J(cat_mat, cat_number) %*% RFm
	cat_muscle_names <- as.character(unlist(cat_mat$muscles))
	cat_wrench_names <- c("fx","fy","fz","mx","my","mz")
	colnames(H_matrix_cat) <- cat_muscle_names
	rownames(H_matrix_cat) <- cat_wrench_names
	bounds_tuple_of_numeric_cat <- rep(list(list(lower = 0, upper = 1)), 31)
return(list(H_matrix = H_matrix_cat, bounds_tuple_of_numeric = bounds_tuple_of_numeric)
	)}


mat_path <- "~/Documents/GitHub/bc/stfeasibility/data/Sohn2013_hinlimb_models.mat"
library(R.matlab)
cat_mat <- readMat(mat_path)
cat1 <- get_cat_H_matrix(cat_mat, 1)
cat2 <- get_cat_H_matrix(cat_mat, 2)
cat3 <- get_cat_H_matrix(cat_mat, 3)

