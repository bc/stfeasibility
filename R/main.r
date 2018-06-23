load_all("~/Documents/GitHub/bc/frontiers2017")
source('R/helper_functions.r')
la <- load_all
te <- test
atp <- auto_test_package

run_full_long_tests <- TRUE

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