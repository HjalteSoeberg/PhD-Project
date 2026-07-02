library(targets)
library(tarchetypes)
setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")

# Set target options:
tar_option_set(
    packages = c("data.table","riskRegression","arrow","lava","stats"),
    format = "parquet"
)

tar_source("functions")
# tar_source("other_functions.R") # Source other scripts as needed.

### targets
#tar_map to vary coeffecients
list(
    #
    ## Sim setup 1
    #
    tar_target(
        name = does_BS_find_correct_pars,
        command = get_does_BS_find_correct_pars(n = 5000,N = 20,tau = 2)
    ),
    tar_target(
        name = simulation_cox_exp,
        command = get_simulation_cox_exp(n = 400,N = 40,tau = 1.5,true_par <- c(beta1 = 1.1, beta2 = -0.6, lambda01 = 0.8, lambda02 = 0.2))
    ),
    # With censoring
    tar_target( #C=0.5 is about 30% censoring with the given parameters
        name = simulation_cox_exp_cens,
        command = get_simulation_cox_exp_cens(n = 500,N = 40,C = 0.5,tau = 1, true_par = c(beta1 = 1.1, beta2 = -0.6, lambda01 = 0.8, lambda02 = 0.2))
    ),
    #
    ## Sim setup 2
    #
    tar_target(
        name = simulation_cox_weibull,
        command = get_simulation_cox_weibull(n = 500, N = 40, tau = 3)
    ),
    tar_target(
        name = simulation_cox_weibull_cens,
        command = get_simulation_cox_weibull_cens(n = 500,N = 40,tau = 3)
    ),
    #
    ## Sim setup 3
    #
    tar_target(
        name = simulation_non_prop,
        command = get_simulation_non_prop(n = 500,N = 20,tau = 1)
    ),
    # With censoring
    tar_map(
        list(tau = c(1,3,4,5)),
        tar_target(
            name = simulation_non_prop_cens,
            command = get_simulation_non_prop_cens(n = 300,N = 40,tau = tau, C = 0.3)
        )
    ),
    #
    ## "real" data example
    #
    tar_target(
        name = real_data_example,
        command = get_real_data_example(n = 5000,N = 40,tau = 5)
    ),   
    #
    ## Try to optimize Brier score seperately for each cause and compare with the combined result
    #
    tar_target(
        name = individual_BS_vs_BSS,
        command = get_individual_BS_vs_BSS(n = 500, N = 20, tau = 2)
    )
)
