library(targets)
library(tarchetypes)
setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")
tar_load_globals()
tar_load(c(does_BS_find_correct_pars,
         simulation_non_prop,
         simulation_cox_exp,
         simulation_cox_exp_cens,
         simulation_non_prop_cens))

### BS finds correct parameters
does_BS_find_correct_pars

### performance of BS, likelihood, CSC and logistic regression when data is cox-exponentially distributed
simulation_cox_exp[Model != "Brier_beta"]

### performance of BS, likelihood, CSC and logistic regression when data is non-proportionally hazards
simulation_non_prop[Model != "Brier_beta"]

### performance of BS, likelihood, CSC and logistic regression when data is cox-exponentially distributed and 30% of data is censored
simulation_cox_exp_cens[Model != "Brier_beta"]

### performance of BS, likelihood, CSC and logistic regression when data is non-proportionally hazards and 30% of data is censored
simulation_non_prop_cens[Model != "Brier_beta"]
