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




######################
library(targets)
library(tarchetypes)
setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")
tar_load_globals()
tar_load(c(simulation_non_prop_cens_1,
           simulation_non_prop_cens_3,
           simulation_non_prop_cens_4,
           simulation_non_prop_cens_6,
           simulation_non_prop_cens_8))

simulation_non_prop_cens_1
simulation_non_prop_cens_3
simulation_non_prop_cens_4
simulation_non_prop_cens_6
simulation_non_prop_cens_8


####### Show that new method doesnt get total probability > 1
library(riskRegression)
library(survival)
library(data.table)
library(lava)
library(prodlim)
library(mets)
library(targets)
library(tarchetypes)
setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")
tar_load_globals()

set.seed(1)
x <- rnorm(10,2,1)
times <- runif(10,0,2)
event <- rbinom(10, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
# This sums to over 1 while one and two are smaller than 1
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times),
                                   cause = 1,
                                   product.limit = FALSE)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times),
                                   cause = 2,
                                   product.limit = FALSE)
one+two
# BS 
BS_fit <- BSS(data = df, formula = Hist(times,event)~x, tau = max(times))
probs <- F_multi(t = max(times), X = mean(df$x), beta1 = BS_fit$true_pars[1], beta2 = BS_fit$true_pars[2], lambda1 = BS_fit$true_pars[3], lambda2 = BS_fit$true_pars[4])
probs[1]+probs[2]

