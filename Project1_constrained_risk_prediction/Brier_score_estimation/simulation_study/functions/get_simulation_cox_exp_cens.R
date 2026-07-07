get_simulation_cox_exp_cens <- function(n,N,C,tau,true_par){
    if (FALSE){
        library(targets)
        setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")
        tar_load_globals()
        n = 2000
        N = 100
        tau = 0.4
        C = 0.3
        true_par <- c(beta1 = 0.7, beta2 = -1.6, lambda01 = 0.5, lambda02 = 1.2)
    }
    BS <- c()
    BS_cens <- c()
    logi <- c()
    cscc <- c()
    true <- c()
    for (k in 1:N){
        tryCatch({
            d <- sim_cox_exp_cens(n, true_par = as.numeric(true_par))
            train_n <- floor(n*0.7)
            d_train <- d[1:train_n]
            ## d_test <- d[(train_n+1):n]
            d_test <- sim_cox_exp(2000, true_par = as.numeric(true_par))
            d_train[,':='(y1 = 0, y2 = 0)]
            d_train[event == 1 & times <= tau, y1 := 1]
            d_train[event == 2 & times <= tau, y2 := 1]
            d_test[,':='(y1 = 0, y2 = 0)]
            d_test[event == 1 & times <= tau, y1 := 1]
            d_test[event == 2 & times <= tau, y2 := 1]
            # fit csc
            csc <- CSC(data = d_train, Hist(times,event)~X)
            cscc <- append(cscc,BrierScore_CSC(model = csc, data = d_test, tau = tau))
            # fit logistic regression
            logi1 <- glm(data = d_train, formula = y1~X, family = binomial)
            logi2 <- glm(data = d_train, formula = y2~X, family = binomial)
            logi <- append(logi,BrierScore_glm(m1 = logi1, m2 = logi2, data = d_test, tau = tau))
            # Fit BSS_cens and BSS
            BS_cens_fit <- BSS_cens(data = d_train, formula = Hist(times,event)~X, tau = tau)
            BS_cens <- append(BS_cens, BrierScore_multi(data = d_test, parms = c(BS_cens_fit$par[1:2],BS_cens_fit$par[4:5]), T = tau, covariates = c("X")))                      
            #
            BS_fit <- BSS(data = d_train, formula = Hist(times,event)~X, tau = tau)
            BS <- append(BS, BrierScore_multi(data = d_test, parms = BS_fit$par, T = tau, covariates = c("X")))                      
            true <- append(true,BrierScore_multi(data = d_test, parms = as.numeric(c(true_par[1:2],log(true_par[3:4]))), T = tau, covariates = c("X")))
        }, error = function(e){})
    }    

    df <- data.table(Model = c("logistic", "CSC","Brier","Oracle","Brier_cens"),
                     Mean = c(100*mean(logi), 100*mean(cscc), 100*mean(BS), 100*mean(true),100*mean(BS_cens)))
    setkey(df,Mean)
    df[]
}



######################################################################
### get_simulation_cox_exp_cens.R ends here
