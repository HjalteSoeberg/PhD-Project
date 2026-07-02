get_real_data_example <- function(n,N,tau){
    if (FALSE){
        library(targets)
        setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")        
        tar_load_globals()
        n = 100000
        N = 10
        tau = 5
    }
    BS <- c()
    logi <- c()
    cscc <- c()
    for (k in 1:N){
        tryCatch({
            d <- simulateStenoT1(n)
            train_n <- floor(n*0.7)
            d_train <- d[1:train_n]
            d_test <- d[(train_n+1):n]
            d_train[,':='(y1 = 0, y2 = 0)]
            d_train[event == 1 & times <= tau, y1 := 1]
            d_train[event == 2 & times <= tau, y2 := 1]
            d_test[,':='(y1 = 0, y2 = 0)]
            d_test[event == 1 & times <= tau, y1 := 1]
            d_test[event == 2 & times <= tau, y2 := 1]
            # fit csc
            csc <- CSC(data = d_train, Hist(times,event)~age+diabetes_duration+value_SBP+value_LDL+value_HBA1C+eGFR)
            cscc <- append(cscc,BrierScore_CSC(model = csc, data = d_test, tau = tau))
            # fit logistic regression
            logi1 <- glm(data = d_train, formula = y1~age+diabetes_duration+value_SBP+value_LDL+value_HBA1C+eGFR, family = binomial)
            logi2 <- glm(data = d_train, formula = y2~age+diabetes_duration+value_SBP+value_LDL+value_HBA1C+eGFR, family = binomial)
            logi <- append(logi,BrierScore_glm(m1 = logi1, m2 = logi2, data = d_test, tau = tau))
            # BSE
            BS_fit <- BSS(data = d_train, formula = Hist(times,event)~age+diabetes_duration+value_SBP+value_LDL+value_HBA1C+eGFR, tau = tau)
            BS <- append(BS, BrierScore_multi(data = d_test, parms = BS_fit$par, T = tau, covariates = c("age","diabetes_duration","value_SBP","value_LDL","value_HBA1C","eGFR")))                      
        }, error = function(e){})
    }    

    df <- data.table(Model = c("logistic", "CSC","Brier"),
                     Mean = c(100*mean(logi), 100*mean(cscc), 100*mean(BS)))
    setkey(df,Mean)
    df[]
}



######################################################################
### get_real_data_example.R ends here
