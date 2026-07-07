get_simulation_non_prop_cens <- function(n,N,tau,C){
    if (FALSE){
        library(targets)
        setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")        
        tar_load_globals()
        n = 3000
        N = 10
        tau = 2
        C = 0.3
    }
    BS <- c()
    BS_cens <- c()
    ## BS_cens_beta <- c()
    logi <- c()
    cscc <- c()
    for (k in 1:N){
        tryCatch({
            ## d <- sim_non_prop_haz_cens(n)
            ## train_n <- floor(n*0.7)
            ## d_train <- d[1:train_n]
            ## d_test <- d[(train_n+1):n]
            d_train <- sim_non_prop_haz_cens(n)
            d_test <- sim_non_prop_haz(n)
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
            ## BS_cens_beta_fit <- optim(par = c(0,0,0), fn = BrierScore_cens_beta, lambdas = start_cens[4:6], data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            ## BS_cens_beta <- append(BS_cens_beta, BrierScore(data = d_test, parms = c(BS_cens_beta_fit$par[1:2],start_cens[4:5]), T = tau))
            #
            BS_fit <- BSS(data = d_train, formula = Hist(times,event)~X, tau = tau)
            BS <- append(BS, BrierScore_multi(data = d_test, parms = BS_fit$par, T = tau, covariates = c("X")))                      
        }, error = function(e){})
    }    
    df <- data.table(Model = c("logistic", "CSC","Brier","Brier_cens"),
                     Mean = c(100*mean(logi), 100*mean(cscc), 100*mean(BS),100*mean(BS_cens)))
    setkey(df,Mean)
    df[]
}

######################################################################
### get_simulation_non_prop_cens.R ends here
