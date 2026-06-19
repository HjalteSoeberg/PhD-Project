get_simulation_cox_exp_cens <- function(n,N,C,tau,true_par){
    if (FALSE){
        library(targets)
        setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")
        tar_load_globals()
        n = 300
        N = 10
        tau = 2
        C = 0.3
        true_par <- c(beta1 = 1.1, beta2 = -0.6, lambda01 = 0.8, lambda02 = 0.2)
    }
    BS <- c()
    BS_cens <- c()
    ## BS_cens_beta <- c()
    logi <- c()
    cscc <- c()
    true <- c()
    for (k in 1:N){
        tryCatch({
            d <- sim_cox_exp_cens(n, true_par = as.numeric(true_par))
            train_n <- floor(n*0.7)
            d_train <- d[1:train_n]
            d_test <- d[(train_n+1):n]
            # censor 30% of the training set
            d_train_3 <- copy(d_train)
            d_train_3[event == 0, ':='(event = 3)]
            d_train[,':='(y1 = 0, y2 = 0)]
            d_train[event == 1 & times <= tau, y1 := 1]
            d_train[event == 2 & times <= tau, y2 := 1]
            d_test[,':='(y1 = 0, y2 = 0)]
            d_test[event == 1 & times <= tau, y1 := 1]
            d_test[event == 2 & times <= tau, y2 := 1]
            # fit csc
            csc <- CSC(data = d_train, Hist(times,event)~X)
            bh1 <- survival::basehaz(csc$models[[1]], centered = FALSE)
            bh2 <- survival::basehaz(csc$models[[2]], centered = FALSE)
            start <- c(
                as.numeric(coef(csc)[1]),
                as.numeric(coef(csc)[2]),
                bh1$hazard[max(which(bh1$time <= tau))]/tau,
                bh2$hazard[max(which(bh2$time <= tau))]/tau
            )                
            cscc <- append(cscc,BrierScore_CSC(model = csc, data = d_test, tau = tau))
            csc <- CSC(data = d_train_3, Hist(times,event)~X)
            bh1_cens <- survival::basehaz(csc$models[[1]], centered = FALSE)
            bh2_cens <- survival::basehaz(csc$models[[2]], centered = FALSE)
            bh3_cens <- survival::basehaz(csc$models[[3]], centered = FALSE)           
            start_cens <- c(
                as.numeric(coef(csc)[1]),
                as.numeric(coef(csc)[2]),
                as.numeric(coef(csc)[3]),
                bh1_cens$hazard[max(which(bh1_cens$time <= tau))]/tau,
                bh2_cens$hazard[max(which(bh2_cens$time <= tau))]/tau,
                bh3_cens$hazard[max(which(bh3_cens$time <= tau))]/tau
            )                
            # fit logistic regression
            logi1 <- glm(data = d_train, formula = y1~X, family = binomial)
            logi2 <- glm(data = d_train, formula = y2~X, family = binomial)
            logi <- append(logi,BrierScore_glm(m1 = logi1, m2 = logi2, data = d_test, tau = tau))
            # Fit BSS_cens and BSS
            BS_cens_fit <- optim(par = start_cens, fn = BrierScore_cens, data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            BS_cens <- append(BS_cens, BrierScore(data = d_test, parms = c(BS_cens_fit$par[1:2],BS_cens_fit$par[4:5]), T = tau))
            #
            ## BS_cens_beta_fit <- optim(par = c(0,0,0), fn = BrierScore_cens_beta, lambdas = start_cens[4:6], data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            ## BS_cens_beta <- append(BS_cens_beta, BrierScore(data = d_test, parms = c(BS_cens_beta_fit$par[1:2],start_cens[4:5]), T = tau))
            #
            BS_fit <- optim(par = start, fn = BrierScore, data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            BS <- append(BS, BrierScore(data = d_test, parms = BS_fit$par, T = tau))                      
            true <- append(true,BrierScore(data = d_test, parms = as.numeric(true_par), T = tau))
        }, error = function(e){})
    }    

    df <- data.table(Model = c("logistic", "CSC","Brier","Oracle","Brier_cens"),
                     Mean = c(100*mean(logi), 100*mean(cscc), 100*mean(BS), 100*mean(true),100*mean(BS_cens)))
    setkey(df,Mean)
    df[]
}



######################################################################
### get_simulation_cox_exp_cens.R ends here
