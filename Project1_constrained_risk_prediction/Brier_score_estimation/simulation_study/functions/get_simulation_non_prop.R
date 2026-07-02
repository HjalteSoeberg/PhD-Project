get_simulation_non_prop <- function(n,N,tau){
    if (FALSE){
        library(targets)
        setwd("/home/hmik0048/PhD-Project/Project1_constrained_risk_prediction/Brier_score_estimation/simulation_study")
        tar_load_globals()
        n = 500
        N = 10
        tau = 1
    }
    LL <- c()
    BS <- c()
    logi <- c()
    cscc <- c()
    BS_beta <- c()
    for (k in 1:N){
        tryCatch({
            ## d <- sim_cox_exp(n)
            ## d <- sim_dpnext(n)
            ## d <- sim_cox_weibull(n)
            d <- sim_non_prop_haz(n)
            d[,':='(y1 = 0, y2 = 0)]
            d[event == 1 & times <= tau, y1 := 1]
            d[event == 2 & times <= tau, y2 := 1]
            train_n <- floor(n*0.7)
            d_train <- d[1:train_n]
            d_test <- d[(train_n+1):n]
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

            # fit logistic regression
            logi1 <- glm(data = d_train, formula = y1~X, family = binomial)
            logi2 <- glm(data = d_train, formula = y2~X, family = binomial)
            logi <- append(logi,BrierScore_glm(m1 = logi1, m2 = logi2, data = d_test, tau = tau))
            cscc <- append(cscc,BrierScore_CSC(model = csc, data = d_test, tau = tau))
            fit_LL_1 <- lava::NR(
                                  start = c(start[1], log(start[3])),
                                  gradient = est_func_LL,
                                  objective = obj_LL,
                                  args = list(data = d_train, cause = 1),
                                  control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                              )
            fit_LL_2 <- lava::NR(
                                  start = c(start[2], log(start[4])),
                                  gradient = est_func_LL,
                                  objective = obj_LL,
                                  args = list(data = d_train, cause = 2),
                                  control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                              )
            par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],exp(fit_LL_1$par[2]),exp(fit_LL_2$par[2]))
            LL <- append(LL,BrierScore(data = d_test,parms = par_LL, T = tau))                     
            BS_fit <- optim(par = start, fn = BrierScore, data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            BS <- append(BS, BrierScore(data = d_test, parms = BS_fit$par, T = tau))

            BS_fit_beta <- optim(par = c(0,0), fn = BrierScore_beta, lambdas = start[3:4],data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            BS_beta <- append(BS_beta, BrierScore(data = d_test, parms = c(BS_fit_beta$par,start[3:4]), T = tau))                      

            ## true <- append(true,BrierScore(data = d_test, parms = as.numeric(c(start[1:2],exp(start[3:4]))), T = tau))
        }, error = function(e){})
    }
    df <- data.table(Model = c("logistic", "CSC", "Likelihood","Brier","Brier_beta"),
                     Mean = c(100*mean(logi), 100*mean(cscc), 100*mean(LL), 100*mean(BS), 100*mean(BS_beta)))
    setkey(df,Mean)
    df[]
}



######################################################################
### get_simulation_non_prop.R ends here
