get_does_BS_find_correct_pars <- function(n,N,tau){
    if (FALSE){
        tar_load_globals()
        n = 300
        N = 10
        tau = 2
    }
    true_pars <- c(
        beta1    = 0.8,
        beta2    = -0.4,
        lambda01 = 0.2,
        lambda02 = 0.1
    )
    LL_beta1 <- c()
    LL_beta2 <- c()
    LL_lambda01 <- c()
    LL_lambda02 <- c()
    BS_beta1 <- c()
    BS_beta2 <- c()
    BS_lambda01 <- c()
    BS_lambda02 <- c()
    for (k in 1:N){
        tryCatch({
            d <- sim_cox_exp(n, true_par = as.numeric(c(true_pars[1:2],true_pars[3:4])))
            # fit csc
            csc <- CSC(data = d, Hist(times,event)~X, fitter = "coxph")
            bh1 <- survival::basehaz(csc$models[[1]], centered = FALSE)
            bh2 <- survival::basehaz(csc$models[[2]], centered = FALSE)           
            start <- c(
                as.numeric(coef(csc)[1]),
                as.numeric(coef(csc)[2]),
                bh1$hazard[max(which(bh1$time <= tau))]/tau,
                bh2$hazard[max(which(bh2$time <= tau))]/tau
            )                
            fit_LL_1 <- lava::NR(
                                  start = c(start[1], log(start[3])),
                                  gradient = est_func_LL,
                                  objective = obj_LL,
                                  args = list(data = d, cause = 1),
                                  control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                              )
            fit_LL_2 <- lava::NR(
                                  start = c(start[2], log(start[4])),
                                  gradient = est_func_LL,
                                  objective = obj_LL,
                                  args = list(data = d, cause = 2),
                                  control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                              )
            LL_beta1 <- append(LL_beta1,fit_LL_1$par[1])
            LL_beta2 <- append(LL_beta2,fit_LL_2$par[1])
            LL_lambda01 <- append(LL_lambda02,exp(fit_LL_1$par[2]))
            LL_lambda02 <- append(LL_lambda02,exp(fit_LL_2$par[2]))
            fit_BS <- optim(par = start, fn = BrierScore, data = d, T = tau, method = "BFGS", control = list(maxit = 1000))
            ## Estimates on the original lambda scale
            theta_hat <- fit_BS$par
            BS_beta1 <- append(BS_beta1,theta_hat[1])
            BS_beta2 <- append(BS_beta2,theta_hat[2])
            BS_lambda01 <- append(BS_lambda02,theta_hat[3])
            BS_lambda02 <- append(BS_lambda02,theta_hat[4])
        }, error = function(e){})
    }
    
    df1 <- data.table(Model = "Brier",
                      Parameter = c("Beta1","Beta2", "lambda01", "lambda02"),
                      Mean_Diff = c(abs(mean(BS_beta1)-start[1]),abs(mean(BS_beta2)-start[2]),abs(mean(BS_lambda01)-start[3]),abs(mean(BS_lambda02)-start[4])),
                      SD = c(sd(BS_beta1),sd(BS_beta2),sd(BS_lambda01),sd(BS_lambda02)))
    df2 <- data.table(Model = "Likelihood",
                      Parameter = c("Beta1","Beta2", "lambda01", "lambda02"),
                      Mean_Diff = c(abs(mean(LL_beta1)-start[1]),abs(mean(LL_beta2)-start[2]),abs(mean(LL_lambda01)-start[3]),abs(mean(LL_lambda02)-start[4])),
                      SD = c(sd(LL_beta1),sd(LL_beta2),sd(LL_lambda01),sd(LL_lambda02)))
    df <- rbind(df1,df2)
    df[]
}


######################################################################
### get_does_BS_find_correct_pars.R ends here
