get_individual_BS_vs_BSS <- function(n,N,tau){
    if (FALSE){
        tar_load_globals()
        n = 500
        N = 10
        tau = 2
    }
    start <- c(
        beta1    = 1.4,
        beta2    = -0.4,
        lambda01 = 0.8,
        lambda02 = 0.2
    )
    BSS_1 <- c()
    BSS_2 <- c()
    BS1_1 <- c()
    BS1_2 <- c()
    BS2_1 <- c()
    BS2_2 <- c()
    BSS_time <- c()
    BS1_time <- c()
    BS2_time <- c()
    for (k in 1:N){
        tryCatch({
            d <- sim_cox_exp(n, true_par = as.numeric(start))
            ## d <- sim_dpnext(n)
            ## d <- sim_cox_weibull(n)
            ## d <- sim_non_prop_haz(n)
            train_n <- floor(n*0.7)
            d_train <- d[1:train_n]
            d_test <- d[(train_n+1):n]
            X <- Sys.time()
            pr <- optim(par = as.numeric(start), fn = BrierScore, data = d_train, T = tau, method = "BFGS", control = list(maxit = 1000))
            BSS_time <- append(BSS_time,Sys.time()-X)
            X <- Sys.time()
            pr1 <- optim(par = as.numeric(start), fn = BrierScore, data = d_train, T = tau, BS = "BS1", method = "BFGS", control = list(maxit = 1000))
            BS1_time <- append(BS1_time,Sys.time()-X)
            X <- Sys.time()
            pr2 <- optim(par = as.numeric(start), fn = BrierScore, data = d_train, T = tau, BS = "BS2", method = "BFGS", control = list(maxit = 1000))
            BS2_time <- append(BS2_time,Sys.time()-X)
            BSS_1 <- append(BSS_1,BrierScore(parms = pr$par, data = d_test, T = tau, BS = "BS1"))
            BSS_2 <- append(BSS_2,BrierScore(parms = pr$par, data = d_test, T = tau, BS = "BS2"))
            BS1_1 <- append(BS1_1,BrierScore(parms = pr1$par, data = d_test, T = tau, BS = "BS1"))
            BS1_2 <- append(BS1_2,BrierScore(parms = pr1$par, data = d_test, T = tau, BS = "BS2"))
            BS2_1 <- append(BS2_1,BrierScore(parms = pr2$par, data = d_test, T = tau, BS = "BS1"))
            BS2_2 <- append(BS2_2,BrierScore(parms = pr2$par, data = d_test, T = tau, BS = "BS2"))
        }, error = function(e){})
    }    
    df <- data.table(Model = c("BrierScoreSum", "BrierScore1", "BrierScore2"),
                     BS1 = c(100*mean(BSS_1), 100*mean(BS1_1), 100*mean(BS2_1)),
                     BS2 = c(100*mean(BSS_2), 100*mean(BS1_2), 100*mean(BS2_2)),
                     Time = c(mean(BSS_time), mean(BS1_time), mean(BS2_time)))
    df[]
}
######################################################################
### get_individual_BS_vs_BSS.R ends here
