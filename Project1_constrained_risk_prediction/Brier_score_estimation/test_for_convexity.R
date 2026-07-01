### test_for_convexity.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Apr 24 2026 (09:25) 
## Version: 
## Last-Updated: Apr 24 2026 (13:47) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 10
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


library(data.table)
library(rms)
library(survival)
library(riskRegression)
library(lava)
safe_exp <- function(z, cap = 700) {
  exp(pmin(z, cap))
}
F <- function(t, X, beta1, beta2, lambda1, lambda2){
    haz1 <- safe_exp(beta1*X)*lambda1
    haz2 <- safe_exp(beta2*X)*lambda2
    numerator <- 1-safe_exp(t*(-haz1-haz2))
    denominator <- haz1+haz2
    #return cumulative incidence functions for both cause 1 and 2
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    return(c(F1,F2))
}
BrierScore <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- safe_exp(parms[3])
    lambda2 <- safe_exp(parms[4])
    BS1 <- 0
    BS2 <- 0
    paste(beta1,beta2,lambda1,lambda2)
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
            } else{
                o2 <- 1
                o1 <- 0
            }
        } else{
            o1 <- 0
            o2 <- 0
        }
        BS1 <- BS1 + (F(t = T,X = df$x[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)^2
        BS2 <- BS2 + (F(t = T,X = df$x[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)^2
    }
#    print(paste0("BS1: ",BS1/nrow(df), "   BS2: ",BS2/nrow(df)))
    return((BS1+BS2)/nrow(df))
}
### Differentiated Brier-score-sum as estimating equations
g_j <- function(t, X, beta1, beta2, lambda1, lambda2){
    haz1 <- exp(beta1*X)*lambda1
    haz2 <- exp(beta2*X)*lambda2
    numerator <- 1-exp(t*(-haz1-haz2))
    denominator <- haz1+haz2
    #return cumulative incidence functions for both cause 1 and 2
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    return(c(F1,F2))
}
BrierScoreSum_beta1 <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
    BSS1 <- 0
    BSS2 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
            } else{
                o2 <- 1
                o1 <- 0
            }
        } else{
            o1 <- 0
            o2 <- 0
        }
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g1 <- ((X*haz1)/(haz12^2))*(haz2*(1-exp(-T*haz12))+T*haz1*haz12*exp(-T*haz12))
        d_g2 <- ((X*haz1*haz2)/(haz12^2))*((1+T*haz12)*exp(-T*(haz12))-1)             
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)
        BSS2 <- BSS2 + 2*d_g2*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)
    }
    return((BSS1+BSS2))
}
BrierScoreSum_beta2 <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
    BSS1 <- 0
    BSS2 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
            } else{
                o2 <- 1
                o1 <- 0
            }
        } else{
            o1 <- 0
            o2 <- 0
        }
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g2 <- ((X*haz2)/(haz12^2))*(haz1*(1-exp(-T*haz12))+T*haz2*haz12*exp(-T*haz12))
        d_g1 <- ((X*haz2*haz1)/(haz12^2))*((1+T*haz12)*exp(-T*(haz12))-1)             
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)
        BSS2 <- BSS2 + 2*d_g2*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)
    }
    return((BSS1+BSS2))
}
BrierScoreSum_lambda1 <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
    BSS1 <- 0
    BSS2 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
            } else{
                o2 <- 1
                o1 <- 0
            }
        } else{
            o1 <- 0
            o2 <- 0
        }
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g1 <- (exp(beta1*X)/(haz12^2))*(exp(-T*haz12)*haz1^2*T+haz2*(exp(-T*haz12)*haz1*T-exp(-T*haz12)+1))
        #d_g1 <- exp(beta1*X)*((1-exp(-T*haz12))/haz12)+haz1*((haz12*(-T*exp(beta1*X)*exp(-T*haz12))-(1-exp(-T*haz12))*exp(beta1*X))/(haz12^2))
        d_g2 <- ((haz2*exp(beta1*X))/(haz12^2))*((1+T*haz12)*exp(-T*(haz12))-1)       
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)
        BSS2 <- BSS2 + 2*d_g2*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)
    }
    return((BSS1+BSS2))
}
BrierScoreSum_lambda2 <- function(parms, data, T = 1){
    df <- copy(data)
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
    BSS1 <- 0
    BSS2 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
            } else{
                o2 <- 1
                o1 <- 0
            }
        } else{
            o1 <- 0
            o2 <- 0
        }
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g2 <- (exp(beta2*X)/(haz12^2))*(exp(-T*haz12)*haz2^2*T+haz1*(exp(-T*haz12)*haz2*T-exp(-T*haz12)+1))
        d_g1 <- ((haz1*exp(beta2*X))/(haz12^2))*((1+T*haz12)*exp(-T*(haz12))-1)       
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)
        BSS2 <- BSS2 + 2*d_g2*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)
    }
    return((BSS1+BSS2))
}
est_func_BSS <- function(theta, data){
    #reparamitze the baseline hazards to always be greater than 0
    parms <- theta
    parms[3:4] <- exp(theta[3:4])
    val <- c(
        BrierScoreSum_beta1(parms,data),
        BrierScoreSum_beta2(parms,data),
        BrierScoreSum_lambda1(parms,data),
        BrierScoreSum_lambda2(parms,data)
    )
    if (any(!is.finite(val))) {
        message("Non-finite BSSU at theta = ", paste(parms, collapse = ", "))
    }
    val
}
obj_BSS <- function(parms, data) {
    u <- est_func_BSS(parms, data)
    sum(u^2)
}
# likelihood
LogLikelihood_beta <- function(parms, data, cause){
    df <- data
    beta <- parms[1]
    lambda <- parms[2]
    L <- 0
    for (i in 1:nrow(df)){
        if (df$event[i] == cause){
            L <- L - df$x[i]
        }
        L <- L + df$times[i]*df$x[i]*exp(beta*df$x[i])*lambda
    }
    return(L)
}
LogLikelihood_lambda <- function(parms, data, cause){
    df <- data
    beta <- parms[1]
    lambda <- parms[2]
    L <- 0
    for (i in 1:nrow(df)){
        if (df$event[i] == cause){
            L <- L - 1/lambda
        }
        L <- L + df$times[i]*exp(beta*df$x[i])
    }
    return(L)
}
est_func_LL <- function(theta, data, cause){
    parms <- c(theta[1],exp(theta[2]))
    val <- c(
        LogLikelihood_beta(parms,data,cause),
        LogLikelihood_lambda(parms,data,cause)
    )
    if (any(!is.finite(val))) {
        message("Non-finite LLU at theta = ", paste(parms, collapse = ", "))
    }
    val
}
obj_LL <- function(parms, data, cause) {
    u <- est_func_LL(parms, data)
    sum(u^2)
}

### Simulating
n <- 200
m <- lvm()
regression(m) <- y ~ x
distribution(m, ~x) <- normal.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
set.seed(6)
d <- lava::sim(m,n)
d <- setDT(d)
setnames(d,"status","event")
d <- d[,.(times,event,x)]
train_n <- floor(n*0.7)
d_train <- d[1:train_n]
d_test <- d[(train_n+1):n]
# Maximum likelihood estimator
fit_LL_1 <- lava::NR(
                      start = c(0, 0),
                      gradient = est_func_LL,
                      objective = obj_LL,
                      args = list(data = d_train, cause = 1),
                      control = list(tol = 1e-10, stepsize = 0.1)
                  )
fit_LL_2 <- lava::NR(
                      start = c(0, 0),
                      gradient = est_func_LL,
                      objective = obj_LL,
                      args = list(data = d_train, cause = 2),
                      control = list(tol = 1e-10, stepsize = 0.1)
                  )
par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],fit_LL_1$par[2],fit_LL_2$par[2])
BrierScore(data = d_test,parms = par_LL)
# Brier-Score-sum estimator using differentiated Brierscoresum as estimating equations
fit <- lava::NR(
                 start = c(0,0,0,0),
                 gradient = est_func_BSS,
                 objective = obj_BSS,
                 args = list(data = d_train),
                 control = list(tol = 1e-10, stepsize = 0.1)
             )
par <- fit$par
BrierScore(data = d_train, parms = par)
# Maximizing the Brier-score-sum directly 
est <- lava::NR(c(0,0,0,0), args = list(data =  d_train), BrierScore, control = list(tol = 1e-10, stepsize = 0.1, iter.max = 200))
parms_NR <- est$par
BrierScore(data = d_test,parms = parms_NR)

b <- c()
par <- seq(-100,100,by = 0.1)
for (k in 1:length(par)){
    parms_NR <- c(-4.036065, -2.165443, 10.796811,par[k])
    b <- append(b,BrierScore(data = d,parms = parms_NR))
}

df <- data.table(beta1 = par, BS = b)

library(ggplot2)
ggplot(df, aes(x = beta1, y = BS))+
    geom_line()
setkey(df,BS)

setkey(df,beta1)
#parms_NR <- c(par,0.5,0.5,0.5) #-1.45
#    parms_NR <- c(par[k],1,0.5,0.5)   # -0.75
######################################################################
### test_for_convexity.R ends here
