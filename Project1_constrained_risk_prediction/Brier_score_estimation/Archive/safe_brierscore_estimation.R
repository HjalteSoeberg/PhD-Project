### safe_brierscore_estimation.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Apr 24 2026 (12:45) 
## Version: 
## Last-Updated: Apr 24 2026 (14:06) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

# Stable log(exp(a) + exp(b))
logsumexp2 <- function(a, b) {
  m <- pmax(a, b)
  m + log1p(exp(-abs(a - b)))
}
safe_time_terms <- function(time, logH) {
  time_full <- time + 0 * logH  # recycle scalar time to length of logH
  if (any(time_full < 0, na.rm = TRUE)) {
    stop("time must be non-negative")
  }
  logz <- log(time_full) + logH
  logz[time_full == 0] <- -Inf
  A <- B <- C <- rep(NA_real_, length(logz))
  # z is finite and representable
  ok <- is.finite(logz) & logz <= log(.Machine$double.xmax)
  z <- exp(logz[ok])
  # Stable versions
  A[ok] <- -expm1(-z)                 # 1 - exp(-z)
  B[ok] <- exp(logz[ok] - z)          # z * exp(-z), but no z * exp(-z)
  C[ok] <- expm1(log1p(z) - z)        # (1 + z) * exp(-z) - 1
  # z is too large to represent.
  # These are the mathematical limiting values:
  # A -> 1, B -> 0, C -> -1
  huge <- !is.na(logz) & logz > log(.Machine$double.xmax)
  A[huge] <- 1
  B[huge] <- 0
  C[huge] <- -1
  # z = 0
  zero <- !is.na(logz) & logz == -Inf
  A[zero] <- 0
  B[zero] <- 0
  C[zero] <- 0
  list(A = A, B = B, C = C, logz = logz)
}
safe_g1_derivatives <- function(x,
                                beta1,
                                beta2,
                                lambda01,
                                lambda02,
                                T) {
  if (any(lambda01 <= 0, na.rm = TRUE) || any(lambda02 <= 0, na.rm = TRUE)) {
    stop("lambda01 and lambda02 must be positive")
  }
  # Log hazards:
  # h1 = lambda01 * exp(x * beta1)
  # h2 = lambda02 * exp(x * beta2)
  logh1 <- log(lambda01) + x * beta1
  logh2 <- log(lambda02) + x * beta2
  # logH = log(h1 + h2)
  logH <- logsumexp2(logh1, logh2)
  # p1 = h1 / H
  # p2 = h2 / H
  # plogis() is stable for large positive/negative inputs
  p1 <- plogis(logh1 - logh2)
  p2 <- plogis(logh2 - logh1)
  # Terms for the different time arguments
  terms_T       <- safe_time_terms(T,       logH)
  # Useful shorthand:
  # A = 1 - exp(-time * H)
  # B = time * H * exp(-time * H)
  # C = (1 + time * H) * exp(-time * H) - 1
  bracket_T <- p2 * terms_T$A + p1 * terms_T$B
  bracket_T <- p2 * terms_T$A + p1 * terms_T$B
  # exp(beta1 * x) / H
  e1_over_H <- exp(beta1 * x - logH)
  # exp(beta2 * x) / H
  e2_over_H <- exp(beta2 * x - logH)
  # 1. dg1 / dbeta1
  dg1_dbeta1 <- x * p1 * bracket_T
  # 2. dg1 / dbeta2
  dg1_dbeta2 <- x * p1 * p2 * terms_T$C
  # 3. dg1 / dlambda01
  dg1_dlambda01 <- e1_over_H * bracket_T
  # 4. dg1 / dlambda02
  dg1_dlambda02 <- p1 * e2_over_H * terms_T$C
  list(
    dg1_dbeta1    = dg1_dbeta1,
    dg1_dbeta2    = dg1_dbeta2,
    dg1_dlambda01 = dg1_dlambda01,
    dg1_dlambda02 = dg1_dlambda02
  )
}

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
        d_g1 <- safe_g1_derivatives(X, beta1 = beta1,beta2 = beta2, lambda01 = lambda1, lambda02 = lambda2, T = T)$dg1_dbeta1
        d_g2 <- safe_g1_derivatives(X, beta1 = beta2,beta2 = beta1, lambda01 = lambda2, lambda02 = lambda1, T = T)$dg1_dbeta2
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
        d_g1 <- safe_g1_derivatives(X, beta1 = beta1,beta2 = beta2, lambda01 = lambda1, lambda02 = lambda2, T = T)$dg1_dbeta2
        d_g2 <- safe_g1_derivatives(X, beta1 = beta2,beta2 = beta1, lambda01 = lambda2, lambda02 = lambda1, T = T)$dg1_dbeta1
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
        d_g1 <- safe_g1_derivatives(X, beta1 = beta1,beta2 = beta2, lambda01 = lambda1, lambda02 = lambda2, T = T)$dg1_dlambda01
        d_g2 <- safe_g1_derivatives(X, beta1 = beta2,beta2 = beta1, lambda01 = lambda2, lambda02 = lambda1, T = T)$dg1_dlambda02
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
        d_g1 <- safe_g1_derivatives(X, beta1 = beta1,beta2 = beta2, lambda01 = lambda1, lambda02 = lambda2, T = T)$dg1_dlambda02
        d_g2 <- safe_g1_derivatives(X, beta1 = beta2,beta2 = beta1, lambda01 = lambda2, lambda02 = lambda1, T = T)$dg1_dlambda01
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
N <- 10
n <- 500
LL <- c()
BSS <- c()
dBSS <- c()
m <- lvm()
regression(m) <- y ~ x
distribution(m, ~x) <- normal.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
for (k in 1:N){
    # k = 6 produces funny result for BS -> check for convexity of the Brier score using that data and plot the BS against the parameters one by one Keeping the rest constant
    set.seed(k)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    d <- d[,.(times,event,x)]
#    d[times < 0.1,times := 0.1]
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
    LL <- append(LL,BrierScore(data = d_test,parms = par_LL))
    # Brier-Score-sum estimator using differentiated Brierscoresum as estimating equations
    fit <- lava::NR(
                     start = c(0,0,0,0),
                     gradient = est_func_BSS,
                     objective = obj_BSS,
                     args = list(data = d_train),
                     control = list(tol = 1e-10, stepsize = 0.1)
                 )
    par <- fit$par
    dBSS <- append(dBSS,BrierScore(data = d_test, parms = par))
    # Maximizing the Brier-score-sum directly 
    ## est <- lava::NR(c(0,0,0,0), args = list(data =  d_train), BrierScore, control = list(tol = 1e-15, stepsize = 0.5, iter.max = 300))
    ## parms_NR <- est$par
    ## BSS <- append(BSS, BrierScore(data = d_test,parms = parms_NR))
}

mean(LL[-5])
mean(dBSS[-5])


######################################################################
### safe_brierscore_estimation.R ends here
