### BrierScore_vs_likelihood_sim.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Apr 17 2026 (08:31) 
## Version: 
## Last-Updated: May  5 2026 (09:41) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 87
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



### Simulate survival data with one numeric covariate
set.seed(433)
m <- lvm()
regression(m) <- y ~ x
regression(m) <- x ~ exp(0.6*s-0.5*z)
distribution(m,~s+z) <- binomial.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
d <- lava::sim(m,200)
d <- setDT(d)
setnames(d,"status","event")
d <- d[,.(times,event,x)]

set.seed(9)
m <- lvm()
regression(m) <- y ~ x
distribution(m,~x) <- normal.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.2, shape = ~(x))
eventTime(m) <- times ~ min(y = 1,death = 2)
d <- lava::sim(m,100)
d <- setDT(d)
setnames(d,"status","event")
d <- d[,.(times,event,x)]

# Maximum likelihood estimator
fit_LL_1 <- lava::NR(
  start = c(0, 0),
  gradient = est_func_LL,
  objective = obj_LL,
  args = list(data = d, cause = 1),
  control = list(tol = 1e-15, stepsize = 0.5)
)
fit_LL_2 <- lava::NR(
  start = c(0, 0),
  gradient = est_func_LL,
  objective = obj_LL,
  args = list(data = d, cause = 2),
  control = list(tol = 1e-15, stepsize = 0.5)
)
par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],fit_LL_1$par[2],fit_LL_2$par[2])
BrierScore(data = d,parms = par_LL)

# Brier-Score-sum estimator using differentiated Brierscoresum as estimating equations
set.seed(1)
fit <- lava::NR(
  start = c(0,0,0,0),
  gradient = est_func_BSS,
  objective = obj_BSS,
  args = list(data = d),
  control = list(tol = 1e-15, stepsize = 0.4)
)
par <- fit$par
BrierScore(data = d, parms = par)

# Maximizing the Brier-score-sum directly 
est <- lava::NR(c(0,0,0,0), args = list(data =  d), BrierScore, control = list(tol = 1e-10, stepsize = 0.5))
parms_NR <- est$par
BrierScore(data = d,parms = parms_NR)


### Simulating
N <- 1
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

mean(LL)
mean(dBSS)


BrierScore(data = d_test, parms = c(par[1:3],1))


######################################################################


#### safer version
#reparamitize lambda
lambda_new <- function(eta, upper = 4000000000000){
    upper*plogis(eta)
}
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
    lambda1 <- lambda_new(parms[3])
    lambda2 <- lambda_new(parms[4])
    BS1 <- 0
    BS2 <- 0
    df[,o := 0]
    df[T >= times, o := 1]
    for (i in 1:nrow(df)){
        if (df$event[i] == 1){
            o1 <- df$o[i]
            o2 <- 0
        } else{
            o2 <- df$o[i]
            o1 <- 0
        }       
        BS1 <- BS1 + (F(t = T,X = df$x[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)^2
        BS2 <- BS2 + (F(t = T,X = df$x[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)^2
    }
    return((BS1+BS2)/nrow(df))
}
BrierScoreSum_beta1 <- function(parms, data, T = 1) {
  df <- copy(data)
  beta1   <- parms[1]
  beta2   <- parms[2]
  lambda1 <- parms[3]
  lambda2 <- parms[4]
  if (lambda1 <= 0 || lambda2 <= 0) return(1e10)
  log_lambda1 <- log(lambda1)
  log_lambda2 <- log(lambda2)
  BSS1 <- 0
  BSS2 <- 0
  df[, o := 0]
  df[T >= times, o := 1]
  log_max_double <- log(.Machine$double.xmax)
  for (i in 1:nrow(df)) {
    if (df$event[i] == 1) {
      o1 <- df$o[i]
      o2 <- 0
    } else {
      o2 <- df$o[i]
      o1 <- 0
    }
    X <- df$x[i]
    ## log hazards
    a <- log_lambda1 + X * beta1
    b <- log_lambda2 + X * beta2
    ## stable p1 = haz1 / (haz1 + haz2), p2 = haz2 / (haz1 + haz2)
    ## use plogis to avoid overflow
    p1 <- plogis(a - b)
    p2 <- plogis(b - a)
    ## stable log(haz1 + haz2)
    m <- max(a, b)
    log_haz12 <- m + log(exp(a - m) + exp(b - m))
    ## z = T * haz12, but computed safely
    if (is.infinite(log_haz12) || log_haz12 > log_max_double - log(T)) {
      z <- Inf
    } else {
      z <- T * exp(log_haz12)
    }
    ## stable evaluations of 1 - exp(-z) and z*exp(-z)
    if (is.infinite(z)) {
      one_minus_exp_neg_z <- 1
      z_exp_neg_z <- 0
    } else {
      one_minus_exp_neg_z <- -expm1(-z)   # stable for small z
      z_exp_neg_z <- z * exp(-z)
    }
    ## stable d_g1
    d_g1 <- X * (p1 * p2 * one_minus_exp_neg_z + p1^2 * z_exp_neg_z)
    ## your original d_g2 is also risky; a safer rewrite is:
    ## d_g2 = X * p1 * p2 * ( ((T+1)/T) * z * exp(-z) - 1 )
    ## assuming T > 0
    if (is.infinite(z)) {
      d_g2 <- -X * p1 * p2
    } else {
      d_g2 <- X * p1 * p2 * (((T + 1) / T) * z_exp_neg_z - 1)
    }
    gj <- g_j(
      t = T, X = X,
      beta1 = beta1, beta2 = beta2,
      lambda1 = lambda1, lambda2 = lambda2
    )
    BSS1 <- BSS1 + 2 * d_g1 * (gj[1] - o1)
    BSS2 <- BSS2 + 2 * d_g2 * (gj[2] - o2)
  }
  out <- BSS1 + BSS2
  if (!is.finite(out)) out <- 1e10
  out
}
stable_hazard_terms <- function(X, beta1, beta2, lambda1, lambda2, T) {
  if (lambda1 <= 0 || lambda2 <= 0 || T <= 0) {
    return(NULL)
  }
  a <- log(lambda1) + X * beta1
  b <- log(lambda2) + X * beta2
  ## Stable hazard shares
  p1 <- plogis(a - b)   # h1 / (h1 + h2)
  p2 <- plogis(b - a)   # h2 / (h1 + h2)
  ## Stable log(h1 + h2)
  m <- max(a, b)
  log_h <- m + log(exp(a - m) + exp(b - m))
  ## z = T * (h1 + h2), computed safely
  log_max_double <- log(.Machine$double.xmax)
  if (log_h > log_max_double - log(T)) {
    z <- Inf
    one_minus_exp_neg_z <- 1
    z_exp_neg_z <- 0
    inv_h <- 0
  } else {
    h <- exp(log_h)
    z <- T * h
    one_minus_exp_neg_z <- -expm1(-z)
    z_exp_neg_z <- z * exp(-z)
    inv_h <- 1 / h
  }
  ## exp(X*beta1) = h1/lambda1, exp(X*beta2) = h2/lambda2
  exp_xb1_over_h <- p1 / lambda1   # exp(X*beta1) / h
  exp_xb2_over_h <- p2 / lambda2   # exp(X*beta2) / h
  list(
    p1 = p1,
    p2 = p2,
    z = z,
    one_minus_exp_neg_z = one_minus_exp_neg_z,
    z_exp_neg_z = z_exp_neg_z,
    inv_h = inv_h,
    exp_xb1_over_h = exp_xb1_over_h,
    exp_xb2_over_h = exp_xb2_over_h
  )
}
BrierScoreSum_beta2 <- function(parms, data, T = 1) {
  df <- copy(data)
  beta1   <- parms[1]
  beta2   <- parms[2]
  lambda1 <- parms[3]
  lambda2 <- parms[4]
  if (lambda1 <= 0 || lambda2 <= 0 || T <= 0) return(1e10)
  BSS1 <- 0
  BSS2 <- 0
  df[, o := 0]
  df[T >= times, o := 1]
  for (i in 1:nrow(df)) {
    if (df$event[i] == 1) {
      o1 <- df$o[i]
      o2 <- 0
    } else {
      o2 <- df$o[i]
      o1 <- 0
    }
    X <- df$x[i]
    st <- stable_hazard_terms(X, beta1, beta2, lambda1, lambda2, T)
    if (is.null(st)) return(1e10)
    p1 <- st$p1
    p2 <- st$p2
    one_minus_exp_neg_z <- st$one_minus_exp_neg_z
    z_exp_neg_z <- st$z_exp_neg_z
    ## Stable rewrites:
    ## d_g2 = X * [ p1*p2*(1-exp(-z)) + p2^2*z*exp(-z) ]
    ## d_g1 = X * p1*p2 * ( ((T+1)/T) * z*exp(-z) - 1 )
    d_g2 <- X * (p1 * p2 * one_minus_exp_neg_z + p2^2 * z_exp_neg_z)
    d_g1 <- X * p1 * p2 * (((T + 1) / T) * z_exp_neg_z - 1)
    gj <- g_j(
      t = T, X = X,
      beta1 = beta1, beta2 = beta2,
      lambda1 = lambda1, lambda2 = lambda2
    )
    BSS1 <- BSS1 + 2 * d_g1 * (gj[1] - o1)
    BSS2 <- BSS2 + 2 * d_g2 * (gj[2] - o2)
  }
  out <- BSS1 + BSS2
  if (!is.finite(out)) out <- 1e10
  out
}
BrierScoreSum_lambda1 <- function(parms, data, T = 1) {
  df <- copy(data)
  beta1   <- parms[1]
  beta2   <- parms[2]
  lambda1 <- parms[3]
  lambda2 <- parms[4]
  if (lambda1 <= 0 || lambda2 <= 0 || T <= 0) return(1e10)
  BSS1 <- 0
  BSS2 <- 0
  df[, o := 0]
  df[T >= times, o := 1]
  for (i in 1:nrow(df)) {
    if (df$event[i] == 1) {
      o1 <- df$o[i]
      o2 <- 0
    } else {
      o2 <- df$o[i]
      o1 <- 0
    }
    X <- df$x[i]
    st <- stable_hazard_terms(X, beta1, beta2, lambda1, lambda2, T)
    if (is.null(st)) return(1e10)
    p1 <- st$p1
    p2 <- st$p2
    one_minus_exp_neg_z <- st$one_minus_exp_neg_z
    z_exp_neg_z <- st$z_exp_neg_z
    exp_xb1_over_h <- st$exp_xb1_over_h
    ## Original:
    ## d_g1 = (exp(beta1*X)/h^2) * [ exp(-Th)*h1^2*T + h2*(exp(-Th)*h1*T - exp(-Th) + 1) ]
    ##
    ## Stable:
    ## d_g1 = (exp(X*beta1)/h) * [ p1*z*exp(-z) + p2*(z*p1*exp(-z) - exp(-z) + 1) ]
    ##      = (exp(X*beta1)/h) * [ p1*z*exp(-z) + p2*(p1*z*exp(-z) + 1 - exp(-z)) ]
    ##
    ## Since 1 - exp(-z) = one_minus_exp_neg_z
    d_g1 <- exp_xb1_over_h * (
      p1 * z_exp_neg_z +
      p2 * (p1 * z_exp_neg_z + one_minus_exp_neg_z)
    )
    ## Original:
    ## d_g2 = ((h2*exp(beta1*X))/h^2) * (((T+1)*exp(-Th)*h) - 1)
    ##
    ## Stable:
    ## d_g2 = (exp(X*beta1)/h) * p2 * ( ((T+1)/T)*z*exp(-z) - 1 )
    d_g2 <- exp_xb1_over_h * p2 * (((T + 1) / T) * z_exp_neg_z - 1)
    gj <- g_j(
      t = T, X = X,
      beta1 = beta1, beta2 = beta2,
      lambda1 = lambda1, lambda2 = lambda2
    )
    BSS1 <- BSS1 + 2 * d_g1 * (gj[1] - o1)
    BSS2 <- BSS2 + 2 * d_g2 * (gj[2] - o2)
    ## optional debug
    ## print(paste0(i, " :BSS1: ", BSS1))
    ## print(paste0(i, " :BSS2: ", BSS2))
  }
  out <- BSS1 + BSS2
  if (!is.finite(out)) out <- 1e10
  out
}
BrierScoreSum_lambda2 <- function(parms, data, T = 1) {
  df <- copy(data)
  beta1   <- parms[1]
  beta2   <- parms[2]
  lambda1 <- parms[3]
  lambda2 <- parms[4]
  if (lambda1 <= 0 || lambda2 <= 0 || T <= 0) return(1e10)
  BSS1 <- 0
  BSS2 <- 0
  df[, o := 0]
  df[T >= times, o := 1]
  for (i in 1:nrow(df)) {
    if (df$event[i] == 1) {
      o1 <- df$o[i]
      o2 <- 0
    } else {
      o2 <- df$o[i]
      o1 <- 0
    }
    X <- df$x[i]
    st <- stable_hazard_terms(X, beta1, beta2, lambda1, lambda2, T)
    if (is.null(st)) return(1e10)
    p1 <- st$p1
    p2 <- st$p2
    one_minus_exp_neg_z <- st$one_minus_exp_neg_z
    z_exp_neg_z <- st$z_exp_neg_z
    exp_xb2_over_h <- st$exp_xb2_over_h
    ## Stable rewrite of:
    ## d_g2 = (exp(beta2*X)/h^2) * [ exp(-Th)*h2^2*T + h1*(exp(-Th)*h2*T - exp(-Th) + 1) ]
    d_g2 <- exp_xb2_over_h * (
      p2 * z_exp_neg_z +
      p1 * (p2 * z_exp_neg_z + one_minus_exp_neg_z)
    )
    ## Stable rewrite of:
    ## d_g1 = ((h1*exp(beta2*X))/h^2) * (((T+1)*exp(-Th)*h) - 1)
    d_g1 <- exp_xb2_over_h * p1 * (((T + 1) / T) * z_exp_neg_z - 1)
    gj <- g_j(
      t = T, X = X,
      beta1 = beta1, beta2 = beta2,
      lambda1 = lambda1, lambda2 = lambda2
    )
    BSS1 <- BSS1 + 2 * d_g1 * (gj[1] - o1)
    BSS2 <- BSS2 + 2 * d_g2 * (gj[2] - o2)
  }
  out <- BSS1 + BSS2
  if (!is.finite(out)) out <- 1e10
  out
}
LogLikelihood_beta <- function(parms, data, cause, big = 1e10) {
  df <- data
  beta   <- parms[1]
  lambda <- parms[2]
  if (!is.finite(beta) || !is.finite(lambda) || lambda <= 0) {
    return(big)
  }
  eta <- beta * df$x
  eeta <- safe_exp(eta)
  term1 <- -sum(df$x[df$event == cause])
  term2 <- sum(df$times * df$x * eeta * lambda)
  out <- term1 + term2
  if (!is.finite(out)) out <- big
  out
}
LogLikelihood_lambda <- function(parms, data, cause, big = 1e10) {
  df <- data
  beta   <- parms[1]
  lambda <- parms[2]
  if (!is.finite(beta) || !is.finite(lambda) || lambda <= 0) {
    return(big)
  }
  eta <- beta * df$x
  eeta <- safe_exp(eta)
  n_cause <- sum(df$event == cause)
  term1 <- -n_cause / lambda
  term2 <- sum(df$times * eeta)
  out <- term1 + term2
  if (!is.finite(out)) out <- big
  out
}
est_func_LL <- function(theta, data, cause){
    # reparamitize the baseline hazard
    parms <- c(theta[1],lambda_new(theta[2]))
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
    u <- est_func_LL(parms, data, cause)
    sum(u^2)
}
est_func_BSS <- function(theta, data){
    #reparamitze the baseline hazards to always be greater than 0
    parms <- theta
    parms[3:4] <- lambda_new(theta[3:4])
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

N <- 5
n <- 100
LL <- c()
BSS <- c()
dBSS <- c()
m <- lvm()
## regression(m) <- y ~ x
## distribution(m, ~x) <- normal.lvm()
## distribution(m,~death) <- coxWeibull.lvm(scale = 1)
## distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~x)
## eventTime(m) <- times ~ min(y = 1,death = 2)
regression(m) <- y ~ x
distribution(m, ~x) <- normal.lvm()
distribution(m,~death) <- coxExponential.lvm(rate = 0.5, timecut = 0)
distribution(m,~y) <- coxExponential.lvm(rate = 0.4, timecut = 0)
eventTime(m) <- times ~ min(y = 1,death = 2)
for (k in 1:N){
    set.seed(k)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    d <- d[,.(times,event,x)]
    train_n <- floor(n*0.7)
    d_train <- d[1:train_n]
    d_test <- d[(train_n+1):n]
    # Maximum likelihood estimator
    fit_LL_1 <- lava::NR(
                          start = c(0,0),
                          gradient = est_func_LL,
                          objective = obj_LL,
                          args = list(data = d_train, cause = 1),
                          control = list(tol = 1e-10, stepsize = 0.1, iter.max = 500)
                      )
    fit_LL_2 <- lava::NR(
                          start = c(0,0),
                          gradient = est_func_LL,
                          objective = obj_LL,
                          args = list(data = d_train, cause = 2),
                          control = list(tol = 1e-10, stepsize = 0.1, iter.max = 500)
                      )
    par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],fit_LL_1$par[2],fit_LL_2$par[2])
    LL <- append(LL,BrierScore(data = d_test,parms = par_LL))
    # Brier-Score-sum estimator using differentiated Brierscoresum as estimating equations
    fit <- lava::NR(
                     start = c(0,0,0,0),
                     gradient = est_func_BSS,
                     objective = obj_BSS,
                     args = list(data = d_train),
                     control = list(tol = 1e-10, stepsize = 0.1, iter.max = 200)
                 )
    par <- fit$par
    dBSS <- append(dBSS,BrierScore(data = d_test, parms = par))
    # Maximizing the Brier-score-sum directly 
    ## est <- lava::NR(c(1,1,1,1), args = list(data =  d_train), BrierScore, control = list(tol = 1e-10, stepsize = 0.1))
    ## parms_NR <- est$par
    ## BSS <- append(BSS, BrierScore(data = d_test,parms = parms_NR))
    print(par_LL)
}

mean(LL)
mean(dBSS)


############################################################
### Try to solve for only 1 cause and compare with survreg
# answer: The likelihood implemented likelihood method and survreg gives the same
library(data.table)
library(rms)
library(survival)
library(riskRegression)
library(lava)
F <- function(t, X, beta1, lambda1){
    haz1 <- exp(beta1*X)*lambda1
    numerator <- 1-exp(t*(-haz1))
    denominator <- haz1
    #return cumulative incidence functions for both cause 1 and 2
    F1 <- haz1*(numerator/denominator)
    return(c(F1))
}
BrierScore <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    lambda1 <- exp(parms[2])
    BS1 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
            } else{
                o1 <- 0
            }
        } else{
            o1 <- 0
        }
        BS1 <- BS1 + (F(t = T,X = df$x[i], beta1 = beta1, lambda1 = lambda1)[1] - o1)^2
    }
    return((BS1)/nrow(df))
}
### Differentiated Brier-score-sum as estimating equations
g_j <- function(t, X, beta1, lambda1){
    haz1 <- exp(beta1*X)*lambda1
    numerator <- 1-exp(t*(-haz1))
    denominator <- haz1
    #return cumulative incidence functions for both cause 1 and 2
    F1 <- haz1*(numerator/denominator)
    return(c(F1))
}
BrierScoreSum_beta1 <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- 0
    lambda1 <- parms[2]
    lambda2 <- 0
    BSS1 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
            } else{
                o1 <- 0
            }
        } else{
            o1 <- 0
        }
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g1 <- ((X*haz1)/(haz12^2))*(haz2*(1-exp(-T*haz12))+T*haz1*haz12*exp(-T*haz12))
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, lambda1 = lambda1)[1] - o1)
    }
    return((BSS1))
}
BrierScoreSum_lambda1 <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- 0
    lambda1 <- parms[2]
    lambda2 <- 0
    BSS1 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
            } else{
                o1 <- 0
            }
        } else{
            o1 <- 0
        }
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g1 <- (exp(beta1*X)/(haz12^2))*(exp(-T*haz12)*haz1^2*T+haz2*(exp(-T*haz12)*haz1*T-exp(-T*haz12)+1))
        #d_g1 <- exp(beta1*X)*((1-exp(-T*haz12))/haz12)+haz1*((haz12*(-T*exp(beta1*X)*exp(-T*haz12))-(1-exp(-T*haz12))*exp(beta1*X))/(haz12^2))
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, lambda1 = lambda1)[1] - o1)
    }
    return((BSS1))
}
est_func_BSS <- function(theta, data){
    #reparamitze the baseline hazards to always be greater than 0
    parms <- theta
    parms[2] <- exp(theta[2])
    val <- c(
        BrierScoreSum_beta1(parms,data),
        BrierScoreSum_lambda1(parms,data)
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
    parms <- cv(theta[1],exp(theta[2]))
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



set.seed(9)
m <- lvm()
regression(m) <- y ~ x
distribution(m,~x) <- normal.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.2, shape = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
d <- lava::sim(m,1000)
d <- setDT(d)
setnames(d,"status","event")
d <- d[,.(times,event,x)]
# change all events to 1
d[,event := 1]
d[times <= 0.00001, times := 0.1]


# Maximum likelihood estimator
fit_LL_1 <- lava::NR(
  start = c(0, 0),
  gradient = est_func_LL,
  objective = obj_LL,
  args = list(data = d, cause = 1),
  control = list(tol = 1e-15, stepsize = 0.5)
)
par_LL <- fit_LL_1$par
BrierScore(data = d,parms = par_LL)

m <- survreg(Surv(times,event) ~ x, dist = "exponential", data = d)
par_survreg <- -1*as.numeric(c(m$coefficients[2],m$coefficients[1]))
BrierScore(data = d,parms = par_survreg)

# Brier-Score-sum estimator using differentiated Brierscoresum as estimating equations
set.seed(1)
fit <- lava::NR(
  start = c(0,0),
  gradient = est_func_BSS,
  objective = obj_BSS,
  args = list(data = d),
  control = list(tol = 1e-15, stepsize = 0.5)
)
par <- fit$par
BrierScore(data = d, parms = par)

# Maximizing the Brier-score-sum directly 
est <- lava::NR(c(0,0), args = list(data =  d), BrierScore, control = list(tol = 1e-10, stepsize = 0.5))
parms_NR <- est$par
BrierScore(data = d,parms = parms_NR)


######################

# Check if the Brier score is convex
# try solve() (Can maybe work for likelihood, but brier score is non-linear so doesnt work there)
# Try for binary x to get explicit solution
# look at lava::estimate
# Write the setup better and more precise in fixing_the_problem.org

