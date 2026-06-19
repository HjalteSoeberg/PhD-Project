### logistic_vs_CSC_vs_brier.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: May 11 2026 (11:50) 
## Version: 
## Last-Updated: May 13 2026 (11:48) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 74
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
library(prodlim)
source("Project1_constrained_risk_prediction/Brier_score_estimation/data/simulateSteno1.R")
F <- function(t, X, beta1, beta2, lambda1, lambda2){
    haz1 <- exp(beta1*X)*lambda1
    haz2 <- exp(beta2*X)*lambda2
    numerator <- 1-exp(t*(-haz1-haz2))
    denominator <- haz1+haz2
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    return(c(F1,F2))
}
BrierScore <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
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
        BS1 <- BS1 + (F(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)^2
        BS2 <- BS2 + (F(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)^2
    }
    return((BS1+BS2)/nrow(df))
}
.bs_parse_par <- function(par, log_rates = TRUE) {
  if (length(par) != 4L) {
    stop("par must have length 4.")
  }
  nms <- names(par)
  if (!is.null(nms) && any(nzchar(nms))) {
    if (isTRUE(log_rates)) {
      log_nm <- c("beta1", "beta2", "log_lambda01", "log_lambda02")
      eta_nm <- c("beta1", "beta2", "eta01", "eta02")
      lam_nm <- c("beta1", "beta2", "lambda01", "lambda02")
      if (all(log_nm %in% nms)) {
        par <- par[log_nm]
      } else if (all(eta_nm %in% nms)) {
        par <- par[eta_nm]
      } else if (all(lam_nm %in% nms)) {
        stop(
          "With log_rates = TRUE, pass log_lambda01 and log_lambda02, ",
          "not lambda01 and lambda02."
        )
      }
    } else {
      lam_nm <- c("beta1", "beta2", "lambda01", "lambda02")
      log_nm <- c("beta1", "beta2", "log_lambda01", "log_lambda02")
      if (all(lam_nm %in% nms)) {
        par <- par[lam_nm]
      } else if (all(log_nm %in% nms)) {
        stop(
          "With log_rates = FALSE, pass lambda01 and lambda02, ",
          "not log_lambda01 and log_lambda02."
        )
      }
    }
  }
  par <- as.numeric(par)
  if (any(!is.finite(par))) {
    stop("par contains non-finite values.")
  }
  par
}
## h(s) = (1 - exp(-tau*s)) / s
## h'(s), h''(s)
.bs_h_terms <- function(s, tau) {
  z <- tau * s
  E <- exp(-z)
  h   <- -expm1(-z) / s
  hp  <- ((1 + z) * E - 1) / s^2
  hpp <- (2 - (z^2 + 2 * z + 2) * E) / s^3
  ## Taylor expansion for very small tau*s
  small <- is.finite(z) & abs(z) < 1e-6
  if (any(small)) {
    ss <- s[small]
    tt <- tau[small]
    h[small] <-
      tt -
      tt^2 * ss / 2 +
      tt^3 * ss^2 / 6 -
      tt^4 * ss^3 / 24 +
      tt^5 * ss^4 / 120
    hp[small] <-
      -tt^2 / 2 +
      tt^3 * ss / 3 -
      tt^4 * ss^2 / 8 +
      tt^5 * ss^3 / 30 -
      tt^6 * ss^4 / 144
    hpp[small] <-
      tt^3 / 3 -
      tt^4 * ss / 4 +
      tt^5 * ss^2 / 10 -
      tt^6 * ss^3 / 36
  }
  list(h = h, hp = hp, hpp = hpp)
}
.bs_eval <- function(par, data, tau, log_rates = TRUE) {
  required <- c("times", "event", "X")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("data is missing columns: ", paste(missing, collapse = ", "))
  }
  par <- .bs_parse_par(par, log_rates = log_rates)
  X     <- as.numeric(data$X)
  times <- as.numeric(data$times)
  event <- as.character(data$event)
  n <- length(X)
  if (n == 0L) {
    stop("data has zero rows.")
  }
  if (any(!is.finite(X)) || any(!is.finite(times))) {
    stop("X and times must be finite.")
  }
  if (!(length(tau) %in% c(1L, n))) {
    stop("tau must have length 1 or nrow(data).")
  }
  tau <- as.numeric(tau)
  tau <- if (length(tau) == 1L) rep(tau, n) else tau
  if (any(!is.finite(tau)) || any(tau < 0)) {
    stop("tau must be finite and non-negative.")
  }
  beta1 <- par[1]
  beta2 <- par[2]
  if (isTRUE(log_rates)) {
    eta01 <- par[3]
    eta02 <- par[4]
    a <- exp(eta01 + beta1 * X)  # lambda01 * exp(beta1*x)
    b <- exp(eta02 + beta2 * X)  # lambda02 * exp(beta2*x)
    nm <- c("beta1", "beta2", "log_lambda01", "log_lambda02")
  } else {
    lambda01 <- par[3]
    lambda02 <- par[4]
    if (lambda01 <= 0 || lambda02 <= 0) {
      stop("lambda01 and lambda02 must be positive. Prefer log_rates = TRUE.")
    }
    e1 <- exp(beta1 * X)
    e2 <- exp(beta2 * X)
    a <- lambda01 * e1
    b <- lambda02 * e2
    nm <- c("beta1", "beta2", "lambda01", "lambda02")
  }
  s <- a + b
  if (any(!is.finite(s)) || any(s <= 0)) {
    stop("a + b must be positive and finite.")
  }
  ht <- .bs_h_terms(s, tau)
  h   <- ht$h
  hp  <- ht$hp
  hpp <- ht$hpp
  ## g1 = a*h(s), g2 = b*h(s)
  g1 <- a * h
  g2 <- b * h
  o1 <- as.numeric(event == "1" & times <= tau)
  o2 <- as.numeric(event == "2" & times <= tau)
  r1 <- g1 - o1
  r2 <- g2 - o2
  value <- mean(r1^2 + r2^2)
  p <- 4L
  ## First derivatives of a and b wrt parameters
  da <- matrix(0, nrow = n, ncol = p)
  db <- matrix(0, nrow = n, ncol = p)
  ## Second derivatives of a and b wrt parameters
  d2a <- array(0, dim = c(n, p, p))
  d2b <- array(0, dim = c(n, p, p))
  if (isTRUE(log_rates)) {
    ## par = beta1, beta2, log_lambda01, log_lambda02
    da[, 1] <- X * a
    da[, 3] <- a
    db[, 2] <- X * b
    db[, 4] <- b
    d2a[, 1, 1] <- X^2 * a
    d2a[, 1, 3] <- X * a
    d2a[, 3, 1] <- X * a
    d2a[, 3, 3] <- a
    d2b[, 2, 2] <- X^2 * b
    d2b[, 2, 4] <- X * b
    d2b[, 4, 2] <- X * b
    d2b[, 4, 4] <- b
  } else {
    ## par = beta1, beta2, lambda01, lambda02
    e1 <- exp(beta1 * X)
    e2 <- exp(beta2 * X)
    da[, 1] <- X * a
    da[, 3] <- e1
    db[, 2] <- X * b
    db[, 4] <- e2
    d2a[, 1, 1] <- X^2 * a
    d2a[, 1, 3] <- X * e1
    d2a[, 3, 1] <- X * e1
    d2b[, 2, 2] <- X^2 * b
    d2b[, 2, 4] <- X * e2
    d2b[, 4, 2] <- X * e2
  }
  ## Derivatives of g1 wrt a,b
  fa1  <- h + a * hp
  fb1  <- a * hp
  faa1 <- 2 * hp + a * hpp
  fab1 <- hp + a * hpp
  fbb1 <- a * hpp
  ## Derivatives of g2 wrt a,b
  fa2  <- b * hp
  fb2  <- h + b * hp
  faa2 <- b * hpp
  fab2 <- hp + b * hpp
  fbb2 <- 2 * hp + b * hpp
  ## Gradients of g1 and g2 wrt par
  grad1 <- sweep(da, 1L, fa1, "*") + sweep(db, 1L, fb1, "*")
  grad2 <- sweep(da, 1L, fa2, "*") + sweep(db, 1L, fb2, "*")
  colnames(grad1) <- nm
  colnames(grad2) <- nm
  ## Gradient of BS objective
  gradient <- 2 / n * colSums(
    sweep(grad1, 1L, r1, "*") +
      sweep(grad2, 1L, r2, "*")
  )
  names(gradient) <- nm
  ## Hessian of BS objective:
  ## H = 2/N sum_j sum_i [grad(g_j) grad(g_j)' + residual_j Hessian(g_j)]
  hessian <- matrix(0, nrow = p, ncol = p, dimnames = list(nm, nm))
  for (k in seq_len(p)) {
    for (l in k:p) {
      H1_kl <-
        faa1 * da[, k] * da[, l] +
        fab1 * (da[, k] * db[, l] + db[, k] * da[, l]) +
        fbb1 * db[, k] * db[, l] +
        fa1 * d2a[, k, l] +
        fb1 * d2b[, k, l]
      H2_kl <-
        faa2 * da[, k] * da[, l] +
        fab2 * (da[, k] * db[, l] + db[, k] * da[, l]) +
        fbb2 * db[, k] * db[, l] +
        fa2 * d2a[, k, l] +
        fb2 * d2b[, k, l]
      hessian[k, l] <- 2 / n * sum(
        grad1[, k] * grad1[, l] + r1 * H1_kl +
          grad2[, k] * grad2[, l] + r2 * H2_kl
      )
      hessian[l, k] <- hessian[k, l]
    }
  }
  list(
    value = value,
    gradient = gradient,
    hessian = hessian,
    g1 = g1,
    g2 = g2,
    o1 = o1,
    o2 = o2
  )
}
BS_objective <- function(par, data, tau, log_rates = TRUE) {
  .bs_eval(par, data = data, tau = tau, log_rates = log_rates)$value
}
BS_gradient <- function(par, data, tau, log_rates = TRUE) {
  .bs_eval(par, data = data, tau = tau, log_rates = log_rates)$gradient
}
BS_hessian <- function(par, data, tau, log_rates = TRUE) {
  .bs_eval(par, data = data, tau = tau, log_rates = log_rates)$hessian
}
### Likelihood
LogLikelihood_beta <- function(parms, data, cause){
    df <- data
    beta <- parms[1]
    lambda <- parms[2]
    L <- 0
    for (i in 1:nrow(df)){
        if (df$event[i] == cause){
            L <- L - df$X[i]
        }
        L <- L + df$times[i]*df$X[i]*exp(beta*df$X[i])*lambda
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
        L <- L + df$times[i]*exp(beta*df$X[i])
    }
    return(L)
}
est_func_LL <- function(theta, data, cause){
    parms <- c(theta[1],exp(theta[2]))
    val <- c(
        LogLikelihood_beta(parms,data,cause),
        LogLikelihood_lambda(parms,data,cause)
    )
    val
}
obj_LL <- function(parms, data, cause) {
    u <- est_func_LL(parms, data)
    sum(u^2)
}
# Non prop-hazards data, numeric x
sim_non_prop_haz <- function(n){
    # Discrete-time simulation settings
    x <- rnorm(n)
    dt <- 0.01
    tmax <- 10
    times <- seq(dt, tmax, by = dt)
    event <- rep(NA_integer_, n)
    time_to_event <- rep(NA_real_, n)
    for (i in seq_len(n)) {
        for (t in times) {
            # Cause-specific hazards with time-varying covariate effects
            h1 <- 0.08 * exp( 1 * x[i] - 0.45 * x[i] * t)
            h2 <- 0.06 * exp(-0.8 * x[i] + 0.30 * x[i] * t)
            # Event probabilities over small interval dt
            p1 <- h1 * dt
            p2 <- h2 * dt    
            u <- runif(1)   
            if (u < p1) {
                event[i] <- 1
                time_to_event[i] <- t
                break
            } else if (u < p1 + p2) {
                event[i] <- 2
                time_to_event[i] <- t
                break
            }
        }
        # Force no censoring if no event occurred by tmax
        if (is.na(event[i])) {
            h1 <- 0.08 * exp( 1 * x[i] - 0.35 * x[i] * tmax)
            h2 <- 0.06 * exp(-0.8 * x[i] + 0.30 * x[i] * tmax)
            probs <- c(h1, h2) / (h1 + h2)
            event[i] <- sample(c(1, 2), size = 1, prob = probs)
            time_to_event[i] <- tmax
        }
    }
    dat <- data.table(
        event = event,
        times = time_to_event,
        x = x
    )
    dat[]
}
sim_cox_exp <- function(n, true_par = c(1.4, -0.4, 0.4, 0.2)){
    m <- lvm()
    regression(m, y~X) <- true_par[1]
    regression(m, death~X) <- true_par[2]
    distribution(m, ~X) <- normal.lvm()
    distribution(m,~death) <- coxExponential.lvm(rate = true_par[4])
    distribution(m,~y) <- coxExponential.lvm(rate = true_par[3])
    eventTime(m) <- times ~ min(y = 1,death = 2)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    ## setnames(d,c("time","age"),c("times","X"))
    d <- d[,.(times,event,X)]
    d[]
}
sim_dpnext <- function(n){
    m <- lvm()
    distribution(m,~age) <- normal.lvm(mean = 57.0602154546317, sd = 14.1226171463087)
    distribution(m,~y) <- coxWeibull.lvm(scale = 4.99939534464252e-05, shape = 1.6080293052532)
    regression(m, y~hba1c+age) <- c(0.121823881773495, 0.00536060442429844)
    distribution(m, ~death) <- coxWeibull.lvm(scale = 2.95018879855583e-05, shape = 1.24062643614)
    regression(m, death~hba1c+age) <- c(-0.014226863536, 0.096357141521148)
    m <- eventTime(m,time ~ min(y = 1, death = 2), 'event')
    distribution(m, ~hba1c) <- normal.lvm(mean = 30.4609161720932, sd = 3.75945937876573)
    regression(m, hba1c~age) <- c(0.100349744003182)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,c("time","age"),c("times","X"))
    d <- d[,.(times,event,X)]
    d[]
}
sim_cox_weibull <- function(n){
    ### coxWeibull
    m <- lvm()
    regression(m) <- y ~ X
    regression(m) <- death ~ -0.5*X
    distribution(m, ~X) <- normal.lvm()
    distribution(m,~death) <- coxWeibull.lvm(scale = 1)
    distribution(m,~y) <- coxWeibull.lvm(scale = 0.1)
    eventTime(m) <- times ~ min(y = 1,death = 2)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    d <- d[,.(times,event,X)]
    d[]
}

BrierScore_glm <- function(m1,m2, data, tau){
    df <- data
    BS1 <- 0
    BS2 <- 0
    bs1_risk <- as.numeric(predict(m1, newdata = df, type = "response"))
    bs2_risk <- as.numeric(predict(m2, newdata = df, type = "response"))
    for (i in 1:nrow(df)){
        if (tau >= df$times[i]){
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
        BS1 <- BS1 + (bs1_risk[i] - o1)^2
        BS2 <- BS2 + (bs2_risk[i] - o2)^2
    }
    return((BS1+BS2)/nrow(df))
}
BrierScore_CSC <- function(model, data, tau){
    df <- data
    BS1 <- 0
    BS2 <- 0
    bs1_risk <- predict(model, newdata = df, cause = 1, times = tau)$absRisk
    bs2_risk <- predict(model, newdata = df, cause = 2, times = tau)$absRisk
    for (i in 1:nrow(df)){
        if (tau >= df$times[i]){
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
        BS1 <- BS1 + (bs1_risk[i] - o1)^2
        BS2 <- BS2 + (bs2_risk[i] - o2)^2
    }
    return((BS1+BS2)/nrow(df))
}

n <- 5e3
## d <- simulateStenoT1(n)
d <- sim_cox_exp(n)
## d <- sim_dpnext(n)
## d <- sim_cox_weibull(n)
## d <- sim_non_prop_haz(n)
    
tau <- 4
d[,':='(y1 = 0, y2 = 0)]
d[event == 1 & times <= tau, y1 := 1]
d[event == 2 & times <= tau, y2 := 1]
train_n <- floor(n*0.7)
d_train <- d[1:train_n]
d_test <- d[(train_n+1):n]
# fit csc
csc <- CSC(data = d_train, Hist(times,event)~X)
# fit logistic regression
logi1 <- glm(data = d_train, formula = y1~X, family = binomial)
logi2 <- glm(data = d_train, formula = y2~X, family = binomial)

BrierScore_glm(m1 = logi1, m2 = logi2, data = d_test, tau = tau)
BrierScore_CSC(model = csc, data = d_test, tau = tau)

start <- c(
    beta1        = true_par[1],
    beta2        = true_par[2],
    log_lambda01 = log(true_par[3]),
    log_lambda02 = log(true_par[4])
)
fit_LL_1 <- lava::NR(
                      start = c(start[1], start[3]),
                      gradient = est_func_LL,
                      objective = obj_LL,
                      args = list(data = d_train, cause = 1),
                      control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                  )
fit_LL_2 <- lava::NR(
                      start = c(start[2], start[4]),
                      gradient = est_func_LL,
                      objective = obj_LL,
                      args = list(data = d_train, cause = 2),
                      control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                  )
par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],exp(fit_LL_1$par[2]),exp(fit_LL_2$par[2]))
BrierScore(data = d_test,parms = par_LL, T = tau)
fit_BS <- lava::NR(
                    start     = c(par_LL[1:2],log(par_LL[3:4])),
                    objective = BS_objective,
                    gradient  = BS_gradient,
                    hessian   = BS_hessian,
                    args      = list(data = d_train, tau = tau, log_rates = TRUE),
                    control   = list(
                        trace = 0,
                        iter.max = 200,
                        tol = 1e-10,
                        stepsize = 0.5,
                        backtrack = "armijo"
                    )
                )
## Estimates on the original lambda scale
theta_hat <- fit_BS$par
estimates <- c(
    theta_hat[1],
    theta_hat[2],
    exp(theta_hat[3]),
    exp(theta_hat[4])
)
BrierScore(data = d_test, parms = estimates, T = tau)
BrierScore(data = d_test, parms = as.numeric(c(start[1:2],exp(start[3:4]))), T = tau)

#######################################################################33


### Simulation study
start <- c(
    beta1        = 1.4,
    beta2        = -0.4,
    log_lambda01 = log(0.4),
    log_lambda02 = log(0.2)
)
n <- 1000 # 200,500,1000,2000
N <- 20 #1000
tau = 3
LL <- c()
BS <- c()
logi <- c()
cscc <- c()
true <- c()
set.seed(7)
for (k in 1:N){
    tryCatch({
        d <- sim_cox_exp(n)
        ## d <- sim_dpnext(n)
        ## d <- sim_cox_weibull(n)
        ## d <- sim_non_prop_haz(n)
        d[,':='(y1 = 0, y2 = 0)]
        d[event == 1 & times <= tau, y1 := 1]
        d[event == 2 & times <= tau, y2 := 1]
        train_n <- floor(n*0.7)
        d_train <- d[1:train_n]
        d_test <- d[(train_n+1):n]
        # fit csc
        csc <- CSC(data = d_train, Hist(times,event)~X)
        # fit logistic regression
        logi1 <- glm(data = d_train, formula = y1~X, family = binomial)
        logi2 <- glm(data = d_train, formula = y2~X, family = binomial)
        logi <- append(logi,BrierScore_glm(m1 = logi1, m2 = logi2, data = d_test, tau = tau))
        cscc <- append(cscc,BrierScore_CSC(model = csc, data = d_test, tau = tau))
        fit_LL_1 <- lava::NR(
                              start = c(start[1], start[3]),
                              gradient = est_func_LL,
                              objective = obj_LL,
                              args = list(data = d_train, cause = 1),
                              control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                          )
        fit_LL_2 <- lava::NR(
                              start = c(start[2], start[4]),
                              gradient = est_func_LL,
                              objective = obj_LL,
                              args = list(data = d_train, cause = 2),
                              control = list(trace = 0,iter.max = 200,tol = 1e-10,stepsize = 0.5)
                          )
        par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],exp(fit_LL_1$par[2]),exp(fit_LL_2$par[2]))
        LL <- append(LL,BrierScore(data = d_test,parms = par_LL, T = tau))                     
        fit_BS <- lava::NR(
                            start     = c(par_LL[1:2],log(par_LL[3:4])),
                            objective = BS_objective,
                            gradient  = BS_gradient,
                            hessian   = BS_hessian,
                            args      = list(data = d_train, tau = tau, log_rates = TRUE),
                            control   = list(
                                trace = 0,
                                iter.max = 200,
                                tol = 1e-10,
                                stepsize = 0.5,
                                backtrack = "armijo"
                            )
                        )
        ## Estimates on the original lambda scale
        theta_hat <- fit_BS$par
        estimates <- c(
            theta_hat[1],
            theta_hat[2],
            exp(theta_hat[3]),
            exp(theta_hat[4])
        )
        BS <- append(BS,BrierScore(data = d_test, parms = estimates, T = tau))
        true <- append(true,BrierScore(data = d_test, parms = as.numeric(c(start[1:2],exp(start[3:4]))), T = tau))
    }, error = function(e){})
}

df <- data.table(Model = c("logistic", "CSC", "Likelihood","Brier","Oracle"),
                Mean = c(mean(logi), mean(cscc), mean(LL), mean(BS), mean(true)))
setkey(df,Mean)
df[]


######################################################################
### logistic_vs_CSC_vs_brier.R ends here
