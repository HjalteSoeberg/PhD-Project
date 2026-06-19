### does_BS_find_datagenerating_parameters.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: May  5 2026 (12:13) 
## Version: 
## Last-Updated: May  5 2026 (15:10) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 13
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
## ============================================================
## Brier-score type objective for competing risks:
##   data columns: times, event, x
##   event is 1 or 2
##   tau is the fixed prediction horizon
##
## Default parameter scale:
##   par = c(beta1, beta2, log_lambda01, log_lambda02)
##
## If log_rates = FALSE:
##   par = c(beta1, beta2, lambda01, lambda02)
## but this does NOT enforce lambda > 0.
## ============================================================

library(data.table)
library(rms)
library(survival)
library(riskRegression)
library(lava)
library(prodlim)

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
        BS1 <- BS1 + (F(t = T,X = df$x[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)^2
        BS2 <- BS2 + (F(t = T,X = df$x[i], beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)^2
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
  required <- c("times", "event", "x")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("data is missing columns: ", paste(missing, collapse = ", "))
  }
  par <- .bs_parse_par(par, log_rates = log_rates)
  x     <- as.numeric(data$x)
  times <- as.numeric(data$times)
  event <- as.character(data$event)
  n <- length(x)
  if (n == 0L) {
    stop("data has zero rows.")
  }
  if (any(!is.finite(x)) || any(!is.finite(times))) {
    stop("x and times must be finite.")
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
    a <- exp(eta01 + beta1 * x)  # lambda01 * exp(beta1*x)
    b <- exp(eta02 + beta2 * x)  # lambda02 * exp(beta2*x)
    nm <- c("beta1", "beta2", "log_lambda01", "log_lambda02")
  } else {
    lambda01 <- par[3]
    lambda02 <- par[4]
    if (lambda01 <= 0 || lambda02 <= 0) {
      stop("lambda01 and lambda02 must be positive. Prefer log_rates = TRUE.")
    }
    e1 <- exp(beta1 * x)
    e2 <- exp(beta2 * x)
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
    da[, 1] <- x * a
    da[, 3] <- a
    db[, 2] <- x * b
    db[, 4] <- b
    d2a[, 1, 1] <- x^2 * a
    d2a[, 1, 3] <- x * a
    d2a[, 3, 1] <- x * a
    d2a[, 3, 3] <- a
    d2b[, 2, 2] <- x^2 * b
    d2b[, 2, 4] <- x * b
    d2b[, 4, 2] <- x * b
    d2b[, 4, 4] <- b
  } else {
    ## par = beta1, beta2, lambda01, lambda02
    e1 <- exp(beta1 * x)
    e2 <- exp(beta2 * x)
    da[, 1] <- x * a
    da[, 3] <- e1
    db[, 2] <- x * b
    db[, 4] <- e2
    d2a[, 1, 1] <- x^2 * a
    d2a[, 1, 3] <- x * e1
    d2a[, 3, 1] <- x * e1
    d2b[, 2, 2] <- x^2 * b
    d2b[, 2, 4] <- x * e2
    d2b[, 4, 2] <- x * e2
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

### Simulation study
true_par <- c(1.4, -0.4, 0.4, 0.2)
start <- c(
  beta1        = true_par[1],
  beta2        = true_par[2],
  log_lambda01 = log(true_par[3]),
  log_lambda02 = log(true_par[4])
)
n <- 50000 # 200,500,1000,2000
time = 4 #prop at 3, non prop at 6 (or 5)
m <- lvm()
### coxWeibull
regression(m) <- y ~ 1.4*x
regression(m) <- death ~ -0.4*x
distribution(m, ~x) <- normal.lvm()
distribution(m,~death) <- coxExponential.lvm(rate = true_par[4])
distribution(m,~y) <- coxExponential.lvm(rate = true_par[3])
eventTime(m) <- times ~ min(y = 1,death = 2)
set.seed(123)
d <- setDT(sim(m,n))
setnames(d,"status","event")
d <- d[,.(times,event,x)]

fit_LL_1 <- lava::NR(
                      start = c(start[1], start[3]),
                      gradient = est_func_LL,
                      objective = obj_LL,
                      args = list(data = d, cause = 1),
                      control = list(trace = 0,iter.max = 300,tol = 1e-15,stepsize = 0.5)
                  )
fit_LL_2 <- lava::NR(
                      start = c(start[2], start[4]),
                      gradient = est_func_LL,
                      objective = obj_LL,
                      args = list(data = d, cause = 2),
                      control = list(trace = 0,iter.max = 300,tol = 1e-15,stepsize = 0.5)
                  )
par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],exp(fit_LL_1$par[2]),exp(fit_LL_2$par[2]))
LL <- BrierScore(data = d,parms = par_LL, T = time)
fit_BS <- lava::NR(
                    start     = start,
                    objective = BS_objective,
                    gradient  = BS_gradient,
                    hessian   = BS_hessian,
                    args      = list(data = d, tau = time, log_rates = TRUE),
                    control   = list(
                        trace = 0,
                        iter.max = 300,
                        tol = 1e-15,
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
BS <- BrierScore(data = d, parms = estimates, T = time)

LL
BS
BrierScore(data = d, parms = true_par, T = time)
par_LL
estimates
true_par


######################################################################
### does_BS_find_datagenerating_parameters.R ends here
