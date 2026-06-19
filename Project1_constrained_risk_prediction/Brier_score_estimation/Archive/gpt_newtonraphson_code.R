### gpt_newtonraphson_code.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Apr 27 2026 (09:35) 
## Version: 
## Last-Updated: May 11 2026 (09:18) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 85
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

########################
#### safer version
## ============================================================
## Stable BS objective, gradient and Hessian for lava::NR()
##
## Parameter vector:
##   par = c(beta1, beta2, log_lambda01, log_lambda02)
##
## Data must contain:
##   times, event, x
##
## This version avoids computing:
##   exp(log_lambda01 + beta1*x) + exp(log_lambda02 + beta2*x)
## directly, so it is much less likely to fail from overflow.
## ============================================================
.bs_parse_par_stable <- function(par) {
  if (length(par) != 4L) {
    stop("par must have length 4: beta1, beta2, log_lambda01, log_lambda02.")
  }
  nms <- names(par)
  if (!is.null(nms) && all(c("beta1", "beta2", "log_lambda01", "log_lambda02") %in% nms)) {
    par <- par[c("beta1", "beta2", "log_lambda01", "log_lambda02")]
  }
  par <- as.numeric(par)
  if (any(!is.finite(par))) {
    stop("par contains non-finite values.")
  }
  par
}
.bs_q_terms_from_logz <- function(logz) {
  ## z = tau * s
  ## q    = 1 - exp(-z)
  ## q_u  = dq / d log(s)       = z * exp(-z)
  ## q_uu = d2q / d log(s)^2    = z * exp(-z) * (1 - z)
  ##
  ## For very large z, q = 1 and q_u = q_uu = 0 numerically.
  q   <- numeric(length(logz))
  qu  <- numeric(length(logz))
  quu <- numeric(length(logz))
  big <- logz > 36
  mid <- !big & is.finite(logz)
  if (any(mid)) {
    z <- exp(logz[mid])
    ez <- exp(-z)
    q[mid]   <- -expm1(-z)
    qu[mid]  <- z * ez
    quu[mid] <- qu[mid] * (1 - z)
  }
  if (any(big)) {
    q[big]   <- 1
    qu[big]  <- 0
    quu[big] <- 0
  }
  ## logz = -Inf corresponds to z = 0, so q = qu = quu = 0.
  list(q = q, qu = qu, quu = quu)
}
.bs_logsumexp2 <- function(A, B) {
  m <- pmax(A, B)
  m + log1p(exp(-abs(A - B)))
}
.bs_eval_stable <- function(par, data, tau, ridge = 0) {
  required <- c("times", "event", "x")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("data is missing columns: ", paste(missing, collapse = ", "))
  }
  par <- .bs_parse_par_stable(par)
  beta1        <- par[1]
  beta2        <- par[2]
  log_lambda01 <- par[3]
  log_lambda02 <- par[4]
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
  ## A = log a, B = log b
  ## a = lambda01 * exp(beta1*x)
  ## b = lambda02 * exp(beta2*x)
  A <- log_lambda01 + beta1 * x
  B <- log_lambda02 + beta2 * x
  if (any(is.nan(A)) || any(is.nan(B))) {
    stop("linear predictors became NaN.")
  }
  ## pi1 = a / (a + b), pi2 = b / (a + b)
  pi1 <- plogis(A - B)
  pi2 <- 1 - pi1
  if (any(!is.finite(pi1)) || any(!is.finite(pi2))) {
    stop("cause probabilities became non-finite.")
  }
  ## log_s = log(a + b), computed stably
  log_s <- .bs_logsumexp2(A, B)
  log_tau <- rep(-Inf, n)
  pos_tau <- tau > 0
  log_tau[pos_tau] <- log(tau[pos_tau])
  qt <- .bs_q_terms_from_logz(log_tau + log_s)
  q   <- qt$q
  qu  <- qt$qu
  quu <- qt$quu
  ## g1 = pi1 * q
  ## g2 = pi2 * q
  g1 <- pi1 * q
  g2 <- pi2 * q
  o1 <- as.numeric(event == "1" & times <= tau)
  o2 <- as.numeric(event == "2" & times <= tau)
  r1 <- g1 - o1
  r2 <- g2 - o2
  value <- mean(r1^2 + r2^2)
  p <- 4L
  nm <- c("beta1", "beta2", "log_lambda01", "log_lambda02")
  gradient <- numeric(p)
  hessian  <- matrix(0, nrow = p, ncol = p)
  for (i in seq_len(n)) {
    xi <- x[i]
    ## A = log_lambda01 + beta1*x
    ## B = log_lambda02 + beta2*x
    ##
    ## Derivatives of A and B wrt par:
    VA <- c(xi, 0, 1, 0)
    VB <- c(0, xi, 0, 1)
    V  <- rbind(VA, VB)
    p1 <- pi1[i]
    p2 <- pi2[i]
    pp <- p1 * p2
    ## Derivatives of pi1 and pi2 wrt A,B
    dpi1 <- c(pp, -pp)
    dpi2 <- -dpi1
    d2pi1 <- pp * (1 - 2 * p1) * matrix(
      c(1, -1,
        -1, 1),
      nrow = 2,
      byrow = TRUE
    )
    d2pi2 <- -d2pi1
    ## Derivatives of log_s wrt A,B
    du <- c(p1, p2)
    d2u <- pp * matrix(
      c(1, -1,
        -1, 1),
      nrow = 2,
      byrow = TRUE
    )
    ## Derivatives of q wrt A,B
    dq <- qu[i] * du
    d2q <- quu[i] * tcrossprod(du) + qu[i] * d2u
    ## g1 = pi1*q
    gradAB1 <- dpi1 * q[i] + p1 * dq
    hessAB1 <-
      d2pi1 * q[i] +
      outer(dpi1, dq) +
      outer(dq, dpi1) +
      p1 * d2q
    ## g2 = pi2*q
    gradAB2 <- dpi2 * q[i] + p2 * dq
    hessAB2 <-
      d2pi2 * q[i] +
      outer(dpi2, dq) +
      outer(dq, dpi2) +
      p2 * d2q
    ## Chain rule from A,B to beta/log-lambda parameters
    grad1 <- as.vector(gradAB1 %*% V)
    grad2 <- as.vector(gradAB2 %*% V)
    hess1 <- crossprod(V, hessAB1 %*% V)
    hess2 <- crossprod(V, hessAB2 %*% V)
    gradient <- gradient + 2 * (r1[i] * grad1 + r2[i] * grad2)
    hessian <- hessian +
      2 * (
        tcrossprod(grad1) + r1[i] * hess1 +
          tcrossprod(grad2) + r2[i] * hess2
      )
  }
  gradient <- gradient / n
  hessian  <- hessian / n
  ## Optional tiny ridge penalty.
  ## This can help when NR tries to run to extreme parameter values.
  if (ridge > 0) {
    value <- value + ridge * sum(par^2)
    gradient <- gradient + 2 * ridge * par
    hessian <- hessian + diag(2 * ridge, p)
  }
  names(gradient) <- nm
  dimnames(hessian) <- list(nm, nm)
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

BS_objective2 <- function(par, data, tau, ridge = 0) {
  .bs_eval_stable(par, data = data, tau = tau, ridge = ridge)$value
}
BS_gradient2 <- function(par, data, tau, ridge = 0) {
  .bs_eval_stable(par, data = data, tau = tau, ridge = ridge)$gradient
}
BS_hessian2 <- function(par, data, tau, ridge = 0) {
  .bs_eval_stable(par, data = data, tau = tau, ridge = ridge)$hessian
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
start <- c(
  beta1        = 1,
  beta2        = -0.8,
  log_lambda01 = log(0.08),
  log_lambda02 = log(0.06)
)
n <- 300 # 200,500,1000,2000
N <- 10 #1000
time = 3 #prop at 3, non prop at 6 (or 5)
m <- lvm()
### coxWeibull
regression(m) <- y ~ x
distribution(m, ~x) <- normal.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
LL <- c()
BS <- c()
BS_safe <- c()
set.seed(7)
for (k in 1:N){
    tryCatch({
        ## d <- lava::sim(m,n)
        ## d <- setDT(d)
        ## setnames(d,"status","event")
        ## d <- d[,.(times,event,x)]
        ### Non proportional hazards
        d <- non_prop_haz(n)
        train_n <- floor(n*0.7)
        d_train <- d[1:train_n]
        d_test <- d[(train_n+1):n]
        fit_LL_1 <- lava::NR(
                              start = c(start[1], start[3]),
                              gradient = est_func_LL,
                              objective = obj_LL,
                              args = list(data = d_train, cause = 1),
                              control = list(trace = 0,iter.max = 200,tol = 1e-15,stepsize = 0.5)
                          )
        fit_LL_2 <- lava::NR(
                              start = c(start[2], start[4]),
                              gradient = est_func_LL,
                              objective = obj_LL,
                              args = list(data = d_train, cause = 2),
                              control = list(trace = 0,iter.max = 200,tol = 1e-15,stepsize = 0.5)
                          )
        par_LL <- c(fit_LL_1$par[1],fit_LL_2$par[1],exp(fit_LL_1$par[2]),exp(fit_LL_2$par[2]))
        LL <- append(LL,BrierScore(data = d_test,parms = par_LL, T = time))
        ## fit_BS_safe <- lava::NR(
        ##                          start     = start,
        ##                          objective = BS_objective2,
        ##                          gradient  = BS_gradient2,
        ##                          hessian   = BS_hessian2,
        ##                          args      = list(data = d_train, tau = time),
        ##                          control   = list(
        ##                              trace = 0,
        ##                              iter.max = 200,
        ##                              tol = 1e-15,
        ##                              stepsize = 0.5,
        ##                              backtrack = "armijo"
        ##                          )
        ##                      )
        ## ## Estimates on the original lambda scale
        ## theta_hat_safe <- fit_BS_safe$par
        ## estimates_safe <- c(
        ##     theta_hat_safe[1],
        ##     theta_hat_safe[2],
        ##     exp(theta_hat_safe[3]),
        ##     exp(theta_hat_safe[4])
        ## )
        ## BS_safe <- append(BS_safe,BrierScore(data = d_test, parms = estimates_safe, T = time))
        fit_BS <- lava::NR(
                            start     = c(par_LL[1:2],log(par_LL[3:4])),
                            objective = BS_objective,
                            gradient  = BS_gradient,
                            hessian   = BS_hessian,
                            args      = list(data = d_train, tau = time, log_rates = TRUE),
                            control   = list(
                                trace = 0,
                                iter.max = 200,
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
        BS <- append(BS,BrierScore(data = d_test, parms = estimates, T = time))
    }, error = function(e){})
}

#BS_safe <- BS_safe[!is.na(BS_safe)]
N-length(LL)
#N-length(BS_safe)
N-length(BS)
mean(LL)
#mean(BS_safe)
mean(BS)



# Non prop-hazards data, numeric x
non_prop_haz <- function(n){
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

d <- non_prop_haz(5e2)
cc <- prodlim(Hist(times, event) ~ x, data = d)
plot(cc, cause = 1, col = 1:2, mark.time = FALSE)
plot(cc, cause = 2, col = 1:2, mark.time = FALSE)
cc <- prodlim(Surv(times,event)~x,data=d)
plot(cc,col=1:2,mark.time=FALSE)


# Non prop-hazards data, Binary x
.rpexp_two_piece <- function(rate_before, rate_after, timecut) {
    z <- rexp(length(rate_before))
    ifelse(
        z <= rate_before*timecut,
        z/rate_before,
        timecut + (z - rate_before*timecut)/rate_after
    )
}
sim_competing_risks_nonph <- function(n, p_x = 0.5, timecut = 1, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    x <- rbinom(n, size = 1, prob = p_x)
    ## Piecewise cause-specific hazards. The hazard ratios for x change at
    ## timecut, so these data do not satisfy proportional hazards.
    lambda1_before <- ifelse(x == 1, 0.30, 0.10) # HR before timecut: 3
    lambda1_after <- ifelse(x == 1, 0.06, 0.24)  # HR after timecut: 0.25
    lambda2_before <- ifelse(x == 1, 0.08, 0.16) # HR before timecut: 0.5
    lambda2_after <- ifelse(x == 1, 0.30, 0.15)  # HR after timecut: 2
    t1 <- .rpexp_two_piece(lambda1_before, lambda1_after, timecut)
    t2 <- .rpexp_two_piece(lambda2_before, lambda2_after, timecut)
    data.table(
        times = pmin(t1, t2),
        event = ifelse(t1 <= t2, 1L, 2L),
        x = x
    )
}

### Example data that are not proportional hazards
d <- sim_competing_risks_nonph(5e3, seed = 1234)
stopifnot(all(d$event %in% c(1L, 2L)))
table(d$x)
table(d$event)
table(d$x, d$event)
cc <- prodlim(Hist(times, event) ~ x, data = d)
plot(cc, cause = 1, col = 1:2, mark.time = FALSE)
plot(cc, cause = 2, col = 1:2, mark.time = FALSE)
cc <- prodlim(Surv(times,event)~x,data=d)
plot(cc,col=1:2,mark.time=FALSE)


m <- lvm()
### coxWeibull
regression(m) <- y ~ x
distribution(m, ~x) <- normal.lvm()
distribution(m,~death) <- coxExponential.lvm(rate = 0.2)
distribution(m,~y) <- coxExponential.lvm(rate = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
d <- setDT(sim(m,5e3))
setnames(d,"status","event")
d <- d[,.(times,event,x)]

######################################################################
### gpt_newtonraphson_code.R ends here
