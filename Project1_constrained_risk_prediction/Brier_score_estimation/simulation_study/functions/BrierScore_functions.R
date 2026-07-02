F <- function(t, X, beta1, beta2, lambda1, lambda2){
    haz1 <- exp(beta1*X)*lambda1
    haz2 <- exp(beta2*X)*lambda2
    numerator <- 1-exp(t*(-haz1-haz2))
    denominator <- haz1+haz2
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    return(c(F1,F2))
}
BrierScore <- function(parms, data, T = 1, BS = "both"){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
    BS1 <- 0
    BS2 <- 0
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
    if (BS == "both"){
        return((BS1+BS2)/nrow(df))
    }
    if (BS == "BS1"){
        return((BS1)/nrow(df))
    }
    if (BS == "BS2"){
        return((BS2)/nrow(df))
    }  
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
BrierScore_CSC <- function(model, data, tau, BS = "both"){
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
    if (BS == "both"){
        return((BS1+BS2)/nrow(df))
    }
    if (BS == "BS1"){
        return((BS1)/nrow(df))
    }
    if (BS == "BS2"){
        return((BS2)/nrow(df))
    }  
}

### Weibull
F_weibull <- function(t, X, beta1, beta2, lambda1, lambda2, v1, v2){
    haz1 <- exp(beta1*X)*lambda1*v1*t^(v1-1)
    haz2 <- exp(beta2*X)*lambda2*v2*t^(v2-1)
    
    numerator <- 1-exp(t*(-haz1-haz2))
    denominator <- haz1+haz2
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    return(c(F1,F2))
}
BrierScore_weibull <- function(parms, data, T = 1, BS = "both"){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
    BS1 <- 0
    BS2 <- 0
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
    if (BS == "both"){
        return((BS1+BS2)/nrow(df))
    }
    if (BS == "BS1"){
        return((BS1)/nrow(df))
    }
    if (BS == "BS2"){
        return((BS2)/nrow(df))
    }  
}




### Only estimate betas
BrierScore_beta <- function(parms, lambdas, data, T = 1, BS = "both"){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- lambdas[1]
    lambda2 <- lambdas[2]
    BS1 <- 0
    BS2 <- 0
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
    if (BS == "both"){
        return((BS1+BS2)/nrow(df))
    }
    if (BS == "BS1"){
        return((BS1)/nrow(df))
    }
    if (BS == "BS2"){
        return((BS2)/nrow(df))
    }  
}


### With censoring
F_cens <- function(t, X, beta1, beta2, beta3, lambda1, lambda2, lambda3){
    haz1 <- exp(beta1*X)*lambda1
    haz2 <- exp(beta2*X)*lambda2
    haz3 <- exp(beta3*X)*lambda3
    numerator <- 1-exp(t*(-haz1-haz2-haz3))
    denominator <- haz1+haz2+haz3
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    F3 <- haz3*(numerator/denominator)
    return(c(F1,F2,F3))
}
BrierScore_cens <- function(parms, data, T = 1, BS = "both"){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    beta3 <- parms[3]
    lambda1 <- parms[4]
    lambda2 <- parms[5]
    lambda3 <- parms[6]
    BS1 <- 0
    BS2 <- 0
    BS3 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
                o3 <- 0
            }
            if (df$event[i] == 2){
                o1 <- 0
                o2 <- 1
                o3 <- 0
            }
            if (df$event[i] == 0){
                o1 <- 0
                o2 <- 0
                o3 <- 1
            }                
        } else{
            o1 <- 0
            o2 <- 0
            o3 <- 0
        }
        BS1 <- BS1 + (F_cens(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, beta3 = beta3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)[1] - o1)^2
        BS2 <- BS2 + (F_cens(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, beta3 = beta3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)[2] - o2)^2
        BS3 <- BS3 + (F_cens(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, beta3 = beta3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)[3] - o3)^2
    }
    if (BS == "both"){
        return((BS1+BS2+BS3)/nrow(df))
    }
    if (BS == "BS1"){
        return((BS1)/nrow(df))
    }
    if (BS == "BS2"){
        return((BS2)/nrow(df))
    }
    if (BS == "BS3"){
        return((BS3)/nrow(df))
    }   
}

BrierScore_cens_beta <- function(parms, data, lambdas, T = 1, BS = "both"){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    beta3 <- parms[3]
    lambda1 <- lambdas[1]
    lambda2 <- lambdas[2]
    lambda3 <- lambdas[3]
    BS1 <- 0
    BS2 <- 0
    BS3 <- 0
    for (i in 1:nrow(df)){
        if (T >= df$times[i]){
            if (df$event[i] == 1){
                o1 <- 1
                o2 <- 0
                o3 <- 0
            }
            if (df$event[i] == 2){
                o1 <- 0
                o2 <- 1
                o3 <- 0
            }
            if (df$event[i] == 0){
                o1 <- 0
                o2 <- 0
                o3 <- 1
            }                
        } else{
            o1 <- 0
            o2 <- 0
            o3 <- 0
        }
        BS1 <- BS1 + (F_cens(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, beta3 = beta3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)[1] - o1)^2
        BS2 <- BS2 + (F_cens(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, beta3 = beta3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)[2] - o2)^2
        BS3 <- BS3 + (F_cens(t = T,X = df$X[i], beta1 = beta1, beta2 = beta2, beta3 = beta3, lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)[3] - o3)^2
    }
    if (BS == "both"){
        return((BS1+BS2+BS3)/nrow(df))
    }
    if (BS == "BS1"){
        return((BS1)/nrow(df))
    }
    if (BS == "BS2"){
        return((BS2)/nrow(df))
    }
    if (BS == "BS3"){
        return((BS3)/nrow(df))
    }   
}

### Brier-Score for multiple variables
F_multi <- function(t, X, beta1, beta2, lambda1, lambda2) {
    # Ensure X is a matrix
    X <- as.matrix(X)
    # Linear predictors
    lp1 <- as.vector(X %*% beta1)
    lp2 <- as.vector(X %*% beta2)
    haz1 <- exp(lp1) * lambda1
    haz2 <- exp(lp2) * lambda2  
    numerator <- 1 - exp(-t * (haz1 + haz2))
    denominator <- haz1 + haz2  
    F1 <- haz1 * (numerator / denominator)
    F2 <- haz2 * (numerator / denominator) 
    cbind(F1 = F1, F2 = F2)
}
BrierScore_multi <- function(parms, data, covariates, T, BS = "both") {
    df <- data
    p <- length(covariates)
    beta1 <- parms[1:p]
    beta2 <- parms[(p + 1):(2 * p)]
    # Reparamitize lambda
    lambda1 <- exp(parms[2 * p + 1])
    lambda2 <- exp(parms[2 * p + 2])
    X <- as.matrix(df[, ..covariates])
    pred <- F_multi(
        t = T,
        X = X,
        beta1 = beta1,
        beta2 = beta2,
        lambda1 = lambda1,
        lambda2 = lambda2
    )
    o1 <- ifelse(T >= df$times & df$event == 1, 1, 0)
    o2 <- ifelse(T >= df$times & df$event != 1, 1, 0)
    BS1 <- mean((pred[, "F1"] - o1)^2)
    BS2 <- mean((pred[, "F2"] - o2)^2)
    if (BS == "both") {
        return(BS1 + BS2)
    }
    if (BS == "BS1") {
        return(BS1)
    }
    if (BS == "BS2") {
        return(BS2)
    }  
    stop("BS must be one of: 'both', 'BS1', or 'BS2'")
}

BSS <- function(data, formula,tau){
    covariates <- all.vars(formula[[3]])
    # fit csc
    csc <- CSC(data = data, formula)
    bh1 <- survival::basehaz(csc$models[[1]], centered = FALSE)
    bh2 <- survival::basehaz(csc$models[[2]], centered = FALSE)
    start <- c(
        as.numeric(coef(csc)[[1]]),
        as.numeric(coef(csc)[[2]]),
        log(bh1$hazard[max(which(bh1$time <= tau))]/tau),
        log(bh2$hazard[max(which(bh2$time <= tau))]/tau)
    )
    BS_fit <- optim(par = start, fn = BrierScore_multi, data = data, T = tau, covariates = covariates,method = "BFGS", control = list(maxit = 1000))
    BS <- BrierScore_multi(parms = BS_fit$par,data = data,covariates = covariates,T = tau,BS = "both")
    p <- length(covariates)
    beta1 <- BS_fit$par[1:p]
    beta2 <- BS_fit$par[(p + 1):(2 * p)]
    lambda1 <- exp(BS_fit$par[2 * p + 1])
    lambda2 <- exp(BS_fit$par[2 * p + 2])
    x <- list(
        "true_pars" = c(BS_fit$par[1:(2*p)],exp(BS_fit$par[(2 * p + 1):(2 * p + 2)])),
        "pars" = BS_fit$par,
        "Cause1 pars" = c("beta" = beta1,"lambda" = lambda1),
        "Cause2 pars" = c("beta" = beta2,"lambda" = lambda2),
        "Brier Score" = BS)
    names(x$'Cause1 pars') <- c(covariates,"lambda")
    names(x$'Cause2 pars') <- c(covariates,"lambda")
    return(x)
}


######################################################################
### BrierScore_functions.R ends here
