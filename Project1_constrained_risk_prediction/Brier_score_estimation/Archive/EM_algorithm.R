### EM_algorithm.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Mar  5 2026 (11:14) 
## Version: 
## Last-Updated: Mar 11 2026 (09:27) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 25
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(data.table)
library(riskRegression)


set.seed(1)
x <- rnorm(10,2,1)
times <- runif(10,0,2)
event <- rbinom(10, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times),
                                   cause = 1)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times),
                                   cause = 2)
one+two



# Define estimating equations
beta1 <- 0.3
beta2 <- 0.5
t <- 1.8
basecumhaz1 <- data.table(hazard = seq(0,1,0.1),time = c(0,times[order(times)]))
basecumhaz2 <- data.table(hazard = seq(0,1,0.1),time = c(0,times[order(times)]))
etimes <- sort(unique(times[times <= t]))

# Estimating equations from the differentiated Brier score
equations <- function(par, df, t, basecumhaz1, basecumhaz2, etimes){
  beta1 <- par[1]
  beta2 <- par[2]
  # quick lookups (assumes basecumhaz1/2 have columns: times, basehaz)
  # and contain time 0 and all etimes.
  H1 <- setNames(basecumhaz1$hazard, basecumhaz1$time)
  H2 <- setNames(basecumhaz2$hazard, basecumhaz2$time)
  f1 <- 0
  f2 <- 0
  for(i in 1:nrow(df)){
    X <- df$x[i]
    o <- as.integer(df$times[i] <= t && df$event[i] == 1)
    int2 <- 0
    int1_1 <- 0
    int1_2 <- 0
    prev_t <- 0
    for(ti in etimes){
      H01 <- H1[as.character(prev_t)]
      H02 <- H2[as.character(prev_t)]
      dH01 <- H1[as.character(ti)] - H1[as.character(prev_t)]
      # pieces used in both
      S1 <- exp(-exp(beta1*X)*H01)
      S2 <- exp(-exp(beta2*X)*H02)
      e1 <- exp(beta1*X)
      e2 <- exp(beta2*X)
      # equation1 int1 / int2
      int1_1 <- int1_1 + X*e1*exp(-e1*H01)*(-H01*e1 + 1)*S2*dH01
      int2    <- int2    + S1*S2*e1*dH01
      # equation2 int1
      int1_2 <- int1_2 + (-X*H02*e2*exp(-e2*H02)*S1*e1*dH01)
      prev_t <- ti
    }
    int2 <- int2 - o
    f1 <- f1 + int1_1 * 2 * int2
    f2 <- f2 + int1_2 * 2 * int2
  }
  c(f1/nrow(df), f2/nrow(df))
}

library(nleqslv)
solve_one <- function(start = c(0, 0), df, t, basecumhaz1, basecumhaz2, etimes,
                      tol = 1e-8, maxit = 200){
  fit <- nleqslv(
    x = start,
    fn = function(par) equations(par, df, t, basecumhaz1, basecumhaz2, etimes),
    control = list(ftol = tol, xtol = tol, maxit = maxit)
  )
  list(
    beta1 = fit$x[1],
    beta2 = fit$x[2],
    residual = fit$fvec,
    max_abs_residual = max(abs(fit$fvec)),
    termcd = fit$termcd,
    message = fit$message
  )
}


starts <- expand.grid(
  beta1 = seq(-3, 3, length.out = 5),
  beta2 = seq(-3, 3, length.out = 5)
)

solutions <- list()
for(i in 1:nrow(starts)){
  fit <- try(
      solve_one(start = as.numeric(starts[i,]), df = df, t = t, basecumhaz1 = basecumhaz1,  basecumhaz2 = basecumhaz2,  etimes = etimes)
    ,
    silent = TRUE
  )
  if(!inherits(fit, "try-error")){
    solutions[[length(solutions)+1]] <- c(fit$beta1,fit$beta2)
  }
}
solutions

# There are multiple solutions to the equation system so need to find the solution that minimizes the brier-score

#Brier Score
BS <- function(df, t, beta1, beta2, basecumhaz1, basecumhaz2, etimes){
    # quick lookups (assumes basecumhaz1/2 have columns: times, basehaz)
    # and contain time 0 and all etimes.
    H1 <- setNames(basecumhaz1$hazard, basecumhaz1$time)
    H2 <- setNames(basecumhaz2$hazard, basecumhaz2$time)
    brier <- 0
    for(i in 1:nrow(df)){
        X <- df$x[i]
        o <- as.integer(df$times[i] <= t && df$event[i] == 1)
        int2 <- 0
        prev_t <- 0
        for(ti in etimes){
            H01 <- H1[as.character(prev_t)]
            H02 <- H2[as.character(prev_t)]
            dH01 <- H1[as.character(ti)] - H1[as.character(prev_t)]
            # pieces used in both
            S1 <- exp(-exp(beta1*X)*H01)
            S2 <- exp(-exp(beta2*X)*H02)
            e1 <- exp(beta1*X)
            int2   <- int2 + S1*S2*e1*dH01
            prev_t <- ti
        }
        brier <- brier + (int2 - o)^2
    }
    return(brier/nrow(df))
}

BS(df = df, t = t, beta1 = beta1, beta2 = beta2, basecumhaz1 = basecumhaz1, basecumhaz2 = basecumhaz2, etimes = etimes)


brier_score <- rep(1,length(solutions))
for (i in 1:length(solutions)){
    brier_score[i] <- BS(df = df,t = t, beta1 = solutions[[i]][1], beta2 = solutions[[i]][2], basecumhaz1 = basecumhaz1, basecumhaz2 = basecumhaz2, etimes = etimes)
}
beta_opt <- as.numeric(solutions[order(brier_score)[1]][[1]])

### Breslow estimator
breslow_cr_dt <- function(dt, cause, beta, xvars = names(beta)) {
    # ensure beta in same order as xvars
    beta <- beta[xvars]
    # compute weights w = exp(X %*% beta)
    # (matrix multiply is fast; we avoid .SD evaluation inside huge loops)
    X <- as.matrix(dt[, ..xvars])
    w <- as.numeric(exp(X %*% beta))
    work <- dt[, .(times, event)]
    work[, w := w]
    # dNk(t): number of events of the chosen cause at each event time t
    ev <- work[event == cause, .(dNk = .N), keyby = .(times)]
    # risk_sum(t) = sum_{i: T_i >= t} w_i
    # Efficient trick: sort by time ascending, compute suffix sums.
    # Let times_sorted be increasing; for each unique time u,
    # sum_{T_i >= u} w_i = total_w - sum_{T_i < u} w_i
    tmp <- work[order(times)]
    tmp[, cum_w := cumsum(w)]
    total_w <- tmp[, sum(w)]
    # precompute sum_w_less_than(u) for each event time u
    # We need the last cum_w among rows with t < u.
    # Use non-equi join: for each ev$t, find max cum_w where tmp$t < ev$t.
    setkey(tmp, times)
    ev2 <- copy(ev)
    ev2[, u := times]
    # join: for each u, take last cum_w where t < u (rolling join on (u - eps))
    # data.table non-equi join:
    #   tmp[.(u), on=.(t < u), mult="last"] gives last row with t < u
    less <- tmp[ev2, on = .(times < u), mult = "last", .(u, cum_w)]
    ev2 <- less[ev2, on = "u"]
    ev2[is.na(cum_w), cum_w := 0]  # if no times < u, then cum_w is 0
    ev2[, risk_sum := total_w - cum_w]
    ev2[, dH0 := dNk / risk_sum]
    ev2[, H0 := cumsum(dH0)]
    out <- ev2[, .(time = u, hazard = H0)]
    tmp[,hazard := 0]
    tmp[event == cause, hazard := out$hazard]
    tmp[, hazard := fifelse(is.na(hazard <- nafill(replace(hazard, hazard == 0, NA), "locf")), 0, hazard)]
    return(rbind(data.table(hazard = 0, time = 0),tmp[,.(hazard,time = times)]))
}
basecumhaz1 <- breslow_cr_dt(dt = df, cause = 1, beta = c(x = beta_opt[1]), xvars = c("x"))
basecumhaz2 <- breslow_cr_dt(dt = df, cause = 2, beta = c(x = beta_opt[2]), xvars = c("x"))


### EM algorithm:

# Define starting parameters
beta_opt <- c(3,3)
t <- 1.8
etimes <- sort(unique(times[times <= t]))
N <- 20
beta_opt_iter <- data.table(beta1 = rep(1,N), beta2 = 1, BS = 1)

for (t in 1:N){
# Estimating equations from the differentiated Brier score
### Breslow estimator
basecumhaz1 <- breslow_cr_dt(dt = df, cause = 1, beta = c(x = beta_opt[1]), xvars = c("x"))
basecumhaz2 <- breslow_cr_dt(dt = df, cause = 2, beta = c(x = beta_opt[2]), xvars = c("x"))

starts <- expand.grid(
  beta1 = seq(-3, 3, length.out = 5),
  beta2 = seq(-3, 3, length.out = 5)
)
solutions <- list()
for(i in 1:nrow(starts)){
  fit <- try(
      solve_one(start = as.numeric(starts[i,]), df = df, t = t, basecumhaz1 = basecumhaz1,  basecumhaz2 = basecumhaz2,  etimes = etimes)
    ,
    silent = TRUE
  )
  if(!inherits(fit, "try-error")){
    solutions[[length(solutions)+1]] <- c(fit$beta1,fit$beta2)
  }
}

#Brier Score
brier_score <- rep(1,length(solutions))
for (i in 1:length(solutions)){
    brier_score[i] <- BS(df = df,t = t, beta1 = solutions[[i]][1], beta2 = solutions[[i]][2], basecumhaz1 = basecumhaz1, basecumhaz2 = basecumhaz2, etimes = etimes)
}
    beta_opt <- as.numeric(solutions[order(brier_score)[1]][[1]])
    beta_opt_iter[t,':='(beta1 = beta_opt[1],beta2 = beta_opt[2], BS = brier_score[order(brier_score)][1])]
}




######################################################################
### EM_algorithm.R ends here
