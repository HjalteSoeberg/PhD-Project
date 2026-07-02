### minimizing_score.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Feb 10 2026 (09:31) 
## Version: 
## Last-Updated: Apr 17 2026 (08:33) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 121
#----------------------------------------------------------------------
## 
### Commentary: Here I will try to use the brier score as the loss function
##              when estimating the parameters for a cox regression model
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
set.seed(7)
x <- rnorm(100,2,1)
times <- runif(100,0,2)
event <- rbinom(100, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)

### Using score to calculate the brier score and then using an optimizer to minimize it
#define the parameters
# Function that can change the parametres in a fitted model
set_psm_params <- function(fit, beta, coef_names = NULL) {
  # beta: c(scale, intercept, slope) for your example
  stopifnot(length(beta) >= 3)
  fit2 <- fit
  # figure out coefficient names (Intercept + covariates)
  if (is.null(coef_names)) {
    coef_names <- names(coef(fit2))
    if (length(coef_names) != 2) {
      stop("This helper assumes 1 covariate + intercept (2 coefficients). ",
           "Provide coef_names manually if your model differs.")
    }
  }
  new_coef <- c(beta[2], beta[3])
  names(new_coef) <- coef_names
  # Update rms-level coefficients
  fit2$coefficients <- new_coef
  # Update underlying fit (survreg-like) coefficients
  if (!is.null(fit2$fit) && !is.null(fit2$fit$coefficients)) {
    fit2$fit$coefficients <- new_coef
  }
  # Update scale in both places
  fit2$scale <- beta[1]
  if (!is.null(fit2$fit) && !is.null(fit2$fit$scale)) {
    fit2$fit$scale <- beta[1]
  }
  fit2
}

# define a function that takes in data and the parameters and outputs a Brier score
m <- psm(Surv(times,y) ~ x, dist = "weibull", data = df)
init <- c(m$scale,m$coefficients[[1]],m$coefficients[[2]])
test <- function(beta){
    m2 <- set_psm_params(m,beta)
    x <- Score(list(m2),formula = Surv(times,y)~1,data = df,times = 1, confint = FALSE, split.method = "none",contrasts = FALSE, metrics = "brier")
    brier <- x$Brier[1]$score$Brier[2]
    return(brier)
}

## minimize
library(DEoptim)
opt_par <- DEoptim(fn = test, df = df, lower = c(0.000001,-20,-20), upper = c(30,70,30), control = list(itermax = 40)) #0.168015
opt_par$optim$bestmem
## See perfomance of model with new parameters
m_opt <- set_psm_params(m, beta = c(as.numeric(opt_par$optim$bestmem)))
x <- Score(list("opt" = m_opt, "m" = m),formula = Surv(times,y)~1,data = df,times = 1, confint = FALSE, split.method = "none",contrasts = FALSE, metrics = "brier")
summary(x)

## minimize with Newton-Raphson
est <- lava::NR(init, test, control = list(iter.max = 40, stepsize = 0.5))


### Parametric Cox model with exopnential baseline hazard
F <- function(t, X, beta1, beta2, lambda1, lambda2){
    haz1 <- exp(beta1*X)*lambda1
    haz2 <- exp(beta2*X)*lambda2
    numerator <- 1-exp(t*(-haz1-haz2))
    denominator <- haz1+haz2
    #return cumulative incidence functions for both cause 1 and 2
    F1 <- haz1*(numerator/denominator)
    F2 <- haz2*(numerator/denominator)
    return(c(F1,F2))
}

LogLikelihood1 <- function(parms, data){
    df <- data
    cause = 1
    beta <- parms[1]
    lambda <- parms[2]
    L <- 0
    for (i in 1:nrow(df)){
        if (df$event[i] == cause){
            L <- L + log(lambda*exp(df$x[i]*beta))
        }
        L <- L + -df$times[i]*exp(beta*df$x[i])*lambda
    }
    return(-L)
}

LogLikelihood2 <- function(parms, data){
    df <- data
    cause = 2
    beta <- parms[1]
    lambda <- parms[2]
    L <- 0
    for (i in 1:nrow(df)){
        if (df$event[i] == cause){
            L <- L + log(lambda*exp(df$x[i]*beta))
        }
        L <- L + -df$times[i]*exp(beta*df$x[i])*lambda
    }
    return(-L)
}

BrierScore <- function(parms, data, T = 1){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- parms[3]
    lambda2 <- parms[4]
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
    


BrierScore(data = df, parms = c(0.2, 0.5, 0.8, 0.5))
LogLikelihood2(data = df, parms = c(0.2, 0.5))


df_train <- df[1:80]
df_test <- df[81:100]
library(DEoptim)
set.seed(1)
#likelihood
opt_par_likelihood1 <- DEoptim(fn = LogLikelihood1, data = df_train, lower = c(-20,0.000001), upper = c(100,10000), control = list(itermax = 100)) 
l1_par <- as.numeric(opt_par_likelihood1$optim$bestmem)
opt_par_likelihood2 <- DEoptim(fn = LogLikelihood2, data = df_train, lower = c(-20,0.000001), upper = c(100,10000), control = list(itermax = 100)) 
l2_par <- as.numeric(opt_par_likelihood2$optim$bestmem)

parms_L <- c(l1_par[1],l2_par[1],l1_par[2],l1_par[2])
#brier score
opt_par_BS <- DEoptim(fn = BrierScore, data = df_train, lower = c(-50,-50,0.000001,0.000001), upper = c(50,50,50,50), control = list(itermax = 100))
parms_BS <- as.numeric(opt_par_BS$optim$bestmem)

BrierScore(data = df_test,parms = parms_BS)
BrierScore(data = df_test,parms = parms_L)

## minimize with Newton-Raphson
# Need to pick multiple starting values to find the global minimum
starts <- expand.grid(
  beta1 = c(-20,-0.29),
  beta2 = c(-20),
  lambda1 =  c(0.5,5),
  lambda2 =  c(0.1,40)
)
df_NS <- data.table(iter = 1:nrow(starts), BS = 1)
for (l in 1:nrow(starts)){
    tryCatch({
    init <- c(starts$beta1[l],starts$beta2[l],starts$lambda1[l],starts$lambda2[l])
    est <- lava::NR(init, args = list(data =  df_train), BrierScore, control = list(iter.max = 80))
    parms_NR <- est$par
    score <- BrierScore(data = df_train, parms = parms_NR)
    df_NS[iter == l, BS := score]
    }, error = function(e){})
}
setkey(df_NS,BS)

init <- c(starts$beta1[df_NS$iter[1]],starts$beta2[df_NS$iter[1]],starts$lambda1[df_NS$iter[1]],starts$lambda2[df_NS$iter[1]])
    
est <- lava::NR(init, args = list(data =  df_train), BrierScore, control = list(tol = 1e-10, stepsize = 0.5))
parms_NR <- est$par
BrierScore(data = df_train,parms = parms_NR)

lambdas <- seq(0,50,by = 0.5)
df_lambda2 <- data.table(lambda2 = lambdas, Brier = 1)
for (l in 1:length(lambdas)){
    score <- BrierScore(data = df_train, parms = c(-0.3, -19.9771502,lambdas[l],50.6040874))
    df_lambda2[l, Brier := score]
}

ggplot(data = df_lambda2, aes(x = lambda2, y = Brier))+
    geom_line()


#########
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
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g1 <- ((X*haz1)/(haz12^2))*(haz2*(1-exp(-T*haz12))+T*haz1*haz12*exp(-T*haz12))
        d_g2 <- ((X*haz1*haz2)/(haz12^2))*((T+1)*exp(-T*(haz12))*haz12-1)             
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
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g2 <- ((X*haz2)/(haz12^2))*(haz1*(1-exp(-T*haz12))+T*haz2*haz12*exp(-T*haz12))
        d_g1 <- ((X*haz2*haz1)/(haz12^2))*((T+1)*exp(-T*(haz12))*haz12-1)             
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
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g1 <- (exp(beta1*X)/(haz12^2))*(exp(-T*haz12)*haz1^2*T+haz2*(exp(-T*haz12)*haz1*T-exp(-T*haz12)+1))
        #d_g1 <- exp(beta1*X)*((1-exp(-T*haz12))/haz12)+haz1*((haz12*(-T*exp(beta1*X)*exp(-T*haz12))-(1-exp(-T*haz12))*exp(beta1*X))/(haz12^2))
        d_g2 <- ((haz2*exp(beta1*X))/(haz12^2))*((T+1)*exp(-T*(haz12))*haz12-1)       
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
        X = df$x[i]
        haz1 <- lambda1*exp(X*beta1)
        haz2 <- lambda2*exp(X*beta2)
        haz12 <- haz1+haz2
        d_g2 <- (exp(beta2*X)/(haz12^2))*(exp(-T*haz12)*haz2^2*T+haz1*(exp(-T*haz12)*haz2*T-exp(-T*haz12)+1))
        #d_g2 <- exp(beta2*X)*((1-exp(-T*haz12))/haz12)+haz2*((haz12*(-T*exp(beta2*X)*exp(-T*haz12))-(1-exp(-T*haz12))*exp(beta2*X))/(haz12^2))
        d_g1 <- ((haz1*exp(beta2*X))/(haz12^2))*((T+1)*exp(-T*(haz12))*haz12-1)       
        BSS1 <- BSS1 + 2*d_g1*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[1] - o1)
        BSS2 <- BSS2 + 2*d_g2*(g_j(t = T,X = X, beta1 = beta1, beta2 = beta2, lambda1 = lambda1,lambda2 = lambda2)[2] - o2)
    }
    return((BSS1+BSS2))
}
est_func_BSS <- function(parms, data){
    c(
        BrierScoreSum_beta1(parms,data),
        BrierScoreSum_beta2(parms,data),
        BrierScoreSum_lambda1(parms,data),
        BrierScoreSum_lambda2(parms,data)
    )
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

est_func_LL <- function(parms, data, cause){
    c(
        LogLikelihood_beta(parms,data,cause),
        LogLikelihood_lambda(parms,data,cause)
    )
}
obj_LL <- function(parms, data, cause) {
  u <- est_func_LL(parms, data)
  sum(u^2)
}



### Simulate survival data with one numeric covariate
library(lava)
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

m <- lvm()
regression(m) <- y ~ x
distribution(m, ~x) <- normal.lvm()
distribution(m,~s+z) <- binomial.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~x)
eventTime(m) <- times ~ min(y = 1,death = 2)
d <- lava::sim(m,200)
d <- setDT(d)
setnames(d,"status","event")
d <- d[,.(times,event,x)]


# Maximum likelihood estimator
fit_LL_1 <- lava::NR(
  start = c(1, 1),
  gradient = est_func_LL,
  objective = obj_LL,
  args = list(data = d, cause = 1),
  control = list(tol = 1e-15, stepsize = 0.5)
)
fit_LL_2 <- lava::NR(
  start = c(1, 1),
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
  start = par_LL,#c(1,1,1,1),
  gradient = est_func_BSS,
  objective = obj_BSS,
  args = list(data = d),
  control = list(tol = 1e-15, stepsize = 0.5)
)
par <- fit$par
BrierScore(data = d, parms = par)

# Maximizing the Brier-score-sum directly 
est <- lava::NR(par_LL, args = list(data =  d), BrierScore, control = list(tol = 1e-10, stepsize = 0.5))
parms_NR <- est$par
BrierScore(data = d,parms = parms_NR)


# Set up simulation study comparing the 3 methods using train and test set, and use the Brier-Score-sum as measure
