### over1example.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jan 23 2026 (09:22) 
## Version: 
## Last-Updated: Mar 19 2026 (12:35) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 94
#----------------------------------------------------------------------
## 
### Commentary: Also find an example where each cause uses a different covariate
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(riskRegression)
library(survival)
library(data.table)
library(lava)
library(prodlim)
library(mets)

# Example where probability of cause 1 and 2 are less than 1 respectively but summed are 1.002
# This is even though I only used covariate values that are in the parameter space
set.seed(433)
m <- lvm()
regression(m) <- y ~ z+v
regression(m) <- s ~ exp(0.6*x-0.5*z)
distribution(m,~x+z) <- binomial.lvm()
distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~s)
eventTime(m) <- time ~ min(y = 1,death = 2)
d <- lava::sim(m,200)
d <- setDT(d)
#censor 10 random people
d[1:10,status := 0]
d[1:10,time := time-(time/5)]
model <- CSC(data = d,
             formula = Hist(time,status) ~ z + v + s + x,
             fitter = "coxph")
one <- riskRegression::predictRisk(object = model,
            newdata = data.table(z = 0,v = (max(d$v)),s = (max(d$s)),x = 1),
            times = 1.5,
            cause = 1)
two <- riskRegression::predictRisk(object = model,
            newdata = data.table(z = 0,v = (max(d$v)),s = (max(d$s)),x = 1),
            times = 1.5,
            cause = 2)
one+two
#survival prob
riskRegression::predictRisk(model,
            newdata = data.table(z = 0,v = (max(d$v)),s = (max(d$s)),x = 1),
        times = 1.5,
        type = "survival")


#Using exp approximation instead of product integral
one <- riskRegression::predictRisk(object = model,
            newdata = data.table(z = 0,v = (max(d$v)),s = (max(d$s)),x = 1),
            times = 1.5,
            cause = 1,
            product.limit = FALSE)
two <- riskRegression::predictRisk(object = model,
            newdata = data.table(z = 0,v = (max(d$v)),s = (max(d$s)),x = 1),
            times = 1.5,
            cause = 2,
            product.limit = FALSE)
one+two
#survival prob
riskRegression::predictRisk(model,
            newdata = data.table(z = 0,v = (max(d$v)),s = (max(d$s)),x = 1),
        times = 1.5,
        type = "survival",
        product.limit = FALSE)

################

# Simpler example with only one covariate where we predict for mean(x)
set.seed(1)
x <- rnorm(10,2,1)
times <- runif(10,0,2)
event <- rbinom(10, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
# This sums to over 1 while one and two are smaller than 1
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times),
                                   cause = 1,
                                   product.limit = FALSE)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times),
                                   cause = 2,
                                   product.limit = FALSE)
one+two


#mets::mlogit
set.seed(4)
x <- rnorm(10,2,1)
times <- runif(10,0,2)
event <- rbinom(10, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)   
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
# This sums to over 1 while one and two are smaller than 1 but 4.5 > max(df$x)
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times)-0.1,
                                   cause = 1)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = mean(df$x)),
                                   times = max(times)-0.1,
                                   cause = 2)
one+two

# Example where it happens for a binary variable, with censoring.
set.seed(146)
x <- rbinom(20,1,0.4)
times <- runif(20,0,2)
event <- rbinom(20, 1, 0.3+0.5*x)+1
x <- as.factor(x)
df <- data.table(times,event,x)
df[1:2,':='(event = 0, times = 2)]
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = "1"),
                                   times = 1.7,
                                   cause = 1)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = "1"),
                                   times = 1.7,
                                   cause = 2)
one+two
df[,max(times),by = "x"]


####################################

### Aalen johansen
setkey(df,times)
n <- nrow(df)
times <- sort(unique(df$times[df$event != 0]))

S <- 1               # overall survival
cif1 <- 0
cif2 <- 0

out <- data.frame(
  time = times,
  S = NA,
  CIF1 = NA,
  CIF2 = NA
)
for (i in seq_along(times)) { 
  t <- times[i]
  # Risk set
  Y <- sum(df$time >= t)
  # Events at time t
  d1 <- sum(df$time == t & df$event == 1)
  d2 <- sum(df$time == t & df$event == 2)
  # Cause-specific hazard increments
  h1 <- d1 / Y
  h2 <- d2 / Y  
  # Update CIFs (uses S before updating)
  cif1 <- cif1 + S * h1
  cif2 <- cif2 + S * h2  
  # Update survival
  S <- S * (1 - h1 - h2)  
  # Store
  out$S[i] <- S
  out$CIF1[i] <- cif1
  out$CIF2[i] <- cif2
}




## How I found the examples
for (i in 1:1000){
    set.seed(355)
    x <- rbinom(20,1,0.4)
    times <- runif(20,0,2)
    event <- rbinom(20, 1, 0.3+0.5*x)+1
    x <- as.factor(x)
    df <- data.table(times,event,x)
    df[times > 1.2,':='(event = 0, times = 1.2)]
    model <- CSC(data = df,
                 formula = Hist(times,event) ~ x,
                 fitter = "coxph")
    one <- riskRegression::predictRisk(object = model,
                                       newdata = data.table(x = "0"),
                                       times = 1.2,
                                       cause = 1)
    two <- riskRegression::predictRisk(object = model,
                                       newdata = data.table(x = "0"),
                                       times = 1.2,
                                       cause = 2)
    if ((!is.na(one) & !is.na(two)) &  (one <= 1 & one >= 0 & two >= 0 & two <= 1) & (one+two>1.001 | one+two < 0)){
        print(i)
    }
    one <- riskRegression::predictRisk(object = model,
                                       newdata = data.table(x = "1"),
                                       times = 1.2,
                                       cause = 1)
    two <- riskRegression::predictRisk(object = model,
                                       newdata = data.table(x = "1"),
                                       times = 1.2,
                                       cause = 2)
    if ((!is.na(one) & !is.na(two)) & (one <= 1 & one >= 0 & two >= 0 & two <= 1) & (one+two>1.001 | one+two < 0)){
        print(i)
    }
}


######################################################################
### over1example.R ends here


### Try to find a parametric example of where it happens (can't for exponential or weibull)

library(data.table)
library(flexsurv)

for (i in 1000:2000){
    tryCatch({
    set.seed(i)
    x <- rnorm(10, 2, 1)
    times <- runif(10, 0, 2)
    event <- rbinom(10, 1, pmax(0, x / max(x))) + 1
    df <- data.table(times, event, x)
    # Fit one parametric cause-specific PH model per cause
    fit1 <- flexsurvreg(Surv(times, event == 1) ~ x,
                        data = df,
                        dist = "exp")
    fit2 <- flexsurvreg(Surv(times, event == 2) ~ x,
                        data = df,
                        dist = "exp")
    newx <- data.frame(x = mean(df$x))
    t0   <- max(df$times)
    # Cause-specific hazard and cumulative hazard functions
    h1 <- function(u) {
        summary(fit1, newdata = newx, t = u, type = "hazard")[[1]][,"est"]
    }
    h2 <- function(u) {
        summary(fit2, newdata = newx, t = u, type = "hazard")[[1]][,"est"]
    }
    H1 <- function(u) {
        summary(fit1, newdata = newx, t = u, type = "cumhaz")[[1]][,"est"]
    }
    H2 <- function(u) {
        summary(fit2, newdata = newx, t = u, type = "cumhaz")[[1]][,"est"]
    }
    Sall <- function(u) exp(-(H1(u) + H2(u)))
    # CIFs
    F1 <- integrate(function(u) Sall(u) * h1(u), lower = 0, upper = t0)$value
    F2 <- integrate(function(u) Sall(u) * h2(u), lower = 0, upper = t0)$value
    if (F1+F2>1){
        print(i)
    }
    }, error = function(e){})
}


# Check if proportional hazards assumption holds
cc <- prodlim(Surv(time,status)~x+z,data=d)
plot(cc,col = 1:2,mark.time=FALSE)
