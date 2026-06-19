### manual_calculation_of_risks_CSC.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jan 30 2026 (12:31) 
## Version: 
## Last-Updated: Mar 19 2026 (10:35) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 57
#----------------------------------------------------------------------
## 
### Commentary: Here I want to calculate the absolute risk of an event given a CSC model.
##              This is just to learn how to do it by hand. And is done here such that I
##              can check if its correct.
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

set.seed(7)
x <- rnorm(10,2,1)
times <- runif(10,0,2)
event <- rbinom(10, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
# This sums to over 1 while one and two are smaller than 1 but 4.5 > max(df$x)
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = 4.5),
                                   times = 1.5,
                                   cause = 1)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = 4.5),
                                   times = 1.5,
                                   cause = 2)
one+two
##################################################


### Calcualting the cause-specific risks manually
x_new <- data.table(x = 4.5)
t0 <- 1.5

base_cumhaz <- function(t, cause) {
  bh <- basehaz(model$models[[cause]], centered = FALSE)
  bh$hazard[max(which(bh$time <= t))]
}

cumhaz <- function(t, cause, X) {
    if(t < min(times)){
        return(0)
    } else{
        return(as.numeric(base_cumhaz(t, cause) * exp(X * coef(model)[[cause]])))
    }
}

### S calculated using exp() aproximation
## S <- function(t, X){
##     if(t < min(times)){
##         return(1)
##     } else{
##         return(exp(-cumhaz(t, 1, X)-cumhaz(t, 2, X)))
##     }
## }

### S calculated using product integral
S <- function(t, X){
    # distinct ordered event times up to t
    etimes <- sort(unique(times[times <= t]))
    if(length(etimes)==0) return(1)
    Sval <- 1
    prev_t <- 0
    for(ti in etimes){
        # hazard jumps for each cause
        dcumhaz1 <- cumhaz(ti,1,X) - cumhaz(prev_t,1,X)
        dcumhaz2 <- cumhaz(ti,2,X) - cumhaz(prev_t,2,X)
        dcumhaz_total <- dcumhaz1 + dcumhaz2
        Sval <- Sval * (1 - dcumhaz_total)
        prev_t <- ti
    }
    return(Sval)
}


F <- function(t, cause, X){
    F_times <- c(0,times[order(times)])
    F_times <- F_times[F_times <= t]
    F_int <- 0
    for (i in 2:length(F_times)){
        F_int <- F_int + S(t = F_times[i-1],X)*(cumhaz(t = F_times[i], cause, X)-cumhaz(t = F_times[i-1], cause, X))
    }
    return(F_int)
}

one <- riskRegression::predictRisk(object = model,
                                   newdata = x_new,
                                   times = t0,
                                   cause = 1,
                                   productLimit = FALSE)
two <- riskRegression::predictRisk(object = model,
                                   newdata = x_new,
                                   times = t0,
                                   cause = 2,
                                   productLimit = FALSE)
F(t0,1, x_new$x)
one
F(t0,2, x_new$x)
two

# Here the cause-specific risks sum to more than one, but adding the survival probability makes the sum=1
# this means the survival probability is negative
F(1.5,1, 4.5)+F(1.5,2, 4.5)
F(1.5,1, 4.5)+F(1.5,2, 4.5)+S(1.5,4.5)


F(1.5,2,4.5)

### Parametric cox weibull
df[,event2 := event-1]
df[event == 1, event2 := 2]

library(flexsurv)
m1 <- flexsurvreg(Surv(times,event) ~ x, dist = "exp",data = df)
# Predicts the survival probablity i.e. the probability of not experinceing event 1
one <- predict(m1,
               newdata = data.table(x = 4.5),
               times = 1.5,
               cause = 1,
               type = "survival")
one <- 1-one[[2]]

m2 <- flexsurvreg(Surv(times,event2) ~ x, dist = "exp",data = df)
# Predicts the probability of not experiencing event 2
two <- predict(m2,
               newdata = data.table(x = 4.5),
               times = 1.5,
               cause = 1,
               type = "survival")
two <- 1-two[[2]]


predict(m1,
        newdata = data.table(x = 4.5),
        times = times[order(times)][3],
        cause = 1,
        type = "cumhaz")
cumhaz(times[order(times)][6],cause = 2,4.5)

######################################################################
### manual_calculation_of_risks_CSC.R ends here






