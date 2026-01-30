### over1example.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jan 23 2026 (09:22) 
## Version: 
## Last-Updated: Jan 30 2026 (10:31) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 39
#----------------------------------------------------------------------
## 
### Commentary: 
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


################

# Simpler example with only one covariate
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

########################

# Example where it happens for a binary variable
set.seed(15)
x <- rbinom(20,1,0.4)
times <- runif(20,0,2)
event <- rbinom(20, 1, 0.3+0.5*x)+1
x <- as.factor(x)
df <- data.table(times,event,x)
model <- CSC(data = df,
             formula = Hist(times,event) ~ x,
             fitter = "coxph")
one <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = "0"),
                                   times = 1.7,
                                   cause = 1)
two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = "0"),
                                   times = 1.7,
                                   cause = 2)
one+two

####################################

## How I found the examples
for (i in 1:1000){
set.seed(i)
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
    if (  (one <= 1 & one >= 0 & two >= 0 & two <= 1) & (one+two>1.001 | one+two < 0)){
        print(i)
    }
}



######################################################################
### over1example.R ends here
