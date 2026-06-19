### breslow_estimator.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Mar 19 2026 (12:34) 
## Version: 
## Last-Updated: Mar 19 2026 (13:28) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 8
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

#mets::mlogit
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


# Breslow Estimator
basecumhaz <- function(df,model,cause){
    beta <- model$model[[cause]]$coef
    etimes <- times[order(times)]
    setkey(df,times)
    cumhaz <- c()
    for (t in 1:length(etimes)){
        cumhazsum <- 0
        for (i in 1:nrow(df)){           
            if (df[i]$times <= etimes[t] & df[i]$event == cause){
                cumhazsum <- cumhazsum + 1/sum(exp(beta*df[times >= df[i]$times]$x))
            } else{
                cumhazsum <- cumhazsum+0
            }
        }
        cumhaz <- append(cumhaz,cumhazsum)
    }
    return(cumhaz)
}
basecumhaz(df,model,2)
basehaz(model$model[[1]], centered = FALSE)

# function that calculates the cause-specific cumulative incidence functions given a time point and covariate.
# Uses the Breslow estimator already build in the survival package
basecumhazsum <- function(model, x, t){
    haz01 <- setDT(basehaz(model$model[[1]], centered = FALSE))
    haz01 <- haz01[time <= t]
    haz01[,hazard := hazard*exp(model$model[[1]]$coef*x)]
    haz01[,diff := hazard - shift(hazard)]
    haz01sum <- sum(haz01$hazard)
    haz02 <- setDT(basehaz(model$model[[2]], centered = FALSE))
    haz02 <- haz02[time <= t]
    haz02[,hazard := hazard*exp(model$model[[2]]$coef*x)]
    haz02[,diff := hazard - shift(hazard)]
    haz02sum <- sum(haz02$hazard)
    return(max(haz01$diff, na.rm = TRUE)+max(haz02$diff, na.rm = TRUE))
}
basecumhazsum(model,2,max(times)) #0.9879
basecumhazsum(model,2.1,max(times)) #1.0005
basecumhazsum(model,1,max(times)) #1.054
basecumhazsum(model,3,max(times)) #1.0878
basecumhazsum(model,mean(df$x),max(times)) #1.1147


N <- seq(1.1,1.5,0.001)
risk <- c()
sumcumhaz <- c()
for (i in 1:length(N)){
    n <- N[i]
    one <- riskRegression::predictRisk(object = model,
                                  newdata = data.table(x = n),
                                   times = max(times),
                                   cause = 1)
    two <- riskRegression::predictRisk(object = model,
                                   newdata = data.table(x = n),
                                   times = max(times),
                                   cause = 2)
    risk <- append(risk,one+two)
    sumcumhaz <- append(sumcumhaz,basecumhazsum(model,n,max(times)))
}
dt <- data.table(risk = risk, cumhaz = sumcumhaz)
setkey(dt,cumhaz)
cumhaz > 7.775906 #risk bigger than 1
7.779529 


######################################################################
### breslow_estimator.R ends here
