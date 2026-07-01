### Packages ---
library(lava)
library(data.table)
devtools::install_github("tagteam/riskRegression")
library(riskRegression)
library(survival)
library(randomForestSRC)
library(rms)
source("simulateStenoT1.R")
set.seed(111)
play <- simulateStenoT1(n=4000)
play[,age_cat := factor(play$age < 40,levels = c(TRUE,FALSE),labels = c("young", "old"))]

# keep some unpenalized and use ridge for one and lasso for the other
fit <- CSC(list(Hist(time, event) ~ unpenalized(age) + unpenalized(sex) + diabetes_duration+value_SBP+value_LDL+value_HBA1C+value_Albuminuria+log(eGFR)*age_cat+
                    value_Smoking+value_Motion,
                Hist(time, event) ~ age + sex + diabetes_duration),
           data = play,
           fitter = c("glmnet","glmnet"),
           fitter_arguments = list(list(alpha = 0),list(alpha = 1)))
fit$models$"Cause 1"$penalty.factor.used
fit$models$"Cause 2"$penalty.factor.used
# can also give more penalty to some variables
fit2 <- CSC(list(Hist(time, event) ~ unpenalized(age) + unpenalized(sex) + diabetes_duration+value_SBP+value_LDL+value_HBA1C+value_Albuminuria+log(eGFR)*age_cat+
                     value_Smoking+value_Motion,
                 Hist(time, event) ~ age + sex + diabetes_duration),
            data = play,
            fitter = c("glmnet","glmnet"),
            fitter_arguments = list(list(alpha = 0),list(alpha = 1,penalty.factor = c("age" = 0,"sex1" = 1,"diabetes_duration" = 3))))
fit2$models$"Cause 2"$penalty.factor.used


# use rcs (needs testing)
fit3 <- CSC(list(Hist(time, event) ~ unpenalized(age) + unpenalized(sex) + diabetes_duration+rcs(value_SBP,3)+value_LDL+value_HBA1C+value_Albuminuria+log(eGFR)*age_cat+
                    value_Smoking+value_Motion,
                Hist(time, event) ~ age + sex + diabetes_duration),
           data = play,
           fitter = c("glmnet","glmnet"),
           fitter_arguments = list(list(alpha = 0.5),list(alpha = 1)))
fit3$models$"Cause 1"$penalty.factor.used

# Can only penalize the overall spline and not the individual terms
fit4 <- CSC(list(Hist(time, event) ~ unpenalized(age) + unpenalized(sex) + diabetes_duration+rcs(value_SBP,3)+value_LDL+value_HBA1C+value_Albuminuria+log(eGFR)*age_cat+
                    value_Smoking+value_Motion,
                Hist(time, event) ~ age + sex + diabetes_duration),
           data = play,
           fitter = c("glmnet","glmnet"),
           fitter_arguments = list(list(alpha = 0.5,penalty.factor = c("value_SBP" = 3)),list(alpha = 1)))
fit4$models$"Cause 1"$penalty.factor.used
