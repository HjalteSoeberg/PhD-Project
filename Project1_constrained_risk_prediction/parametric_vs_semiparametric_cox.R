### parametric_vs_semiparametric_cox.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Feb 10 2026 (09:45) 
## Version: 
## Last-Updated: Mar  4 2026 (09:26) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 47
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
# data

set.seed(433)
m <- lvm()
regression(m) <- y ~ x+v
regression(m) <- s ~ exp(0.6*x)
distribution(m,~x) <- binomial.lvm()
#distribution(m,~death) <- coxWeibull.lvm(scale = 1)
distribution(m,~y) <- coxWeibull.lvm(scale = 0.1, shape = ~s)
distribution(m,~cens) <- coxWeibull.lvm(scale = 0.05)
eventTime(m) <- time ~ min(y = 1,cens = 0)


d <- setDT(lava::sim(m,200))
nd <- setDT(lava::sim(m,1000))
m_semiparametric <- cph(data = d,formula = Surv(time,status) ~ x+v,x = TRUE,y = TRUE)
m_parametric <- psm(Surv(time,status) ~ x+v, dist = "weibull", data = d)
x <- Score(list(m_semiparametric,m_parametric),formula = Surv(time,status)~1,data = nd,summary = "risk",times = 4)
plotRisk(x,times = 4)



#########
sigma <- m_parametric$scale
k     <- 1 / sigma
H0 <- function(t) {
  t^k
}
H <- function(t, newdata) {
  eta <- predict(m_parametric, newdata = newdata, type = "lp")
  t^k * exp(-k * eta)
}
H0_sp <- function(t){
    setDT(basehaz(m_semiparametric, centered = FALSE))[time-t <= 0][abs(time-t) == min(abs(time-t))]$hazard[[1]]
}
H_fun <- function(t, newdata) {
    H0_sp(t)*exp(coef(m_semiparametric)["x"]*newdata$x+coef(m_semiparametric)["v"]*newdata$v)
}

nd <- data.frame(x = 1, v = 1)
H_fun(4, nd)
H(4,nd)


####################3
# Parametric cox regression, with baseline hazard as exponentil distribution (i.e. cosntant baseline)
set.seed(2028)
x <- rbinom(20,1,0.4)
times <- runif(20,0,2)
event <- rbinom(20, 1, 0.3+0.5*x)+1
x <- as.factor(x)
df <- data.table(times,event,x)
#df[1:2,':='(event = 0, times = 2)]
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

beta <- function(df, cause){
    times_x0 <- sum(df[x == "0",.(times)]$times)
    times_x1 <- sum(df$times)-times_x0
    N_event <- nrow(df[event == cause])
    sum_x <- nrow(df[event == cause & x == "1"])
    return(log(times_x0/( ((N_event/sum_x)-1)*times_x1)))
}
b1 <- beta(df, 1)
b2 <- beta(df, 2)

base_haz <- function(beta_hat, cause){
    N_event <- nrow(df[event == cause])
    t_sum <- sum(df$times*exp(beta_hat*(as.numeric(df$x)-1)))
    return(N_event/t_sum)
}
haz01 <- base_haz(beta_hat = b1, cause = 1)
haz02 <- base_haz(beta_hat = b2, cause = 2)

# exponential approximation
s_prob <- function(t,x){
    haz_1 <- haz01*exp(b1*x)
    haz_2 <- haz02*exp(b2*x)
    return(exp(-t*(haz_1+haz_2)))
}

CIF <- function(t,x){
    haz_1 <- haz01*exp(b1*x)
    haz_2 <- haz02*exp(b2*x)
    int_s <- -exp(-t*(haz_1+haz_2))/(haz_1+haz_2)+exp(-0*(haz_1+haz_2))/(haz_1+haz_2)
    return(c(haz_1*int_s,haz_2*int_s))
}

# Using product integral
cumhaz <- function(t, cause, X) {
    cumhaz_1 <- t*haz01*exp(b1*X)
    cumhaz_2 <- t*haz02*exp(b2*X)
    return(c(cumhaz_1,cumhaz_2)[cause])
}

S <- function(t, X){
    # distinct ordered event times up to t
    etimes <- seq(0.01,t,by = 0.01)
#    etimes <- sort(unique(df$times[times <= t]))
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
    F_times <- c(0,df$times[order(times)])
    F_times <- F_times[F_times <= t]
    F_int <- 0
    for (i in 2:length(F_times)){
        F_int <- F_int + S(t = F_times[i-1],X)*(cumhaz(t = F_times[i], cause, X)-cumhaz(t = F_times[i-1], cause, X))
    }
    return(F_int)
}

S(t = 1.7,X = 0)
F(t = 1.7,X = 0, cause = 1)+F(t = 1.7,X = 0, cause = 2)

s_prob(1.7,0)
CIF(1.7,0)


################################################333
for (i in 2000:10000){
set.seed(i)
x <- c(0,0,0,0,0,rep(1,15))
times <- runif(20,0,2)
event <- rbinom(20, 1, 0.3+0.5*x)+1
x <- as.factor(x)
df <- data.table(times,event,x)

N1 <- length(event[event == 1])
N2 <- length(event[event == 2])
tR <- sum(df[x == 0]$times)
X1 <- sum(as.numeric(df[event == 1]$x)-1)
X2 <- sum(as.numeric(df[event == 2]$x)-1)
tR/(N1-X1+N2-X2)


beta <- function(df, cause){
    times_x0 <- sum(df[x == "0",.(times)]$times)
    times_x1 <- sum(df$times)-times_x0
    N_event <- nrow(df[event == cause])
    sum_x <- nrow(df[event == cause & x == "1"])
    return(log(times_x0/( ((N_event/sum_x)-1)*times_x1)))
}
b1 <- beta(df, 1)
b2 <- beta(df, 2)

base_haz <- function(beta_hat, cause){
    N_event <- nrow(df[event == cause])
    t_sum <- sum(df$times*exp(beta_hat*(as.numeric(df$x)-1)))
    return(N_event/t_sum)
}
haz01 <- base_haz(beta_hat = b1, cause = 1)
haz02 <- base_haz(beta_hat = b2, cause = 2)

# exponential approximation
s_prob <- function(t,x){
    haz_1 <- haz01*exp(b1*x)
    haz_2 <- haz02*exp(b2*x)
    return(exp(-t*(haz_1+haz_2)))
}

CIF <- function(t,x){
    haz_1 <- haz01*exp(b1*x)
    haz_2 <- haz02*exp(b2*x)
    int_s <- -exp(-t*(haz_1+haz_2))/(haz_1+haz_2)+exp(-0*(haz_1+haz_2))/(haz_1+haz_2)
    return(c(haz_1*int_s,haz_2*int_s))
}

# Using product integral
cumhaz <- function(t, cause, X) {
    cumhaz_1 <- t*haz01*exp(b1*X)
    cumhaz_2 <- t*haz02*exp(b2*X)
    return(c(cumhaz_1,cumhaz_2)[cause])
}

S <- function(t, X){
    # distinct ordered event times up to t
    etimes <- seq(tR/(N1-X1+N2-X2)-0.001,t,by = tR/(N1-X1+N2-X2)-0.001)
    #    etimes <- sort(unique(df$times[times <= t]))
#    etimes<-t
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
S(t = 2*(tR/(N1-X1+N2-X2)-0.001),X = 0)

if(!is.na(S(t = 1.7,X = 0)) & S(t = 1.7,X = 0)<0){
    print(i)
}
}

#########################################
###Find maximum likelihood estimator by solving equation system numerically
#
# sim data
set.seed(631)
x <- rnorm(10,2,1)
times <- runif(10,0,2)
event <- rbinom(10, 1, pmax(0,x/max(x)))+1
df <- data.table(times,event,x)
df[1:2,event := 0]
# make solver
library(nleqslv)
solve_xy_posx <- function(df, start = c(u = 0, beta = 0), cause) {
    n <- nrow(df[event == cause])
    k <- df$x
    t <- df$t
    k1 <- df[event == cause]$x
  F <- function(par) {
    u <- par[1]
    beta <- par[2]
    h0 <- exp(u)
    e <- exp(beta * k)
    eq1 <- n / h0 - sum(t * e)
    eq2 <- sum(k1) - h0 * sum(t * k * e)
    c(eq1, eq2)
  }
  res <- nleqslv(start, F, method = "Broyden")
  list(h0 = exp(res$x[1]), beta = res$x[2])
}

haz01 <- solve_xy_posx(df, start = c(-3,0), cause = 1)$h0
b1 <- solve_xy_posx(df, start = c(-3,0), cause = 1)$beta
haz02 <- solve_xy_posx(df, start = c(-1,0), cause = 2)$h0
b2 <- solve_xy_posx(df, start = c(-1,0), cause = 2)$beta

cumhaz <- function(t, cause, X) {
    cumhaz_1 <- t*haz01*exp(b1*X)
    cumhaz_2 <- t*haz02*exp(b2*X)
    return(c(cumhaz_1,cumhaz_2)[cause])
}

S <- function(t, X){
    # distinct ordered event times up to t
    etimes <- sort(unique(df$times[times <= t]))
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

# Here the problem happens even if we use mean(x)
S(max(times),mean(x))
F(max(times),cause = 1,mean(x))+F(max(times),cause = 2,mean(x))






