sim_non_prop_haz <- function(n){
    m <- lvm()
    regression(m) <- y ~ s
    regression(m, death~s) <- -0.1
    distribution(m,~s) <- normal.lvm(mean = 0, sd = 0.8)
    distribution(m,~death) <- coxWeibull.lvm(scale=0.2, shape = 1)
    distribution(m,~y) <- a <- coxWeibull.lvm(scale=0.1,shape=~s)
    eventTime(m) <- times ~ min(death = 2,y=1)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,c("status","s"),c("event","X"))
    d <- d[,.(times,event,X)]
    ## d <- d[times <= 10]
    ## d <- d[1:n]
    d[]
}

# Check if data is proportional hazards
## cc <- prodlim::prodlim(survival::Surv(times,event)~X,data=d)
## plot(cc,mark.time=FALSE)



sim_non_prop_haz_cens <- function(n, C = 0.1){
    m <- lvm()
    regression(m) <- y ~ s
    regression(m, death~s) <- -0.1
    distribution(m,~s) <- normal.lvm(mean = 0, sd = 0.8)
    distribution(m,~cens) <- coxWeibull.lvm(scale=C)
    distribution(m,~death) <- coxWeibull.lvm(scale=0.2, shape = 1)
    distribution(m,~y) <- a <- coxWeibull.lvm(scale=0.1,shape=~s)
    eventTime(m) <- times ~ min(death = 2,y=1,cens=0)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,c("status","s"),c("event","X"))
    d <- d[,.(times,event,X)]
    # Censor all observation where times>10
    ## d[times >10, ':='(times = 10, event = 0)]
    d[]
}


sim_cox_exp <- function(n, true_par = c(1.4, -0.4, 0.4, 0.2)){
    m <- lvm()
    regression(m, y~X) <- true_par[1]
    regression(m, death~X) <- true_par[2]
    distribution(m, ~X) <- normal.lvm()
    distribution(m,~death) <- coxExponential.lvm(rate = true_par[4])
    distribution(m,~y) <- coxExponential.lvm(rate = true_par[3])
    eventTime(m) <- times ~ min(y = 1,death = 2)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    ## setnames(d,c("time","age"),c("times","X"))
    d <- d[,.(times,event,X)]
    d[]
}

sim_cox_exp_cens <- function(n, true_par = c(1.4, -0.4, 0.4, 0.2), C = 0.12){
    m <- lvm()
    regression(m, y~X) <- true_par[1]
    regression(m, death~X) <- true_par[2]
    distribution(m, ~X) <- normal.lvm()
    distribution(m,~cens) <- coxWeibull.lvm(scale=C)
    distribution(m,~death) <- coxExponential.lvm(rate = true_par[4])
    distribution(m,~y) <- coxExponential.lvm(rate = true_par[3])
    eventTime(m) <- times ~ min(y = 1,death = 2, cens = 0)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    ## setnames(d,c("time","age"),c("times","X"))
    d <- d[,.(times,event,X)]
    d[]
}

sim_dpnext <- function(n){
    m <- lvm()
    distribution(m,~age) <- normal.lvm(mean = 57.0602154546317, sd = 14.1226171463087)
    distribution(m,~y) <- coxWeibull.lvm(scale = 4.99939534464252e-05, shape = 1.6080293052532)
    regression(m, y~hba1c+age) <- c(0.121823881773495, 0.00536060442429844)
    distribution(m, ~death) <- coxWeibull.lvm(scale = 2.95018879855583e-05, shape = 1.24062643614)
    regression(m, death~hba1c+age) <- c(-0.014226863536, 0.096357141521148)
    m <- eventTime(m,time ~ min(y = 1, death = 2), 'event')
    distribution(m, ~hba1c) <- normal.lvm(mean = 30.4609161720932, sd = 3.75945937876573)
    regression(m, hba1c~age) <- c(0.100349744003182)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,c("time","age"),c("times","X"))
    d <- d[,.(times,event,X)]
    d[]
}

sim_cox_weibull <- function(n){
    ### coxWeibull
    m <- lvm()
    regression(m) <- y ~ X
    regression(m) <- death ~ -0.5*X
    distribution(m, ~X) <- normal.lvm()
    distribution(m,~death) <- coxWeibull.lvm(scale = 1)
    distribution(m,~y) <- coxWeibull.lvm(scale = 0.1)
    eventTime(m) <- times ~ min(y = 1,death = 2)
    d <- lava::sim(m,n)
    d <- setDT(d)
    setnames(d,"status","event")
    d <- d[,.(times,event,X)]
    d[]
}

## sim_non_prop_haz <- function(n){
##     # Discrete-time simulation settings
##     x <- rnorm(n)
##     dt <- 0.01
##     tmax <- 10
##     times <- seq(dt, tmax, by = dt)
##     event <- rep(NA_integer_, n)
##     time_to_event <- rep(NA_real_, n)
##     for (i in seq_len(n)) {
##         for (t in times) {
##             # Cause-specific hazards with time-varying covariate effects
##             h1 <- 0.08 * exp( 1 * x[i] - 0.45 * x[i] * t)
##             h2 <- 0.06 * exp(-0.8 * x[i] + 0.30 * x[i] * t)
##             # Event probabilities over small interval dt
##             p1 <- h1 * dt
##             p2 <- h2 * dt    
##             u <- runif(1)   
##             if (u < p1) {
##                 event[i] <- 1
##                 time_to_event[i] <- t
##                 break
##             } else if (u < p1 + p2) {
##                 event[i] <- 2
##                 time_to_event[i] <- t
##                 break
##             }
##         }
##         # Force no censoring if no event occurred by tmax
##         if (is.na(event[i])) {
##             h1 <- 0.08 * exp( 1 * x[i] - 0.35 * x[i] * tmax)
##             h2 <- 0.06 * exp(-0.8 * x[i] + 0.30 * x[i] * tmax)
##             probs <- c(h1, h2) / (h1 + h2)
##             event[i] <- sample(c(1, 2), size = 1, prob = probs)
##             time_to_event[i] <- tmax
##         }
##     }
##     dat <- data.table(
##         event = event,
##         times = time_to_event,
##         x = x
##     )
##     setnames(dat,"x","X")
##     dat[]
## }
## cc <- prodlim::prodlim(survival::Surv(times,event)~X,data=d)
## plot(cc,mark.time=FALSE)


######################################################################
### simulate_data_functions.R ends here
