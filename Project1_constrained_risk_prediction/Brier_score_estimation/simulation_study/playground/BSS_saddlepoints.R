# Plot the BSS function to find out if it contains saddle points (maybe set lambda1,2 to they real values such its only the betas that vary)
library(targets)
tar_load_globals()
n = 300
N = 10
tau = 2
start <- c(
    beta1        = 1,
    beta2        = -0.5
)
d <- sim_cox_exp(n, true_par = as.numeric(c(start[1:2],0.8,0.2)))

BrierScore <- function(parms, data, T = 1, BS = "both"){
    df <- data
    beta1 <- parms[1]
    beta2 <- parms[2]
    lambda1 <- 0.8
    lambda2 <- 0.2
    BS1 <- 0
    BS2 <- 0
    paste(beta1,beta2,lambda1,lambda2)
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

b1 <- seq(-10,10,by = 0.4)
b2 <- seq(-10,10,by = 0.4)
df <- setDT(expand.grid(b1,b2))
setnames(df,c("Var1","Var2"), c("beta1","beta2"))
df[,BS := BrierScore(parms = c(beta1,beta2),data = d, T = tau), by = 1:nrow(df)]
p <- lattice::wireframe(BS ~ beta1 * beta2, data = df)
npanel <- c(4, 2)
rotx <- c(-50, -80)
rotz <- seq(30, 300, length = npanel[1]+1)
update(p[rep(1, prod(npanel))], layout = npanel,
    panel = function(..., screen) {
        lattice::panel.wireframe(..., screen = list(z = rotz[lattice::current.column()],
                                           x = rotx[lattice::current.row()]))
    })



######################################################################
### BSS_saddlepoints.R ends here
