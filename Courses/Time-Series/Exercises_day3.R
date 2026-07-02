### Exercises_day3.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jun  4 2026 (11:09) 
## Version: 
## Last-Updated: Jun  4 2026 (12:51) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

A <- matrix(c(0.8,0,0.1,0.6), nrow = 2)
B <- matrix(c(1,0.5),nrow = 2)
K <- matrix(c(0.2,0.1),nrow = 2)
C <- matrix(c(1,0.5),nrow = 1)
D <- 0
eigen(A)

library(data.table)
data_gen <- function(N){
    y <- c()
    yminus <- c()
    u <- c()
    uminus <- c()
    for (i in 0:(N-1)){
        if (i == 0){
            u_minus <- rnorm(1,mean = 0, sd = 1)
            y_minus <- 0
            xt <- matrix(c(y_minus,u_minus),nrow = 2)
        }
        ut <- rnorm(1,mean = 0, sd = 1)
        et <- rnorm(1,mean = 0, sd = 0.1)
        x_plus <- A%*%xt+B*ut+K*et
        yt <- C%*%xt+et
        y <- append(y,yt)
        yminus <- append(yminus,y_minus)
        u <- append(u,ut)
        uminus <- append(uminus,u_minus)
        xt <- x_plus
        y_minus <- yt
        u_minus <- ut
    }
    df <- data.table(y = y,yminus = yminus,u = u,uminus = uminus)
    df[]
}

df <- data_gen(10000)

######################################################################
### Exercises_day3.R ends here
