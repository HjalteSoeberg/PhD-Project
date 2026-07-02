### Exercises_day2.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jun  3 2026 (11:18) 
## Version: 
## Last-Updated: Jun  3 2026 (12:49) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 12
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

# Exercise 10
library(sysid)

A <- -1*matrix(c(0,-0.15,1,-0.8), nrow = 2, ncol = 2)
B <- matrix(c(0,1),nrow = 2)
C <- matrix(c(0.5,0.5),nrow = 1)
D <- 0
F <- matrix(c(1,0.5),nrow = 2)
H <- 0.1

#1
N <- 1000

# x(t)=[y(t-1), u(t-1)]^T
# x(t+1)=A*x(t)+B*u(t) and y(t)=C*x(t)+D*u(t)
library(data.table)
data <- function(N){
    y <- c()
    yminus <- c()
    u <- c()
    uminus <- c()
    for (i in 0:N){
        if (i == 0){
            u_minus <- rnorm(1,mean = 0, sd = 1)
            y_minus <- 0
            xt <- matrix(c(y_minus,u_minus),nrow = 2)
        }
        ut <- rnorm(1,mean = 0, sd = 1)
        x_plus <- A%*%xt+B*ut
        yt <- C%*%xt
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

df <- data(1000)



d <- idframe(output = matrix(df$y,ncol = 1), input = matrix(df$u,ncol = 1), unit = "hours")

m <- armax(d, c(1,1,1,2))

m1 <- lm(data = df, y~yminus+u+uminus)












######################################################################
### Exercises_day2.R ends here
