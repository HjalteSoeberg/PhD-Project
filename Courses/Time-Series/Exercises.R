### Exercises.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jun  2 2026 (10:30) 
## Version: 
## Last-Updated: Jun  2 2026 (14:46) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 25
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


# 5
# Hint: eigen value of A


#6
# M_T=3^(T-1)
# M_0 =1, M_1=3, M_2=9,...
# This means all the columns are linear dependent and the rank of the matrix is 1

#7
# compute matrix product in for loop
B <- matrix(c(1,1,2,1), nrow = 4, ncol = 1)
C <- matrix(c(3/2,6,-7/2,3/2), nrow = 1, ncol = 4)
A1 <- matrix(c(1/10,9/40,9/40,3/40, -2/5,-1/5,-7/10,-11/10, 2/5,9/40,29/40,7/8, 0,-9/40,-9/40,1/40), nrow = 4, ncol = 4)
A2 <- matrix(c(6/5,-79/8,-79/8,-357/40, 7/10,20,39/2,101/5, -7/10,-79/8,-75/8,-413/40, 0,79/8,79/8,81/8), nrow = 4,ncol = 4)

M1 <- function(k){C%*%(A1^k)%*%B}
M2 <- function(k){C%*%(A2^k)%*%B}

HN <- function(M,N){
    H <- c()
    for (i in 0:N){
        for (t in i:(i+N-1)){
            H <- append(H,M(t))
        }
    }
    return(matrix(H, nrow = N, ncol = N+1))
}

SVD <- svd(HN(M = M1,N = 2))
U <- SVD$u
SIG <- diag(SVD$d)
V <- SVD$v
U%*%SIG%*%t(V)

O <- U%*%(SIG^0.5)
R <- (SIG^0.5)%*%t(V)


### Bonus exercise
# Generate data and estimate parameters using linear regression
data <- function(a1,b0,b1,N,var){
    y <- c()
    u <- c()
    y_1 <- c()
    u_1 <- c()
    for (i in 1:N){
        if (i == 1){
            y_minus <- 0
            u_minus <- rnorm(1,mean = 0, sd = var^2)
        }
        ut <- rnorm(1,mean = 0, sd = var^2)
        et <- rnorm(1,mean = 0, sd = 1)
        y_1 <- append(y_1,y_minus)
        u_1 <- append(u_1,u_minus)
        y_minus <- a1*y_minus+b0*ut+b1*u_minus+rnorm(1,mean = 0, sd = 1)
        u_minus <- ut
        y <- append(y,y_minus)
        u <- append(u,ut)
    }
    return(data.table(y = y,u = u,y1 = y_1,u1 = u_1))
}

df <- data(a1 = 0.3,b0 = 1,b1 = -2,N = 1000,var = 1)
m1 <- lm(data = df, y~y1+u+u1)
df2 <- data(a1 = 0.3,b0 = 1,b1 = -2,N = 1000,var = 5)
m2 <- lm(data = df2, y~y1+u+u1)

         

#8
# 


######################################################################
### Exercises.R ends here
