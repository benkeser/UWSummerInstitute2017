# install the MASS package if it's not already installed
if(!("MASS" %in% installed.packages())){
    install.packages("MASS", repos = "https://ftp.osuosl.org/pub/cran/")
}

# load the MASS package
library(MASS)

sim_U <- function(n){
    U <- matrix(mvrnorm(
        n = n, # draw n values 
        mu=c(0,0,0), # mean vector
        Sigma = diag(3) # identity covariance matrix
    ))
    # give the columns names
    colnames(U) <- c("U_W","U_A","U_Y")
    
    # return the matrix of simulated values
    return(U)
}

# test out the function
set.seed(123)
sim_U(n=1)
sim_U(n=3)

# f_W(U_W) = U_W
f_W <- function(U_W){
    return(U_W)
}
# f_A(W, U_A) = 1/(1 + exp(U_A + W))
f_A <- function(W, U_A){
    return(1/(1 + exp(U_A + W)))
}
# f_Y(W,A,U_Y) &= exp(-2 + W - A + U_Y) 
f_Y <- function(W, A, U_Y){
    return(exp(-2 + W - A + U_Y))
}

# use U_W = -1 to generate W
W <- f_W(U_W = -1)
# use U_A = 0 and W to generate A
A <- f_A(W = W, U_A = 0)
# use U_Y = 1, A, and W to generate Y
Y <- f_Y(W = W, A = A, U_Y = 1)
# print out the answer
cat("W = ",W,", A = ",A,", Y = ",Y)

# use U_W = -1 to generate W
W <- f_W(U_W = 2)
# use U_A = 0 and W to generate A
A <- f_A(W = W, U_A = 0.5)
# use U_Y = 1, A, and W to generate Y
Y <- f_Y(W = W, A = A, U_Y = -0.25)
# print out the answer
cat("W = ",W,", A = ",A,", Y = ",Y)

sim_SCM <- function(n){
    # draw U from P_U using function sim_U
    U <- sim_U(n=n)
    # generate W via f_W with input U_W
    W <- f_W(U_W = U$U_W)
    # generate A via f_A with input (W, U_A)
    A <- f_A(W = W, U_A = U$U_A)
    # generate Y via f_Y with input (W, A, U_Y)
    Y <- f_Y(W = W, A = A, U_Y = U$U_Y)
    # return a data.frame with data
    return(data.frame(W = W, A = A, Y = Y))
}

# try it out
set.seed(123)
sim_SCM(n=1)
sim_SCM(n=3)