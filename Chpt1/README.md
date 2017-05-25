Chapter 1
=========

Simulating simple SCM
---------------------

In this example, we illustrate how to simulate from a simple SCM. We
will need the MASS package, which can be installed as follows.

    # install the MASS package if it's not already installed
    if(!("MASS" %in% installed.packages())){
        install.packages("MASS", repos = "https://ftp.osuosl.org/pub/cran/")
    }

    # load the MASS package
    library(MASS)

First, we write a function that draws *n* values of *U* from a
multivariate normal distribution

    sim_U <- function(n){
        U <- matrix(mvrnorm(
            n = n, # draw n values 
            mu=c(0,0,0), # mean vector
            Sigma = diag(3) # identity covariance matrix
                ),ncol=3)
        # give the columns names
        colnames(U) <- c("U_W","U_A","U_Y")
        
        # return the matrix of simulated values
        return(U)
    }

    # test out the function
    set.seed(123)
    sim_U(n=1)

    ##           U_W        U_A        U_Y
    ## [1,] 1.558708 -0.2301775 -0.5604756

    sim_U(n=3)

    ##             U_W        U_A        U_Y
    ## [1,] -0.4456620  0.4609162 0.07050839
    ## [2,]  1.2240818 -1.2650612 0.12928774
    ## [3,]  0.3598138 -0.6868529 1.71506499

Next we write a function for each structural equation.

    # f_W(U_W) = U_W
    f_W <- function(U_W){
        return(U_W)
    }
    # f_A(W, U_A) = 1/(1 + exp(U_A + W))
    f_A <- function(W, U_A){
        return(as.numeric(1/(1 + exp(U_A + W))>0.5))
    }
    # f_Y(W,A,U_Y) &= exp(-2 + W - A + U_Y) 
    f_Y <- function(W, A, U_Y){
        return(exp(-2 + W - A + U_Y))
    }

Now we can use these functions to answer: What value would nature assign
if we drew *U* = ( − 1, 0, 1)?

    # use U_W = -1 to generate W
    W <- f_W(U_W = -1)
    # use U_A = 0 and W to generate A
    A <- f_A(W = W, U_A = 0)
    # use U_Y = 1, A, and W to generate Y
    Y <- f_Y(W = W, A = A, U_Y = 1)
    # print out the answer
    cat("W = ",W,", A = ",A,", Y = ",Y)

    ## W =  -1 , A =  1 , Y =  0.04978707

What value would nature assign if we drew *U* = (2, 0.5, −0.25)?

    # use U_W = -1 to generate W
    W <- f_W(U_W = 2)
    # use U_A = 0 and W to generate A
    A <- f_A(W = W, U_A = 0.5)
    # use U_Y = 1, A, and W to generate Y
    Y <- f_Y(W = W, A = A, U_Y = -0.25)
    # print out the answer
    cat("W = ",W,", A = ",A,", Y = ",Y)

    ## W =  2 , A =  0 , Y =  0.7788008

Now we can write a function that simulates the entire SCM

    sim_SCM <- function(n){
        # draw U from P_U using function sim_U
        U <- sim_U(n=n)
        # generate W via f_W with input U_W
        W <- f_W(U_W = U[,1])
        # generate A via f_A with input (W, U_A)
        A <- f_A(W = W, U_A = U[,2])
        # generate Y via f_Y with input (W, A, U_Y)
        Y <- f_Y(W = W, A = A, U_Y = U[,3])
        # return a data.frame with data
        return(data.frame(W = W, A = A, Y = Y))
    }

    # try it out
    set.seed(123)
    sim_SCM(n=1)

    ##            W A         Y
    ## U_W 1.558708 0 0.3672299

    sim_SCM(n=3)

    ##            W A          Y
    ## 1 -0.4456620 0 0.09300021
    ## 2  1.2240818 1 0.19269812
    ## 3  0.3598138 1 0.39648337
