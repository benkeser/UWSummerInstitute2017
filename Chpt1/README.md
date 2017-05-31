Chapter 1
=========

Simulating simple SCM
---------------------

In this example, we illustrate how to simulate from a simple SCM. We will need the MASS package, which can be installed as follows.

``` r
# install the MASS package if it's not already installed
if(!("MASS" %in% installed.packages())){
    install.packages("MASS", repos = "https://ftp.osuosl.org/pub/cran/")
}

# load the MASS package
library(MASS)
```

First, we write a function that draws *n* values of *U* from a multivariate normal distribution

``` r
sim_U_1 <- function(n){
    U <- matrix(mvrnorm(
        n = n, # draw n values 
        mu=c(0,0,0), # mean vector
        Sigma = diag(3) # identity covariance matrix
            ),ncol=3,byrow=TRUE)
    # give the columns names
    colnames(U) <- c("U_W","U_A","U_Y")
    
    # return the matrix of simulated values
    return(U)
}

# test out the function
set.seed(123)
sim_U_1(n=1)
```

    ##           U_W        U_A        U_Y
    ## [1,] 1.558708 -0.2301775 -0.5604756

``` r
sim_U_1(n=3)
```

    ##              U_W        U_A        U_Y
    ## [1,] -0.44566197  1.2240818  0.3598138
    ## [2,]  0.46091621 -1.2650612 -0.6868529
    ## [3,]  0.07050839  0.1292877  1.7150650

Next we write a function for each structural equation.

``` r
# f_W(U_W) = I(U_W > 0)
f_W_1 <- function(U_W){
    return(as.numeric(U_W > 0))
}
# f_A(W, U_A) = I(W + U_A > 1)
f_A_1 <- function(W, U_A){
    return(as.numeric(W + U_A > 1))
}
# f_Y(W,A,U_Y) = I(W + A + U_Y > 2)
f_Y_1 <- function(W, A, U_Y){
    return(as.numeric(W + A + U_Y > 2))
}
```

Now we can use these functions to answer: What value would Nature assign if we drew *U* = ( − 1, 0, 1)?

``` r
# use U_W = -1 to generate W
W <- f_W_1(U_W = -1)
# use U_A = 0 and W to generate A
A <- f_A_1(W = W, U_A = 0)
# use U_Y = 1, A, and W to generate Y
Y <- f_Y_1(W = W, A = A, U_Y = 1)
# print out the answer
cat("W = ",W,", A = ",A,", Y = ",Y)
```

    ## W =  0 , A =  0 , Y =  0

What value would Nature assign if we drew *U* = (2, 0.5, −0.25)?

``` r
# use U_W = 2 to generate W
W <- f_W_1(U_W = 2)
# use U_A = 0.5 and W to generate A
A <- f_A_1(W = W, U_A = 0.5)
# use U_Y_1 = -0.25, A, and W to generate Y
Y <- f_Y_1(W = W, A = A, U_Y = -0.25)
# print out the answer
cat("W = ",W,", A = ",A,", Y = ",Y)
```

    ## W =  1 , A =  1 , Y =  0

Now we can write a function that simulates the entire SCM

``` r
sim_SCM_1 <- function(n){
    # draw U from P_U using function sim_U_1
    U <- sim_U_1(n=n)
    # generate W via f_W with input U_W
    W <- f_W_1(U_W = U[,1])
    # generate A via f_A with input (W, U_A)
    A <- f_A_1(W = W, U_A = U[,2])
    # generate Y via f_Y with input (W, A, U_Y)
    Y <- f_Y_1(W = W, A = A, U_Y = U[,3])
    # return a data.frame with data
    return(data.frame(W = W, A = A, Y = Y))
}

# try it out
set.seed(123)
sim_SCM_1(n=1)
```

    ##   W A Y
    ## 1 1 0 0

``` r
sim_SCM_1(n=3)
```

    ##   W A Y
    ## 1 0 1 0
    ## 2 1 0 0
    ## 3 1 1 1

Intervening on simple SCM
-------------------------

In this example, we illustrate how the above SCM can be manipulated to generate counterfactual random variables. Be sure to run the code from the first example so that the functions `sim_U_1`, `f_W_1`, `f_A_1`, and `f_Y_1` are read into `R`'s memory. Now we can use these functions to answer: What value would Nature assign to our ideal data if we drew *U* = ( − 1, 0, 1)?

``` r
# use U_W = -1 to generate W
W <- f_W_1(U_W = -1)
# rather than using _1 to generate A, we set it's value to 1
A <- 1
# use U_Y = 1, A, and W to generate Y
Y_a <- f_Y_1(W = W, A = A, U_Y = 1)
# print out the answer
cat("W = ",W,", A = ",A,", Y_a = ",Y_a)
```

    ## W =  0 , A =  1 , Y_a =  0

What value would Nature assign if we drew *U* = (2, 0.5, −0.25)?

``` r
# use U_W = 2 to generate W
W <- f_W_1(U_W = 2)
# rather than using f_A to generate A, we set it's value to 1
A <- 1
# use U_Y = -0.25, A, and W to generate Y
Y_a <- f_Y_1(W = W, A = A, U_Y = -0.25)
# print out the answer
cat("W = ",W,", A = ",A,", Y_a = ",Y_a)
```

    ## W =  1 , A =  1 , Y_a =  0

Similarly as above, we can define a function to generate multiple draws of counterfactual data. Whereas the function `sim_SCM_1` simulated data according to the observed experimental conditions, we now require a function that simulates data according to the ideal experimental conditions.

``` r
# we add an option (a) to the function to set the level 
# of the variable A
sim_intervention_SCM_1 <- function(n, a){
    # draw U from P_U using function sim_U_1
    U <- sim_U_1(n=n)
    # generate W via f_W with input U_W
    W <- f_W_1(U_W = U$U_W)
    #  replace f_A with a
    A <- a
    # generate Y via f_Y with input (W, A, U_Y)
    Y_a <- f_Y_1(W = W, A = A, U_Y = U$U_Y)
    # return a data.frame with counterfactual data
    return(data.frame(W = W, A = A, Y_a = Y))
}
```

Notice that this function is exactly the same as `sim_SCM_1` except that it replaces the line `A <- f_A_1(W = W, U_A = U$U_A)` with `A <- a`. This change in code represents intervening in the observed data SCM.

SCM vs. statistical model
-------------------------

This exercise illustrates how different choices of *P*<sub>*U*</sub> and *F* result in different distributions for the observed data. Recall that an SCM is a collection of different distributions for *U* and different structural equations *F*. In the above exercises, we wrote a function `sim_SCM_1` that drew *U* from a multivariate normal distribution with identity covariance matrix (via `sim_U_1`) and then used `f_W_1`, `f_A_1`, and `f_Y_1` to generate the observed data. We used `_1` to denote that these were just one choice of distribution for *U* and structural equations that could be in our SCM.

Let's see how choices of *P*<sub>*U*</sub> and *F* imply a distribution for the observed data. Because *W*, *A*, and *Y* are all binary variables, there are 2<sup>3</sup> = 8 possible values for an observation *O*. The distribution for *O* is therefore defined by the probability that *O* takes each of these 8 values. To approximate these probabilities, we will simulate a large data set using `sim_SCM_1` and see how many observations are equal to each of the possible values.

``` r
# simulate 1 million observations
set.seed(123)
bigData_1 <- sim_SCM_1(n = 1e6)

# make two 2x2 tables displaying the probabilities 
# for each possible combination of W,A,Y
round(table(bigData_1)/1e6,2)
```

    ## , , Y = 0
    ## 
    ##    A
    ## W      0    1
    ##   0 0.41 0.07
    ##   1 0.21 0.12
    ## 
    ## , , Y = 1
    ## 
    ##    A
    ## W      0    1
    ##   0 0.01 0.01
    ##   1 0.04 0.12

The two 2x2 tables display the probability that *O* assumes each value. For example, we see that *P*(*W* = 0, *A* = 0, *Y* = 0)=0.41.

The choices for the distribution of *U* and the structural equations used in the above example are just one of an infinite number of choices that might be in our model. Let's see how the choice of a different distribution for *U* and different choices for *F* affect the distribution of the observed data.

``` r
# define a new function that draws U from a 
# multivariate normal distribution with larger variance
# than sim_U_1
sim_U_2 <- function(n){
    U <- matrix(mvrnorm(
        n = n, # draw n values 
        mu=c(0,0,0), # mean vector
        Sigma = diag(x=2, 3) # increase the variance of error distribution
            ),ncol=3,byrow=TRUE)
    # give the columns names
    colnames(U) <- c("U_W","U_A","U_Y")
    
    # return the matrix of simulated values
    return(U)
}

# change structural equation for W
f_W_2 <- function(U_W){
    return(as.numeric(U_W > 1))
}
# change structural equation for A
f_A_2 <- function(W, U_A){
    return(as.numeric(W + U_A > 0))
}
# change structural equation for Y
f_Y_2 <- function(W, A, U_Y){
    return(as.numeric(W + A + U_Y > -1))
}


# simulate from a different distribution in our SCM
sim_SCM_2 <- function(n){
    # draw U from P_U using function sim_U
    U <- sim_U_2(n=n)
    # generate W via f_W with input U_W
    W <- f_W_2(U_W = U[,1])
    # generate A via f_A with input (W, U_A)
    A <- f_A_2(W = W, U_A = U[,2])
    # generate Y via f_Y with input (W, A, U_Y)
    Y <- f_Y_2(W = W, A = A, U_Y = U[,3])
    # return a data.frame with data
    return(data.frame(W = W, A = A, Y = Y))
}
```

These functions now provide a way to simulate observed data from a different choice of error distribution and structural equations that is in our model. Let's repeat the above exercise to see how these choices changes the implied distribution for the observed data.

``` r
# simulate 1 million observations
set.seed(123)
bigData_2 <- sim_SCM_2(n = 1e6)

# make two 2x2 tables displaying the probabilities 
# for each possible combination of W,A,Y
round(table(bigData_2)/1e6,2)
```

    ## , , Y = 0
    ## 
    ##    A
    ## W      0    1
    ##   0 0.09 0.03
    ##   1 0.00 0.00
    ## 
    ## , , Y = 1
    ## 
    ##    A
    ## W      0    1
    ##   0 0.29 0.35
    ##   1 0.05 0.18

The two 2x2 tables display the probability that *O* assumes each value. For example, we see that *P*(*W* = 0, *A* = 0, *Y* = 0) has now changed from 0.41 to 0.09.

The SCM is comprised of every choice of error distribution and structural equation that we believe to be plausible scientifically. As demonstrated above, each of these choices results in a different distribution of the observed data. We call this collection of distributions for the observed data that are implied by our SCM the statistical model.
