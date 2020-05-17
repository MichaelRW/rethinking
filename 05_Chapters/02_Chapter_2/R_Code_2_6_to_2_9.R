

## R code 2.6

library(rethinking)

globe.qa <- quap(  # Quadratic Approximation
    alist(
        W ~ dbinom( W + L ,p ),  # Binomial Likelihood
        p ~ dunif(0,1)  # Uniform Prior
    ),
    data = list( W=6, L=3 ) )

# Display summary of quadratic approximation.
precis( globe.qa )


## R code 2.7

# Analytical Calculation
W <- 6  # Number of times Water occurs.
L <- 3  # Number of time Land occurs.
    curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )

# Quadratic Approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )
    legend( 0.0, 2.0, legend=c("True", "Quad. Approx."), lty=1:2, cex=0.8 )


## R code 2.8
    
n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
    p_new <- rnorm( 1 , p[i-1] , 0.1 )
    if ( p_new < 0 ) p_new <- abs( p_new )
    if ( p_new > 1 ) p_new <- 2 - p_new
    q0 <- dbinom( W , W+L , p[i-1] )
    q1 <- dbinom( W , W+L , p_new )
    p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}


## R code 2.9

dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )
     legend( 0.0, 2.0, legend=c("MCMC Estimate", "Quad. Approx."), lty=1:2, cex=0.8 )


