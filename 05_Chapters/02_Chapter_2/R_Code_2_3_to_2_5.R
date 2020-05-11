

## ------------------------------------------------------------
## R Code 2.3

# Define Grid
p_grid <- seq( from=0 , to=1 , length.out=20 )

# Define Prior
prior <- rep( 1 , 20 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
#prior <- exp( -5*abs( p_grid - 0.5 ) )
dev.new(); plot( p_grid, prior, type="b" )

# Compute likelihood at each value in grid.
likelihood <- dbinom( 6 , size=9 , prob=p_grid )

# Compute product of likelihood and prior.
unstd.posterior <- likelihood * prior

# Standardize the posterior, so it sums to 1.
posterior <- unstd.posterior / sum(unstd.posterior)


## ------------------------------------------------------------
## R Code 2.4

plot( p_grid , posterior , type="b" ,
    xlab="Probability of Water" , ylab="Posterior Probability" )
mtext( "20 points" )
grid( nx = 20, ny = nx, col = "lightblue", lty = "dotted", lwd = par("lwd"), equilogs = TRUE )


## ------------------------------------------------------------
## R Code 2.5

prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- exp( -5*abs( p_grid - 0.5 ) )


