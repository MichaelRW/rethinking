

## Chapter 4, End of Chapter Questions

## Q1 - Easy (Monday, June 15, 2020 at 6:41 EST)
#
# The second and third lines are likelihood functions for the mean and
# standard deviation.


## Q2 - Easy (Monday, June 15, 2020 at 6:44 EST)
#
# There are two parameters in the posterior distribution.


## Q3 - Easy (Monday, June 15, 2020 at 6:45 EST)
#
# Pr( yi | mu, sigma ) = Normal( y | mu, sigma ) * Normal(y | 0, 10) * Exponential(sigma | 1) / ...
#                        Integral( NOrmal(y | 0, 10) * Exponential(sigma | 1) ) dy


## Q4 - Easy (Monday, June 15, 2020 at 6:58 EST)
#
# The second line is the linear model.
#
# See page 93.


## Q5 - Easy (Monday, June 15, 2020 at 7:04 EST)
#
# There are three parameters in the posterior distribution.
#
# See page 93.


## Q6 - Medium (Monday, June 15, 2020 at 7:25 EST)
#
library(rethinking)

curve( dnorm(x, 0, 10), from=-50, to=50 )  # Distribution for mean (mu).
curve( dexp(x, rate=1, log=FALSE), from=-2, to=10)  # Distribution for standard deviatin (sigma).

N <- 1e4
sample_mu <- rnorm(N, 0, 10)
sample_sigma <- rexp(N, rate=1)

prior_h <- rnorm(N, sample_mu, sample_sigma)
dens(prior_h)
