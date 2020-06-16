

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


## 4M1 - Medium (Monday, June 15, 2020 at 7:25 EST)
#
library(rethinking)

curve( dnorm(x, 0, 10), from=-50, to=50 )  # Distribution for mean (mu).
curve( dexp(x, rate=1, log=FALSE), from=-2, to=10)  # Distribution for standard deviatin (sigma).

N <- 1e4; set.seed(2971)  # Arbitrary value for seed (see page 95 of textbook).
sample_mu <- rnorm(N, 0, 10)
sample_sigma <- rexp(N, rate=1)

prior_h <- rnorm(N, sample_mu, sample_sigma)
dens(prior_h)
precis(prior_h); boxplot(prior_h); plot(prior_h)


## 4M2 - Medium (Tuesday, June 16, 2020 at 9:15 EST)
#
# See page 88, R Code 4.27 to 4.29
#
flist <- alist(
     height ~ dnorm( mu , sigma ) ,
     mu ~ dnorm( mean=0 , std10 ) ,
     sigma ~ dexp( rate=1 )
)


## 4M3 - Medium (Tuesday, June 16, 2020 at 9:22 EST)
#
# See question 4E4.
#
# yi ~ Normal(mu, sigma)
# mui = a + b*xi
# a ~ Normal(0, 10)
# b ~ Uniform(0, 1)
# sigma ~ Exponential(1)


## 4M4 - Medium (Tuesday, June 16, 2020 at 9:26 EST)
#
# Data
# Year 1 -> S1_height, S2_height, ... SN_height
# Year 2 -> S1_height, S2_height, ... SN_height
# Year 3 -> S1_height, S2_height, ... SN_height
#
# 




