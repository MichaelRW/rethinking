


## R Code 4.1 - Section 4.1.1, Normal by Addition - Done May 31, 2020
pos <- replicate( 1e3 , sum( runif(16,-1,1) ) )
dens( pos, norm.comp = TRUE )


## R Code 4.2 - Section 4.1.2, Normal by Multiplication - Done May 31, 2020
prod( 1 + runif( 12, 0, 0.1 ) )


## R Code 4.3 - Section 4.1.2 - Done May 31, 2020
growth <- replicate( 10000 , prod( 1 + runif( 12, 0, 0.1 ) ) )
dens( growth , norm.comp=TRUE )


## R Code 4.4 - Section 4.1.2 - Done May 31, 2020
big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
     dens(big, norm.comp=TRUE)
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )
     dens(small, norm.comp = TRUE)


## R Code 4.5 - Section 4.1.3, Normal by log-multiplication - Done May 31, 2020
log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
     dens(log.big, norm.comp = TRUE)


## R Code 4.6 - Section 4.2.1, Re-describing the Globe Tossing Model
w <- 6; n <- 9;
p_grid <- seq(from=0,to=1,length.out=100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)


## R Code 4.7 - Section 4.3, Gaussian Model of Height
library(rethinking)
data(Howell1)
d <- Howell1


## R Code 4.8 - Section 4.3, Gaussian Model of Height
str( d )


## R Code 4.9 - Section 4.3, Gaussian Model of Height
precis( d )


## R Code 4.10 - Section 4.3, Gaussian Model of Height
d$height


## R Code 4.11 - Section 4.3, Gaussian Model of Height
d2 <- d[ d$age >= 18 , ]


## R Code 4.12 - Section 4.3, Gaussian Model of Height
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )


## R Code 4.13 - Section 4.3, Gaussian Model of Height
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )


## R Code 4.14 - Section 4.3, Gaussian Model of Height
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


## R Code 4.15 - Section 4.3, Gaussian Model of Height
sample_mu <- rnorm( 1e4 , 178 , 100 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


## R Code 4.16 - Section 4.3, Gaussian Model of Height
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
     dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
     dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )


## R Code 4.17 - Section 4.3, Gaussian Model of Height
dev.new(); contour_xyz( post$mu , post$sigma , post$prob )

## R Code 4.18 - Section 4.3, Gaussian Model of Height
dev.new(); image_xyz( post$mu , post$sigma , post$prob )


## R Code 4.19 - Section 4.3, Gaussian Model of Height
sample.rows <- sample( 1:nrow(post) , size=1e3 , replace=TRUE, prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]


## R Code 4.20 - Section 4.3, Gaussian Model of Height
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )


## R Code 4.21 - Section 4.3, Gaussian Model of Height
windows(); dens( sample.mu )
windows(); dens( sample.sigma )


## R Code 4.22 - Section 4.3, Gaussian Model of Height
PI( sample.mu )
PI( sample.sigma )


## R Code 4.23 - Section 4.3, Gaussian Model of Height
d3 <- sample( d2$height , size=20 )


## R Code 4.24 - Section 4.3, Gaussian Model of Height
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
     sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
                 log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
     dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )


## R Code 4.25 - Section 4.3, Gaussian Model of Height
dens( sample2.sigma , norm.comp=TRUE )


## R Code 4.26 - Section 4.3.5, Finding the Posterior Distribution with QUAP.
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]


## R Code 4.27 - Section 4.3.5, Finding the Posterior Distribution with QUAP.
flist <- alist(
     height ~ dnorm( mu , sigma ) ,
     mu ~ dnorm( 178 , 20 ) ,
     sigma ~ dunif( 0 , 50 )
)


## R Code 4.28 - Section 4.3.5, Finding the Posterior Distribution with QUAP.
m4.1 <- quap( flist , data=d2 )


## R Code 4.29 - Section 4.3.5, Finding the Posterior Distribution with QUAP.
precis( m4.1 )


## R Code 4.30 - Section 4.3.5, Finding the Posterior Distribution with QUAP.
start <- list(
     mu=mean(d2$height),
     sigma=sd(d2$height)
)
m4.1 <- quap( flist , data=d2 , start=start )


## R Code 4.31 - Section 4.3.5, Finding the Posterior Distribution with QUAP.
m4.2 <- quap(
     alist(
          height ~ dnorm( mu , sigma ) ,
          mu ~ dnorm( 178 , 0.1 ) ,
          sigma ~ dunif( 0 , 50 )
     ) , data=d2 )
precis( m4.2 )


## R Code 4.32 - Section 4.3.6, Sampling from a QUAP
vcov( m4.1 )


## R Code 4.33 - Section 4.3.6, Sampling from a QUAP
diag( vcov( m4.1 ) )
cov2cor( vcov( m4.1 ) )


## R Code 4.34 - Section 4.3.6, Sampling from a QUAP
library(rethinking)
post <- extract.samples( m4.1 , n=1e4 )
head(post)


## R Code 4.35 - Section 4.3.6, Sampling from a QUAP
precis(post, hist=FALSE)
plot(post, asp=1)


## R Code 4.36 - Section 4.3.6, Sampling from a QUAP
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )


## R Code 4.37 - Section 4.4, Linear Prediction
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
plot( d2$height ~ d2$weight, asp = 1 )


## R Code 4.38 - Section 4.4.1, The Linear Model Strategy
set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )


## R Code 4.39 - Section 4.4.1, The Linear Model Strategy
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )


## R Code 4.40 - Section 4.4.1, The Linear Model Strategy
b <- rlnorm( 1e4 , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )


## R Code 4.41 - Section 4.4.1, The Linear Model Strategy
set.seed(2971)
N <- 100                   # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )


## R Code 4.42 - Section 4.4.2, Finding the Posterior Distribution
# load data again, since it's a long way back
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]


# define the average weight, x-bar
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
     alist(
          height ~ dnorm( mu , sigma ) ,
          mu <- a + b*( weight - xbar ) ,
          a ~ dnorm( 178 , 20 ) ,
          b ~ dlnorm( 0 , 1 ) ,
          sigma ~ dunif( 0 , 50 )
     ) , data=d2 )


## R Code 4.43 - Section 4.4.2, Finding the Posterior Distribution
m4.3b <- quap(
     alist(
          height ~ dnorm( mu , sigma ) ,
          mu <- a + exp(log_b)*( weight - xbar ),
          a ~ dnorm( 178 , 20 ) ,
          log_b ~ dnorm( 0 , 1 ) ,
          sigma ~ dunif( 0 , 50 )
     ) , data=d2 )


## R Code 4.44 - Section 4.4.3, Interpreting the Posterior Distribution
precis( m4.3 )


## R Code 4.45 - Section 4.4.3, Interpreting the Posterior Distribution
round( vcov( m4.3 ) , 3 )


## R Code 4.46 - Section 4.4.3.2 - Plotting Posterior Inference Against the Data
plot( height ~ weight , data=d2 , col=rangi2, asp=1 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )


## R Code 4.47 - Section 4.4.3.3 - Additing Uncertainty Around the Mean
post <- extract.samples( m4.3 )
post[1:5,]


## R Code 4.48 - Section 4.4.3.3 - Additing Uncertainty Around the Mean
N <- 10  # 10 data points from dataset.
dN <- d2[ 1:N , ]  # Get 10 rows with every column.
mN <- quap(
     alist(
          height ~ dnorm( mu , sigma ) ,
          mu <- a + b*( weight - mean(weight) ) ,
          a ~ dnorm( 178 , 20 ) ,
          b ~ dlnorm( 0 , 1 ) ,
          sigma ~ dunif( 0 , 50 )
     ) , data=dN )


## R Code 4.49 - Section 4.4.3.3 - Adding Uncertainty Around the Mean
# Extract 20 samples from the posterior.
post <- extract.samples( mN , n=20 )


# Display raw data and sample size.
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height", asp=1 )
mtext(concat("N = ",N))

# Plot the lines with transparency.
for ( i in 1:20 )
     curve( post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
            col=col.alpha("black",0.3) , add=TRUE )


## R Code 4.50 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
post <- extract.samples( m4.3 )  # Default to 1e4 samples.
mu_at_50 <- post$a + post$b * ( 50 - xbar )


## R Code 4.51 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )


## R Code 4.52 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
PI( mu_at_50 , prob=0.89 )


## R Code 4.53 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
mu <- link( m4.3 )  # Default 1e3 samples.
str(mu)  # Compact display the structure of arbitrary R object.


## R Code 4.54 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
#
# Define sequence of weights to compute predictions for on the horizontal axis.
weight.seq <- seq( from=25 , to=70 , by=1 )

# Use LINK to compute mu for each sample from posterior and for each weight in weights.seq.
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)


## R Code 4.55 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
plot( height ~ weight , d2 , type="n" )  # Use type="n" to hide raw data.

# Loop over samples and plot each mu value.
for ( i in 1:100 )
     points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )


## R Code 4.56 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
#
# Summarize the distribution of mu.
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )


## R Code 4.57 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
#
# Plot raw data with fading out points to make line and interval more visible.
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5), asp=1 )

# Plot the MAP line (aka the mean mu for each weight).
lines( weight.seq , mu.mean )

# Plot a shaded region for 89% PI.
shade( mu.PI , weight.seq )


## R Code 4.58 - Section 4.4.3.4 - Plotting Regression Intervals and Contours
post <- extract.samples(m4.3)
mu.link <- function(weight) post$a + post$b*( weight - xbar )
weight.seq <- seq( from=25 , to=70 , by=1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.CI <- apply( mu , 2 , PI , prob=0.89 )


## R Code 4.59 - Section 4.4.3.5 - Prediction Intervals
sim.height <- sim( m4.3 , data=list(weight=weight.seq) )
str(sim.height)


## R Code 4.60 - Section 4.4.3.5 - Prediction Intervals
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


## R Code 4.61 - Section 4.4.3.5 - Prediction Intervals
#
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5), asp=1 )  # Plot raw data.
lines( weight.seq , mu.mean )  # Draw maximum aposterior (MAP) line.
shade( mu.HPDI , weight.seq )  # Draw highest posterior density interval (HPDI) region for line.
shade( height.PI , weight.seq )  # Draw percentile (PI) region for simulated heights.


## R Code 4.62 - Section 4.4.3.5 - Prediction Intervals
sim.height <- sim( m4.3 , data=list(weight=weight.seq) , n=1e4 )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


## R Code 4.63 - Section 4.4.3.5 - Prediction Intervals
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply( weight.seq , function(weight)
     rnorm(
          n=nrow(post) ,
          mean=post$a + post$b*( weight - xbar ) ,
          sd=post$sigma ) )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


## R Code 4.64 - Section 4.5 - Curves from Lines
library(rethinking)
data(Howell1)
d <- Howell1


## R Code 4.65 - Curves from Lines
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
     alist(
          height ~ dnorm( mu , sigma ) ,
          mu <- a + b1*weight_s + b2*weight_s2 ,
          a ~ dnorm( 178 , 20 ) ,
          b1 ~ dlnorm( 0 , 1 ) ,
          b2 ~ dnorm( 0 , 1 ) ,
          sigma ~ dunif( 0 , 50 )
     ) , data=d )


## R Code 4.66 - Curves from Lines
precis( m4.5 )


## R Code 4.67 - Curves from Lines
weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


## R Code 4.68 - Curves from Lines
plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )


## R Code 4.69 - Curves from Lines
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
     alist(
          height ~ dnorm( mu , sigma ) ,
          mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
          a ~ dnorm( 178 , 20 ) ,
          b1 ~ dlnorm( 0 , 1 ) ,
          b2 ~ dnorm( 0 , 10 ) ,
          b3 ~ dnorm( 0 , 10 ) ,
          sigma ~ dunif( 0 , 50 )
     ) , data=d )


## R Code 4.70 - Curves from Lines
plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) , xaxt="n" )


## R Code 4.71 - Curves from Lines
at <- c(-2,-1,0,1,2)
labels <- at*sd(d$weight) + mean(d$weight)
axis( side=1 , at=at , labels=round(labels,1) )


## R Code 4.72
library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
precis(d)

## R Code 4.73
d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

## R Code 4.74
library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )

## R Code 4.75
plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )

## R Code 4.76
m4.7 <- quap(
     alist(
          D ~ dnorm( mu , sigma ) ,
          mu <- a + B %*% w ,
          a ~ dnorm(100,10),
          w ~ dnorm(0,10),
          sigma ~ dexp(1)
     ), data=list( D=d2$doy , B=B ) ,
     start=list( w=rep( 0 , ncol(B) ) ) )

## R Code 4.77
post <- extract.samples( m4.7 )
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

## R Code 4.78
mu <- link( m4.7 )
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )

## R Code 4.79
m4.7alt <- quap(
     alist(
          D ~ dnorm( mu , sigma ) ,
          mu <- a + sapply( 1:827 , function(i) sum( B[i,]*w ) ) ,
          a ~ dnorm(100,1),
          w ~ dnorm(0,10),
          sigma ~ dexp(1)
     ),
     data=list( D=d2$doy , B=B ) ,
     start=list( w=rep( 0 , ncol(B) ) ) )


