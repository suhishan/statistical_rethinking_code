library(rethinking)
library(tidyverse)
library(splines)

data("Howell1")
d <- Howell1

plot(height ~ weight, d)

d <- d %>% 
  mutate(
    weight_s = (weight - mean(weight))/sd(weight),
    weight_s2 = weight_s ^ 2
  )

m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_s + b2 * weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

precis(m4.5)


# Plotting ----------------------------------------------------------------

weight.seq <- seq(-2.2, 2.2, length.out = 30)
pred_dat <- list(weight_s = weight.seq, weight_s2  = weight.seq ^ 2)

mu <- link(m4.5, pred_dat)

mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

# Simulate posterior observations

sim.height <- sim(m4.5, pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)


# Now plotting it

plot(height ~ weight_s, d)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)



# Splines -----------------------------------------------------------------
# B-Splines

data("cherry_blossoms")
c <- cherry_blossoms
precis(c)

plot(doy ~ year, c)


# Let's make splines ------------------------------------------------------
c <- c[complete.cases(c$doy), ]
num_knots <- 15 # specifying the number of knots for the spline
knot_list <- quantile(c$year, probs = seq(0,1,length.out = num_knots))

# Constructing Basis functions

B <- bs(c$year,
        knots = knot_list[-c(1,num_knots)],
        degree = 3,
        intercept = TRUE)
knots <- knot_list[-c(1,num_knots)]
View(B)

# Plotting Basis Functions
plot( NULL , xlim=range(c$year) , ylim=c(0,1) , xlab="year" , ylab="basis" )
for ( i in 1:ncol(B) ) lines( c$year , B[,i] )


# Spline model Babey
m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(D = c$doy, B = B),
  start = list(w = rep(0, ncol(B)))
)

# Posterior Predictions
post <- extract.samples(m4.7)
w <- apply(post$w, 2, mean)
plot( NULL , xlim=range(c$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( c$year , w[i]*B[,i] )


# 97% Posterior interval for mu at each year.

mu <- link(m4.7)
mu_PI <- apply(mu,2,PI,0.97)
plot( c$year , c$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , c$year , col=col.alpha("black",0.5) )
