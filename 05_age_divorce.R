library(rethinking)
library(tidyverse)
library(dagitty)

data("WaffleDivorce")
d <- WaffleDivorce

# Standardize the predictors and the outcome.

d <- d %>% 
  mutate(
    D = standardize(Divorce),
    M = standardize(Marriage),
    A = standardize(MedianAgeMarriage)
  )

# Quadratic Approximation of the Posterior.
m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

# let's look at the priors.

precis(m5.1)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A = c(-2, 2)) )

plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2), xlab = "Median Age at Marriage",
     ylab = "Divorce Rate")
for (i in 1: 50) lines(c(-2,2), mu[i, ])

# Computing the psoterior mean with its percentage intervals alongside the data.
A_seq <- seq(-3, 3.2, length.out = 30)
mu.post <- link(m5.1, data = list(A = A_seq)) # Extracting mu for each age.
mu.mean <- apply(mu.post, 2, mean)
mu.PI <- apply(mu.post, 2, PI)

# Plotting everything.
plot(D ~ M, data = d, xlab = "Median Age at Marriage", ylab = "Divorce Rate", col = rangi2)
lines(A_seq, mu.mean)
shade(mu.PI, A_seq)



# Posterior Distribution for Multiple Regression Model. -------------------

m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    sigma ~ dexp(1),
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5)
  ),
  data = d
)

precis(m5.3)
plot(coeftab(m5.1, m5.3))



# As a sidenote: we can also run simulations ------------------------------

N <- 50 # 50 states.
age <- rnorm(N)
mar <- rnorm(N, -age) # A -> D
div <- rnorm(N, age) # A -> M



# Posterior Predictive Plots ----------------------------------------------

# extract the linear mean from the posterior.
mu <- link(m5.3)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


# simulate observations
D_sim <- sim(m5.3, n=1e4)
D_PI <- apply(D_sim, 2, PI)

# Plotting predicted vs observed.
plot(mu.mean ~ d$D, ylim = range(mu.PI), xlab = "Observed divorce", ylab = "Predicted divorce")
abline(a = 0, b=1, lty = 2)
for (i in 1:nrow(d)) lines(rep(d$D[i],2), mu.PI[,i])

# identifying outliers.
identify( x=d$D , y=mu.mean , labels=d$Loc )


# Simulating spurious correlations ----------------------------------------

N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <-  rnorm(N, x_real)
data <- data.frame(x_real, x_spur, y)
pairs(data)


# Counterfactual Plots ----------------------------------------------------

m5.3_A <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 ),
    ## A -> M
    M ~ dnorm( mu_M , sigma_M ),
    mu_M <- aM + bAM*A,
    aM ~ dnorm( 0 , 0.2 ),
    bAM ~ dnorm( 0 , 0.5 ),
    sigma_M ~ dexp( 1 )
  ) , data = d )


precis(m5.3_A)

# Now, let's manipulate A.
A_seq <- seq(from = -2, to = 2, length.out = 30)

# Now, use sim, to simulate 'observations' from model m5.3_A.
sim_dat <- data.frame(A = A_seq)
s <- sim(m5.3_A, data = sim_dat, vars = c("M", "D"))

# Plotting actual against counterfactual.
plot(sim_dat$A, colMeans(s$D), ylim = c(-2,2), 
     xlab = "manipulated A", ylab = "counterfactual D", type = "l")
shade(apply(s$D, 2, PI), sim_dat$A)
  