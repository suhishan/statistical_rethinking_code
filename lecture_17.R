library(rethinking)
library(tidyverse)
library(conflicted)


# Parental and Child Income Example ---------------------------------------

N <- 500
P <- rnorm(N) # real parental income.
C <- rnorm(N, 0*P) # real child income independent of real income.
Pstar <- rnorm(N, 0.8*P+0.2*C) # observed parental income influenced by real child income.

mCP <- ulam(
  alist(
    C ~ normal(mu, sigma),
    mu <- a + b * Pstar,
    a ~ normal(0,1),
    b ~ normal(0,1),
    sigma ~ exponential(1)
  ), data = list (Pstar= Pstar, C = C), chains = 4
)

post <- extract.samples(mCP)


# The Divorce and Marriage example ----------------------------------------

data("WaffleDivorce")
d <- WaffleDivorce
dat <- list(
  D_obs = standardize(d$Divorce),
  D_sd = d$Divorce.SE/sd(d$Divorce),
  M = standardize(d$Marriage),
  A = standardize(d$MedianAgeMarriage),
  N = nrow(d)
)

# Model 1 where we look at measurement errors in Divorce Rates.
m15.1 <- ulam(
  alist(
    # model for D that is observed,
    D_obs ~ normal(D_true, D_sd),
    
    # model for D_true, which is now a parameter.
    vector[N]:D_true ~ normal(mu, sigma),
    mu <- a + bA*A + bM * M,
    c(bA,bM) ~ normal(0,0.5),
    a ~ normal(0, 0.2),
    sigma ~ exponential(1)
  ), data = dat, chains = 4, cores = 4
)
precis(m15.1, depth = 3)


# let's compare D_true with D_obs
plot(dat$A, dat$D_obs, xlab = "Age of Marriage(std)",
     ylab = "Divorce Rate(std)",
     col = "black",
     pch = 1, 
     lwd = 2)

post <- extract.samples(m15.1)
D_mu <- apply(post$D_true, 2, mean)
points(dat$A, D_mu, col = "red",
       lwd = 2, pch = 1)

# Model 2 where we look at measurement errors in Marriage Rate.
dat2 <- list(
  D_obs = standardize(d$Divorce),
  D_sd = d$Divorce.SE/sd(d$Divorce),
  M_obs = standardize(d$Marriage),
  M_sd = d$Marriage.SE/sd(d$Marriage),
  A = standardize(d$MedianAgeMarriage),
  N = nrow(d)
)

m15.2 <- ulam(
  alist(
    # model for D that is observed,
    D_obs ~ normal(D_true, D_sd),
    
    # model for D_true, which is now a parameter.
    vector[N]:D_true ~ normal(mu, sigma),
    mu <- a + bA*A + bM * M_true[i],
    c(bA,bM) ~ normal(0,0.5),
    a ~ normal(0, 0.2),
    sigma ~ exponential(1),
    
    # model for M that is observed,
    M_obs ~ normal(M_true, M_sd),
    
    # regression Model for M_true
    vector[N]:M_true ~ normal(nu, tao),
    nu <- aM + bAM * A,
    aM ~ normal(0,0.2),
    bAM ~ normal(0, 0.5),
    tao ~ exponential(1)
  ), data = dat2, chains = 4, cores = 4
)
precis(m15.2, depth = 3)
