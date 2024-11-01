library(rethinking)
library(tidyverse)
library(conflicted)
# Making our own dataset:

sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

d <- d %>% mutate(
  mass_std = standardize(mass),
  brain_std = brain/ max(brain)
)

m7.1 <- quap(
  alist(
    brain_std ~ dnorm(mu, exp(log_sigma)),
    mu <- a + b*mass_std,
    a ~ dnorm(0.5, 1),
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ), data = d
)
precis(m7.1)


# Computing Rsquare -------------------------------------------------------

set.seed(12)


R2_is_bad <- function(quap_fit) {
  s <- sim(quap_fit, refresh = 0) # simulating posterior predictive distribution for each observation (predictor i.e. brain mass).
  r <- apply(s, 2, mean) - d$brain_std # calculating residuals
  1 - var2(r)/var2(d$brain_std)
}


# Log Pointwise predictive density (LPPD) ----------------------------------------

sum(lppd(m7.1, n = 1e4))
