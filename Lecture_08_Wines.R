library(rethinking)
library(tidyverse)

data("Wines2012")
d <- Wines2012
dat <- d %>% mutate(
  S = standardize(score),
  J = as.numeric(judge),
  W = as.numeric(wine),
  X = ifelse(wine.amer == 1,1,2),
  Z = ifelse(judge.amer == 1,1,2),
) %>% select(S,J,W,X,Z)

mQ <- ulam(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- Q[W],
    Q[W] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dat, chains = 4, cores = 4
)

precis(mQ, 2)
