library(rethinking)
library(tidyverse)
library(dagitty)


data("milk")
d <- milk
dcc <- d %>% mutate(
  K  = standardize(kcal.per.g),
  N = standardize(neocortex.perc),
  M = standardize(mass)
) %>% filter(
  !is.na(K) & !is.na(N) & ! is.na(M)
)

m5.5 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bN*N,
    sigma ~ dexp(1),
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5)
  ), data = dcc
)

# Are the priors reasonable?

prior <- extract.prior(m5.5)
mu <- link(m5.5, post = prior, data = list(N = c(-3, 3)) )

plot(NULL, xlim = c(-2,2), ylim = c(-2,2))
for (i in 1:50) lines(c(-2,2), mu[i,])

# Now, let's look at the posterior.
precis(m5.5)

# Let's plot the posterior.
xseq <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu <- link(m5.5, data = list(N = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# plotting
plot(K ~ N, data = dcc)
lines(xseq, mu_mean)
shade(mu_PI, xseq)


# case of a masked relationship. 
pairs(~ K + M + N, dcc)


# Markov equivalence sets
dag5.7 <- dagitty( "dag{
M -> K <- N
M -> N }" )
coordinates(dag5.7) <- list( x=c(M=0,K=1,N=2) , y=c(M=0.5,K=1,N=0.5) )
MElist <- equivalentDAGs(dag5.7)
drawdag(MElist)
