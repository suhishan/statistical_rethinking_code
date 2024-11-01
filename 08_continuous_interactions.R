library(rethinking)
library(tidyverse)
library(conflicted)

data(tulips)
d <- tulips

d <- d %>% mutate(
  water_cent = water - mean(water),
  shade_cent = shade - mean(shade),
  blooms_std = blooms / max(blooms)
)

# Model without interactions:

m8.4 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent,
    a ~ dnorm(0.5, 0.25),
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8.4)

# Let's fit a model with interaction between water and shade.


m8.5 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent,
    a ~ dnorm(0.5, 0.25),
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    bws ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), data = d
)

precis(m8.5)
a <- c(5,6,7)


# Let's plot the interaction in a triptysch plot:

par(mfrow = c(1,3)) #Three plots in one row.

for (w in -1:1){
  index <- which(d$water_cent == w)
  plot(d$shade_cent[index], d$blooms_std[index], xlim = c(-1,1), ylim = c(0,1),
       xlab = "shade", ylab = "blooms")
  mtext(paste("Water",w))
  mu <- link(m8.5, data = data.frame(water_cent = w, shade_cent = -1:1))
  for(i in 1:20) lines(-1:1, mu[i,])
}
  