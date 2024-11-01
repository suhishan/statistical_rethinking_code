library(rethinking)
d <- sim_happiness(seed = 1977, N_years = 1000)
d2 <- d[d$age > 17,]
d2$A <- (d2$age - 18)/ (65 - 18)

d2$mid <- d2$married + 1 #1 is single 2 is married.

# posterior approximation by conditioning on a collider.
m6.9 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a[mid] + bA * A,
    a[mid] ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = d2
)
precis(m6.9, depth = 2) # shows a spurious negative association between age and happiness.

# Without conditioning on the collider.
m6.10 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = d2
)
precis(m6.10)
