library(rethinking)
library(tidyverse)
library(conflicted)
library(dagitty)


# Question number 1 -------------------------------------------------------

data("foxes") # load data
d <- foxes
d <- d %>% rename(
  A = area,
  F = avgfood,
  G = groupsize,
  W = weight
)

d <- d %>% mutate(
    A = standardize(A),
    F = standardize(F),
    W = standardize(W), 
    G = standardize(G)
  )

# let's make the dag
fox_dag <- dagitty( "dag{F -> G; F -> W; G -> W; A -> F}" )
impliedConditionalIndependencies(fox_dag)
precis(d$F)
# Causal influence of Area on avgfood.
m.AF <- quap(
  alist(
    F ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 1),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m.AF)


# sample from the priors to see all the possible lines.
prior <- extract.prior(m.AF)
A_seq <- seq(-2.5,2.5, length.out = 30)
mu <- link(m.AF, post = prior, data = list(A=A_seq))

plot(NULL, xlab = "Area(std)", ylab = "Average Food (std)", xlim = c(-2, 2), ylim = c(-2.5, 2.5))
for (i in 1: 50) lines(A_seq, mu[i,])

# let's plot the posterior.

post <- link(m.AF, data = list(A = A_seq))
mu <- apply(post, 2, mean)
PI <- apply(post, 2, PI)
plot(F ~ A, data = d, xlab = "Area (std)",
     ylab = "Average Food(std)",
     main = "Causal Effect of Area on Average Food available for Foxes.",
     sub = " The shaded area is 89% confidence intervals.")
lines(A_seq, mu)
shade(PI, A_seq)


# Question number 2 -------------------------------------------------------
precis(d$F)
# Total Causal Effect of Adding food to a foxes' weight.
m.FW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bF * F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m.FW)
# Let's simulate an intervention of Food.
F_seq <- seq(-2.5, 2.5, length.out = 30)
sim_dat <- data.frame(F = F_seq)
s <- sim(m.FW, data = sim_dat, vars = "W")

plot(F_seq, colMeans(s), xlab = "Manipulated F", ylab = "Counterfactual Weights", ylim = c(-2,2))
shade(apply(s, 2, PI), sim_dat$F)


# Question number 3 -------------------------------------------------------

m.FW.direct <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bF * F + bG * G,
    sigma ~ dexp(1),
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    bG ~ dnorm(0, 0.5)
  ),
  data = d
)
precis(m.FW.direct)
