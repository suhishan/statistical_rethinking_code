library(rethinking)
library(tidyverse)
library(conflicted)

N <- 1e3 # number of synthetic applicants.

G <- sample(1:2, size = N, replace = TRUE) # sample genders 1 and 2

D <- rbern(N, ifelse(G == 1, 0.3, 0.8))+1 # gender 1 much more likely to
# apply to department 1 and gender 2 to 2.

#matrix of acceptance rate [dept, gender]. the matrix is made so that
# gender 1 faces structural discrimination from both departments.
accept_rates <- matrix(c(0.05, 0.2, 0.2, 0.3), nrow = 2)

A <- rbern(N, accept_rates[D,G])


# Let's run two models. ---------------------------------------------------

# Total effect of Gender(G) on admission acceptance(A)
dat_sim = list(A = A, G = G, D = D)
m1 <- ulam(
  alist(
    A ~ dbern(p),
    logit(p) <- a[G],
    a[G] ~ dnorm(0,1)
  ), data = dat_sim, chains = 4, cores = 4
)

# Direct effect of Gender on Admissions stratifying by department(D)
m2 <- ulam(
  alist(
    A ~ dbern(p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ dnorm(0,1)
  ), data = dat_sim, chains = 4, cores = 4
)
precis(m2, depth = 3)
inv_logit(coef(m2))

