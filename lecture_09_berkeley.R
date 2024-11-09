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


# Let's use this in the real data for UCBerkeley --------------------------

data("UCBadmit")
d <- UCBadmit

d <- as.list(d %>% mutate(
  A = admit,
  D = as.integer(dept),
  G = ifelse(applicant.gender == "female",1,2),
  N = applications
) %>% select(A,D,G,N))

# total causal effect.

mG <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <- a[G],
    a[G] ~ dnorm(0, 1)
  ), data = d, chains = 4, cores = 4
)

# direct effect after including stratification for departments.

mGD <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ dnorm(0,1)
  ), data = d, chains = 4, cores = 4
)

# Let's plot the constrasts for the direct effect.

postG <- extract.samples(mG)
View(postG$a)
diffG <- inv_logit(postG$a[,1]) - inv_logit(postG$a[,2])
dens(diffG, lwd = 1, col = 2, xlab = "Gender contrasts (in terms of probability")


# let's the plot the posterior contrasts for the second model department wise.
postGD  <- extract.samples(mGD)
diffGD <- sapply(1:6, function(i)
  inv_logit(postGD$a[,1,i]) - inv_logit(postGD$a[,2,i]))
plot(NULL, xlim = c(-0.2, 0.3), ylim = c(0,25),
     ylab = "Density",
     xlab = "Gender Contrasts (probability)")
for (i in 1:6) {
  dens(diffGD[,i], add = TRUE, col = i+1, lwd = 2)
}
abline(v = 0, lty = 3)




# Simulating an intervention ----------------------------------------------

# Assuming the gender perception of the referees change.
total_apps <- sum(d$N)

apps_per_dept <- sapply(1:6, function(i)
  sum(d$N[d$D == i]))

# simulating as if all apps were from women
# We have simulated the intervention where the data is in long form i.e.
# in the form of Bernoulli trials and not binomial ones as used in the
# model above. Not to worry though, the results will be the same. 
p_G1 <- link(mGD, data = list(
  D = rep(1:6, times = apps_per_dept),
  G = rep(1, total_apps),
  N = rep(1, total_apps)
))

p_G2 <- link(mGD, data = list(
  D = rep(1:6, times = apps_per_dept),
  G = rep(2, total_apps),
  N = rep(1, total_apps)
))

dens(p_G1 - p_G2)
