library(rethinking)
library(tidyverse); library(conflicted)

data("reedfrogs")
d <- reedfrogs

d$tank <- 1:nrow(d)
dat <- list(
  S = d$surv,
  D = d$density,
  T = d$tank
)


# Question number 1 -------------------------------------------------------

# Prior Predictive Distribution of Tank Survival probabilities.

N <- 10000
width <- c(0.1,1,10)
plot(NULL, xlim = c(0,1), ylim = c(0,3))
for (i in 1:3) {
  a_bar <- rnorm(N, 0, 1)
  sigma <- rexp(N,width[i])
  a <- rnorm(N,a_bar, sigma)
  p <- inv_logit(a)
  dens(p, add = TRUE, col = 1+i, lwd = 2, adj = 0.1)
}


# Question number 2 -------------------------------------------------------

# First lets start with the simple varying effects model.

mST <- ulam(
  alist(
    S ~ binomial(D, p),
    logit(p) <- a[T],
    a[T] ~ normal(a_bar, sigma),
    a_bar ~ normal(0, 1.5),
    sigma ~ exponential(1)
  ), data = dat, chains = 4, log_lik = TRUE
)

# Making all four conditions, each for various combinations of size and predator.
dat$G = ifelse(d$size == "big",1,0)
dat$P = ifelse(d$pred == "pred",1,0)
dat$treatment = 1 + dat$G + 2*dat$P


# The model which looks at the effect of size and predators on tadpoles survival.

mTreat <- ulam(
  alist(
    S ~ binomial(D, p),
    logit(p) <-  a[T] + bT[treatment],
    a[T] ~ normal(a_bar, sigma),
    a_bar ~ normal(0, 1.5),
    sigma ~ dexp(1),
    bT[treatment] ~ normal(0,1)
  ), data = dat, chains = 4, log_lik = TRUE
)
precis(mTreat, 3, pars = c("bT", "sigma"))
# Checking the posterior distributions for bT.
post <- extract.samples(mTreat)
plot(NULL, xlim  =c(0,1), ylim = c(0,5))
for (i in 1:4) {
  dens(inv_logit(post$bT[,i]), add = TRUE, col = 1+i, lwd = 2)
}


# Question number 3 : The effect of density on survival. ------------------

# keeping density as a continuous metric. 
dat$Pr = dat$P + 1
dat$Dstd = standardize(dat$D)
mDens <- ulam(
  alist(
    S ~ binomial(D, p),
    logit(p) <-  a[T] + bD[Pr] * Dstd,
    a[T] ~ normal(a_bar, sigma),
    a_bar ~ normal(0, 1.5),
    sigma ~ dexp(1),
    bD[Pr]~ normal(0,1)
  ), data = dat, chains = 4, log_lik = TRUE
) 

precis(mDens, depth = 3, pars =c("bD", "sigma"))
dat$Dgroup <- factor(dat$D, levels = c(10,25,35), labels = c(1,2,3), ordered = TRUE)
# Keeping density as an ordered monotonic predictor.
dat$b <- rep(2,2)
mDensOrd <- ulam(
  alist(
    S ~ binomial(D,p),
    logit(p) <- a[T] + bD[Pr] * sum(delta_j[1:Dgroup]),
    a[T] ~ normal(a_bar, sigma),
    a_bar ~ normal(0, 1.5),
    sigma ~ dexp(1),
    bD[Pr] ~ normal(0, 0.5),
    vector[3] : delta_j <<- append_row(0, delta),
    simplex[2] : delta ~ dirichlet(b)
  ), data = dat, chains = 4, log_lik = TRUE
)
precis(mDensOrd,3,pars = c("bD", "sigma", "delta"))
