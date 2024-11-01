library(rethinking)
library(tidyverse)
data(Howell1)
d <- Howell1
d <- d[d$age >=18, ]

# Synthetic Data for Male and Female Categories ---------------------------

#S = 1 is female; 2 is male
sim_HW <- function(S, b, a) {
  N <- length(S)
  H <- ifelse(S == 1, 150, 160) + rnorm(N, 0, 5)
  W <- a[S] + b[S]*H + rnorm(N, 0, 5)
  data.frame(S, H, W)
}

S <- rbern(100) + 1
dat <- sim_HW(S, b = c(0.5, 0.6), a = c(0,0))
plot(W ~ H, data = dat, col = S)



# Testing the priors for the causal effect of sex on weight ---------------

# Simulate female weights
S <- rep(1, 100)
dat.F <- sim_HW(S, b = c(0.5, 0.6), a = c(0,0))

# Simulate male sample
S <- rep(2, 100)
dat.M <- sim_HW(S, b = c(0.5, 0.6), a = c(0,0))

# Average Difference in weights

mean(dat.M$W - dat.F$W)


# The Posterior Distribution for the Synthetic Sample ----------------------------------------------
S <- rbern(100) + 1
dat <- sim_HW(S, b = c(0.5, 0.7), a = c(0,0))

# estimate posterior

m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60, 10),
    sigma ~ dunif(0, 10)
  ),
  data = dat
)

precis(m_SW, depth = 2)


# Posterior for the real data ---------------------------------------------
datum <- list(
  W = d$weight,
  S = d$male + 1 #1 for female 2 for male.
)

# Mu is not a linear function of height here but just intercept only model.
# Overall causal effect of sex on weight (both itself and through height)
m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60, 10),
    sigma ~ dunif(0, 10)
  ),
  data = datum
)

precis(m_SW, depth = 2)

# Extracting samples for the posterior and drawing densities. -------------

post <- extract.samples(m_SW)
dens(post$a[,1], col = 2, lwd = 2, xlim = c(39, 50))
dens(post$a[,2], col =3, lwd = 2, add = TRUE)

# Extracting actual weight samples.

W1 <- rnorm(1e3, post$a[,1], post$sigma)
W2 <- rnorm(1e3, post$a[,2], post$sigma)
dens(W1)
dens(W2, add = TRUE)


# Mean contrast: Always be contrasting. -----------------------------------

mu_constrast <- post$a[,2] - post$a[,1]
dens(mu_constrast)



# Direct causal effect of sex on weight ------------------
# Before, all effect of sex on weight, came through height i.e. sex affected
# height which in turn affected weight.
# Now, we aim to see the direct effect of sex on weight. For that, we need to
# stratify by H i.e. for people with same height, what is the effect of sex
# on weight?

S <- rbern(100)+1
dat <- sim_HW(S, b = c(0.5, 0.5), a = c(0,10))


datum <- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height),
  S = d$male + 1#  1 = female, 2 is male
)

m_SHW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S] + b[S]*(H-Hbar),
    a[S] ~ dnorm(60, 10),
    b[S] ~ dunif(0,1),
    sigma ~ dunif(0, 10)
  ),
  data = datum
)

precis(m_SHW, depth = 2)


# Difference (in categories) in expected weight at each height ------------
# Compute Posterior predictive for women and men and find the difference.

