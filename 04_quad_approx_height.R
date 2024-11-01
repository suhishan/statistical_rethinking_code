library(rethinking)
library(MASS)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18,  ]


# Specify the definition model into a list --------------------------------
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20), # plausible prior
  sigma ~ dunif(0, 50)
)

# Fit the model.

m4.1 <- quap(flist, data = d2)
precis(m4.1)

# Note: These are Gaussian Approximations for each parameter's marginal distributions.
# i.e. plausibility of each value of mu, after averaging over plausibilities of
# each value of sigma, by a Gaussian dist with mean 154.61 and s.d. 0.41


# Changing priors: Making very specific priors ----------------------------

m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1), # very specific sd prior
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

precis(m4.2)


# Sampling from the quap --------------------------------------------------

vcov(m4.1)
diag(vcov(m4.1)) # Vector of variances.
cov2cor(vcov(m4.1)) #a correlation matrix that tells us how changes in any para-
# meter lead to correlated changes in others.

# Extracting samples from a multi-dimensional Gaussian.
post <- extract.samples(m4.1, n = 1e4)

# Another way
post.2 <- mvrnorm(n = 1e4, mu = coef(m4.1), Sigma = vcov(m4.1))
plot(post.2)
