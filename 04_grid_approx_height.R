library(rethinking)
library(tidyverse)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,]


# Checking on priors ------------------------------------------------------

curve(dnorm(x, 178, 20), from = 100, to = 250) # prior for mu
curve(dunif(x, 0, 50), from  = -10, to = 60) #prior for S.D.

# prior predictive for height
# expected distribution of heights averaged over the prior.
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h, norm.comp = TRUE)




# Grid approximation to compute Posterior Example for clarification ----------------------------

mu.list <- seq(from = 150, to = 160, length.out = 100) # a vector of mus
sigma.list <- seq(from = 7, to = 9, length.out  = 100)  # a vector of sigmas
post <- expand.grid(mu = mu.list, sigma = sigma.list) # making a matrix i.e. 100*100

# Computing log-likelihood, the garden of forking data and just counting.
# What parameter could've generated the height
post$LL <- sapply(1:nrow(post),
                  function(i) sum(dnorm(d2$height, post$mu[i],post$sigma[i], log = TRUE )))


#Computing the posterior i.e. product of likelihood and prior
# In this case summation because we are working on the log-scale.
# Posterior ~ Likelihood * Prior
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)

# Normalizing to make the product a probability
post$prob <- exp(post$prod - max(post$prod)) # Scaling to counter rounding errors.
# So the probabilities aren't exactly probabilities but relative posterior probabilities.

contour_xyz( post$mu , post$sigma , post$prob ) #contour map
image_xyz( post$mu , post$sigma , post$prob ) #heat map


# Sampling from the posterior
# Sampling out row numbers based on posterior probability
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]
dens(sample.mu)
# plotting to see
plot(sample.mu, sample.sigma, cex = 1, pch = 16, col=col.alpha(rangi2,0.1))

#Summarizing
dens(sample.sigma)
plot(density(sample.mu))
PI(sample.mu)


