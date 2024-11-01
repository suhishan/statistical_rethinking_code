library(rethinking)


# Posterior Distribution for 4 Water and 11 Land --------------------------

# parameter values
N <- 1e3
p_grid <- seq(0,1, length.out = N)
# prior
prior <- runif(N, 1, 1)

# likelihood
prob_data <- dbinom(4, size = 15, prob = p_grid)

# computing the posterior
posterior <- prob_data * prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior, type = "l")

# Compute the posterior predictive for next five tosses -------------------
# First generate samples using posterior plausibility
samples <- sample(p_grid, size = N, prob = posterior, replace =TRUE)

# Use these samples to extract posterior predictive
posterior_pred <- rbinom(N, size = 5, prob = samples)
simplehist(posterior_pred)

# Probability of 3 or more landing of Water in 5 tosses using posterior pred.
sum(posterior_pred >=3) / N
x <- pbinom(3, size = 5, prob = p_grid, lower.tail = FALSE)

