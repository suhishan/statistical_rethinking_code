library(rethinking)


# 1. First we need the posterior ---------------------------------------------

p_grid <- seq(0,1, length.out = 1000)
prob_p <- runif(1000, 1, 1)
#likelihood
prob_data <- dbinom(6, size = 9, prob = p_grid)
#posterior i.e. the probability of proportion p (parameter) conditional on data
posterior <- prob_data * prob_p
#standardizing the posterior
posterior <- posterior / sum(posterior)
plot(p_grid, posterior)


# 2. Now we need to take samples from the posterior -----------------------
#Posterior is a bucket full of parameter values whose relative plausibility
#is given by the posterior distribution

#Therefore, if we take 10,000 samples, the individual values of p will
#appear in our samples in proportion to the posterior plausibility of each
#value.

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
dens(samples)



# Different types of estimates --------------------------------------------


#Posterior probability that the proportion of water is less than 0.5
#one way
sum(posterior[p_grid < 0.5])
#or
sum(samples < 0.5) / length(samples)
sum(samples > 0.5 & samples < 0.75) / length(samples)


# Probability mass and compatibility intervals (generally called
# confidence intervals.) We can call them percentile intervals.

quantile(samples, 0.8)

# Highest Posterior Density Interval: Narrowest interval with the
# given probability mass.

HPDI(samples, prob = 0.5)
PI(samples, prob = 0.5)

# Point Estimates

# 1. Reporting parameter value with the highest posterior probability. Also called
# maximum a posteriori (MAP)

p_grid[which.max(posterior)]
chainmode(samples)



# Loss function - minimizing expected loss --------------------------------
# Suppose my decision is that I bet that p = 0.5 Water.
sum(posterior * abs(0.5 - p_grid)) #weighted average loss where weight is posterior plausibility.

# Doing this for every decision like for eg. 0.5
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
plot(p_grid,loss, type = "l")

# Parameter value that minimizes the loss
p_grid[which.min(loss)]
median(samples)



# Sampling to simulate prediction -----------------------------------------

dbinom(0:2, size = 2, prob = 0.7)
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
table(dummy_w)/1e5
simplehist(dummy_w)


# Sampling to see if the model is accurate
w <- rbinom(1e4, size = 9, prob = 0.6)

#Now for all samples from the posterior in accordance with their plausbility.
w_all <- rbinom(1e4, size = 9, prob = samples)
simplehist(w_all)
