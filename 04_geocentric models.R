library(rethinking)


# Simulating Normal by addition ------------------------------------------



# using uniform distribution
pos <- replicate(1e3, sum(runif(16, -1, 1)))
dens(pos)

# using binomial distribution
pos_binom <- replicate(1e3, sum(2*rbinom(1000, 1, 0.5)-1))
dens(pos_binom)

curve(dnorm(x, 0, 31.9), add = TRUE)


# Simulating Normal by multiplication -------------------------------------

prod(1 + runif(12, 0, 0.1))

growth <- replicate(1e4, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE, xlab = "Growth", col = "blue")

# Comparison with huge growths to see its convergence
big_growth <- replicate(1e4, prod(1 + runif(12, 0, 0.5)))
dens(big_growth, norm.comp = TRUE, col = "blue")
# Multiplications of large deviations don't produce gaussian distributions
# because large deviations multiplied produce even larger ones and therefore
# skew the density.


# Normal by log multiplication --------------------------------------------

log_big_growth <- replicate(1e4, log(prod(1 + runif(12, 0, 0.5))))
dens(log_big_growth, norm.comp = TRUE)
