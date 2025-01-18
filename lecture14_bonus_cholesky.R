N <- 1e4
sigma1 <- 2
sigma2 <- 0.5
rho <- 0.6

# independent z-scores
z1 <- rnorm(N)
z2 <- rnorm(N)

#use cholesky to blend in correlation
a1 <- z1 * sigma1
a2 <- (rho*z1 + sqrt(1-rho^2)*z2) * sigma2
cor(a1,a2)
