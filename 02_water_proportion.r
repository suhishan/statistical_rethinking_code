library(rethinking)


# Finding probabilities for a simple four sided globe ---------------------

sample <- c("W","L","W","W","W","L","W","L","W")
W <- sum(sample == "W")
L <-  sum(sample == "L")
p <- seq(0,1, 0.25)
ways <- sapply(p, function(q) (q*4)^W * ((1-q)*4)^L)
prob <- ways/sum(ways)
cbind(p,ways, prob)
barplot(prob)


# Testing -----------------------------------------------------------------

#function for tossing a globe covered by p water N times

sim_globe <- function(p = 0.7, N = 9) {
  sample(c("W","L"), size = N, prob = c(p, 1-p), replace = TRUE)
}

#testing extremes
sim_globe(p = 1, N = 11)
sum(sim_globe(p = 0.5, N = 1e4) == "W") / 1e4


# Code the estimator ------------------------------------------------------



computer_posterior <- function(the_sample, poss = seq(0,1,0.2)) {
  W <- sum(the_sample == "W")
  L <-  sum(the_sample == "L")
  ways <- sapply(poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways/sum(ways)
  bars <- sapply(post, function(q) make_bar(q))
  data.frame(poss, ways, post = round(post, 3), bars)
}

computer_posterior(sim_globe())


# Sampling the posterior --------------------------------------------------

post_samples <- rbeta(1e3, 6+1, 3+1)
dens(post_samples)
curve(dbeta(x, 6+1, 3+1), add = TRUE )



dbinom(2, 3, 0.75)


# Grid Approximation ------------------------------------------------------

#All parameter grids. Here we have taken 20 grids.
p_grid <-  seq(0, 1, length.out = 20)

#Computing the prior
prior <- exp( -5*abs( p_grid - 0.5 ) )

#Computing the likelihood for the sample of 6 Ws and 3 Ls.
likelihood <- dbinom(6, 9, prob = p_grid)

#Computing Unstandardised posterior
ustd.posterior <- prior * likelihood

#Standardize the posterior.
posterior <- ustd.posterior / sum(ustd.posterior)
plot(p_grid, posterior, type = "b")


# Quadratic Approximation (Also Gaussian Approximation) -------------------------------------------------

globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p),
    p ~ dunif(0, 1)
  ),
  data = list(W = 6, L = 3)
)

precis(globe.qa)


