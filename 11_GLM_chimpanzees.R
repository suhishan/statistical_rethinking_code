library(rethinking)
library(tidyverse)

data("chimpanzees")
d <- chimpanzees
?chimpanzees
d <- d %>% mutate(
  treatment = 1 + prosoc_left + 2*condition
)


# Determining priors in GLMs is essential. Let's do it. -------------------

m11.1 <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10) # a flat prior
  ), data = d
)

prior <- extract.prior(m11.1, n = 1e4)
p <- inv_logit(prior$a) # we need to convert the parameter into the outcome scale.
dens(p, adj = 0.1)


# Let's get our hands dirty: Hamiltonian Monte Carlo ----------------------

dat_list <- list(
  pulled_left = d$pulled_left,
  treatment = as.integer(d$treatment),
  actor = d$actor
)

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ), data = dat_list, chains=4, cores = 4, log_lik = TRUE
)
post <- extract.samples(m11.4)
p_left <- inv_logit(post$a)
plot(precis(as.data.frame(p_left)))
plot(precis(m11.4, depth = 2, pars="b"))

# what we are looking for is differences so:

diff <- list(
  db13 = post$b[,1] - post$b[,3],
  db24 = post$b[,2] - post$b[,4]
)
plot(precis(diff))



# Posterior Predictions Check ---------------------------------------------

# summarize prop of left pulls for each actor in each treatme

pl <- d %>% group_by(actor, treatment) %>% 
  summarize(
   prop_pl= mean(pulled_left)
  ) %>% pivot_wider(names_from = treatment, values_from = prop_pl)
View(pl)

