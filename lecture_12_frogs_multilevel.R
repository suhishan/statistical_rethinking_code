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

# First hierarchical model. Partial Pooling using sigma. 
mST <- ulam(
  alist(
    S ~ binomial(D, p),
    logit(p) <- a[T],
    a[T] ~ normal(a_bar, sigma),
    a_bar ~ normal(0, 1.5),
    sigma ~ exponential(1)
  ), data = dat, chains = 4, log_lik = TRUE
)

# Including the effect of predators in the model.
str(d$pred)
dat$P = ifelse(d$pred == "pred", 1, 0)
mSTP <- ulam(
  alist(
    S ~ binomial(D, p),
    logit(p) <- a[T] + bP*P,
    a[T] ~ normal(a_bar, sigma),
    a_bar ~ normal(0, 1.5),
    bP ~ normal(0,0.5),
    sigma ~ dexp(1)
  ), data = dat, chains = 4, log_lik = TRUE
)

# Let's plot the posterior predictions for a model with no predators.
post <- extract.samples(mSTP)
pSTP <- apply(inv_logit(post$a), 2, mean)
View(inv_logit(post$a))
plot(d$propsurv)

for (i in 1:48) {
  pi <- PI(inv_logit(post$a[,i]))
  lines(c(i,i), pi, lwd = 2)
}
points(1:48, pSTP, col = 'red')
precis(mSTP, depth = 3)
postcheck(mSTP)
