library(rethinking)
library(tidyverse)
library(conflicted)


data(milk)
d <- milk
d <- d %>% mutate(
  clade_id  = as.integer(clade),
  K = standardize(kcal.per.g)
)

# compute the posterior.
m5.9 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <-  a[clade_id],
    a[clade_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)
labels <- levels(d$clade)
plot(precis(m5.9, depth = 2, pars = "a"), labels = labels)

# let's compute a posterior contrast for ape and old world monkey.
post <- extract.samples(m5.9)
diff_13 <- post$a[,1] - post$a[,3]
precis(diff_13)
dens(diff_13)
