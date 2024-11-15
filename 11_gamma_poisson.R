library(rethinking)
library(tidyverse)
library(conflicted)

# the monastery publishing manuscripts example.
num_days <- 30
y <- rpois(num_days, 1.5) # supposing the monastery produces 1.5 manuscripts per day.

# next monastery only keeps weekly records.
num_weeks <- 4
y_new <- rpois(num_weeks, 0.5*7)

y_all <- c(y, y_new)
exposure <- c( rep(1,30) , rep(7,4) )
monastery <- c( rep(0,30) , rep(1,4) )
d <- data.frame( y=y_all , days=exposure , monastery=monastery )
View(d)


# computing the offset.
d <- d %>% mutate(log_days = log(days))

m11.12 <- quap(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log_days + a + b* monastery,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ), data = d
)
precis(m11.12)

#posterior
post <- extract.samples(m11.12)
lambda_old <- exp(post$a)
lambda_new <- exp(post$a + post$b)
precis(data.frame(lambda_old, lambda_new))
