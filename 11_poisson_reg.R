library(rethinking)
library(tidyverse)
library(conflicted)

data("Kline")
d <- Kline

d <- d %>% mutate(
  P = scale(log(population)),
  contact_id = ifelse(contact == "high",2,1)
)

#let's think about the priors on alpha (a)
curve(dlnorm(x, 3, 0.5), 0, 100, n = 200)

#let's plot the prior preditive distribution by ourselves.
N <- 100
a <- rnorm(N, 3, 0.5)
b <- rnorm(N, 0, 0.5)
plot(NULL, xlim = c(-2,2), ylim = c(0,100))
for (i in 1:N) curve(exp(a[i]+ b[i]*x),add = TRUE)


# Let's compute the posterior distribution --------------------------------

 dat <- list(
   T = d$total_tools,
   P = d$P,
   cid = d$contact_id
 )

#intercept only model.
m11.9 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 0.5) 
  ), data = dat, chains = 4, log_lik = TRUE
)

# full model

m11.10 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a[cid] + b[cid]*P,
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ), data = dat, chains = 4, log_lik = TRUE
)
precis(m11.10, depth = 3)
compare(m11.9, m11.10, func = PSIS)

# let's plot the posterior predictions

# setting up the sequences:
P_seq <- seq(-2,3, length.out = 100)

plot(dat$T~dat$P, xlim = c(-2,3), ylim = c(0,100), xlab = "log popn(std)",
     ylab = "Tools",
     pch = ifelse(dat$cid == 1,1,16))

#predictions for cid = 1, low contact
lambda_1 <- link(m11.10, data = data.frame(P = P_seq, cid = 1))
lines(P_seq, apply(lambda_1, 2, mean), lty = 2)
shade(apply(lambda_1, 2, PI), P_seq)

#predictions for cid = 2, high contact.
lambda_2 <- link(m11.10, data = data.frame(P = P_seq, cid = 2))
lines(P_seq, apply(lambda_2, 2, mean), lty = 1)
shade(apply(lambda_2, 2, PI), P_seq, xpd = TRUE)
