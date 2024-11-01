library(rethinking)
library(tidyverse)
library(conflicted)

data(rugged)
d <- rugged

d <- d %>% mutate(log_gdp = log(rgdppc_2000))
dd <- d %>% filter(!is.na(rgdppc_2000))

dd <- dd %>% mutate(
  log_gdp_std = log_gdp / mean(log_gdp),
  rugged_std  = rugged/ max (rugged)
)

# First model for association between terrain ruggedness and gdp.

m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a + b*(rugged_std - 0.215),
    a ~ dnorm(1, 0.1),
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = dd
)

# Drawing the prior predictives.

prior <- extract.prior(m8.1)

plot(NULL, xlim = c(0,1), ylim = c(0.5, 1.5))
rugged_seq <- seq(-0.1, 1.1, length.out = 30)
mu <- link(m8.1, post = prior, data = data.frame(rugged_std = rugged_seq))

for (i in 1:50) lines(rugged_seq, mu[i,]) # making lines.

# Summary output
precis(m8.1)


# Now let's make a model inclusive of continential differences.

# Categories for continent ID; 1 if the country is in Africa and 2 if it isn't
dd <- dd %>% mutate(cid = ifelse(cont_africa==1, 1, 2))

m8.2 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = dd
)

# Compare the two models using WAIC:

compare(m8.1, m8.2)
precis(m8.2, depth = 2)

# Posterior contrasts between the two intercepts (log-gdp)
post <- extract.samples(m8.2)
View(post$a)
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI(diff_a1_a2)


# Plot the posterior for within Africa and outside of Africa:

mu.NotAfrica <- link(m8.2, data = data.frame(cid = 2, rugged_std = rugged_seq))
mu.Africa <-link(m8.2, data = data.frame(cid = 1, rugged_std = rugged_seq))

mu.NotAfrica_mu <- apply(mu.NotAfrica, 2, mean)
mu.Africa_mu <- apply(mu.Africa, 2, mean)
mu.NotAfrica_CI <- apply(mu.NotAfrica,2, PI)
mu.Africa_CI <- apply(mu.Africa, 2, PI)


plot(log_gdp_std ~ rugged_std, data = dd, col = cid )
lines(rugged_seq, mu.Africa_mu, col = "black")
shade(mu.Africa_CI, rugged_seq)

lines(rugged_seq, mu.NotAfrica_mu, col = 2)
shade(mu.NotAfrica_CI, rugged_seq)



# Using Interaction in the 'slope' parameter ------------------------------


m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ), data = dd
)

precis(m8.3, depth = 2)

# Let's compare the models using PSIS
compare(m8.1, m8.2, m8.3, func =PSIS)
