library(rethinking)
library(tidyverse)

# Revisiting the terrian ruggedness example:
# Data

data(rugged)
d <- rugged
dd <- d %>% filter(!is.na(rgdppc_2000)) %>% mutate(
  log_gdp = log(rgdppc_2000),
  log_gdp_std = log_gdp / mean(log_gdp),
  rugged_std = rugged / max(rugged),
  cid = ifelse(cont_africa == 1, 1, 2)
)

 # The quap from chapter 8 on interactions:

m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )
precis( m8.3 , depth=2 )



# Hamiltonian Monte Carlo -------------------------------------------------

dat_slim <- as.list(dd %>% mutate( #only the required variables & in form of a list.
  cid  = as.integer(cid)
) %>% select(log_gdp_std, rugged_std, cid))
str(dat_slim)  


# Hamiltonian Monte Carlo Stan --------------------------------------------

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim, chains = 4, cores = 4 )
precis(m9.1, depth = 2)

show(m9.1) # model formula and how long every chain took.
pairs(m9.1) # bivariate plot for all parameters.


# Diagnostics.

traceplot(m9.1)


# Diagnosis of Divergent Transitions .

y <- c(-1,1)
set.seed(11)

m9.2 <- ulam(
  alist(
  y ~ dnorm(mu, sigma),
  mu <- alpha,
  alpha ~ dnorm(0, 1000),
  sigma ~ dexp(0.0001)
  ), data = list(y = y), chains = 3
)

precis(m9.2)
pairs(m9.2)
traceplot(m9.2)
