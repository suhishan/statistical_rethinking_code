library(rethinking)
library(tidyverse)
library(conflicted)

data("WaffleDivorce")
d <- WaffleDivorce

d <- d %>% mutate(
  A = standardize(MedianAgeMarriage),
  M = standardize(Marriage),
  D = standardize(Divorce)
)

m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

# Posterior Summaries
plot(coeftab(m5.1, m5.2, m5.3))

# Comparing these three models using PSIS:

compare(m5.1, m5.2, m5.3, func = PSIS)
