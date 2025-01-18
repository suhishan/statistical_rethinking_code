library(rethinking)
library(tidyverse)
library(conflicted)

data("Trolley")
d <- Trolley

dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)

m1 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bA * A+ bI* I+bC * C,
    c(bA, bI, bC ) ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ), data = dat, chains = 4, cores = 4
)
precis(m1, depth = 3)


# plot posterior predictive distributions for each treatment.

values <- c(0,0,1)
simul <- mcreplicate(100,
                     sim(m1, data = list(A = values[1], I = values[2], C= values[3])),
                     mc.cores = 8)
simplehist(as.vector(simul))



# Education as an ordered monotonic predictor. ----------------------------

edu_levels <- c(6,1,8,4,7,2,5,3)
edu_new <- edu_levels[d$edu]                     

dat$E <- edu_new
dat$a <- rep(2, 7) #dirichlet prior

mRXE <- ulam(
  alist(
    R ~ ordered_logistic( phi , alpha ),
    phi <- bE*sum( delta_j[1:E] ) + bA*A + bI*I + bC*C,
    alpha ~ normal( 0 , 1 ),
    c(bA,bI,bC,bE) ~ normal( 0 , 0.5 ),
    vector[8]: delta_j <<- append_row( 0 , delta ),
    simplex[7]: delta ~ dirichlet( a )
  ), data = dat, chains = 4, cores = 8, threads = 2
)
precis(mRXE, depth = 3)
