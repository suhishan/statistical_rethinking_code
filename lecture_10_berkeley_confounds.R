library(rethinking)
library(tidyverse)
library(conflicted)

# Generative Simulation ---------------------------------------------------


N <- 2000 # number of applicants.

G <- sample(1:2, size = N, replace = TRUE) # gender 1 and 2 evenly distributed.

# Generate Ability : 1 for highly able and 0 for average.

u <- rbinom(N, 1, 0.1)
# Using gender and ability, generate application to departments.
# Among G == 1, only those with exceptional ability are applying to department 2.
D <- rbern(N, ifelse(G == 1, u*1, 0.75)) + 1

#let's create a matrix of acceptance rates [dept, gender]
# We are going to make it so that for average people u=0, department 2 discriminates
# against gender 1.

p_u1 <- matrix(c(0.3, 0.3, 0.5, 0.5), nrow = 2)
p_u0 <- matrix(c(0.1,0.1, 0.1, 0.3), nrow = 2)
p_u <- list(p_u0, p_u1)

# Let's now determine the admissions count using G, D and u.
p <- sapply(1:N, function(i) p_u[[1+u[i]]][G[i], D[i]])
A <- rbern(N, p)
table(A, u)

# The simulation is done, Now we need a dataset for monte carlo.

dat_sim <- list(
  A = A, D = D, G = G
)


# Confounded Model from simulated data. ------------------------------------


# total effect of gender

m1 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G],
    a[G] ~ normal(0,1)
  ), data = dat_sim, chains = 4, cores= 4, log_lik = TRUE
)

# confounded model.
m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data = dat_sim, chains = 4, cores= 4, log_lik = TRUE
)
precis(m2, depth = 3)



# Sensitivity Analysis: Analysing the strength of the confounds. ----------

# How strong must the confound me to change conclusions.

datl <- dat_sim
datl$D2 <-  ifelse(D == 2,1,0)
datl$N  <-  length(D)
# we are assuming the effect of ability on the probability of admissions
# is positive for both genders.
datl$b <-  c(1,1)
# gamma parameter : we are assuming the effect of ability on department
# choice differs by gender. Only gender 1 is affected by latent ability;
# they are more likely to apply to the discriminatory department 2.
datl$g <- c(1,0)


mGDu <- ulam(
  alist(
    # A model on probability of admissions:
    A ~ bernoulli(p),
    logit(p) <- a[G,D] + b[G] * u[i],
    matrix[G,D]:a ~ normal(0,1),
    
    # Department choice model for department 2.
    D2 ~ bernoulli(q),
    logit(q) <- delta[G] + g[G]*u[i],
    delta[G] ~ normal(0,1),
    
    # unobserved u, 2000 parameters.
    vector[N]:u ~ normal(0, 1)
  ), data = datl, chains = 4, cores = 4
)
