library(rethinking)
library(dagitty)

set.seed(71)
N <- 100

h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)

# a clean data frame
d <- data.frame(h0 = h0, treatment = treatment, fungus = fungus, h1 = h1)
precis(d)

# Simulating the plant treatment model with post treatment bias -----------


# Let's consider the outcome of interest hi as a function of h0 and some pro-
# portion of it p. Now, p can be written as a linear function of the predictors
# treatment(T) and presence or absence of fungus(F)

# The posterior distribution will look something like this.
m6.7 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0 * p,
    p <- a + bt*treatment + bf * fungus,
    a ~ dlnorm(0, 0.25),
    bt ~ dnorm(0, 0.5),
    bf ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

precis(m6.6)
# This result has post-treatment bias because fungus is more or less a function of treat-
# ment itself. Once we know about the state of fungus, the model assumes that we won't
# need the info available in treatment.


# Approximating posterior without the post-treatment predictor ------------


# If we want the effect of treatment, we must remove the post-treatment variable fungus.

m6.8 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0 * p,
    p <- a + bt*treatment,
    a ~ dlnorm(0, 0.25),
    bt ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)
precis(m6.8)

# Drawing some DAGs and thinking causally ---------------------------------

plant_dag <- dagitty("dag{H_0 -> H_1  F -> H_1 T -> F}")
drawdag(plant_dag)
impliedConditionalIndependencies(plant_dag)
