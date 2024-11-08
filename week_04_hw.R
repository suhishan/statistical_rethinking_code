library(rethinking)
library(tidyverse)
library(conflicted)


# Question no. 1 ----------------------------------------------------------
# Data
d <- sim_happiness(seed = 1977, N_years = 1000)
d2 <- d[d$age > 17,]
d2$A <- (d2$age - 18)/ (65 - 18)
d2$mid <- d2$married + 1 # 1 is single and 2 is married.

# Non causal model where we are conditoning on a collider i.e. Marriage (mid)
hm6.9 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a[mid] + bA * A,
    a[mid] ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = d2
)
precis(hm6.9, depth = 2)

# Causal Model omitting the collider marriage status.

hm6.10 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ), data = d2
)
precis(hm6.10)

#Let's compare these models using PSIS and WAIC.

compare(hm6.9, hm6.10, func = PSIS)
compare(hm6.9, hm6.10, func = WAIC)

# The first non-causal model is much better at prediction because including marriage
# status, although not a cause of happiness, is associated with it i.e. happiness 
# influences marital status. And age doesn't influence happiness but only marriage status,
# and so non-association looks like a causal association due a collider. The first 
# model does predict better because including marriage status is more info that the
# model can use; albeit a non causal info. 


# Question number 2 -------------------------------------------------------

# I am not going to write the model again but run the week 03 hw.

compare( m.FW, m.FW.direct, func  = PSIS)
compare( m.FW, m.FW.direct, func  = WAIC)
precis(m.FW.direct)

# The model that takes groupsize into account is better at prediction.
# In the best scoring model, bF is the direct causal effect of avgfood on weight.
# bG is the direct effect of groupsize on weight of foxes. 


# Question Number 3 -------------------------------------------------------

data("cherry_blossoms")
c <- cherry_blossoms
str(c)
c <- c %>% filter(!is.na(doy) & !is.na(temp)) %>% mutate(
  D = standardize(doy),
  T = standardize(temp),
  T_sq  = T^2
)

# linear relationship

c4.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bT*T,
    a ~ dnorm(0, 1),
    bT ~ dnorm(0, 1), 
    sigma ~ dexp(1)
  ), data = c
)

# quadratic relationship with only the square term

c4.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bTs*(T^2),
    a ~ dnorm(0, 1),
    bTs ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = c
)
precis(c4.2)

#let's construct a prior predictive for c4.2
prior <- extract.prior(c4.2)
T_seq <- seq(-3,3.5, length.out = 100)
prior_pred <- link(c4.2, post = prior, data = list(T = T_seq))
plot(NULL, xlim = c(-3,3.5), ylim = c(-2,2))
for (i in 1:100) lines(T_seq, prior_pred[i,])

# Full quadratic relationship.
c4.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bT * T + bTs*(T^2),
    a ~ dnorm(0, 1),
    bT ~ dnorm(0,1),
    bTs ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = c
)
precis(c4.3)
# prior predictive for the third model.
prior_3 <- extract.prior(c4.3)
T_seq <- seq(-3,3.5, length.out = 100)
prior_pred_3 <- link(c4.2, post = prior_3, data = list(T = T_seq))
plot(NULL, xlim = c(-3,3.5), ylim = c(-2,2))
for (i in 1:100) lines(T_seq, prior_pred_3[i,])


# Let's plot the posterior for the first,second and third model.
#first model
post_1 <- link(c4.1, data = list(T = T_seq))
mu_1 <- apply(post_1, 2, mean)
PI_1 <- apply(post_1, 2, PI)
plot(D ~ T, data = c)
lines(T_seq, mu_1)
shade(PI_1, T_seq)
#
#second model
post_2 <- link(c4.2, data = list(T = T_seq))
mu_2 <- apply(post_2, 2, mean)
PI_2 <- apply(post_2, 2, PI)
plot(D ~ T, data = c)
lines(T_seq, mu_2)
shade(PI_2, T_seq)
# third model
post_3 <- link(c4.3, data = list(T = T_seq))
mu_3 <- apply(post_3, 2, mean)
PI_3 <- apply(post_3, 2, PI)
plot(D ~ T, data = c)
lines(T_seq, mu_3)
shade(PI_3, T_seq)

# Comparing the models.
compare(c4.1, c4.2, c4.3, func = PSIS)

# The best model seems to be the linear model.
T_2050 <- round((9-mean(c$temp))/sd(c$temp),2)
post_2050 <- sim(c4.1, data = list(T= T_2050))
dens(post_2050*sd(c$doy)+mean(c$doy), xlab = "Day in year of 1st bloom", 
     main = "If temperatures reach 9 degrees in March in 2050",
     lwd = 2,
     col = 2)


# Question number 4 -------------------------------------------------------

data("Dinosaurs")
dino <- Dinosaurs %>% filter(sp_id == 3) %>% mutate(
  A = standardize(age),
  M = standardize(mass)
)

# Let's consider a linear model.

dino.linear<- quap(
  alist(
  M ~ dnorm(mu, sigma),
  mu <- a + bA*A,
  a ~ dnorm(0, 1),
  bA ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = dino)
# plotting the posterior.
dino.post <- link(dino.quad)
mu <- apply(dino.post, 2, mean)
PI <- apply(dino.post,2, PI)
plot(M ~ A, data = dino)
lines(dino$A, mu)
shade(PI, dino$A)

#Let's now consider a quadratic model.

dino.quad<- quap(
  alist(
  M ~ dnorm(mu, sigma),
  mu <- a + bA*A + bAsq*(A^2),
  a ~ dnorm(0, 1),
  bA ~ dnorm(0, 0.5),
  bAsq ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
), data = dino)

# let's compare the models in terms of out of sample predictions.
compare(dino.linear, dino.quad, func = WAIC)

# Since I don't know growth models and I am a bit rusty on differential
# equations I just used a linear and quadratic model.
