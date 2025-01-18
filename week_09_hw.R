library(rethinking)
library(tidyverse)
library(conflicted)

data("Achehunting")
d <- Achehunting
# changing the dataset if require:

d <- d %>% 
  mutate(
    success = ifelse(kg.meat>0, 1, 0)
  )

d  %>% 
  ggplot(aes(age)) +
  geom_histogram()

# prior predictive simulation
A_seq <- seq(10, 100, length.out = 91)
N <- 100
a <- rnorm(N, 0, 2)
bA <- rnorm(N, 0, 0.5)
lambda <- sapply(A_seq, function(x) inv_logit(a + bA * x))

plot(NULL, xlim = c(10,100), ylim = c(0,1), xlab = "Age",
     ylab = "probability of success")
for (i in 1:N) lines(A_seq, lambda[i,])

# The influence of age on probability of success trip.
dat <- list(
  A = standardize(log(d$age)),
  S = d$success,
  I = as.factor(d$id),
  A2 = d$age/80
)
mAS <- ulam(
  alist(
    S ~ bernoulli(p),
    logit(p) <- a + bA * A,
    a ~ normal(0,1),
    bA ~ normal(0, 0.5)
  ), data = dat, chains = 4, cores = 4
)
precis(mAS)


# Let's make a posterior prediction check.
# Let's bin the data.

Around <- round(d$age/10) * 10
Abin <- seq(10, 80, by = 10)
p_means <- sapply(Abin, function (a) mean(d$success[Around == a]))

plot(Abin, p_means,
     xlab = "age(years)",
     ylab = "proportion successful",
     lwd =2, type = "b",
     ylim = c(0,1))
post <- extract.samples(mAS)
for (i in 1:30) {
  curve(
    inv_logit(post$a[i] + post$b[i] * (log(x) - 3.79)/0.335),
    add = TRUE,
    from = 1
  )
}

curve(alpha * (1-exp(-beta1 * x)) * exp(-beta2 * x))



# Question number 2 -------------------------------------------------------

# varying effects of hunter id.

mID <- ulam(
  alist(
    S ~ bernoulli(p),
    logit(p) <- abar + z[I] * sigma,
    z[I] ~ normal(0, 1),
    abar ~ normal(0,1),
    sigma ~ exponential(1),
    
    gq> vector[I]:a <<- abar + z*sigma
  ), data = dat, chains = 4, cores = 4
)

# Plotting posterior predictions for each hunter.
I_seq <- seq(1, 147, by = 1)
post <- link(mID, data = list(
  I = I_seq
))

h_means <- d %>% group_by(id) %>% 
  summarize(
    prop = mean(success)
  )

mu <- apply(post, 2, mean)
pi <- apply(post, 2, PI)

plot(h_means$prop, xlab = "Hunters", ylab ="Proportion of Success")
points(mu, col = "red", pc = 2, cex = 0.5)
shade(pi, I_seq)

# age varying effects.
mIDage <- ulam(
  alist(
    S ~ bernoulli(p),
    logit(p) <- abar + z[I] * sigma +  (bbar + y[I]*sigma_b)*A2,
    z[I] ~ normal(0, 1),
    y[I] ~ normal(0,1),
    abar ~ normal(0,1),
    bbar ~ normal(0, 0.5),
    sigma ~ exponential(1),
    sigma_b ~ exponential(1),
    
    gq> vector[I]:a <<- abar + z*sigma,
    gq> vector[I]:bA <<- bbar + y*sigma_b
  ), data = dat, chains = 4, cores = 4
)

post_age <- extract.samples(mIDage)

#Making my own link function
p_link <- function(hunter, age = 20) {
  logodds <- with(post_age,
      a[,hunter] + bA[,hunter] * age            
    )
  return (inv_logit(logodds))
}

#for age 30
# Does this work I don't know
p_raw <- sapply(1:147, function(i) p_link(i, age = 30))
mu_30 <- apply(p_raw, 2, mean)
plot(h_means$prop, xlab = "Hunters", ylab ="Proportion of Success")
points( mu_30, col = "red", pch = 16, cex = 0.5)
points( apply(
  sapply(1:147, function(i) p_link(i, age = 50)),
  2,
  mean
), col = "blue", pch = 16, cex = 0.5)





