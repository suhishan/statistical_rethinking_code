library(rethinking)
library(tidyverse)
library(conflicted)
library(dagitty)


# Question number 1 -------------------------------------------------------


data("NWOGrants")
d <- NWOGrants
View(d)

#Let's try a generative simulation

N <- 2e3
G <- sample(1:2, N, replace = TRUE) #gender
# Gender 1 are morelikely to be from discipline 1 and gender 2 from 2.
D <- rbern(N, ifelse(G == 1, 0.3, 0.8)) + 1
#Discrimination against gender 1 in department 2.
award_rates <- matrix(c(0.1,0.1,0.05,0.2), nrow = 2)

p <- sapply(1:N, function(i) award_rates[G[i], D[i]])
A <- rbern(N, p) #awarded or not awarded.

# let's run the model on the simulation.
dat_sim <- list(A = A, D =D, G=G)

mSim <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data = dat_sim, chains = 4, cores = 4, log_lik = TRUE
)
post <- extract.samples(mSim)
plot(NULL, xlim = c(0,0.4), ylim = c(0,40), xlab = "probabilities of awards")
dens(inv_logit(post$a[,1,1]), add = TRUE, col = "red", lwd = 1)
dens(inv_logit(post$a[,2,1]), add = TRUE, lwd =1, col = "blue" )
dens(inv_logit(post$a[,1,2]), add = TRUE, col = "red", lwd = 2)
dens(inv_logit(post$a[,2,2]), add = TRUE, lwd = 2, col = "blue")
table(G,A,D)

# Drawing the Dag.
dag <- dagitty("dag{G -> A; G -> D; D -> A}")
coordinates(dag) <- list(x= c(G=0,D=1,A=2), y= c(G=1, D=0, A=1))
drawdag(dag)


# Estimating the total causal effect of Gender on applications funded. ------------

dat_use <- list(
  D = as.integer(d$discipline),
  G = ifelse(d$gender == "f", 1,2) ,
  A = d$awards,
  N = d$applications
)

mG <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <-  a[G],
    a[G] ~ normal(0,1)
  ), data = dat_use, chains = 4, core = 4, log_lik = TRUE
)
precis(mG, depth = 2)

# Computing the contrasts for better inference.
postG <- inv_logit(extract.samples(mG)$a)
dens(postG[,1]- postG[,2], lwd = 2, col = "red",
     xlab = "gender contrasts (probability)")
# Direct causal effect of gender on probability of funding. ---------------

mGD <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <-  a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data = dat_use, chains = 4, core = 4, log_lik = TRUE
)
precis(mGD, depth = 3)
postGD <- inv_logit(extract.samples(mGD)$a)
postGD_contrasts <- sapply(1:9, function(i)
  postGD[,1,i] - postGD[,2,i])

# Plotting contrasts by department
plot(NULL, xlim = c(-0.3, 0.4), ylim = c(0,20),
     xlab = "Gender contrasts (probability)",
     ylab = "Density")
for (i in 1:9) {
  dens(postGD_contrasts[,i], lwd = 2, col = 1+i, add = TRUE)
}
abline(v=0, lty = 2)
text(-0.2, 15, "Women disadvantaged.")
text(0.2, 15, "Men disadvantaged.")


# Estimating the marginal effect i.e average causal effect of gender, weighting each
# discipline by the number of applications.

# simulate as if all applications were from G=1; female.
apps_per_dept <- sapply(1:9, function(i){
  sum(dat_use$N[dat_use$D == i])
})

total_apps <- sum(dat_use$N)
p_g1 <- link(mGD,
    data = list(
      G = rep(1, total_apps),
      D = rep(1:9, times = apps_per_dept),
      N = rep(1, total_apps)
    )
) 

p_g2 <- link(mGD,
    data = list(
      G = rep(2, total_apps),
      D = rep(1:9, times = apps_per_dept),
      N = rep(1, total_apps)
    )
) 

dens(p_g1-p_g2, lwd = 2, col = "blue",xlab = "effect of gender perception")
abline(v=0, lty = 3)
