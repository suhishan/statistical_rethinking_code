library(rethinking)
library(tidyverse)
data("Howell1") ; d <- Howell1 ; d2 <- d[d$age >=18, ]
 
plot(d2$height ~ d2$weight)


# Constructing a prior predictive distribution ----------------------------

set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rlnorm(N, 0, 1)


# Plot the lines using ggplot2
plot.data <- data.frame(a,b)
ggplot(aes(x = weight, y = height), data = d2)+ 
  geom_abline(aes(intercept = a- b*xbar, slope = b), data = plot.data) +
  geom_hline(yintercept = 0)+
  labs(title = "b ~ dlnorm(0,1)",
       x = "Weight", y = "Height") +
  theme_minimal()+
  xlim(30, 90)+
  ylim(-100, 400)



# Quadratic Approximation of the Posterior distribution of parameter --------

xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

# Interpreting Tables
precis(m4.3)
round(vcov(m4.3), 3)
pairs(m4.3)

# Plotting data agains the posterior inference
post <- extract.samples(m4.3, n = 20)


ggplot(data = d2) +
  geom_point(aes(x = weight, y = height))+
  geom_abline(aes(intercept = a - b*xbar, slope = b), data = post)+
  theme_minimal()


# Making contours around the posterior inference --------------------------
post <- extract.samples(m4.3)
mu_at_42 <- post$a + post$b*(42-xbar)
dens(mu_at_42)
PI(mu_at_42)


# Mu for every row in the data
mu <- link(m4.3) # row is just number of samples and the columns are each individual.

#Mu for each unique weights
weight.seq <- seq(25, 70, 1)
mu <- link(m4.3, data = data.frame(weight = weight.seq))

plot(height~weight, d2, type = "n")
for (i in 1:100){
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.2))
}

# Plotting other types of summaries
plot(height ~ weight, data = d2)

# Summary estimates
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)


# Simulated predictions of actual height and not the just the mean --------

sim.height <- sim(m4.3, data = data.frame(weight = weight.seq))

# Summarizing these simulated heights
height.PI <- apply(sim.height, 2, PI, prob = 0.67)
View(height.PI)

# Graph the summary and uncertainty.

plot(height ~ weight, d2)

# draw Maximum A Posteriori Line
lines(weight.seq, mu.mean)

# draw PI region for the line
shade(height.PI, weight.seq)

# draw mu's PI region for the line.
shade(mu.PI, weight.seq)
