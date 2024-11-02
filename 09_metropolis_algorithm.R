library(rethinking)


# Metropolis Algorithm ----------------------------------------------------

num_weeks <- 1e5
position <- rep(0, num_weeks)
current <- 10

for (i in 1:num_weeks){
  position[i] <- current
  
  proposal <- current + sample(c(-1,1), 1)
  
  if(proposal < 1) proposal <- 10
  if(proposal > 10) proposal <- 1
  
  prob_move <- proposal / current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

plot(position[1:1000])
plot(table(position))

# Concentration of Measure ------------------------------------------------

# We are looking at 10 dimensions.
D <- 10
T <- 1e3
Y <- rmvnorm(T, rep(0,D), diag(D))
rad_dist <- function(Y) sqrt(sum(Y^2))
Rd <- sapply(1:T, function(i) rad_dist(Y[i,]))
dens(Rd)
