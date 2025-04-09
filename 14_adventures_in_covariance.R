library(rethinking)
library(MASS)


# Simulating cafe wait times (intercepts) and slopes (difference in 
# morning and afternoon wait times.)

a <- 3.5 # average morning wait time.
b <- (-1) # average difference afternoon waiting time.
sigma_a <- 1
sigma_b <- 0.5
rho <- (-0.7) # correlation between intercepts and slopes.

# Let's draw 20 values from a multivariate gaussian distribution.

N_cafes <- 20

Mu <- c(a, b)
sigmas <- c(sigma_a, sigma_b)
Rho <- matrix(c(1, rho, rho, 1), nrow = 2)
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

# here we are extracting from the multivariate.
vary_effects <- mvrnorm(N_cafes, Mu, Sigma)
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]


# Simulate observations ---------------------------------------------------

N_visits <- 10 # 10 visits to each cafe.

afternoon <- rep(0:1, N_visits * N_cafes/2)
cafe_id <- rep(1:N_cafes, each = N_visits)

mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon
sigma <- 0.5 # std dev within cafes.

wait <- rnorm(N_cafes*N_visits, mu, sigma)
d <- data.frame(cafe = cafe_id, afternoon = afternoon, wait = wait)
View(d)


R <- rlkjcorr(1e3, K = 2, eta = 2)
dens(R[,1,2])


# Let's fit the model -----------------------------------------------------

m14.1 <- ulam(
  alist(
    wait ~ normal(mu, sigma),
    mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
    sigma ~ exponential(1),
    
    #multinormal varying intercepts.
    c(a_cafe, b_cafe)[cafe] ~ multi_normal(c(a,b), Rho, sigma_cafe),
    #hyper priors.
    a ~ normal(5,2),
    b ~ normal(-1, 0.5),
    Rho ~ lkj_corr(2),
    sigma_cafe ~ exponential(1)
  ), data = d, chains = 4, cores = 4
)

post <- extract.samples(m14.1)
plot(NULL, xlim = c(-1,1), ylim =  c(0,3))
dens(post$Rho[,1,2], add = TRUE)
dens(R[,1,2], add = TRUE, col = 'blue')

