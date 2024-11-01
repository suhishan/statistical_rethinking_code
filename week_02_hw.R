
library(tidyverse)
library(rethinking)
library(conflicted)
library(brms)



# Generative Simulation that takes age as an input and simulates h --------

# function to simulate weight and height

sim_hw <- function(age, sd = 5, b_ah = 1.5, b_aw = 1.5, b_hw = 0.4){
  N <- length(age)
  H <- rnorm(N, b_ah*age , sd)
  W <- rnorm(N, b_aw*age + b_hw * H, sd)
  data.frame(age, H, W)
}

dat <- sim_hw(runif(200, min = 1, max = 13))

with(dat, {
  
  plot(H ~ age)
  plot(W ~ age)
  plot(W ~ H)
  
})



# Linear Regression to estimate the total causal effect of each ye --------

data("Howell1")
d <- Howell1[Howell1$age < 13,]

plot(d$weight ~ d$age)


# Look at the priors
n_lines <- 100

# This is what I did as prior predictive which was really complicated.

lines <- 
  tibble(
    n = 1:n_lines,
    alpha = rnorm(n_lines,80, 25), #mean height
    beta = rlnorm(n_lines, 0, 1), # effect of age on height
    a = rnorm(n_lines,20, 5), # mean weight
    b = rlnorm(n_lines,0, 1),  #effect of age on weight
    c = rlnorm (n_lines, 0, 1), #effect of height on weight
  ) %>% 
  expand_grid(age = d$age) %>% 
  mutate(
    height = alpha + beta*(age - mean(age)),
    weight = a + b*(age - mean(age)) + c(height - mean(height))
    )

# Plotting the priors

lines %>% 
  ggplot(aes(x = age, y = weight, group = n)) +
  geom_line()+
  coord_cartesian(ylim = c(-50, 50))


# Simple predictive prior
lines_simple <- 
  tibble(
    n = 1:n_lines,
    a = rnorm(n_lines,20, 5),
    b = rlnorm(n_lines, 0, 1),
  ) %>% 
  expand_grid(age = d$age) %>% 
  mutate(weight = a + b*(age - mean(age)))

lines_simple %>% 
  ggplot(aes(x =age, y =weight, group = n))+
  geom_line()



#Quadratic Approximation,
age_bar = mean(d$age)
height_bar = mean(d$height)
model <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b*(age - age_bar),
    sigma ~ dunif(0, 10),
    a ~ dnorm(5, 1),
    b ~ dlnorm(0, 1)
  ),
  data = d
)

precis(model) # No marginal distributions in this case because there are no confounders.
round(vcov(model), 3)
pairs(model)


# Plotting the posterior inference agains the data.

plot(d$weight ~ d$age)
post <- extract.samples(model)
curve(mean(post$a) + mean(post$b)*(x-age_bar), add = TRUE)

# Linear Regression of age on weight, by category sex ---------------------
head(d)
d <- d %>% 
  mutate(S = male + 1) #1 is female and 2 is male 

model.sex <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a[S] + b[S]*(age - age_bar),
    sigma ~ dexp(1),
    a[S] ~ dnorm(5, 1),
    b[S] ~ dunif(0,10)
  ),
  data = d
)

precis(model.sex, depth = 2)
post.sex <- extract.samples(model.sex)


# Extracting 1000 mu(weight) values for both males and females from the posterior
# for each age group.
muF <- link(model.sex, data = list(S = rep(1, 13), age = 0: 12))
muM <- link(model.sex , data  = list (S = rep(2, 13), age = 0: 12))
piF <- apply(muF, 2, PI)
piM <- apply(muM, 2, PI)
plot.data <- tibble(
  age = 0:12,
  meanF = colMeans(muF),
  meanM = colMeans(muM),
  piF_min = piF[1,],
  piF_max = piF[2,],
  piM_min = piM[1,],
  piM_max = piM[2,]
)

d %>% 
  ggplot()+
  geom_point(aes(x = age, y = weight, color = S))+
  geom_line(aes(x = age, y = meanF), data = plot.data, color = "red")+
  geom_ribbon(aes(x= age,ymin = piF_min, ymax = piF_max), data = plot.data, fill = "red", alpha = 0.2)+
  geom_ribbon(aes(x= age,ymin = piM_min, ymax = piM_max), data = plot.data, fill = "blue", alpha = 0.2)+
  geom_line(aes(x = age, y = meanM), data = plot.data, color = "blue")+
  labs(title = "Linear Predictions of age on weight by sex for children under 13",
       subtitle = "The shaded area shows 89% intervals\n",
       x = "\nAge",
       y = "Weights\n",
       color = "Sex")+
  scale_color_manual(labels = c("Girl", "Boy"), values = c("red", "blue"))+
  theme_minimal()

ggsave("pictures/week_02_model_sex.jpg")

# Another picture of contrast.
mu1 <- sim(model.sex, list(age = age_seq, S = rep(1, 13)))
mu2 <- sim(model.sex, list(age = age_seq, S = rep(2, 13)))
mu_contrast <- mu1 - mu2

# Plotting density for contrasts in each age.
age_seq = 0:12
plot(NULL, xlim = c(0, 13), ylim = c(-15,15), xlab = "Age", ylab = "Weights (Male -Female)")
for(p in c(0.5, 0.67, 0.89, 0.99))
  shade(apply(mu_contrast, 2, PI, prob = p), age_seq)
abline(h = 0, lty = 2, lwd = 2)

