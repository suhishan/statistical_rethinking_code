library(rethinking)
library(tidyverse)
library(conflicted)

data("AustinCats")
d <- AustinCats

dat <- as.list(d %>% mutate(
  days = days_to_event,
  adopted = ifelse(out_event == "Adoption", 1, 0),
  color_id = ifelse(color == "Black", 1, 2)
) %>% select(days, adopted, color_id))

model.cat <- ulam(
  alist(
    days|adopted == 1 ~ exponential(lambda),
    days|adopted == 0 ~ custom(exponential_lccdf(!Y|lambda)),
    lambda <- 1 / mu,
    log(mu) <- a[color_id],
    a[color_id] ~ normal(0, 1)
  ), data = dat, chains = 4, cores = 4
)

post.cat <- extract.samples(model.cat)
plot(NULL, xlim = c(45, 65), ylim = c(0, 1))
dens(exp(post.cat$a[,1]), col = "black", add  = TRUE)
dens(exp(post.cat$a[,2]), col = "red", add = TRUE)
