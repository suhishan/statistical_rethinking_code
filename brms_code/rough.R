a <- tibble(
  sun = rbinom(100, size = 1, 0.4),
  rain = rnorm(100, mean = 5 + 2 * sun, 1)
)
View(a)

# Understanding the dataset.

d %>% group_by(actor) %>% 
  summarize(mean(pulled_left), mean(prosoc_left))

d %>% group_by(actor) %>% 
  summarize(mean(condition))

# Aggregating the data.
# 1 is right, 1 is no partner
d %>% 
  group_by(treatment, actor, side, cond) %>% 
  summarize(sum(pulled_left)) %>% View()
