library(rethinking)
library(tidyverse)

N <-  1000
Z <- rbern(N, 0.5)
X <- rbern(N, (1-Z)*0.1 + Z*0.9)
Y <- rbern(N, (1-Z)*0.1 + Z*0.9)

cols <- c(4,2)

