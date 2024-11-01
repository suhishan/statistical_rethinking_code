packages <- c("ape", "bayesplot", "brms", "broom", "dagitty", "devtools", "flextable", "GGally", "ggdag", "ggdark", "ggmcmc", "ggrepel", "ggthemes", "ggtree", "ghibli", "gtools", "invgamma", "loo", "patchwork", "posterior", "psych", "rcartocolor", "Rcpp", "remotes", "rstan", "santoku", "StanHeaders", "statebins", "tidybayes", "tidyverse", "viridis", "viridisLite", "wesanderson")

install.packages(packages, dependencies = T)
devtools::install_github("paul-buerkner/brms")

library(brms)

