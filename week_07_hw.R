library(rethinking)
library(tidyverse)
library(conflicted)
library(dagitty)

data("bangladesh")
d <- bangladesh


# Question number 1 -------------------------------------------------------

# First let's use the model from the lecture i.e. total effect of urban residence
# on contraceptive use. 

# To account for district level effects, we'll use varying effects.

dat <- list(
  D = as.integer(d$district),
  C = d$use.contraception,
  A = standardize(d$age.centered),
  U = d$urban,
  K = d$living.children
)

mCD <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D],
    vector[61]:a ~ normal(abar, sigma),
    abar ~ normal(0,1),
    sigma ~ exponential(1)
  ), data = dat, chains = 4, cores = 4
)


# Total effect of urban residence on contraceptive use. -------------------

mCDU <- ulam(
  alist(
  C ~ bernoulli(p),
  logit(p) <- a[D] + b[D] * U,
  
  #defining effects using other parameters:
  save> vector[61]:a <<- abar + za * sigma,
  save> vector[61]:b <<- bbar + zb * tau,
  
  #z-scored effects:
  vector[61]:za ~ normal(0,1),
  vector[61]:zb ~ normal(0,1),
  
  #hyper priors:
  c(abar, bbar) ~ normal(0,1),
  c(sigma, tau) ~ normal(0,1)
), data = dat, chains = 4, cores = 4)

precis(mCDU, depth = 3, pars = c('bbar'))


# Now, computing only the direct effect of urban residence ----------------

# We probably must stratify by K i.e. number of kids since its a pipe between 
# U and C.

#Let's a draw a DAG and try to see the conditional independencies.
dag1 <- dagitty("dag{D->C; D->U;U->C;U->K;K->C;A->C;A->K}")
coordinates(dag1) <- list(x = c(A=0,K=1,C=2,U=3,D=4), y = c(C=0, A=1,D=1,K=2,U=2))
adjustmentSets(dag1,'K','C')
drawdag(dag1)
# We need to condition on K and A to find the direct effect of U on C.
# Let's use the number of living children as an ordered monotonic predictor.
dat$Kprior = rep(2,3)
mCDUK <- ulam(
    alist(
      C ~ bernoulli(p),
      logit(p) <- a[D] + b[D]*U + bA*A +
        bK*sum( delta_j[1:K] ),
      
      # ordered monotonic kids
      vector[4]: delta_j <<- append_row( 0 , delta ),
      simplex[3]: delta ~ dirichlet( Kprior ),
      c(bK,bA) ~ normal(0,0.5), 
      
      # non-centered varying effects for D and U
      transpars> vector[61]:a <<- abar[1] + v[,1],
      transpars> vector[61]:b <<- abar[2] + v[,2],
      transpars> matrix[61,2]:v <-
        compose_noncentered( sigma , L_Rho , Z ),
      
      # non-centered priors
      matrix[2,61]:Z ~ normal( 0 , 1 ),
      vector[2]:abar ~ normal(0,1),
      cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky( 4 ),
      vector[2]:sigma ~ exponential(1),
      
      # convert Cholesky to Corr matrix
      gq> matrix[2,2]:Rho <<- Chol_to_Corr(L_Rho)
  ), data = dat, chains = 4, cores = 4
)


# Question number 2: direct effect of K (surviving children) on C --------
# The model is exactly the same as mCDUK.
precis(mCDUK, depth = 2)


# Question number 3 -------------------------------------------------------

# Let's incorporate district effects in the effects of the surviving children K.

mCDUK_dis <- ulam(
    alist(
      C ~ bernoulli(p),
      logit(p) <- a[D] + b[D]*U + bA*A +
        bK[D]*sum( delta_j[1:K] ),
      
      # ordered monotonic kids
      vector[4]: delta_j <<- append_row( 0 , delta ),
      simplex[3]: delta ~ dirichlet( Kprior ),
      bA ~ normal(0,0.5), 
      
      # non-centered varying effects for D and U
      transpars> vector[61]:a <<- abar[1] + v[,1],
      transpars> vector[61]:b <<- abar[2] + v[,2],
      transpars> vector[61]:bK <<- abar[3] + v[,3],
      transpars> matrix[61,3]:v <-
        compose_noncentered( sigma , L_Rho , Z ),
      
      # non-centered priors
      matrix[3,61]:Z ~ normal( 0 , 1 ),
      vector[3]:abar ~ normal(0,1),
      cholesky_factor_corr[3]:L_Rho ~ lkj_corr_cholesky( 4 ),
      vector[3]:sigma ~ exponential(1),
      
      # convert Cholesky to Corr matrix
      gq> matrix[3,3]:Rho <<- Chol_to_Corr(L_Rho)
  ), data = dat, chains = 4, cores = 4
)

precis(mCDUK_dis, depth = 2)
stancode(mCDUK_dis)
