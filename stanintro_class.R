## ----inf-sim1, exercise = FALSE, eval = FALSE----------------------------
set.seed(20190417)
N <- 1000L                                # num. observations
K <- 5L                                   # num. predictors
x <- cbind(                               # design matrix
  rep(1, N),
  matrix(rnorm(N * (K - 1)), N, (K - 1))
)
beta <- rnorm(K, 0, 1)                    # coef. vector
sigma <- 2.5                              # scale parameter
mu <- x %*% beta                          # linear prediction
y.sim <- rnorm(N, mu, sigma)              # simulated outcome

## ----inf-setup, exercise = FALSE, eval = FALSE---------------------------
## Setup
library(rstan)
rstan_options(auto_write = TRUE)             # avoid recompilation of models
options(mc.cores = parallel::detectCores())  # parallelize across all CPUs
Sys.setenv(LOCAL_CPPFLAGS = '-march=native') # improve execution time

## Data (see data block) as list
standat <- list(
  N = N,
  K = K,
  x = x,
  y = y.sim
)

## C++ Compilation
lm.mod <- stan_model(file = "code/lm.stan")

## ----inf-sampl, exercise = FALSE, eval = FALSE---------------------------
lm.est <- sampling(lm.mod,                            # compiled model
                   data = standat,                    # data input
                   algorithm = "NUTS",                # algorithm
                   control = list(                    # control arguments
                     adapt_delta = .85
                   ),
                   save_warmup = FALSE,               # discard warmup sims
                   sample_file = NULL,                # no sample file
                   diagnostic_file = NULL,            # no diagnostic file
                   pars = c("beta", "sigma"),         # select parameters
                   iter = 2000L,                      # iter per chain
                   warmup = 1000L,                    # warmup period
                   thin = 2L,                         # thinning factor
                   chains = 4L,                       # num. chains
                   cores = 4L,                        # num. cores
                   seed = 20190417)                   # seed

## ----inf-out1, exercise = FALSE, eval = TRUE-----------------------------
true.pars <- c(beta, sigma)
names(true.pars) <- c(paste0("beta[", 1:5, "]"), "sigma")
true.pars

## ----inf-out2, exercise = FALSE, eval = TRUE-----------------------------
lm.est
