library(cmdstanr)
library(arrow)
library(rstanarm)
library(rstan)
library(haven)
library(bayesplot)
library(loo)
library(tidyverse)

# Set up stimulations to check STAN Model is working

# number of respondents
n_id <- 100
# number of questions each respondent is given
n_questions <- 20
# number of demographic attirbutes each respondent has
n_demo <- 2
# number of attributes each conjoint profile has
n_conjoint_attr <- 3
# id lookup vector
id <- rep(1:n_id, n_questions)

# matrix of demographic attributes (n_id X n_demo)
Z <- matrix(rnorm(n_demo*n_id,0,1), ncol=n_demo)
# matrix of conjoint attributes ((n_questions*nid) X n_conjoint_attr)
X <- matrix(rnorm(n_questions*n_id*n_conjoint_attr,0,1), ncol=n_conjoint_attr)

# Gamma parameters
gammas <- t(matrix(c(1,2,3,4,5,7), nrow=3, ncol=2))
# Betas - Individual coefficient vectors
betas <- Z %*% gammas  + rnorm(n_id*3, 0, .15)
# alpha (intercept)
alpha <- -2

# Generate outcome variable
y <- rep(0,n_id*n_questions)
for (i in seq(id)) {
    rater_id <- id[i]
    y[i] <- alpha + betas[rater_id,] %*% X[i,] + rnorm(1,0,.1)
}

# Check dimenisons of matrices are correct
stopifnot("Z should have same number of rows as n_id" = nrow(Z) == n_id)
stopifnot("X should have right number of rows" = nrow(X) == n_questions*n_id)
stopifnot("Y and X should have same number of rows" = nrow(X) == nrow(y))

# save(id, X,Z,y, file="simulated_data.Rda")

# Matrices X and Z should have prepended intercept
standata <- list(
                 X = cbind(1,X),
                 Z = cbind(1,Z),
                 n = nrow(X),
                 n_id = n_id,
                 id = id,
                 j = ncol(X)+1,
                 k = ncol(Z)+1,
                 y = y
                 )

model_1 <- cmdstan_model("../stan_code/two_level_lm.stan")
fit_1 <- model_1$sample(data=standata, chains=6, parallel_chains=6, iter_sampling=500, iter_warmup=500)

tol = .1
stopifnot("Population Parameters Not Recovered" = abs(mean(fit_1$draws("Gamma[1,1]")) + 2) < tol)
stopifnot("Population Parameters Not Recovered" = abs(mean(fit_1$draws("Gamma[3,2]")) - 4) < tol)
stopifnot("Population Parameters Not Recovered" = abs(mean(fit_1$draws("Gamma[3,3]")) - 5) < tol)

# fit_1$save_object("test.Rds")
# # summary(fit_1$draws("Gamma"))
# # test <- summary(fit_1$draws("Beta"))
# # test <- test  %>%
# #     mutate(
# #            x = as.numeric(gsub(".*\\[(\\d*),.*", "\\1", variable)),
# #            y = as.numeric(gsub(".*\\[\\d*,(\\d*)\\]", "\\1", variable))
# #     )

# # beta_mat <- matrix(0, nrow=n_id, ncol=ncol(X)+1)
# # for (i in 1:nrow(test)) {
# #     x <- as.numeric(test[i, "x"])
# #     y <- as.numeric(test[i, "y"])
# #     beta_mat[x,y] <- as.numeric(test[i, "mean"])
# # }
# # beta_mat





