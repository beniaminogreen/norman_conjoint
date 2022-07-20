#!/usr/bin/Rscript
#SBATCH --job-name=mcmc_scrambled_ratio_partisanship_ideo
#SBATCH --nodes=1                    # Run all processes on a single node
#SBATCH --ntasks=1                   # Run a single task
#SBATCH --cpus-per-task=4            # Number of CPU cores per task
#SBATCH --time=90-00:00:00
#SBATCH --mem=10G
#SBATCH --output=logs/mcmc_%j.log     # Standard output and error log

library(arrow)
library(rstanarm)
library(cmdstanr)
library(bayesplot)
library(loo)
library(tidyverse)

## Settings
iter_warmup = 150
iter_sampling = 300
max_treedepth = 16
adapt_delta = .99
###

data <- read_parquet("../data/cleaned_data.parquet")
demographic_data <- read_parquet("../data/demographic_data.parquet")

X <- model.matrix(~p_rep + p_dem + p_liberal + p_conservative + p_black + p_hispanic + p_asian + p_midwest + p_south + p_west + p_education + p_inc + p_female + p_jewish + p_muslim + p_atheist + p_scrambled, data=data)
Z <- model.matrix(~ std_age + gender + educ + d_hispanic + d_midwest + d_south + d_west	+ d_black + d_asian + d_female + d_dem + d_rep + d_party_other + d_liberal + d_conservative  + d_scrambled, data=demographic_data)

save(X,Z, file="../data/stan_draws/ratio_scrambled_partisanship_ideology_supp.Rda")

model <- cmdstan_model("../stan_code/two_level_lm_non_centered.stan")

stan_data_1 <- list(
                X = X,
                Z = Z,
                n = nrow(X),
                n_id = nrow(Z),
                id = data$ID,
                j = ncol(X),
                k = ncol(Z),
                y = data$Q1_std
)

fit <- model$sample(data=stan_data_1,
                      chains=4,
                      parallel_chains=4,
                      adapt_delta=adapt_delta,
                      max_treedepth=max_treedepth,
                      iter_sampling=iter_sampling,
                      iter_warmup=iter_warmup)
fit$save_object("../data/stan_draws/ratio_scrambled_partisanship_ideology_draws.Rda")

