library(arrow)
library(rstanarm)
library(cmdstanr)
library(bayesplot)
library(loo)
library(tidyverse)

data <- read_parquet("../data/full_ratio.parquet")

demographic_data <- data %>%
    select(
           ID, weight, starttime, endtime, age, gender, region, race, educ,
           maritalstatus, starts_with("parenty"), starts_with("profile"), pid3,
           ideo5, presvote20post
    ) %>%
    distinct_all() %>%
    mutate(
           p_female = gender==2,
           p_northeast = as.numeric(region == 1),
           p_midwest = as.numeric(region == 2),
           p_south = as.numeric(region == 3),
           p_west = as.numeric(region == 4),
           p_white = as.numeric(race==1),
           p_black = as.numeric(race==2),
           p_hispanic = as.numeric(race==3),
           p_asian = as.numeric(race==4),
           p_other = as.numeric(race %in% 5:8),
    )

# test <- stan_glm(
#     Q1 ~black + hispanic +asian + female + inc + education + jewish + muslim + atheist,
#     data=data,
#     family = gaussian(),
#     prior = cauchy(),
#     prior_intercept = cauchy(),
#     seed = 12345
# )

# test <- lm(
#     Q1 ~black + hispanic +asian + female + inc + education + jewish + muslim + atheist,
#     data=data)

# mcmc_intervals(test, prob_outer=.95)


X <- model.matrix(~party + ideo + black + hispanic +asian + education + inc + female + jewish + muslim + atheist, data=data)
Z <- model.matrix(~age + p_female + p_black + p_hispanic + p_asian + p_other, data=demographic_data)

X <- model.matrix(~black + hispanic +asian + female, data=data)
Z <- model.matrix(~p_black + p_hispanic + p_asian + p_female , data=demographic_data)


model_1 <- cmdstan_model("../stan_code/two_level_lm.stan")

stan_data <- list(
                X = X,
                Z = Z,
                n = nrow(X),
                n_id = nrow(Z),
                id = data$ID,
                j = ncol(X),
                k = ncol(Z),
                y = data$Q1
)
fit_1 <- model_1$sample(data=stan_data, chains=6, parallel_chains=6, iter_sampling=2000, iter_warmup=1000)

mcmc_areas(fit_1$draws("Gamma"))

