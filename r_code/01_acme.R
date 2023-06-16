library(tidyverse)
library(arrow)
library(haven)

data <- read_parquet("../data/cleaned_data.parquet") %>%
    zap_labels()

data <- data %>%
   mutate(dem = pid3==1)
male_data <- data %>%
    filter(gender == 1)
female_data <- data %>%
    filter(gender == 2)
dem_data <- data %>%
    filter(pid3 == 1)
rep_data <- data %>%
    filter(pid3 == 2)
indep_data <- data %>%
    filter(pid3 == 3)
white_data <- data %>%
    filter(race==1)
non_white_data <- data %>%
    filter(race!=1)

conservative_data <- data %>%
    filter(ideo5 %in% c(4,5))
liberal_data <- data %>%
    filter(ideo5 %in% c(1,2))
moderate_data <- data %>%
    filter(ideo5 == 3)

# Overall Sample Models
ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=data, weights=weight)

ratio_model_2 <- lm(Q1 ~ p_dem + p_rep + p_liberal + p_conservative + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=data, weights=weight)


acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=data, weights=weight)
acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=data, weights=weight)
acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=data, weights=weight)

## Split sample estimates:
rep_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=rep_data, weights=weight)
rep_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=rep_data, weights=weight)
rep_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=rep_data, weights=weight)
rep_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=rep_data, weights=weight)

dem_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=dem_data, weights=weight)
dem_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=dem_data, weights=weight)
dem_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=dem_data, weights=weight)
dem_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=dem_data, weights=weight)

indep_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=indep_data, weights=weight)
indep_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=indep_data, weights=weight)
indep_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=indep_data, weights=weight)
indep_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=indep_data, weights=weight)

male_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=male_data, weights=weight)
male_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=male_data, weights=weight)
male_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=male_data, weights=weight)
male_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=male_data, weights=weight)

female_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=female_data, weights=weight)
female_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=female_data, weights=weight)
female_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=female_data, weights=weight)
female_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=female_data, weights=weight)

white_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=white_data, weights=weight)
white_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=white_data, weights=weight)
white_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=white_data, weights=weight)
white_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=white_data, weights=weight)
non_white_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=non_white_data, weights=weight)
non_white_acme_model_1 <- lm(Q2_1 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=non_white_data, weights=weight)
non_white_acme_model_2 <- lm(Q2_2 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=non_white_data, weights=weight)
non_white_acme_model_3 <- lm(Q2_3 ~ p_dem + p_rep + p_ideo + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=non_white_data, weights=weight)

liberal_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_liberal + p_conservative + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=liberal_data, weights=weight)
conservative_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_liberal + p_conservative + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=conservative_data, weights=weight)
moderate_ratio_model_1 <- lm(Q1 ~ p_dem + p_rep + p_liberal + p_conservative + p_black + p_asian + p_hispanic +
              p_midwest + p_south + p_west + p_education + p_inc +
              p_female + p_jewish + p_muslim + p_atheist
          , data=moderate_data, weights=weight)

save(list=ls(pattern="model"),file = "../data/models.Rda")
