library(tidyverse)
library(haven)
library(arrow)

data <- read_sav("../data/uncleaned_data.sav")

     # _       _                        _                 _
  # __| | __ _| |_ __ _   _ __ ___  ___| |__   __ _ _ __ (_)_ __   __ _
 # / _` |/ _` | __/ _` | | '__/ _ \/ __| '_ \ / _` | '_ \| | '_ \ / _` |
# | (_| | (_| | || (_| | | | |  __/\__ \ | | | (_| | |_) | | | | | (_| |
 # \__,_|\__,_|\__\__,_| |_|  \___||___/_| |_|\__,_| .__/|_|_| |_|\__, |
     #                                             |_|            |___/

demographic <- data %>%
    select(-starts_with("src"), -starts_with("Q")) %>%
    mutate(
           ideo5 = na_if(ideo5, 6)
    )

clean_col_names <- function(df, screen) {
    df %>%
        select(ID, starts_with(paste0("src_", screen)), matches(paste0("screen", screen))) %>%
        rename_with(function(x){gsub("src_[0-9]_", "", x)}, starts_with("src")) %>%
        rename_with(function(x){gsub("_screen[0-9]", "", x)}, starts_with("Q")) %>%
        mutate(screen=screen)
}

# Create one table for each conjoint slide:
conjoints <- map_df(1:4, ~clean_col_names(data,.x))

# Code Checks
stopifnot("Observations Lost/Added in first merge" =
          nrow(conjoints) == 4*nrow(data)
)

# Join conjoint questions into full dataset:
full_data <- full_join(conjoints, demographic)

# Code Checks
stopifnot(
      "Observations Lost/Added in second merge" =
          nrow(full_data) == nrow(conjoints)
)
stopifnot("All units did not contribute 4 times" =
          all(table(full_data$ID) == 4)
)

                 # _       _     _
# __   ____ _ _ __(_) __ _| |__ | | ___
# \ \ / / _` | '__| |/ _` | '_ \| |/ _ \
 # \ V / (_| | |  | | (_| | |_) | |  __/
  # \_/ \__,_|_|  |_|\__,_|_.__/|_|\___|

 # _ __ ___  ___ ___   __| (_)_ __   __ _
# | '__/ _ \/ __/ _ \ / _` | | '_ \ / _` |
# | | |  __/ (_| (_) | (_| | | | | | (_| |
# |_|  \___|\___\___/ \__,_|_|_| |_|\__, |
                 #                  |___/

full_data <- full_data %>%
    mutate(
           p1_party = case_when(
                                   attribut1_1 == 2 ~ 3,
                                   attribut1_1 == 3 ~ 2,
                                   attribut1_1 == 1 ~ 1,
                                   ),
           p2_party = case_when(
                                   attribut1_2 == 2 ~ 3,
                                   attribut1_2 == 3 ~ 2,
                                   attribut1_2 == 1 ~ 1,
                                   ),
           p1_rep = as.numeric(p1_party==3),
           p2_rep = as.numeric(p2_party==3),
           p1_dem = as.numeric(p1_party==1),
           p2_dem = as.numeric(p2_party==1),
           p1_ideo = as.numeric(attribut2_1),
           p2_ideo = as.numeric(attribut2_2),
           p1_liberal = as.numeric(attribut2_1==1),
           p2_liberal = as.numeric(attribut2_2==1),
           p1_conservative = as.numeric(attribut2_1==3),
           p2_conservative = as.numeric(attribut2_2==3),
           p1_white = as.numeric(attribut3_1 == 1),
           p2_white = as.numeric(attribut3_2 == 1),
           p1_black = as.numeric(attribut3_1 == 2),
           p2_black = as.numeric(attribut3_2 == 2),
           p1_hispanic = as.numeric(attribut3_1 == 3),
           p2_hispanic = as.numeric(attribut3_2 == 3),
           p1_asian = as.numeric(attribut3_1 == 4),
           p2_asian = as.numeric(attribut3_2 == 4),
           p1_northeast = as.numeric(attribut4_1 == 1),
           p2_northeast = as.numeric(attribut4_2 == 1),
           p1_midwest = as.numeric(attribut4_1 == 2),
           p2_midwest = as.numeric(attribut4_2 == 2),
           p1_south = as.numeric(attribut4_1 == 3),
           p2_south = as.numeric(attribut4_2 == 3),
           p1_west = as.numeric(attribut4_1 == 4),
           p2_west = as.numeric(attribut4_2 == 4),
           p1_education = as.numeric(attribut5_1),
           p2_education = as.numeric(attribut5_2),
           p1_inc = as.numeric(attribut6_1),
           p2_inc = as.numeric(attribut6_2),
           p1_female = as.numeric(attribut7_1 == 2),
           p2_female = as.numeric(attribut7_2 == 2),
           p1_christian = as.numeric(attribut8_1 == 1),
           p2_christian = as.numeric(attribut8_2 == 1),
           p1_jewish = as.numeric(attribut8_1 == 2),
           p2_jewish = as.numeric(attribut8_2 == 2),
           p1_muslim = as.numeric(attribut8_1 == 3),
           p2_muslim = as.numeric(attribut8_2 == 3),
           p1_atheist = as.numeric(attribut8_1 == 4),
           p2_atheist = as.numeric(attribut8_2 == 4)
           ) %>%
    select(-starts_with("attribut"))

attribute_names <- names(full_data)[grepl("p[0-9]_", names(full_data))]
attribute_names <- gsub("p[0-9]_", "", attribute_names)
for (attribute in attribute_names) {
    full_data[,paste0("d_", attribute)] <- full_data[,paste0("p1_",attribute)] - full_data[,paste0("p2_",attribute)]
}

person_1_data <- full_data %>%
    select(-starts_with("p2_")) %>%
    rename_with(~gsub("p1_", "p_", .x))  %>%
    rename(Q1 = Q1a_1) %>%
    mutate(
           Q2_1 = Q2_1 == 1,
           Q2_2 = Q2_2 == 1,
           Q2_3 = Q2_3 == 1,
    )
person_2_data <- full_data %>%
    select(-starts_with("p1_")) %>%
    rename_with(~gsub("p2_", "p_", .x)) %>%
    rename(Q1 = Q1b_1) %>%
    mutate(
           Q2_1 = Q2_1 == 2,
           Q2_2 = Q2_2 == 2,
           Q2_3 = Q2_3 == 2,
    )

stopifnot(
          "Tricky operation succeded" =
          all(person_2_data$Q2_1 == !person_1_data$Q2_1)
          )

full_ratio <- full_join(person_1_data, person_2_data) %>%
    select(-starts_with("d_"))  %>%
    mutate(
           p_scrambled = case_when(
                                   p_rep & p_black ~ T,
                                   p_hispanic & p_rep ~ T,
                                   p_conservative & p_black ~ T,
                                   p_hispanic & p_conservative ~ T,
                                   p_christian & p_dem ~  T,
                                   p_christian & p_liberal ~  T,
                                   p_conservative & p_dem ~  T,
                                   p_liberal & p_rep ~  T,
                                   T ~ F
                                   ),
           Q1_std = Q1 / 6 - (1/6)
           )

stopifnot(all(table(full_ratio$ID)==8))

write_parquet(full_ratio, "../data/cleaned_data.parquet")

demographic_data <- full_ratio %>%
    select(
           ID, weight, starttime, endtime, age, gender, region, race, educ,
           maritalstatus, starts_with("parenty"), starts_with("profile"), pid3,
           ideo5, presvote20post
    ) %>%
    distinct_all() %>%
    mutate(
           d_dem = as.numeric(pid3==1),
           d_rep = as.numeric(pid3==2),
           d_party_other = as.numeric(pid3 %in% c(4,5)),
           d_female = as.numeric(gender==2),
           d_northeast = as.numeric(region == 1),
           d_midwest = as.numeric(region == 2),
           d_south = as.numeric(region == 3),
           d_west = as.numeric(region == 4),
           d_white = as.numeric(race==1),
           d_black = as.numeric(race==2),
           d_hispanic = as.numeric(race==3),
           d_asian = as.numeric(race==4),
           d_other = as.numeric(race %in% 5:8),
           std_age = (age-mean(age))/sd(age),
           d_liberal = as.numeric(ideo5 %in% c(1,2)),
           d_conservative = as.numeric(ideo5 %in% c(4,5)),
           d_scrambled = case_when(
                                   d_liberal & d_rep ~ T,
                                   d_conservative & d_dem ~ T,
                                   d_conservative & d_liberal ~ T,
                                   d_rep & d_black ~ T,
                                   d_rep & d_hispanic ~ T,
                                   d_conservative & d_black ~ T,
                                   d_conservative & d_hispanic ~ T,
                                   T ~ F
                                   )
    )
write_parquet(demographic_data, "../data/demographic_data.parquet")
