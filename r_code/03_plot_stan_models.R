library(arrow)
library(rstanarm)
library(cmdstanr)
library(bayesplot)
library(loo)
library(tidyverse)
library(stargazer)

# data <- read_parquet("../data/cleaned_data.parquet")
# demographic_data <- read_parquet("../data/demographic_data.parquet")

# X <- model.matrix(~p_rep + p_dem + p_ideo + p_black + p_hispanic + p_asian + p_midwest + p_south + p_west + p_education + p_inc + p_female + p_jewish + p_muslim + p_atheist, data=data)
# Z <- model.matrix(~ std_age + gender + educ + d_hispanic + d_midwest + d_south + d_west	+ d_black + d_asian + d_female + d_dem + d_rep + d_party_other , data=demographic_data)
# colnames(X) <- c("Intercept", "Republican", "Democrat", "Ideology (1-3)", "Black", "Hispanic", "Asian", "Midwest", "South", "West", "Education", "Income", "Female", "Jewish", "Muslim", "Atheist")
# colnames(Z) <- c("Intercept", "Age (Standardized)", "Gender", "Education", "Hispanic", "Midwest", "South", "West",
# "Black", "Asian", "Female", "Democrat", "Republican", "Party Other")
# fit <- readRDS("../data/stan_draws/test")


matrix_extractor <- function(draws, statistic, colnames = NULL, rownames=NULL) {
   df <-  draws %>%
     summary() %>%
     mutate(
            x = as.numeric(gsub(".*\\[(\\d*),.*", "\\1", variable)),
            y = as.numeric(gsub(".*\\[\\d*,(\\d*)\\]", "\\1", variable))
     )

    mat <- matrix(0, nrow=max(df$x), ncol=max(df$y))
    for (i in 1:nrow(df)) {
        x <- as.numeric(df[i, "x"])
        y <- as.numeric(df[i, "y"])
        mat[x,y] <- as.numeric(df[i,statistic])
    }

    colnames(mat) <- colnames
    rownames(mat) <- rownames

    return(mat)
}


ben_stargazer <- function (stan_draws, X, Z, by=4) {
    gammas <- stan_draws$draws("Gamma")

    posterior_means <- matrix_extractor(gammas, "mean", rownames=colnames(Z), colnames=colnames(X))
    posterior_means <- round(posterior_means, 3)
    posterior_q5 <- matrix_extractor(gammas, "q5", rownames=colnames(Z), colnames=colnames(X))
    posterior_q95 <- matrix_extractor(gammas, "q95", rownames=colnames(Z), colnames=colnames(X))
    posterior_sd <- matrix_extractor(gammas, "q95", rownames=colnames(Z), colnames=colnames(X))
    sig <- (posterior_q95 < 0) | (posterior_q5 > 0)
    sd <- matrix_extractor(gammas, "sd", rownames=colnames(Z), colnames=colnames(X))
    sd <- round(sd,3)
    p_vals <- 1-pnorm(abs(posterior_means) / sd)
    p_vals <- round(p_vals, 3)
    p_val_string <- ifelse(p_vals<.001,"$^{***}$", ifelse(p_vals<.01,"$^{**}$",ifelse(p_vals<.05, "$^{*}$", "")))
    stopifnot(ncol(posterior_means)==ncol(X))
    stopifnot(nrow(posterior_means)==ncol(Z))

    print_table <- function(min_col, max_col) {
        cat("
\\begin{table}[H] \\centering
\\caption{}
\\label{}
        ")
            cat("\\begin{tabular}{l", rep("c", max_col-min_col +1 ),"}\n ", sep="")
            cat("\\\\[-1.8ex]\\hline\n\\hline \\\\[-1.8ex]")

            cat(" & ")

            for (j in min_col:max_col) {
                cat(colnames(posterior_means)[j])
                if (j!=max_col) {
                    cat(" & ")
                } else{
                    cat(" \\\\ ")
                }
            }

            for (i in 1:nrow(posterior_means)) {
                cat(rownames(posterior_means)[i], "&")
                for (j in min_col:max_col) {
                    cat(posterior_means[i,j],p_val_string[i,j], sep="")
                    if (j!=max_col) {
                        cat(" & ")
                    } else {
                        cat(" \\\\ ")
                    }
                }
                cat("\n & ")
                for (j in min_col:max_col) {
                    cat("(",sd[i,j], ")", sep="")
                    if (j!=max_col) {
                        cat(" & ")
                    } else {
                        cat(" \\\\ ")
                    }
                }
                cat("\n")
                cat(rep("&", max_col-min_col+1), "\\\\")
            }
            cat("\\end{tabular}\n\\end{table}")
    }

    for (idx in seq(1,ncol(X),5)) {
        print_table(idx, min(idx+4, ncol(X)))
     }
}

# load("../data/stan_draws/ratio_partisanship_supp.Rda")
# colnames(X) <- c("Intercept", "Republian", "Democratic", "Black", "Hispanic", "Asian", "Midwest", "South", "West", "Education","Income", "Female" , "Jewish", "Muslim", "Atheist")
# colnames(Z) <- c("Intercept", "Standardized Age", "Gender", "Education", "Hispanic", "Midwest", "South","West", "Black","Asian", "Female", "Democratic", "Republican", "Other Party")
# draws <-readRDS("../data/stan_draws/ratio_partisanship_draws.Rda")

# ben_stargazer(draws, X,Z)

