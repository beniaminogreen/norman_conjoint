lab_notebooks.pdf: ./data/cleaned_data.parquet ./data/models.Rda
	R -e "require(rmarkdown); render('lab_notebooks.Rmd')"

./data/cleaned_data.parquet: ./data/uncleaned_data.sav ./r_code/00_clean.R
	cd r_code/; R CMD BATCH --vanilla 00_clean.R

./data/models.Rda: ./data/cleaned_data.parquet ./r_code/01_acme.R
	cd r_code/; R CMD BATCH --vanilla 01_acme.R
# ./stan_code/two_level_lm: ./stan_code/two_level_lm.stan ./r_code/test_stan_model.R
# 	cd r_code/; R CMD BATCH --vanilla test_stan_model.R


