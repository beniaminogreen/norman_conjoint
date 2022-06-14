./data/cleaned_data.parquet: ./data/uncleaned_data.sav ./r_code/00_clean.R
	cd r_code/; R CMD BATCH --vanilla 00_clean.R
./stan_code/two_level_lm: ./stan_code/two_level_lm.stan ./r_code/test_stan_model.R
	cd r_code/; R CMD BATCH --vanilla test_stan_model.R


