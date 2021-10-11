###############################
# Analysis Script
# Zane Billings
#
# This script loads the processed, cleaned data, does a simple analysis
# and saves the results to the results folder.
###############################

# Load needed packages
library(tidymodels)
library(gtsummary)
library(purrr)

# path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data fitting/statistical analysis
######################################

# A quick fcn to get the tables that I want to report
reg_tab <- function(mod_list, span_list = NULL) {
  mods <- purrr::map(mod_list, ~.x$fit)
  gtsummary::tbl_merge(
    tbls = purrr::map(mods, gtsummary::tbl_regression),
    tab_spanner = span_list
  )
}

###
# Normal models: BodyTemp is the outcome of interest
linear_mod <- linear_reg() |>
  set_mode("regression") |>
  set_engine("lm")

# Model 1: RunnyNose only
temp_mod_simple <- linear_mod |>
  fit(BodyTemp ~ RunnyNose, data = mydata)

# Model 2: all predictors
temp_mod_all <- linear_mod |>
  fit(BodyTemp ~ ., data = mydata)

# Result reports
temp_mod_tbl <- reg_tab(
  list(temp_mod_simple, temp_mod_all),
  c("Runny nose only", "All predictors")
)

####

###
# Binomial models: Nausea is the outcome of interest
# Model 1: RunnyNose only

# Model 2: all predictors

####

#look at fit results
print(lmtable)

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

  