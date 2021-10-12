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

# Model difference analysis of deviance
temp_mod_aov <-
  anova(temp_mod_simple$fit, temp_mod_all$fit, test = "Chisq")

####

###
# Binomial models: Nausea is the outcome of interest
logistic_mod <- logistic_reg() |>
  set_mode("classification") |>
  set_engine("glm")

# Model 1: RunnyNose only
nausea_mod_simple <- logistic_mod |>
  fit(Nausea ~ RunnyNose, data = mydata)

# Model 2: all predictors
nausea_mod_all <- logistic_mod |>
  fit(Nausea ~ ., data = mydata)

# Result reports
nausea_mod_tbl <- reg_tab(
  list(nausea_mod_simple, nausea_mod_all),
  c("Runny nose only", "All predictors")
)

# Model difference analysis of deviance
nausea_mod_aov <-
  anova(nausea_mod_simple$fit, nausea_mod_all$fit, test = "Chisq")

####

# save results tables
saveRDS(temp_mod_tbl, file = here::here("results", "temp_mod_tbl.Rds"))
saveRDS(temp_mod_aov, file = here::here("results", "temp_mod_aov.Rds"))
saveRDS(nausea_mod_tbl, file = here::here("results", "nausea_mod_tbl.Rds"))
saveRDS(nausea_mod_aov, file = here::here("results", "nausea_mod_aov.Rds"))
  