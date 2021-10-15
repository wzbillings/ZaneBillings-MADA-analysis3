###############################
# Analysis Script 2
# Zane Billings
#
# This script loads the processed, cleaned data, does a simple analysis
# and saves the results to the results folder.
###############################

# Load packages
library(here)
library(tidymodels)

# Load data
dat <- readRDS(here::here("data", "processed_data", "processeddata.Rds"))

# Create initial split
set.seed(470)
dat_split <- rsample::initial_split(dat)
train <- rsample::training(dat_split)
test <- rsample::testing(dat_split)

#####
# Fitting nausea GLMs
#####

# Model specifications
logistic_model <- parsnip::logistic_reg() |>
  parsnip::set_mode("classification") |>
  parsnip::set_engine("glm")

### Model with all predictors

# Recipe with all predictors
nausea_all_pred_rec <- recipes::recipe(Nausea ~ ., data = train)

# Workflow with all predictors
nausea_all_pred_wf <- workflows::workflow() |>
  workflows::add_recipe(nausea_all_pred_rec) |>
  workflows::add_model(logistic_model)

# Fit model with all predictors
nausea_all_pred_fit <- nausea_all_pred_wf |> fit(data = train)

# Examine ROC on training data
nausea_all_pred_aug <-
  broom::augment(nausea_all_pred_fit, train)

nausea_all_pred_roc <- nausea_all_pred_aug |>
  yardstick::roc_curve(truth = Nausea, .pred_Yes, event_level = "second") |>
  autoplot()

nausea_all_pred_auc <- nausea_all_pred_aug |>
  yardstick::roc_auc(truth = Nausea, .pred_Yes, event_level = "second")

# Examine ROC on testing data
nausea_all_pred_aug_test <-
  broom::augment(nausea_all_pred_fit, test)

nausea_all_pred_roc_test <- nausea_all_pred_aug_test |>
  yardstick::roc_curve(truth = Nausea, .pred_Yes, event_level = "second") |>
  autoplot()

nausea_all_pred_auc_test <- nausea_all_pred_aug_test |>
  yardstick::roc_auc(truth = Nausea, .pred_Yes, event_level = "second")

### Model with only runny nose

# Recipe with only runny nose
nausea_rn_rec <- recipes::recipe(Nausea ~ RunnyNose, data = train)

# Workflow with only runny nose
nausea_rn_wf <- workflows::workflow() |>
  workflows::add_recipe(nausea_rn_rec) |>
  workflows::add_model(logistic_model)

# Fit model with only runny nose
nausea_rn_fit <- nausea_rn_wf |> fit(data = train)

# Examine ROC on training data
nausea_rn_aug <-
  broom::augment(nausea_rn_fit, train)

nausea_rn_roc <- nausea_rn_aug |>
  yardstick::roc_curve(truth = Nausea, .pred_Yes, event_level = "second") |>
  autoplot()
nausea_rn_roc

nausea_rn_auc <- nausea_rn_aug |>
  yardstick::roc_auc(truth = Nausea, .pred_Yes, event_level = "second")
nausea_rn_auc

# Examine ROC on testing data
nausea_rn_aug_test <-
  broom::augment(nausea_rn_fit, test)

nausea_rn_roc_test <- nausea_rn_aug_test |>
  yardstick::roc_curve(truth = Nausea, .pred_Yes, event_level = "second") |>
  autoplot()
nausea_rn_roc_test

nausea_rn_auc_test <- nausea_rn_aug_test |>
  yardstick::roc_auc(truth = Nausea, .pred_Yes, event_level = "second")
nausea_rn_auc_test
