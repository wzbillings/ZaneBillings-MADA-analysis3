###############################
# Basic ML models
# Zane Billings
# In this script, I will preprocess the data, and fit a few basic ML models.
# The main outcome here will be Body Temperature, and the models I will fit are:
# 1. Regression tree
# 2. LASSO
# 3. Random forest
###############################

# Packages and setup
library(here)
library(tidymodels)

# Load the data
dat_orig <- readRDS(here::here("data", "processed_data", "processeddata.rds"))

###############################
# Data preprocessing
###############################
# In the previous analysis, we had an issue with rank deficient linear models.
# This was due to having variables for presence/absence AND symptom severity
# for Weakness, Cough, and Myalgia. So we will remove the binary versions.
# We will also ensure that the intensity variables are coded as ordinal vars.
dat <- dat_orig |>
  # Remove the unwanted variables
  dplyr::select(-c(CoughYN, CoughYN2, MyalgiaYN, WeaknessYN)) |>
  # Rename CoughIntensity to Cough to save me 10 seconds of typing
  dplyr::rename(Cough = CoughIntensity) |>
  # Code symptom intensities as ordered factors
  dplyr::mutate(
    dplyr::across(
      .cols = c(Cough, Myalgia, Weakness),
      .fns = ~factor(.x,
                     levels = c("None", "Mild", "Moderate", "Severe"),
                     ordered = TRUE)
    )
  )

# Next, we will remove any binary variables with < 50 events. These variables
# are unlikely to be predictive.
dat <- dat |>
  dplyr::select(
    # Select all variables with at least 50 yes
    where(~sum(.x == "Yes") >= 50),
    # Select all variables where the level set is not c("No", "Yes")
    where(~all(levels(.x) != c("No", "Yes")))
  )

###############################
# Modeling setup
###############################
set.seed(123)

# Before we begin modeling, we will split the data into testing and training
# sets. We will also resample the testing set using CV.
dat_split <- rsample::initial_split(dat, prop = .7, strata = BodyTemp)
train <- rsample::training(dat_split)
test <- rsample::testing(dat_split)
resamples <- rsample::vfold_cv(train, v = 5, repeats = 5, strata = BodyTemp)

# Now we will create a recipe for fitting the models.
bt_rec_ord <- recipe(BodyTemp ~ ., data = train) |>
  step_ordinalscore(Cough, Weakness, Myalgia) |>
  step_dummy(all_nominal_predictors())

bt_rec_nom <- recipe(BodyTemp ~ ., data = train) |>
  step_dummy(all_nominal_predictors())

# Define a metric set so we can tune on RMSE only.
bt_met <- metric_set(rmse)

###############################
# Decision tree
###############################
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) |>
  set_engine("rpart") |>
  set_mode("regression")

tree_grid <- grid_regular(
  cost_complexity(), tree_depth(),
  levels = 5
)

tree_wf <- workflow() |>
  add_model(tree_spec) |>
  add_recipe(bt_rec_ord)

tree_res <- tree_wf |>
  tune_grid(
    resamples = resamples,
    grid = tree_grid,
    metrics = bt_met,
    control = control_grid(verbose = TRUE)
  )

best_tree <- select_best(tree_res)

final_tree_wf <- tree_wf |>
  finalize_workflow(best_tree)
