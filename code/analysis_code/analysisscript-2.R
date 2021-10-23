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

##############################
#Brent's Part
##############################

#Make sure all packages are loaded such as Tidyverse 

#First we will set up the model
lm_mod <- linear_reg() %>%
  set_engine("lm")

#Now we will create our recipe using the main continuous outcome of BodyTemp
#instead of Nausea, which we used as our main categorical outcome

#For our first model we will use all predictors
#Create recipe for model
lm_rec_all <- recipes::recipe(BodyTemp ~ ., data = train_data)

#Now we create the workflow using our established recipe
lm_wf_all <- workflows::workflow() %>%
  workflows::add_recipe(lm_rec_all) %>%
  workflows::add_model(lm_mod)

#Now we can fit our model
lm_fit_all <- lm_wf_all %>%
  parsnip::fit(data = train_data)

#Now we will examine our fitted model
lm_fit_all %>%
 parsnip::extract_spec_parsnip() 

#Now we will use the predict function to apply the recipe to the new data and
#then pass the collected information to the new model
#note that we use the test data here and in the next command line 
predict(lm_fit_all, test_data)

#Utilize 'Augment' function
lm_fit_all_aug <- augment(lm_fit_all, test_data)

lm_fit_all_aug %>%
  select(BodyTemp, .pred_class, .pred_Yes)

#We will now create and predict a model with BodyTemp and just the main 
#predictor (RunnyNose)
#Set up recipe as before
lm_rec_main <- recipes::recipe(BodyTemp ~ RunnyNose, data = train_data)

#Create workflow as before
lm_wf_main <- workflows::workflow() %>%
  workflows::add_recipe(lm_rec_main) %>%
  workflows::add_model(lm_mod)

#We will now fit our model as before
lm_fit_main <- lm_wf_main %>%
  parsnip::fit(data = train_data)

#Examine new model
lm_fit_main %>%
  parsnip::extract_spec_parsnip() 

#Utilize predict function as before
predict(lm_fit_main, test_data)

#Utilize augment function as before
lm_fit_main_aug <- augment(lm_fit_main, test_data)

lm_fit_main_aug %>%
  select(BodyTemp, .pred_class, .pred_Yes)

#We will now calculate metrics for each new model
bodytemp_metrics <- bodytemp_predictions %>%
  group_by(data, model) %>%
  metrics(truth = BodyTemp, estimate = .pred)

#plot the metrics
metrics_plot <- bodytemp_metrics %>%
  ggplot(aes(y = model, x = .estimate, fill = data)) +
  geom_bar(position = "dodge") 
  
