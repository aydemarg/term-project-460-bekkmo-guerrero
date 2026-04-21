#Install packages if needed
#install.packages("ranger")
#install.packages("tidymodels")
#install.packages("glmnet")
#install.packages("vip")

# -------------------------------
# 1. Libraries
# -------------------------------
library(tidyverse)
library(tidymodels)
library(glmnet)
library(ranger)
library(vip)

# -------------------------------
# 2. Load data
# -------------------------------
load("data_clean/olympics_final.RData")

#check object name 
#ls()

#assign to data
data = olympics_final

# -----------------------------------
# 3. Basic checks
# -----------------------------------
#dim(data)
#glimpse(data)

# -----------------------------------
# 4. Drop variables with too much missingness
# Keep variables with less than 50% missing
# -----------------------------------
missing_pct = colSums(is.na(data)) / nrow(data)

data = data |>
  select(all_of(names(missing_pct[missing_pct < 0.5])))

# -----------------------------------
# 5. Rename outcome variable
# -----------------------------------
data = data |>
  rename(total_medals = total)

# -----------------------------------
# 6. Remove leakage and ID variables
# gold, silver, bronze should not be predictors
# country is just an identifier
# year can be removed for a simpler first model
# -----------------------------------
data = data |>
  select(-country, -gold, -silver, -bronze, -year)

# -----------------------------------
# 7. Check remaining missingness
# -----------------------------------
#colSums(is.na(data)) |> sort(decreasing = TRUE)
#dim(data)

# -----------------------------------
# 8. Train / test split
# -----------------------------------
set.seed(123)

split = initial_split(data, prop = 0.8)

train_data = training(split)
test_data = testing(split)

# -----------------------------------
# 9. Cross-validation folds
# -----------------------------------
set.seed(123)

cv_folds = vfold_cv(train_data, v = 5)

# -----------------------------------
# 10. Recipe
# Impute missing values inside the pipeline
# Log-transform numeric predictors to help with skewness
# Normalize numeric predictors
# Dummy-code categorical predictors if any remain
# -----------------------------------
recipe_model = recipe(total_medals ~ ., data = train_data) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# inspect processed training data
# recipe_model |> prep() |> juice() |> glimpse()

# -----------------------------------
# 11. Model 1: Linear Regression
# -----------------------------------
lm_model = linear_reg() |>
  set_engine("lm")

lm_workflow = workflow() |>
  add_model(lm_model) |>
  add_recipe(recipe_model)

lm_results = lm_workflow |>
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(rmse, rsq)
  )

lm_metrics = lm_results |>
  collect_metrics()

#lm_metrics

# -----------------------------------
# 12. Model 2: Lasso Regression
# -----------------------------------
lasso_model = linear_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_engine("glmnet")

lasso_workflow = workflow() |>
  add_model(lasso_model) |>
  add_recipe(recipe_model)

set.seed(123)

lasso_results = lasso_workflow |>
  tune_grid(
    resamples = cv_folds,
    grid = 30,
    metrics = metric_set(rmse, rsq)
  )

lasso_best = lasso_results |>
  show_best(metric = "rmse")

#lasso_best

#autoplot(lasso_results) + theme_bw()

# -----------------------------------
# 13. Model 3: Random Forest
# More advanced ML model
# -----------------------------------
rf_model = rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500
) |>
  set_engine("ranger") |>
  set_mode("regression")

rf_workflow = workflow() |>
  add_model(rf_model) |>
  add_recipe(recipe_model)

set.seed(123)

rf_results = rf_workflow |>
  tune_grid(
    resamples = cv_folds,
    grid = 20,
    metrics = metric_set(rmse, rsq)
  )

rf_best = rf_results |>
  show_best(metric = "rmse")

#rf_best

#autoplot(rf_results) + theme_bw()

# -----------------------------------
# 14. Compare cross-validation performance
# -----------------------------------
lm_summary = lm_results |>
  collect_metrics() |>
  mutate(model = "Linear Regression")

lasso_summary = lasso_results |>
  collect_metrics() |>
  group_by(.metric) |>
  slice_min(mean, n = 1) |>
  ungroup() |>
  mutate(model = "Lasso")

rf_summary = rf_results |>
  collect_metrics() |>
  group_by(.metric) |>
  slice_min(mean, n = 1) |>
  ungroup() |>
  mutate(model = "Random Forest")

model_comparison = bind_rows(lm_summary, lasso_summary, rf_summary)

#model_comparison

# -----------------------------------
# 15. Finalize the best model
# Here I assume Random Forest performs best.
# If Lasso performs better, switch rf_workflow/rf_results to lasso_workflow/lasso_results
# -----------------------------------
final_rf = rf_workflow |>
  finalize_workflow(
    select_best(rf_results, metric = "rmse")
  )

# -----------------------------------
# 16. Final test-set evaluation
# Only do this after choosing the model
# -----------------------------------
rf_fit = final_rf |>
  last_fit(split, metrics = metric_set(rmse, rsq))

rf_test_metrics = rf_fit |>
  collect_metrics()

#rf_test_metrics

# -----------------------------------
# 17. Test-set predictions
# -----------------------------------
rf_predictions = rf_fit |>
  collect_predictions()

#head(rf_predictions)

# -----------------------------------
# 18. Plot predicted vs actual medals
# -----------------------------------
# ggplot(rf_predictions, aes(x = .pred, y = total_medals)) +
#   geom_point(alpha = 0.7) +
#   geom_abline(slope = 1, intercept = 0) +
#   labs(
#     title = "Predicted vs Actual Olympic Medal Counts",
#     x = "Predicted medals",
#     y = "Actual medals"
#   ) +
#   theme_minimal()

# -----------------------------------
# 19. Variable importance for Random Forest
# Need to fit finalized workflow on full training data first
# -----------------------------------
rf_model_importance = rand_forest(
  mtry = select_best(rf_results, metric = "rmse")$mtry,
  min_n = select_best(rf_results, metric = "rmse")$min_n,
  trees = 500
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

rf_workflow_importance = workflow() |>
  add_model(rf_model_importance) |>
  add_recipe(recipe_model)

rf_fit_importance = rf_workflow_importance |>
  fit(train_data)

importance_tbl = rf_fit_importance |>
  extract_fit_parsnip() |>
  vip::vi()

#importance_tbl

# -----------------------------------
# 20. Optional: coefficients from best Lasso model
# -----------------------------------
final_lasso = lasso_workflow |>
  finalize_workflow(
    select_best(lasso_results, metric = "rmse")
  )

lasso_fit = final_lasso |>
  fit(train_data)

lasso_coefs = lasso_fit |>
  extract_fit_parsnip() |>
  tidy()

#lasso_coefs
