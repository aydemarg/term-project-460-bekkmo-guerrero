# =============================================================================
# Stage 3: Predictive Analysis of Olympic Medal Counts
# =============================================================================
#Install packages if needed
#install.packages("ranger")
#install.packages("tidymodels")
#install.packages("glmnet")
#install.packages("vip")
#install.packages("gt")
#install.packages("webshot2")

# -------------------------------
# 1. Libraries
# -------------------------------
library(tidyverse)
library(tidymodels)
library(glmnet)
library(ranger)
library(vip)
library(gt)
library(readr)
library(dplyr)
library(webshot2)

# -------------------------------
# 2. Load data
# -------------------------------
load("data_clean/olympics_final.RData")

#check object name 
#ls()

#assign to data
data = olympics_final

# -----------------------------------------------------------------------------
# 3. Data cleaning prior to modeling
# -----------------------------------------------------------------------------
# 3a. Fix duplicate life_expectancy columns.
#     life_expectancy was listed twice in the World Bank indicator vector
#     (once under demographics, once under health), so the join produced
#     life_expectancy.x and life_expectancy.y (identical values). Keep one.
if ("life_expectancy.x" %in% names(data)) {
  data <- data |>
    rename(life_expectancy = life_expectancy.x) |>
    select(-any_of("life_expectancy.y"))
}

# 3b. Drop variables with too much missingness (>50% missing).
missing_pct <- colSums(is.na(data)) / nrow(data)
data <- data |> select(all_of(names(missing_pct[missing_pct < 0.5])))

# 3c. Rename outcome and create log-transformed target.
data <- data |>
  rename(total_medals = total) |>
  mutate(log_total_medals = log1p(total_medals))   # log1p = log(1 + x) handles zeros

# 3d. Drop leakage and raw-target columns.
#     gold/silver/bronze are components of total -> perfect leakage.
#     total_medals is the raw target -> drop now that we have the log version.
#     year is dropped to keep a simple cross-sectional predictor set.
#     country, year, and season are KEPT in the frame (needed for grouped CV) but their role in
#     the recipe is set to "id variable" so they are not used as a predictor.
data <- data |>
  select(-gold, -silver, -bronze, -total_medals)

# Sanity check: the frame should now contain country, season, the WB
# predictors, and log_total_medals.
#glimpse(data)


# -----------------------------------------------------------------------------
# 4. Grouped train/test split
# -----------------------------------------------------------------------------
# group_initial_split ensures that any given country is entirely in either
# training or testing -- never split across them. This gives an honest
# out-of-sample estimate of how well the model predicts for NEW countries.
set.seed(123)
split      <- group_initial_split(data, prop = 0.8, group = country)
train_data <- training(split)
test_data  <- testing(split)

# -----------------------------------------------------------------------------
# 5. Grouped K-fold cross-validation on the training set
# -----------------------------------------------------------------------------
# group_vfold_cv keeps all rows for one country in the same fold. This way,
# hyperparameter tuning via CV also reflects country-generalization ability.
set.seed(123)
cv_folds <- group_vfold_cv(train_data, group = country, v = 5)

# -----------------------------------------------------------------------------
# 6. Preprocessing recipe
# -----------------------------------------------------------------------------
# Steps (executed inside each CV resample so there is NO information leakage):
#   - Mark country, year, and season as an id variable (kept in data, not used as a predictor).
#   - Median-impute numeric predictors (some indicators have missing values).
#   - Mode-impute nominal predictors.
#   - Dummy-encode nominal predictors (season -> one dummy column).
#   - Drop zero-variance predictors (protects lm/glmnet from degenerate cols).
#   - Standardize numeric predictors (helps Lasso; harmless for RF).
recipe_model <- recipe(log_total_medals ~ ., data = train_data) |>
  update_role(country, year, season, new_role = "id variable") |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# Inspect what the model actually sees:
#recipe_model |> prep() |> juice() |> glimpse()

 
# -----------------------------------------------------------------------------
# 7. Baseline: "always predict the training-set mean"
# -----------------------------------------------------------------------------
# If the ML models cannot beat this, they are not learning anything useful.
# For a model that predicts the training mean on every observation, the RMSE
# equals the standard deviation of the target. We use this as the baseline
# CV RMSE so it is directly comparable to the other models' CV metrics.
baseline_pred_value <- mean(train_data$log_total_medals, na.rm = TRUE)
baseline_cv_rmse    <- sd(train_data$log_total_medals, na.rm = TRUE)
baseline_test_rmse  <- sqrt(mean(
  (test_data$log_total_medals - baseline_pred_value)^2, na.rm = TRUE
))

# -----------------------------------------------------------------------------
# 8. Model 1: Linear Regression (baseline parametric model)
# -----------------------------------------------------------------------------
lm_model <- linear_reg() |>
  set_engine("lm")

lm_workflow <- workflow() |>
  add_model(lm_model) |>
  add_recipe(recipe_model)

lm_results <- lm_workflow |>
  fit_resamples(
    resamples = cv_folds,
    metrics   = metric_set(rmse, rsq, mae)
  )

# -----------------------------------------------------------------------------
# 9. Model 2: Lasso Regression (regularized linear model)
# -----------------------------------------------------------------------------
# Lasso applies an L1 penalty that can shrink coefficients exactly to zero --
# useful when we have ~60 candidate predictors, many of which are correlated
# (e.g., unemployment_total, unemployment_male, unemployment_female).
lasso_model <- linear_reg(
  penalty = tune(),
  mixture = 1                  # mixture = 1 -> pure Lasso
) |>
  set_engine("glmnet")

lasso_workflow <- workflow() |>
  add_model(lasso_model) |>
  add_recipe(recipe_model)

set.seed(123)
lasso_results <- lasso_workflow |>
  tune_grid(
    resamples = cv_folds,
    grid      = 30,
    metrics   = metric_set(rmse, rsq, mae)
  )

# -----------------------------------------------------------------------------
# 10. Model 3: Random Forest (non-linear, flexible, handles interactions)
# -----------------------------------------------------------------------------
# This is our "advanced ML" challenge model: RF is not covered in class and
# captures non-linear relationships and interactions automatically. It also
# provides permutation/impurity-based variable importance for interpretation.
rf_model <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 500
) |>
  set_engine("ranger") |>
  set_mode("regression")

rf_workflow <- workflow() |>
  add_model(rf_model) |>
  add_recipe(recipe_model)

set.seed(123)
rf_results <- rf_workflow |>
  tune_grid(
    resamples = cv_folds,
    grid      = 20,
    metrics   = metric_set(rmse, rsq, mae)
  )

# -----------------------------------------------------------------------------
# 11. Collect and compare CV performance across models
# -----------------------------------------------------------------------------
# For each tuned model (Lasso, RF), we first pick the single hyperparameter
# configuration with the lowest CV RMSE, then report all three metrics
# (rmse, rsq, mae) AT THAT CONFIG. Picking metrics independently would give
# a Frankenstein row whose metrics came from different configs.
lm_summary <- lm_results |>
  collect_metrics() |>
  mutate(model = "Linear Regression") |>
  select(model, .metric, mean, std_err)

best_lasso_config <- select_best(lasso_results, metric = "rmse")
lasso_summary <- lasso_results |>
  collect_metrics() |>
  inner_join(best_lasso_config |> select(.config), by = ".config") |>
  mutate(model = "Lasso") |>
  select(model, .metric, mean, std_err)

best_rf_config <- select_best(rf_results, metric = "rmse")
rf_summary <- rf_results |>
  collect_metrics() |>
  inner_join(best_rf_config |> select(.config), by = ".config") |>
  mutate(model = "Random Forest") |>
  select(model, .metric, mean, std_err)

baseline_summary <- tibble(
  model   = "Baseline (training mean)",
  .metric = "rmse",
  mean    = baseline_cv_rmse,
  std_err = NA_real_
)

model_comparison <- bind_rows(
  baseline_summary, lm_summary, lasso_summary, rf_summary
)

# Save both long and wide formats. Wide is easier to read in the report.
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
write_csv(model_comparison, "output/tables/cv_model_comparison_long.csv")

model_comparison_img <- model_comparison |>
  gt() |>
  fmt_number(columns = c(mean, std_err), decimals = 3)

gtsave(model_comparison_img, "output/tables/cv_model_comparison_long.png")

model_comparison_wide <- model_comparison |>
  select(model, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  arrange(rmse)

write_csv(model_comparison_wide, "output/tables/cv_model_comparison_wide.csv")

model_comparison_wide_img <- model_comparison_wide |>
  gt() |>
  fmt_number(columns = c(rmse, mae, rsq), decimals = 3)

gtsave(model_comparison_wide_img, "output/tables/cv_model_comparison_wide.png")

# -----------------------------------------------------------------------------
# 12. Pick the best model and fit on full training data, evaluate on test set
# -----------------------------------------------------------------------------
# Compare CV RMSE (lower is better). We programmatically pick the best of the
# tuned models. If ties, preference order is RF > Lasso > LM (most flexible
# wins) because we selected RF as the "advanced ML" challenge.
cv_rmse <- model_comparison |> filter(.metric == "rmse")
best_model_name <- cv_rmse |>
  filter(model != "Baseline (training mean)") |>
  slice_min(mean, n = 1, with_ties = FALSE) |>
  pull(model)

message("Best model by CV RMSE: ", best_model_name)

# Finalize whichever workflow won, using the tuned best parameters.
if (best_model_name == "Random Forest") {
  final_workflow <- rf_workflow |>
    finalize_workflow(select_best(rf_results, metric = "rmse"))
} else if (best_model_name == "Lasso") {
  final_workflow <- lasso_workflow |>
    finalize_workflow(select_best(lasso_results, metric = "rmse"))
} else {
  final_workflow <- lm_workflow
}

# last_fit refits on the full training set and evaluates ONCE on the held-out
# test set. This is the only time we touch the test set.
final_fit <- final_workflow |>
  last_fit(split, metrics = metric_set(rmse, rsq, mae))

test_metrics <- final_fit |>
  collect_metrics() |>
  mutate(model = best_model_name) |>
  select(model, .metric, .estimate)

# Append the baseline for side-by-side comparison on the test set.
test_metrics_with_baseline <- bind_rows(
  test_metrics,
  tibble(
    model     = "Baseline (training mean)",
    .metric   = "rmse",
    .estimate = baseline_test_rmse
  )
)

write_csv(test_metrics_with_baseline, "output/tables/test_set_metrics.csv")

test_metrics_with_baseline_img <- test_metrics_with_baseline |>
  gt() |>
  fmt_number(columns = .estimate, decimals = 3)

gtsave(test_metrics_with_baseline_img, "output/tables/test_set_metrics.png")

# -----------------------------------------------------------------------------
# 13. Predicted vs. actual on the test set (log space and back-transformed)
# -----------------------------------------------------------------------------
predictions <- final_fit |>
  collect_predictions() |>
  mutate(
    pred_medals   = expm1(.pred),              # back-transform for readability
    actual_medals = expm1(log_total_medals)
  )

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# Plot on the log scale (how the model was actually trained/evaluated).
p_pred_log <- ggplot(predictions, aes(x = .pred, y = log_total_medals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title    = paste0("Predicted vs. Actual (log scale) -- ", best_model_name),
    subtitle = "Test-set predictions. Dashed line is perfect prediction.",
    x        = "Predicted log(1 + medals)",
    y        = "Actual log(1 + medals)"
  ) +
  theme_minimal()

ggsave("output/figures/fig17_pred_vs_actual_log.png",
       plot = p_pred_log, width = 8, height = 5, dpi = 300)

# Plot on the original medal-count scale for intuitive reading.
p_pred_raw <- ggplot(predictions, aes(x = pred_medals, y = actual_medals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title    = paste0("Predicted vs. Actual Medal Counts -- ", best_model_name),
    subtitle = "Test-set predictions, back-transformed to medal scale.",
    x        = "Predicted total medals",
    y        = "Actual total medals"
  ) +
  theme_minimal()

ggsave("output/figures/fig18_pred_vs_actual_counts.png",
       plot = p_pred_raw, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 14. Variable importance (only meaningful if RF wins; compute it anyway)
# -----------------------------------------------------------------------------
# Refit RF with impurity importance on the full training set.
if (best_model_name == "Random Forest") {
  best_rf_params <- select_best(rf_results, metric = "rmse")
  
  rf_model_vip <- rand_forest(
    mtry  = best_rf_params$mtry,
    min_n = best_rf_params$min_n,
    trees = 500
  ) |>
    set_engine("ranger", importance = "impurity") |>
    set_mode("regression")
  
  rf_workflow_vip <- workflow() |>
    add_model(rf_model_vip) |>
    add_recipe(recipe_model)
  
  rf_fit_vip <- rf_workflow_vip |> fit(train_data)
  
  importance_tbl <- rf_fit_vip |>
    extract_fit_parsnip() |>
    vip::vi()
  
  write_csv(importance_tbl, "output/tables/rf_variable_importance.csv")
  
  importance_tbl_img <- importance_tbl |>
    gt() |>
    fmt_number(columns = Importance, decimals = 3)
  
  gtsave(importance_tbl_img, "output/tables/rf_variable_importance.png")
  
  p_vip <- importance_tbl |>
    slice_max(Importance, n = 15) |>
    ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title    = "Top 15 Variables by Random Forest Importance",
      subtitle = "Impurity-based importance, trained on full training set",
      x        = NULL,
      y        = "Importance"
    ) +
    theme_minimal()
  
  ggsave("output/figures/fig19_rf_variable_importance.png",
         plot = p_vip, width = 8, height = 5, dpi = 300)
}

# -----------------------------------------------------------------------------
# 15. Lasso coefficients (for interpretability of the linear model family)
# -----------------------------------------------------------------------------
final_lasso <- lasso_workflow |>
  finalize_workflow(select_best(lasso_results, metric = "rmse"))

lasso_fit <- final_lasso |> fit(train_data)

lasso_coefs <- lasso_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0, term != "(Intercept)") |>
  arrange(desc(abs(estimate)))

write_csv(lasso_coefs, "output/tables/lasso_nonzero_coefficients.csv")

lasso_coefs_img <- lasso_coefs |>
  gt() |>
  fmt_number(columns = c(estimate, penalty), decimals = 3)

gtsave(lasso_coefs_img, "output/tables/lasso_nonzero_coefficients.png")

# -----------------------------------------------------------------------------
# 16. Save all fitted model objects for reproducibility
# -----------------------------------------------------------------------------
save(
  split, cv_folds, recipe_model,
  lm_results, lasso_results, rf_results,
  final_fit, predictions, test_metrics,
  model_comparison,
  file = "output/stage3_model_outputs.RData"
)

# Stop parallel backend if it was started above.
# if (exists("cl")) stopCluster(cl)

message("Script 5 complete. Outputs saved to /output.")

 
 
 
 
 
 
 
 
 
 
 