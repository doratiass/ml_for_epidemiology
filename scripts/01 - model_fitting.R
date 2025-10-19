# ============================================================================ #
# Load Required Packages
# ============================================================================ #
library(tidyverse)
library(tidymodels)
library(broom)
library(patchwork)
library(future)
library(furrr)
source("scripts/00 - funcs.R")
tidymodels_prefer()

# ============================================================================ #
# Definitions and Settings ----------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Define Model-Specific Parameters and Functions -----------------------------
# ---------------------------------------------------------------------------- #
thresh_other <- 0.1 # Threshold for grouping infrequent levels to "other_combined"
thresh_corr <- 0.9 # Correlation threshold for removing highly correlated predictors

# Standardize parallel processing
options(future.globals.maxSize = 4 * 1024^3) # 4GB in bytes
all_cores <- parallel::detectCores()
plan(multisession, workers = all_cores - 1)


ctl_grid <- control_resamples(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = 'everything'
)

get_lm_coefs <- function(x) {
  x %>%
    extract_fit_parsnip()
}

ctl_grid_lasso <- control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = 'everything',
  extract = get_lm_coefs
)

# ============================================================================ #
# Build Model Data Frame -------------------------------------------------------
# ============================================================================ #
# Load synthetic data and prepare for modeling
load("data/syn_data.RData")
ml_df <- syn %>%
  mutate(id = row_number(), .before = dmg_admission_age) %>% # Add ID column for realism
  mutate(
    outcome = factor(outcome, levels = c("centenarian", "not_centenarian"))
  ) %>% # Ensure correct outcome reference level
  mutate_if(is.logical, as.factor) # Convert logicals to factors for modeling

# Identify variables with >10% missing data to exclude from modeling of LR & LASSO
miss_10_vars <- tibble(
  name = colnames(ml_df),
  NAs = sapply(ml_df, function(x) sum(is.na(x))),
  p = round(NAs / nrow(ml_df) * 100, 2)
) %>%
  filter(p > 10) %>%
  pull(name)

print(sprintf(
  "Excluding %d variables with >10%% missing data: %s",
  length(miss_10_vars),
  paste(miss_10_vars, collapse = ", ")
))
# ---------------------------------------------------------------------------- #
## Create ML Data --------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Create stratified train/test split and cross-validation folds
set.seed(2020)
df_split <- initial_split(ml_df, strata = outcome) # Ensure balanced outcomes
df_train <- training(df_split)
df_test <- testing(df_split)
df_train_cv <- vfold_cv(df_train, v = 10, strata = outcome)

# ### Impute Train Data ----------------------------------------------------------
# # Create imputed training data for EDA and SHAP analysis (for later use)
# imp_rec <- recipe(
#   outcome ~ .,
#   data = df_train,
#   strings_as_factors = FALSE
# ) %>%
#   update_role(id, new_role = "ID") %>%
#   step_rm(all_of(!!miss_10_vars)) %>%
#   step_impute_knn(all_predictors(), neighbors = 3) %>%
#   step_date(
#     all_date_predictors(),
#     keep_original_cols = FALSE,
#     features = "month"
#   ) %>%
#   step_corr(
#     all_numeric_predictors(),
#     threshold = thresh_corr,
#     method = "spearman"
#   )

# imp_prep <- imp_rec %>%
#   prep(log_changes = TRUE, verbose = TRUE)

# imp_train_df <- bake(imp_prep, df_train)
# ============================================================================ #
# LASSO Imputed Model --------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Prepare the Data -----------------------------------------------------------
# ---------------------------------------------------------------------------- #
lasso_rec <- recipe(
  outcome ~ .,
  data = df_train,
  strings_as_factors = FALSE
) %>%
  # Remove ID variables from predictors
  update_role(id, new_role = "ID") %>%
  # Remove variables with >10% missing data
  step_rm(dmg_immigration_year, med_fev) %>% # Instead of all_of(miss_10_vars)
  # Handle missing data with KNN imputation
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  # Convert dates to month
  step_date(
    all_date_predictors(),
    keep_original_cols = FALSE,
    features = "month"
  ) %>%
  # Group infrequent categorical levels
  step_other(
    all_nominal_predictors(),
    threshold = thresh_other,
    other = "other_combined"
  ) %>%
  # Create dummy variables
  step_dummy(all_nominal_predictors(), naming = new_sep_names) %>%
  # Remove zero variance predictors
  step_zv(all_numeric_predictors()) %>%
  # Remove highly correlated variables
  step_corr(
    all_numeric_predictors(),
    threshold = thresh_corr,
    method = "spearman"
  ) %>%
  # Normalize numeric variables (Important for LASSO!)
  step_normalize(all_numeric_predictors())

# ---------------------------------------------------------------------------- #
## Setup Model and Tune Hyperparameters ---------------------------------------
# ---------------------------------------------------------------------------- #
# Define LASSO model specification
lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode('classification')

# Create workflow for hyperparameter tuning
lasso_wf <- workflow() %>%
  add_model(lasso_spec) %>% # Add model
  add_recipe(lasso_rec) # Add recipe with preprocessing steps

# Create grid of penalty values for tuning
lambda_grid <- grid_regular(penalty(), levels = 100)

set.seed(2020)
lasso_res <- tune_grid(
  lasso_wf,
  resamples = df_train_cv, # Use defined CV folds
  grid = lambda_grid, # Use defined grid of penalty values
  metrics = metric_set(roc_auc, pr_auc), # Evaluate using ROC-AUC and PR-AUC
  control = ctl_grid_lasso # Use parallel processing controls
)

# Inspect tuning results for optimal penalty value
lasso_res %>%
  autoplot()

lasso_res %>%
  show_best(metric = "roc_auc")
# ---------------------------------------------------------------------------- #
## Select Best LASSO Model -----------------------------------------------------
# ---------------------------------------------------------------------------- #
# Select best model based on ROC-AUC
lasso_best_auc <- lasso_res %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best hyperparameters
final_lasso <- finalize_workflow(
  lasso_wf,
  lasso_best_auc
)

# Fit finalized model on test data
final_lasso_fit <- final_lasso %>%
  last_fit(df_split)

# Collect and display performance metrics
collect_metrics(final_lasso_fit)

# Fit finalized model on training data for SHAP analysis (for later use)
set.seed(2020)
lasso_train_fit <- fit_resamples(
  final_lasso,
  resamples = df_train_cv,
  metrics = metric_set(roc_auc, accuracy, sens, spec),
  control = ctl_grid
)

# ============================================================================ #
# XGBoost Model --------------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Prepare the Data -----------------------------------------------------------
# ---------------------------------------------------------------------------- #
xgb_rec <- recipe(outcome ~ ., data = df_train, strings_as_factors = FALSE) %>%
  # Remove ID variables from predictors
  update_role(id, new_role = "ID") %>%
  # Convert dates to month
  step_date(
    all_date_predictors(),
    keep_original_cols = FALSE,
    features = "month"
  ) %>%
  # Convert ordered factors to integers
  step_integer(all_ordered_predictors()) %>%
  # Group infrequent categorical levels
  step_other(
    all_nominal_predictors(),
    threshold = thresh_other,
    other = "other_combined"
  ) %>%
  # Create Unknown category for missing categorical data
  step_unknown(all_nominal_predictors()) %>%
  # Create dummy variables
  step_dummy(all_nominal_predictors(), naming = new_sep_names) %>%
  # Remove zero variance predictors
  step_zv(all_numeric_predictors()) %>%
  # Remove highly correlated variables
  step_corr(
    all_numeric_predictors(),
    threshold = thresh_corr,
    method = "spearman"
  )

# ---------------------------------------------------------------------------- #
## Setup Model and Tune Hyperparameters -------------------------------------
# ---------------------------------------------------------------------------- #
# Define XGBoost model specification with tunable hyperparameters
xgb_spec <- boost_tree(
  trees = tune(), # Number of boosting iterations (models to ensemble)
  tree_depth = tune(), # Maximum depth of each tree (controls model complexity)
  min_n = tune(), # Minimum samples required in a node before splitting
  loss_reduction = tune(), # Minimum loss reduction required for splits (regularization)
  sample_size = tune(), # Proportion of training data used for each tree
  mtry = tune(), # Number of predictors randomly sampled at each split
  learn_rate = tune() # Learning rate (controls overfitting)
) %>%
  set_engine("xgboost") %>%
  set_mode("classification") # Classification task - XGBoost can also do regression

# Create workflow for hyperparameter tuning
xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

# Create grid of hyperparameter values for tuning
# Using space-filling design for efficient coverage of hyperparameter space
xgb_grid <- grid_space_filling(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 500
)

# Tune hyperparameters using cross-validation
set.seed(2020)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = df_train_cv,
  grid = xgb_grid,
  metrics = metric_set(roc_auc, pr_auc), # Evaluate using ROC-AUC and PR-AUC
  control = ctl_grid
)

# Inspect tuning results for optimal hyperparameters
xgb_res %>%
  autoplot()

xgb_best_auc <- xgb_res %>%
  show_best(metric = "roc_auc", n = 4)

# ---------------------------------------------------------------------------- #
## Select Best XGBoost Model --------------------------------------------------
# ---------------------------------------------------------------------------- #
# Select best model based on ROC-AUC
final_xgb <- finalize_workflow(
  xgb_wf,
  xgb_best_auc[1, ]
)

# Fit finalized model on test data
final_xgb_fit <- final_xgb %>%
  last_fit(df_split)

# Collect and display performance metrics
collect_metrics(final_xgb_fit)

# Fit finalized model on training data for SHAP analysis (for later use)
set.seed(2020)
xgb_train_fit <- fit_resamples(
  final_xgb,
  resamples = df_train_cv,
  metrics = metric_set(roc_auc, accuracy, sens, spec),
  control = ctl_grid
)

plan(sequential)

save(
  ml_df,
  df_split,
  df_train,
  df_test,
  df_train_cv,
  lasso_rec,
  lasso_prep,
  lasso_spec,
  lambda_grid,
  lasso_wf,
  lasso_res,
  final_lasso,
  lasso_train_fit,
  final_lasso_fit,
  xgb_rec,
  xgb_prep,
  xgb_spec,
  xgb_grid,
  xgb_wf,
  xgb_res,
  final_xgb,
  xgb_train_fit,
  final_xgb_fit,
  file = "data/model_fits.RData"
)
