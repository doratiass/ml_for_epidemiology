# ============================================================================ #
# Script: 02 - ml_models.R
# Description: This script is used for model fitting using the tidymodels framework.
# It includes data preparation, imputation, model specification, hyperparameter tuning,
# and evaluation of logistic regression, LASSO, and XGBoost models.
# this script used the tutorials:
# https://juliasilge.com/blog/lasso-the-office/
# https://www.tidymodels.org/learn/models/calibration/
# ============================================================================ #

# ============================================================================ #
# Load Required Packages
# ============================================================================ #
library(tidyverse)
library(MASS)
library(vip)
library(SHAPforxgboost)
library(tidymodels)
library(pdp)
library(gridExtra)
library(ggpubr)
library(parallel)
library(doParallel)
library(klaR)
library(discrim)
library(probably)
library(foreach)
library(stacks)
library(shapviz)
tidymodels_prefer()
source("~/Documents/projects/longevity/scripts/00 - funcs_def.R")
set.seed(45)
cat("\f")

# ------------------------------- #
## Define Model-Specific Parameters and Functions
# ------------------------------- #
thresh_other <- 0.1 # Threshold for grouping infrequent levels to "other_combined"
thresh_corr <- 0.9 # Correlation threshold for removing highly correlated predictors
ctl_grid <- control_resamples(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = 'everything'
)

# new_sep_names: Custom function for dummy variable naming
new_sep_names <- function(var, lvl, ordinal) {
  dummy_names(var = var, lvl = lvl, ordinal = ordinal, sep = "_@_")
}

# ============================================================================ #
# Build Model Data Frame -------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Add Outcome to Data ---------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Load the final merged dataset
load("raw_data/final_df.RData")

# Create ml_df by selecting variables relevant for modeling and recoding outcome.
ml_df <- final_df %>%
  select(starts_with(c(
    "id_nivdaki",
    "outcome_age_lfu",
    "dmg_",
    "med_",
    "lab_",
    "physical_",
    "work_",
    "family_",
    "diet_",
    "comp_"
  ))) %>%
  mutate(
    outcome = factor(
      ifelse(outcome_age_lfu >= 95, "centenarian", "not_centenarian"),
      levels = c("centenarian", "not_centenarian")
    ),
    dmg_immigration_year = ifelse(
      dmg_immigration_year == -2,
      NA,
      dmg_immigration_year
    )
  ) %>%
  filter(!is.na(outcome)) %>%
  select(
    -starts_with(c(
      "death_age_died_2006",
      "dmg_birth_date",
      "dmg_birth_year_1",
      "dmg_birth_year_2",
      "dmg_birth_year",
      "dmg_kibbutz",
      "med_ecg_68",
      "diet_data_useable",
      "outcome_age_lfu"
    ))
  ) %>%
  mutate_if(is.logical, as.factor)

# ---------------------------------------------------------------------------- #
## Remove Variables with > 10% Missing -----------------------------------------
# ---------------------------------------------------------------------------- #
miss_10_vars <- tibble(
  name = colnames(ml_df),
  NAs = sapply(ml_df, function(x) sum(is.na(x))),
  p = round(NAs / nrow(ml_df) * 100, 2)
) %>%
  filter(p > 10) %>%
  pull(name)

# ---------------------------------------------------------------------------- #
## Create ML Data & Define General Variables -----------------------------------
# ---------------------------------------------------------------------------- #
set.seed(1234)
df_split <- initial_split(ml_df, strata = outcome)
df_train <- training(df_split)
df_test <- testing(df_split)
df_train_cv <- vfold_cv(df_train, v = 10, strata = outcome)

### Impute Train Data ----------------------------------------------------------
# Define a recipe to remove unneeded variables, impute missing data using kNN,
# extract date features, and remove highly correlated numeric predictors.
imp_rec <- recipe(outcome ~ ., data = df_train %>% select(-id_nivdaki)) %>%
  step_rm(all_of(c(
    "dmg_immigration_year",
    "med_vital_capacity_index",
    "med_fev",
    "med_shoulder_measure_cm_biacromial",
    "med_pelvic_measure_cm_bicristal",
    "med_left_eye_pressure",
    "med_right_eye_pressure",
    "med_vital_capacity",
    "med_timed_vital_capacity",
    "med_fvc_68",
    "work_hurt_by_superior_forget",
    "work_hurt_by_superior_restrain_retaliate",
    "family_conflict_with_wife",
    "comp_forced_pv"
  ))) %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_date(
    all_date_predictors(),
    keep_original_cols = FALSE,
    features = "month"
  ) %>%
  step_corr(
    all_numeric_predictors(),
    threshold = thresh_corr,
    method = "spearman"
  )

imp_prep <- imp_rec %>%
  prep(strings_as_factors = FALSE, log_changes = TRUE, verbose = TRUE)

imp_train_df <- bake(imp_prep, df_train)

# ============================================================================ #
# Logistic Regression Model --------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Stepwise AIC for Variable Selection -----------------------------------------
# ---------------------------------------------------------------------------- #
nullModel <- glm(outcome ~ 1, data = imp_train_df, family = binomial)
fullModel <- glm(outcome ~ ., data = imp_train_df, family = binomial)

step_model <- stepAIC(
  nullModel,
  direction = 'forward',
  scope = list(upper = fullModel, lower = nullModel),
  trace = 1
)

summary(step_model)

# Rebuild the formula used in the stepwise selection.
step_model_formula <- reformulate(
  unique(str_remove(
    str_remove(
      strsplit(as.character(step_model$formula)[[3]], " + ", fixed = TRUE)[[1]],
      regex("_poly_[1-3]")
    ),
    "\n    "
  )),
  "outcome"
)

# ---------------------------------------------------------------------------- #
## Fit Logistic Regression Model ---------------------------------------------
# ---------------------------------------------------------------------------- #
log_rec <- recipe(
  update(step_model_formula, ~ . + id_nivdaki),
  data = df_train
) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_date(
    all_date_predictors(),
    keep_original_cols = FALSE,
    features = "month"
  ) %>%
  step_zv(all_numeric_predictors()) %>%
  step_other(
    all_nominal_predictors(),
    threshold = thresh_other,
    other = "other_combined"
  ) %>%
  step_dummy(all_nominal_predictors(), naming = new_sep_names)

log_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode('classification')

log_wf <- workflow() %>%
  add_recipe(log_rec) %>%
  add_model(log_spec)

final_log_fit <- log_wf %>%
  last_fit(df_split)

collect_metrics(final_log_fit)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores - 2)
registerDoParallel(cl)

log_train_fit <- fit_resamples(
  log_wf,
  resamples = df_train_cv,
  metrics = metric_set(roc_auc, accuracy, sens, spec),
  control = ctl_grid
)

stopCluster(cl)

# ============================================================================ #
# LASSO Imputed Model --------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Prepare the Data -----------------------------------------------------------
# ---------------------------------------------------------------------------- #
lasso_rec <- recipe(outcome ~ ., data = df_train) %>%
  step_rm(all_of(c(
    "dmg_immigration_year",
    "med_vital_capacity_index",
    "med_fev",
    "med_shoulder_measure_cm_biacromial",
    "med_pelvic_measure_cm_bicristal",
    "med_left_eye_pressure",
    "med_right_eye_pressure",
    "med_vital_capacity",
    "med_timed_vital_capacity",
    "med_fvc_68",
    "work_hurt_by_superior_forget",
    "work_hurt_by_superior_restrain_retaliate",
    "family_conflict_with_wife",
    "comp_forced_pv"
  ))) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_date(
    all_date_predictors(),
    keep_original_cols = FALSE,
    features = "month"
  ) %>%
  step_other(
    all_nominal_predictors(),
    threshold = thresh_other,
    other = "other_combined"
  ) %>%
  step_dummy(all_nominal_predictors(), naming = new_sep_names) %>%
  step_zv(all_numeric_predictors()) %>%
  step_corr(
    all_numeric_predictors(),
    threshold = thresh_corr,
    method = "spearman"
  ) %>%
  step_normalize(all_numeric_predictors())

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode('classification')

# Save prepared recipe for later use in SHAP analysis.
lasso_prep <- lasso_rec %>%
  prep(strings_as_factors = FALSE, log_changes = TRUE, verbose = TRUE)

lambda_grid <- grid_regular(penalty(), levels = 1000)

# ---------------------------------------------------------------------------- #
## Setup Model and Tune Hyperparameters ---------------------------------------
# ---------------------------------------------------------------------------- #
lasso_wf <- workflow() %>%
  add_recipe(lasso_rec) %>%
  add_model(lasso_spec)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores - 2)
registerDoParallel(cl)

lasso_res <- tune_grid(
  lasso_wf,
  resamples = df_train_cv,
  grid = lambda_grid,
  control = ctl_grid
)

stopCluster(cl)

lasso_res %>%
  autoplot()

# ---------------------------------------------------------------------------- #
## Select Best LASSO Model -----------------------------------------------------
# ---------------------------------------------------------------------------- #
lasso_res %>%
  show_best(metric = "roc_auc")

lasso_best_auc <- lasso_res %>%
  select_best(metric = "roc_auc")

final_lasso <- finalize_workflow(
  lasso_wf,
  lasso_best_auc
)

final_lasso_fit <- final_lasso %>%
  last_fit(df_split)

collect_metrics(final_lasso_fit)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores - 2)
registerDoParallel(cl)

lasso_train_fit <- fit_resamples(
  final_lasso,
  resamples = df_train_cv,
  metrics = metric_set(roc_auc, accuracy, sens, spec),
  control = ctl_grid
)

stopCluster(cl)

# ============================================================================ #
# XGBoost Model --------------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## Prepare the Data -----------------------------------------------------------
# ---------------------------------------------------------------------------- #
xgb_rec <- recipe(outcome ~ ., data = df_train) %>%
  update_role(id_nivdaki, new_role = "ID") %>%
  step_date(
    all_date_predictors(),
    keep_original_cols = FALSE,
    features = "month"
  ) %>%
  step_integer(all_ordered_predictors()) %>%
  step_other(
    all_nominal_predictors(),
    threshold = thresh_other,
    other = "other_combined"
  ) %>%
  step_dummy(all_nominal_predictors(), naming = new_sep_names) %>%
  step_zv(all_numeric_predictors()) %>%
  step_corr(
    all_numeric_predictors(),
    threshold = thresh_corr,
    method = "spearman"
  )

xgb_prep <- xgb_rec %>%
  prep(strings_as_factors = FALSE, log_changes = TRUE, verbose = TRUE)

# ---------------------------------------------------------------------------- #
## Setup Model and Tune Hyperparameters -------------------------------------
# ---------------------------------------------------------------------------- #
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)

xgb_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 500
)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores - 2)
registerDoParallel(cl)

xgb_res <- tune_grid(
  xgb_wf,
  resamples = df_train_cv,
  grid = xgb_grid,
  control = ctl_grid
)

stopCluster(cl)

xgb_res %>%
  autoplot()

# ---------------------------------------------------------------------------- #
## Select Best XGBoost Model --------------------------------------------------
# ---------------------------------------------------------------------------- #
xgb_best_auc <- xgb_res %>%
  show_best(metric = "roc_auc", n = 4)

final_xgb <- finalize_workflow(
  xgb_wf,
  xgb_best_auc[1, ]
)

final_xgb_fit <- final_xgb %>%
  last_fit(df_split)

collect_metrics(final_xgb_fit)

set.seed(2020)

all_cores <- parallel::detectCores(logical = TRUE)
cl <- makeCluster(all_cores - 2)
registerDoParallel(cl)

xgb_train_fit <- fit_resamples(
  final_xgb,
  resamples = df_train_cv,
  metrics = metric_set(roc_auc, accuracy, sens, spec),
  control = ctl_grid
)

stopCluster(cl)

# ============================================================================ #
# Save Model Data --------------------------------------------------------------
# ============================================================================ #
save(
  ml_df,
  df_split,
  df_train,
  df_test,
  imp_train_df,
  step_model,
  log_train_fit,
  final_log_fit,
  lasso_res,
  lasso_prep,
  lasso_best_auc,
  final_lasso_fit,
  lasso_train_fit,
  final_xgb,
  final_xgb_fit,
  xgb_res,
  xgb_train_fit,
  xgb_rec,
  xgb_prep,
  file = "raw_data/model_data.RData"
)
