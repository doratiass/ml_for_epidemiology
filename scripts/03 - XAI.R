# ============================================================================ #
# Script: 03 - XAI.R
# Description: This script is for SHAP analysis. It computes SHAP values for the
# logistic regression, LASSO, and XGBoost models, and creates plots to visualize
# variable importance.
# ============================================================================ #

# ============================================================================ #
# Load Required Packages
# ============================================================================ #
library(tidyverse)
library(tidymodels)
library(glmnet)
#library(fastshap)
library(shapviz)
library(pdp)
library(parallel)
library(doParallel)
library(probably)
library(gridExtra)
library(doFuture)
library(ggpubr)
library(ggtext)
#library(ggbump)
source("scripts/00 - funcs.R")
tidymodels_prefer()
load("data/model_fits.RData")
set.seed(45)
cat("\f")

# ============================================================================ #
# SHAP Objects ----------------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## LASSO ----------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
# Save prepared recipe for later use in SHAP analysis.
lasso_prep <- lasso_rec %>%
  prep(log_changes = TRUE, verbose = TRUE)

lasso_shap_df <- bake(
  lasso_prep,
  df_train
)

tmp_coeffs <- final_lasso_fit %>%
  extract_fit_engine() %>%
  coef(
    s = lasso_res %>%
      select_best(metric = "roc_auc") %>%
      pull(penalty)
  )

lasso_imp_vars <- data.frame(
  var = str_remove_all(tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], "`"),
  coef = tmp_coeffs@x
) %>%
  filter(coef != 0, var != "(Intercept)") %>%
  arrange(desc(abs(coef))) %>%
  top_n(var_num, abs(coef))

# ---------------------------------------------------------------------------- #
## XGB ------------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
xgb_prep <- xgb_rec %>%
  prep(log_changes = TRUE, verbose = TRUE)

xgb_shap_data <- bake(
  xgb_prep,
  has_role("predictor"),
  new_data = df_train,
  composition = "matrix"
)

shap_xgb <- shapviz(
  extract_fit_engine(final_xgb_fit),
  X_pred = xgb_shap_data,
  interactions = TRUE
)

# ============================================================================ #
# Figure - Variable Importance Plots ----------------------------------------
# ============================================================================ #
shap_imp_lasso <- lasso_imp_vars %>%
  ggplot(aes(x = reorder(var, abs(coef)), y = coef, fill = var)) +
  geom_col() +
  scale_x_discrete(labels = vars_label) +
  labs(x = "", y = "Coefficient") +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  my_theme_shap +
  theme(legend.position = "none")

shap_imp_xgb <- sv_importance(
  shap_xgb,
  kind = "beeswarm",
  show_numbers = FALSE,
  max_display = var_num
) +
  my_theme_shap +
  scale_y_discrete(labels = vars_label)


# ============================================================================ #
# Save Files -----------------------------------------------------------------
# ============================================================================ #
save(lasso_imp_vars, shap_xgb, file = "data/shap.RData", compress = "xz")
