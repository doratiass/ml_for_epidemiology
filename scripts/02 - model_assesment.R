# ============================================================================ #
# Script: 02 - model_assesment.R
# Description: This script is used for discrimination and calibration assessment.
# It computes bootstrap metrics for ROC and PR, builds ROC and PR curves for each model,
# and generates calibration plots using SCAM for both train and test data.
# ============================================================================ #

# ============================================================================ #
# Load Required Packages
# ============================================================================ #
library(tidyverse)
library(tidymodels)
library(gtsummary)
library(ggpubr)
library(modelr)
library(scam)
source("scripts/00 - funcs.R")
tidymodels_prefer()
load("data/model_fits.RData")
set.seed(45)
cat("\f")

# ============================================================================ #
# Summarise Metrics ------------------------------------------------------------
# ============================================================================ #
model_list <- list(final_lasso_fit, final_xgb_fit)
train_model_list <- list(lasso_train_fit, xgb_train_fit)
model_names <- c("LASSO", "XGBoost")
sum_models_list <- list()

for (i in 1:length(model_list)) {
  bs_df <- bootstraps(model_list[[i]] %>% collect_predictions(), times = 1000)

  par_sum <- bs_df %>%
    mutate(
      pr = map_dbl(splits, bootstrap_pr),
      roc = map_dbl(splits, bootstrap_roc)
    ) %>%
    pivot_longer(cols = !c(splits, id), values_to = "val", names_to = "par") %>%
    group_by(par) %>%
    mutate(model = model_names[i])

  sum_models_list <- append(sum_models_list, list(par_sum))
}

sum_models <- bind_rows(sum_models_list)

sum_models %>%
  filter(par == "roc") %>%
  group_by(model) %>%
  summarise(
    mean = mean(val),
    ul = quantile(val, 0.975),
    ll = quantile(val, 0.025)
  ) -> roc_bootstrap

sum_models %>%
  filter(par == "pr") %>%
  group_by(model) %>%
  summarise(
    mean = mean(val),
    ul = quantile(val, 0.975),
    ll = quantile(val, 0.025)
  ) -> pr_bootstrap

# ============================================================================ #
# Build ROC Curves -------------------------------------------------------------
# ============================================================================ #
lasso_roc <- final_lasso_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian, event_level = "second") %>%
  mutate(
    model = paste0(
      roc_bootstrap[1, 1],
      " - ",
      sprintf(roc_bootstrap[1, 2], fmt = '%#.3f'),
      " (",
      sprintf(roc_bootstrap[1, 4, drop = TRUE], fmt = '%#.3f'),
      "-",
      sprintf(roc_bootstrap[1, 3, drop = TRUE], fmt = '%#.3f'),
      ")"
    ),
    inx = 1
  )

xgb_roc <- final_xgb_fit %>%
  collect_predictions() %>%
  roc_curve(outcome, .pred_centenarian, event_level = "second") %>%
  mutate(
    model = paste0(
      roc_bootstrap[2, 1],
      " - ",
      sprintf(roc_bootstrap[2, 2], fmt = '%#.3f'),
      " (",
      sprintf(roc_bootstrap[2, 4, drop = TRUE], fmt = '%#.3f'),
      "-",
      sprintf(roc_bootstrap[2, 3, drop = TRUE], fmt = '%#.3f'),
      ")"
    ),
    inx = 2
  )

# ============================================================================ #
# Build PR Curves --------------------------------------------------------------
# ============================================================================ #
lasso_pr <- final_lasso_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian, event_level = "second") %>%
  mutate(
    model = paste0(
      pr_bootstrap[1, 1],
      " - ",
      sprintf(pr_bootstrap[1, 2], fmt = '%#.3f'),
      " (",
      sprintf(pr_bootstrap[1, 4, drop = TRUE], fmt = '%#.3f'),
      "-",
      sprintf(pr_bootstrap[1, 3, drop = TRUE], fmt = '%#.3f'),
      ")"
    ),
    inx = 1
  )

xgb_pr <- final_xgb_fit %>%
  collect_predictions() %>%
  pr_curve(outcome, .pred_centenarian, event_level = "second") %>%
  mutate(
    model = paste0(
      pr_bootstrap[2, 1],
      " - ",
      sprintf(pr_bootstrap[2, 2], fmt = '%#.3f'),
      " (",
      sprintf(pr_bootstrap[2, 4, drop = TRUE], fmt = '%#.3f'),
      "-",
      sprintf(pr_bootstrap[2, 3, drop = TRUE], fmt = '%#.3f'),
      ")"
    ),
    inx = 2
  )

# ============================================================================ #
# Visualize Models -------------------------------------------------------------
# ============================================================================ #
# ---------------------------------------------------------------------------- #
## ROC Plot -------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
roc_plot <- rbind(lasso_roc, xgb_roc) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_abline(
    lty = 2,
    alpha = 0.5,
    color = "gray50",
    linewidth = line_size
  ) +
  geom_path(linewidth = line_size) +
  geom_point(aes(x = 0.5, y = 0.2 - 0.05 * inx), shape = 15, size = 3) +
  geom_text(
    aes(x = 0.53, y = 0.2 - 0.05 * inx, label = model, size = 15),
    color = "black",
    hjust = 0,
    check_overlap = TRUE
  ) +
  coord_equal() +
  my_theme +
  theme(legend.position = "none", legend.title = element_blank())

# ---------------------------------------------------------------------------- #
## PR Plot --------------------------------------------------------------------
# ---------------------------------------------------------------------------- #
pr_plot <- rbind(lasso_pr, xgb_pr) %>%
  filter(!is.infinite(.threshold)) %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path() +
  geom_point(aes(x = 0.5, y = 1 - 0.05 * inx), shape = 15, size = 3) +
  geom_text(
    aes(x = 0.53, y = 1 - 0.05 * inx, label = model, size = 15),
    color = "black",
    hjust = 0,
    check_overlap = TRUE
  ) +
  coord_equal() +
  my_theme +
  theme(legend.position = "none", legend.title = element_blank())

# ---------------------------------------------------------------------------- #
## Calibration Plots ----------------------------------------------------------
# ---------------------------------------------------------------------------- #
cal_train <- cal_scam_plot(
  list(final_lasso_fit, final_xgb_fit),
  list(lasso_train_fit, xgb_train_fit),
  split = "train",
  plat = TRUE
) +
  my_theme

cal_test <- cal_scam_plot(
  list(final_lasso_fit, final_xgb_fit),
  list(lasso_train_fit, xgb_train_fit),
  split = "test",
  plat = TRUE
) +
  my_theme

# ============================================================================ #
# Final Plot -------------------------------------------------------------------
# ============================================================================ #
sums_plot <- ggarrange(
  roc_plot,
  pr_plot,
  cal_train,
  cal_test,
  labels = "AUTO", # Automatic labels for subplots
  ncol = 2,
  nrow = 2
)

sums_plot

# ============================================================================ #
# Save Outputs -----------------------------------------------------------------
# ============================================================================ #
save(
  roc_bootstrap,
  pr_bootstrap,
  file = "data/roc_data.RData"
)
