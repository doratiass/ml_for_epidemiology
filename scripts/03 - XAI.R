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
library(fastshap)
library(shapviz)
library(pdp)
library(parallel)
library(doParallel)
library(probably)
library(gridExtra)
library(doFuture)
library(ggpubr)
library(ggtext)
library(ggbump)
source("scripts/00 - funcs.R")
tidymodels_prefer()
load("data/model_fits.RData")
set.seed(45)
cat("\f")
var_num <- 10 # number of variables to importance plot

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
lasso_features = c(
  "#fca50a",
  "#fcffa4",
  "#fca50a",
  "#cf4446",
  "#a52c60",
  "#1b0c41",
  "#ed6925",
  "#4a0c6b",
  "#f7d13d",
  "#781c6d"
)
shap_imp_lasso <- lasso_imp_vars %>%
  ggplot(aes(x = reorder(var, abs(coef)), y = coef, fill = var)) +
  geom_col() +
  scale_x_discrete(labels = vars_label) +
  labs(x = "", y = "Coefficient") +
  scale_fill_manual(values = lasso_features) +
  coord_flip()

xgb_features = c(
  "#4a0c6b",
  "#781c6d",
  "#a52c60",
  "#cf4446",
  "#ed6925",
  "#f7d13d",
  "#fca50a",
  "#fca50a",
  "#fca50a",
  "#fcffa4"
)
shap_imp_bar_xgb <- sv_importance(
  shap_xgb,
  kind = "bar",
  show_numbers = TRUE,
  fill = xgb_features,
  max_display = var_num
) +
  scale_y_discrete(labels = vars_label)

shap_imp_bee_xgb <- sv_importance(
  shap_xgb,
  kind = "beeswarm",
  show_numbers = FALSE,
  max_display = var_num
) +
  scale_y_discrete(labels = vars_label) +
  theme_classic() +
  plot_theme

lasso_shap_vars <- as.character(unique(sapply(
  shap_imp_lasso$data$var,
  label_all
)))
xgb_shap_vars <- as.character(unique(sapply(
  shap_imp_bar_xgb$data$feature,
  label_all
)))

all <- intersect(lasso_shap_vars, xgb_shap_vars)
xgb_lasso <- intersect(xgb_shap_vars, lasso_shap_vars)[
  !(intersect(xgb_shap_vars, lasso_shap_vars) %in% all)
]

linewidth_lasso <- rep(0, length(shap_imp_lasso$data$var))
linewidth_lasso[vars_label(shap_imp_lasso$data$var) %in% all] <- 0.5
linetype_lasso <- rep(0, length(shap_imp_lasso$data$var))
linetype_lasso[vars_label(shap_imp_lasso$data$var) %in% all] <- 1

shap_imp_lasso_high <- shap_imp_lasso +
  scale_x_discrete(
    labels = ~ if_else(
      vars_label(.x) %in% c(all, log_lasso, xgb_lasso),
      paste0("<span style='color: red4'><b>", vars_label(.x), "</b></span>"),
      vars_label(.x)
    )
  ) +
  theme_classic() +
  plot_theme +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(
      box.colour = "red4",
      linewidth = rev(linewidth_lasso),
      linetype = rev(linetype_lasso),
      r = unit(5, "pt"),
      padding = unit(3, "pt")
    )
  )

linewidth_xgb <- rep(0, length(shap_imp_bar_xgb$data$feature))
linewidth_xgb[sapply(shap_imp_bar_xgb$data$feature, label_all) %in% all] <- 0.5
linetype_xgb <- rep(0, length(shap_imp_bar_xgb$data$feature))
linetype_xgb[sapply(shap_imp_bar_xgb$data$feature, label_all) %in% all] <- 1

shap_imp_xgb_high <- shap_imp_bar_xgb +
  scale_y_discrete(
    labels = ~ if_else(
      vars_label(.x) %in% c(all, log_xgb, xgb_lasso),
      paste0("<span style='color: red4'><b>", vars_label(.x), "</b></span>"),
      vars_label(.x)
    )
  ) +
  theme_classic() +
  plot_theme +
  theme(
    axis.text.y = element_markdown(
      box.colour = "red4",
      linewidth = rev(linewidth_xgb),
      linetype = rev(linetype_xgb),
      r = unit(5, "pt"),
      padding = unit(3, "pt")
    )
  )

ggarrange(
  shap_imp_log_high,
  shap_imp_lasso_high,
  shap_imp_xgb_high,
  shap_imp_bee_xgb + rremove("y.text") + rremove("y.ticks") + rremove("y.axis"),
  labels = c(
    "(A) Logistic Regression",
    "(B) LASSO",
    "(C) XGBoost",
    "(D) Bee Swarm"
  ),
  label.y = 1.01,
  font.label = list(size = 20, color = "black", face = "bold"),
  ncol = 2,
  nrow = 2
)

ggsave(
  filename = file.path("graphs", "fig2.pdf"),
  plot = ggplot2::last_plot(),
  width = 50,
  height = 40,
  dpi = 400,
  units = "cm",
  bg = "white"
)

# ============================================================================ #
# Save Files -----------------------------------------------------------------
# ============================================================================ #
save(log_imp_vars, lasso_imp_vars, shap_xgb, file = "raw_data/plots_shap.RData")

save(shap_xgb, file = "raw_data/shap.RData")
