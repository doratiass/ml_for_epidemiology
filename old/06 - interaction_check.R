# ============================================================================ #
# Script: 06 - interaction_check.R
# Description: This script explores and plots interactions between key predictors.
# It creates a subset of data with recoded interaction variables, fits GAMs (with and without
# interactions) to the outcome, extracts p-values for terms, and generates partial dependency
# plots for the interactions.
# ============================================================================ #

# ============================================================================ #
# Load Required Packages
# ============================================================================ #
library(tidyverse)
library(shapviz)
library(gridExtra)
library(mgcv)
library(tidygam)
library(viridis)
source("~/Documents/projects/longevity/scripts/00 - funcs_def.R")
set.seed(45)
cat("\f")

# ============================================================================ #
# Data Preparation -------------------------------------------------------------
# ============================================================================ #
# Uncomment the following lines to explore potential interactions:
# potential_interactions(shap_xgb, v = "med_sbp_mean")[1:50]
# potential_interactions(shap_xgb, v = "med_bmi_mean")[1:50]

int_df <- ml_df %>%
  drop_na(med_sbp_mean, med_bmi_mean) %>%
  transmute(
    med_sbp_mean,
    med_bmi_mean = factor(
      case_when(
        between(med_bmi_mean, 0, 25) ~ "<25",
        between(med_bmi_mean, 25, 27.5) ~ "25-27.5",
        between(med_bmi_mean, 27.5, 30) ~ "27.5-30",
        between(med_bmi_mean, 30, 100) ~ ">30"
      ),
      levels = c("<25", "25-27.5", "27.5-30", ">30")
    ),
    outcome = factor(outcome, levels = c("not_centenarian", "centenarian"))
  )

shap_xgb_dic <- shap_xgb
shap_xgb_dic$X <- shap_xgb_dic$X %>%
  mutate(
    med_bmi_mean = factor(
      case_when(
        between(med_bmi_mean, 0, 25) ~ "<25",
        between(med_bmi_mean, 25, 27.5) ~ "25-27.5",
        between(med_bmi_mean, 27.5, 30) ~ "27.5-30",
        between(med_bmi_mean, 30, 100) ~ ">30"
      ),
      levels = c("<25", "25-27.5", "27.5-30", ">30")
    )
  )

# ============================================================================ #
# Model Fitting for Interaction Exploration ------------------------------------
# ============================================================================ #
# Fit GAM without interaction
gam(
  outcome ~ s(med_sbp_mean) + med_bmi_mean,
  data = int_df,
  family = "binomial"
) -> gam_no_int
summary(gam_no_int)

plot_txt_no_int <- summary(gam_no_int) %$%
  bind_rows(
    tibble(term = rownames(p.table), p.value = p.table[, 4]),
    tibble(term = rownames(s.table), p.value = s.table[, 4])
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    inx = 1:n(),
    txt = paste0(int_labeler(term), ", p.value ", round(p.value, 3))
  )

# Fit GAM with interaction
gam(
  outcome ~ s(med_sbp_mean) + med_bmi_mean + s(med_sbp_mean, by = med_bmi_mean),
  data = int_df,
  family = "binomial"
) -> gam_int
summary(gam_int)

plot_txt_int <- summary(gam_int) %$%
  bind_rows(
    tibble(term = rownames(p.table), p.value = p.table[, 4]),
    tibble(term = rownames(s.table), p.value = s.table[, 4])
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    inx = 1:n(),
    txt = paste0(
      ifelse(
        str_detect(term, ":"),
        paste0(
          int_labeler(str_split(term, ":", simplify = TRUE)[, 1]),
          ":",
          int_labeler(str_split(term, ":", simplify = TRUE)[, 2])
        ),
        int_labeler(term)
      ),
      ", p.value ",
      round(p.value, 3)
    )
  )

# ============================================================================ #
# Plotting Interactions -------------------------------------------------------
# ============================================================================ #
## Pan A: Partial dependency for med_bmi_mean without interactions
int_a <- sv_dependence(
  shap_xgb,
  v = "med_bmi_mean",
  color_var = "med_bmi_mean",
  alpha = 0.5,
  interactions = FALSE
)
int_a$data <- int_a$data %>%
  mutate(
    bmi_group = factor(
      case_when(
        between(med_bmi_mean, 0, 25) ~ "<25",
        between(med_bmi_mean, 25, 27.5) ~ "25-27.5",
        between(med_bmi_mean, 27.5, 30) ~ "27.5-30",
        between(med_bmi_mean, 30, 100) ~ ">30"
      ),
      levels = c("<25", "25-27.5", "27.5-30", ">30")
    )
  ) %>%
  drop_na(bmi_group)

int_a <- int_a +
  geom_point(aes(color = bmi_group), alpha = 0.5) +
  theme_classic() +
  plot_theme +
  scale_color_viridis_d(begin = 0.25, end = 0.85, option = "inferno") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = leg_size_4),
    legend.text = element_text(size = leg_size_4 - 2)
  )
int_a$labels$x <- vars_label(int_a$labels$x)
int_a$labels$colour <- vars_label(int_a$labels$colour)

## Pan B: Partial dependency for med_sbp_mean with interactions (using shap_xgb_dic)
int_b <- sv_dependence(
  shap_xgb_dic,
  v = "med_sbp_mean",
  color_var = "med_bmi_mean",
  alpha = 0.5,
  interactions = TRUE
) +
  geom_smooth(se = FALSE) +
  theme_classic() +
  plot_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = leg_size_4),
    legend.text = element_text(size = leg_size_4 - 2)
  )
int_b$data <- int_b$data %>%
  drop_na(med_bmi_mean)
int_b$labels$x <- vars_label(int_b$labels$x)
int_b$labels$colour <- vars_label(int_b$labels$colour)

## Pan C: Plot GAM without interaction
x_start <- 90
y_start <- -4
int_c <- int_df %>%
  mutate(prob = predict(gam_no_int, data = ., type = "link")) %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = med_bmi_mean)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  geom_point(
    data = plot_txt_no_int,
    aes(x = x_start, y = y_start - 0.25 * inx, color = NULL),
    shape = 15,
    size = 3
  ) +
  geom_text(
    data = plot_txt_no_int,
    aes(x = x_start + 2, y = y_start - 0.25 * inx, label = txt),
    size = 5,
    color = "black",
    hjust = 0
  ) +
  scale_y_continuous(limits = c(-6, -1.8)) +
  scale_color_viridis_d(begin = 0.25, end = 0.85, option = "inferno") +
  theme_classic() +
  labs(y = "Log odds", color = vars_label("med_bmi_mean")) +
  plot_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = leg_size_4),
    legend.text = element_text(size = leg_size_4 - 2)
  )
int_c$labels$x <- vars_label(int_c$labels$x)
int_c$labels$colour <- vars_label(int_c$labels$colour)

## Pan D: Plot GAM with interaction
int_d <- int_df %>%
  mutate(prob = predict(gam_int, data = ., type = "link")) %>%
  ggplot(aes(x = med_sbp_mean, y = prob, color = med_bmi_mean)) +
  geom_point(size = 0.1, alpha = 0.3) +
  geom_smooth() +
  geom_point(
    data = plot_txt_int,
    aes(x = x_start, y = y_start - 0.25 * inx, color = NULL),
    shape = 15,
    size = 3
  ) +
  geom_text(
    data = plot_txt_int,
    aes(x = x_start + 2, y = y_start - 0.25 * inx, label = txt),
    size = 5,
    color = "black",
    hjust = 0
  ) +
  scale_y_continuous(limits = c(-6, -1.8)) +
  scale_color_viridis_d(begin = 0.25, end = 0.85, option = "inferno") +
  theme_classic() +
  labs(y = "", color = vars_label("med_bmi_mean")) +
  plot_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = leg_size_4),
    legend.text = element_text(size = leg_size_4 - 2)
  )
int_d$labels$x <- vars_label(int_d$labels$x)
int_d$labels$colour <- vars_label(int_d$labels$colour)

# ============================================================================ #
# Combine Interaction Plots ----------------------------------------------------
# ============================================================================ #
ggarrange(
  ggarrange(
    int_a,
    int_b,
    labels = c("A", "B"),
    common.legend = TRUE,
    legend = "none",
    font.label = list(size = 20, color = "black", face = "bold"),
    ncol = 2,
    nrow = 1
  ),
  ggarrange(
    int_c,
    int_d,
    labels = c("C", "D"),
    common.legend = TRUE,
    legend = "bottom",
    font.label = list(size = 20, color = "black", face = "bold"),
    ncol = 2,
    nrow = 1
  ),
  ncol = 1,
  nrow = 2
)

ggsave(
  filename = file.path("graphs", "fig4.pdf"),
  plot = ggplot2::last_plot(),
  width = 40,
  height = 30,
  dpi = 300,
  units = "cm",
  bg = "white"
)
