# ============================================================================ #
# Script: 00 - funcs_def.R
# Description: This script defines helper functions and settings used in the
# Longevity Predictors Study. The functions cover variable labeling, formatting,
# performance metric computation, and calibration plot generation (using both
# standard methods and SCAM-based recalibration). Each function’s arguments and
# their purpose are explained inline.
# ============================================================================ #

# ============================================================================ #
# Load Functions and Libraries
# ============================================================================ #
library(tidyverse) # Provides functions for data manipulation and visualization.
library(tidymodels) # A suite of packages for modeling and machine learning.
library(gtsummary) # Helps in summarizing data in a publication-friendly format.
library(ggpubr) # Enhances ggplot2 plots for publication quality.
library(modelr) # Assists with model-related tasks that integrate with tidyverse.
library(readr) # Enables fast reading of CSV and other rectangular data.

# ============================================================================ #
# Theme definitions ------------------------------------------------------------
# ============================================================================ #
color_pal <- 'Set1' # Color palette to be used in plots.

# Define a consistent ggplot theme for visualizations
my_theme_shap <- list(
  theme_minimal(base_size = 14),
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#2c3e50"
    ),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#4f5d75"),
    axis.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
    axis.text = element_text(color = "#666666"),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    # Transparent background for slides
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA, colour = NA),
    strip.background = element_blank()
  )
)

my_theme <- list(
  scale_fill_brewer(palette = color_pal),
  scale_color_brewer(palette = color_pal),
  theme_minimal(base_size = 14),
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#2c3e50"
    ),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#4f5d75"),
    axis.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
    axis.text = element_text(color = "#666666"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    # Transparent background for slides
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA, colour = NA),
    strip.background = element_blank()
  )
)

line_size <- 1.2 # Standard thickness for lines in plots.
leg_size_4 <- 16 # Likely used for setting legend text size in certain plots.
var_num <- 10 # number of variables for SHAP importance plot
# ============================================================================ #
# Functions ------------------------------------------------------------
# ============================================================================ #
# new_sep_names: Custom function for dummy variable naming
new_sep_names <- function(var, lvl, ordinal) {
  dummy_names(var = var, lvl = lvl, ordinal = ordinal, sep = "_@_")
}

# Load a dictionary mapping variable codes to descriptive names.
# The CSV file should contain columns: 'var' (code) and 'name' (description).
vars_dict <- read_csv("data/vars_dict.csv", show_col_types = FALSE)

# -----------------------------------------------------------------------------#
# Function: label_get
#
# Purpose:
#   - Returns a human-readable label for a given variable code.
#
# Arguments:
#   - x: A character string representing a variable code.
#
# Usage:
#   - If the input 'x' is found in the 'var' column of vars_dict, its corresponding
#     'name' is returned. Otherwise, 'x' is returned unchanged.
# -----------------------------------------------------------------------------#
label_get <- function(x) {
  ifelse(
    x %in% vars_dict$var,
    vars_dict[vars_dict$var == x, "name", drop = TRUE],
    x
  )
}

# -----------------------------------------------------------------------------#
# Function: level_get
#
# Purpose:
#   - Formats a variable's level string to a more interpretable format.
#
# Arguments:
#   - x: A character string that may encode a level or factor, possibly starting
#        with an "X" and containing periods or underscores.
#
# Details:
#   - For strings like "X12.34": removes "X" and replaces the dot with a hyphen (-> "12-34").
#   - For strings like "X12.": removes "X" and replaces the dot with a plus sign.
#   - For strings containing underscores, replaces "_" with a space.
#
# Usage:
#   - Used to transform raw level codes into reader-friendly labels.
# -----------------------------------------------------------------------------#
level_get <- function(x) {
  if (str_detect(x, "X[0-9][0-9]\\.[0-9][0-9]")) {
    str_replace_all(str_remove(x, "X"), "\\.", "-")
  } else if (str_detect(x, "X[0-9][0-9]\\.")) {
    str_replace_all(str_remove(x, "X"), "\\.", "+")
  } else if (str_detect(x, "_")) {
    str_replace_all(x, "_", " ")
  } else {
    x
  }
}

# -----------------------------------------------------------------------------#
# Function: label_all
#
# Purpose:
#   - Constructs a complete, descriptive label for a variable, including both the
#     base variable and any level information.
#
# Arguments:
#   - x: A character string that can contain special delimiters ("_@_" or "_poly_")
#        indicating additional formatting is needed.
#
# Details:
#   - If 'x' contains "_@_", it splits the string into two parts:
#       * The first part is processed using label_get.
#       * The second part is processed using level_get unless it equals "other_combined".
#   - If 'x' contains "_poly_", it denotes a polynomial term by combining the
#     base label with an indicator ("poly") for the second term.
#   - Otherwise, it simply calls label_get on x.
#
# Usage:
#   - Used to generate a human-readable label for variables that have been encoded
#     with extra information.
# -----------------------------------------------------------------------------#
label_all <- function(x) {
  if (str_detect(x, "_@_")) {
    if (str_split(x, "_@_", simplify = TRUE)[, 2] == "other_combined") {
      label_get(str_split(x, "_@_", simplify = TRUE)[, 1])
    } else {
      paste0(
        label_get(str_split(x, "_@_", simplify = TRUE)[, 1]),
        " - ",
        level_get(str_split(x, "_@_", simplify = TRUE)[, 2])
      )
    }
  } else if (str_detect(x, "_poly_")) {
    paste0(
      label_get(str_split(x, "_poly_", simplify = TRUE)[, 1]),
      " - poly ",
      label_get(str_split(x, "_poly_", simplify = TRUE)[, 2])
    )
  } else {
    label_get(x)
  }
}

# -----------------------------------------------------------------------------#
# Function: vars_label
#
# Purpose:
#   - Applies the label_all function to a vector of variable names.
#
# Arguments:
#   - x: A vector of character strings representing variable codes.
#
# Usage:
#   - Returns a vector of descriptive labels by mapping each element through label_all.
# -----------------------------------------------------------------------------#
vars_label <- function(x) {
  sapply(x, label_all, USE.NAMES = FALSE)
}

# -----------------------------------------------------------------------------#
# Function: var_get
#
# Purpose:
#   - Retrieves the original variable code given its descriptive name.
#
# Arguments:
#   - x: A character string representing the descriptive name of a variable.
#
# Usage:
#   - If the input 'x' matches an entry in the 'name' column of vars_dict, the
#     corresponding 'var' code is returned. Otherwise, x is returned unchanged.
# -----------------------------------------------------------------------------#
var_get <- function(x) {
  ifelse(
    x %in% vars_dict$name,
    vars_dict[vars_dict$name == x, "var", drop = TRUE],
    x
  )
}

# -----------------------------------------------------------------------------#
# Function: int_labeler
#
# Purpose:
#   - Provides custom labeling for interaction or transformed predictors.
#
# Arguments:
#   - x: A character string representing an interaction or transformed variable name.
#
# Details:
#   - Strips specific characters such as "s(", ")" to clean the string.
#   - Uses conditional logic (case_when) to assign specific labels:
#       * If x equals "med_bmi_mean", returns "BMI".
#       * If x contains "med_bmi_mean", returns a composite label with the remaining
#         part of the string in parentheses.
#       * If x equals "med_sbp_mean", returns "SBP".
#
# Usage:
#   - Ensures that transformed variables, especially interactions, have meaningful labels.
# -----------------------------------------------------------------------------#
int_labeler <- function(x) {
  x <- str_remove_all(x, "s\\(|\\)|\\(")
  case_when(
    x == "med_bmi_mean" ~ "BMI",
    str_detect(x, "med_bmi_mean") ~
      paste0("BMI (", str_remove_all(x, "med_bmi_mean"), ")"),
    x == "med_sbp_mean" ~ "SBP"
  )
}

# -----------------------------------------------------------------------------#
# Function: bootstrap_pr
#
# Purpose:
#   - Computes the Precision-Recall AUC (area under the curve) for a given set of
#     bootstrap resamples.
#
# Arguments:
#   - splits: A resampling object (typically from tidymodels) containing training data.
#
# Details:
#   - The function extracts the analysis (training) set from the splits and then
#     calculates the PR AUC using the outcome and the predicted probability (.pred_centenarian).
#
# Usage:
#   - Used for evaluating model performance on bootstrap samples.
# -----------------------------------------------------------------------------#
bootstrap_pr <- function(splits) {
  x <- analysis(splits) # Extract the analysis (training) set.
  pr_auc(x, outcome, .pred_centenarian, event_level = "second")$.estimate # Calculate and return the PR AUC.
}

# -----------------------------------------------------------------------------#
# Function: bootstrap_roc
#
# Purpose:
#   - Computes the ROC AUC (area under the receiver operating characteristic curve)
#     for a given set of bootstrap resamples.
#
# Arguments:
#   - splits: A resampling object containing the training data.
#
# Details:
#   - Similar to bootstrap_pr, it extracts the analysis set and computes the ROC AUC.
#
# Usage:
#   - Used for evaluating model performance with respect to ROC.
# -----------------------------------------------------------------------------#
bootstrap_roc <- function(splits) {
  x <- analysis(splits) # Extract the analysis (training) set.
  roc_auc(x, outcome, .pred_centenarian, event_level = "second")$.estimate # Calculate and return the ROC AUC.
}

# -----------------------------------------------------------------------------#
# Function: cal_plot_three
#
# Purpose:
#   - Generates a calibration plot for three models (Logistic reg, LASSO, XGBoost)
#     either on the training or test data. Optionally applies Platt scaling (via
#     isotonic regression) for recalibration.
#
# Arguments:
#   - final_fit: A list of fitted models used to generate predictions on test data.
#   - train_fit: A list of fitted models used to generate predictions on training data.
#   - split: A character string, either "train" or "test", indicating which dataset's
#            predictions to plot.
#   - plat: A boolean flag indicating whether to apply Platt scaling (isotonic recalibration).
#
# Details:
#   - Depending on the 'split' and 'plat' parameters, the function collects predictions,
#     optionally applies calibration adjustments, and labels each model accordingly.
#   - Nested helper functions (coef_int and coef_slope) compute the intercept and slope
#     from a simple linear regression of observed outcomes on predictions, which are used
#     for annotating the plot.
#
# Returns:
#   - A ggplot object that visualizes the calibration curves for the three models.
# -----------------------------------------------------------------------------#
cal_plot_three <- function(
  final_fit,
  train_fit,
  split = c("train", "test"),
  plat = FALSE
) {
  if (split == "train") {
    if (plat) {
      # For training set with Platt scaling: adjust predictions using isotonic calibration.
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[1]])) %>%
          mutate(model = "Logistic reg"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[2]])) %>%
          mutate(model = "LASSO"),
        train_fit[[3]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[3]])) %>%
          mutate(model = "XGBoost")
      ) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    } else {
      # For training set without calibration adjustment.
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "Logistic reg"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          mutate(model = "LASSO"),
        train_fit[[3]] %>%
          collect_predictions() %>%
          mutate(model = "XGBoost")
      ) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    }
  } else if (split == "test") {
    if (plat) {
      # For test set with Platt scaling: apply calibration based on the training models.
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[1]])) %>%
          mutate(model = "Logistic reg"),
        final_fit[[2]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[2]])) %>%
          mutate(model = "LASSO"),
        final_fit[[3]] %>%
          collect_predictions() %>%
          cal_apply(cal_estimate_isotonic(train_fit[[3]])) %>%
          mutate(model = "XGBoost")
      ) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    } else {
      # For test set without calibration adjustment.
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "Logistic reg"),
        final_fit[[2]] %>%
          collect_predictions() %>%
          mutate(model = "LASSO"),
        final_fit[[3]] %>%
          collect_predictions() %>%
          mutate(model = "XGBoost")
      ) %>%
        mutate(model = fct_relevel(model, "LASSO", "Logistic reg", "XGBoost"))
    }
  }

  # Nested helper function: coef_int
  # Computes the intercept of the linear model regressing observed outcome on predictions.
  coef_int <- function(y, x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    return(round(lm_cal$coefficients[1], 2))
  }

  # Nested helper function: coef_slope
  # Computes the slope of the linear model regressing observed outcome on predictions.
  coef_slope <- function(y, x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    return(round(lm_cal$coefficients[2], 2))
  }

  # Compute regression coefficients for annotations on the calibration plot.
  plot_txt <- df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>% # Convert outcome to binary (1 if centenarian)
    group_by(model) %>%
    summarise(
      int = coef_int(outcome, `.pred_centenarian`),
      slope = coef_slope(outcome, `.pred_centenarian`)
    ) %>%
    mutate(
      txt = paste0(model, ", int: ", int, ", slope ", slope),
      inx = c(2, 1, 3)
    )

  # Define a title for the plot based on whether Platt scaling is applied.
  title <- ifelse(
    plat,
    paste("Calibration plot -", split, "set - platt scaling"),
    paste("Calibration plot -", split, "set")
  )

  # Create the calibration plot using ggplot2.
  df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>% # Ensure outcome is binary
    ggplot(aes(.pred_centenarian, outcome, color = model)) +
    geom_rug(color = "grey", sides = "tb", alpha = 0.2) + # Add a rug plot for density visualization.
    geom_smooth(
      linewidth = line_size,
      method = "loess",
      se = TRUE,
      fullrange = TRUE
    ) + # Draw smooth calibration curves.
    geom_abline(intercept = 0, slope = 1, linetype = "longdash") + # Add a reference line for perfect calibration.
    geom_point(
      data = plot_txt,
      aes(x = 0.5, y = 0.2 - 0.05 * inx),
      shape = 15,
      size = 3
    ) + # Plot points for annotations.
    geom_text(
      data = plot_txt,
      aes(x = 0.53, y = 0.2 - 0.05 * inx, label = txt, size = 15),
      color = "black",
      hjust = 0
    ) + # Add text annotations with computed coefficients.
    labs(x = "Predicted risk", y = "Observed proportion") +
    theme_bw() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    ) -> plot # Store the plot object.

  return(plot) # Return the calibration plot.
}

# -----------------------------------------------------------------------------#
# Function: cal_scam_plot
#
# Purpose:
#   - Generates calibration plots for the three models using Shape Constrained
#     Additive Models (SCAM) for recalibration.
#
# Arguments:
#   - final_fit: A list of fitted models for the test data.
#   - train_fit: A list of fitted models for the training data.
#   - split: A character string ("train" or "test") indicating the dataset to use.
#   - plat: A boolean flag indicating whether to use Platt scaling adjustments.
#
# Details:
#   - The function first fits three separate SCAM models (one for each prediction model)
#     on the training data. Each SCAM model predicts the binary outcome based on
#     the predicted probability (.pred_centenarian).
#   - Depending on the 'split' and 'plat' arguments, it then collects predictions
#     (and applies SCAM-based recalibration if 'plat' is TRUE) for the test or training set.
#   - Similar to cal_plot_three, it calculates intercept and slope for annotations.
#
# Returns:
#   - A ggplot object visualizing the SCAM-based calibration curves along with
#     annotations.
# -----------------------------------------------------------------------------#
cal_scam_plot <- function(
  final_fit,
  train_fit,
  split = c("train", "test"),
  plat = FALSE
) {
  # Fit a SCAM model for each of the three training fits.
  scam_model_1 <- scam(
    outcome ~ s(.pred_centenarian, bs = "mpi", m = 1),
    data = train_fit[[1]] %>%
      collect_predictions() %>%
      mutate(outcome = if_else(outcome == "centenarian", 1, 0)),
    family = binomial
  )

  scam_model_2 <- scam(
    outcome ~ s(.pred_centenarian, bs = "mpi", m = 1),
    data = train_fit[[2]] %>%
      collect_predictions() %>%
      mutate(outcome = if_else(outcome == "centenarian", 1, 0)),
    family = binomial
  )

  if (split == "train") {
    if (plat) {
      # For training set with Platt scaling: apply SCAM predictions.
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_1, type = "response") %>%
          mutate(model = "LASSO"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_2, type = "response") %>%
          mutate(model = "XGBoost")
      ) %>%
        mutate(model = fct_relevel(model, "LASSO", "XGBoost"))
    } else {
      # For training set without recalibration.
      df <- bind_rows(
        train_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "LASSO"),
        train_fit[[2]] %>%
          collect_predictions() %>%
          mutate(model = "XGBoost")
      ) %>%
        rename(pred = .pred_centenarian) %>% # Rename for consistency.
        mutate(model = fct_relevel(model, "LASSO", "XGBoost"))
    }
  } else if (split == "test") {
    if (plat) {
      # For test set with Platt scaling: use SCAM predictions based on training fits.
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_1, type = "response") %>%
          mutate(model = "LASSO"),
        final_fit[[2]] %>%
          collect_predictions() %>%
          add_predictions(scam_model_2, type = "response") %>%
          mutate(model = "XGBoost")
      ) %>%
        mutate(model = fct_relevel(model, "LASSO", "XGBoost"))
    } else {
      # For test set without recalibration.
      df <- bind_rows(
        final_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "LASSO"),
        final_fit[[1]] %>%
          collect_predictions() %>%
          mutate(model = "XGBoost")
      ) %>%
        rename(pred = .pred_centenarian) %>% # Rename for consistency.
        mutate(model = fct_relevel(model, "LASSO", "XGBoost"))
    }
  }

  # Nested helper function: coef_int
  # Computes the intercept from a regression of observed outcome on SCAM predictions.
  coef_int <- function(y, x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    return(round(lm_cal$coefficients[1], 2))
  }

  # Nested helper function: coef_slope
  # Computes the slope from a regression of observed outcome on SCAM predictions.
  coef_slope <- function(y, x) {
    z = tibble(x = x, y = y)
    lm_cal <- lm(y ~ x, data = z)
    return(round(lm_cal$coefficients[2], 2))
  }

  # Generate text annotations using computed regression coefficients.
  plot_txt <- df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>%
    group_by(model) %>%
    summarise(
      int = coef_int(outcome, pred),
      slope = coef_slope(outcome, pred)
    ) %>%
    mutate(
      txt = paste0(
        model,
        ", int: ",
        sprintf(int, fmt = '%#.2f'),
        ", slope ",
        sprintf(slope, fmt = '%#.2f')
      ),
      inx = c(1, 2)
    )

  # Define plot title based on whether Platt scaling is applied.
  title <- ifelse(
    plat,
    paste(split, "set - platt scaling"),
    paste(split, "set")
  )

  # Create the calibration plot using ggplot2.
  df %>%
    mutate(outcome = as.numeric(outcome == "centenarian")) %>%
    ggplot(aes(pred, outcome, color = model)) +
    geom_rug(color = "grey", sides = "tb", alpha = 0.2) + # Marginal rug plot.
    geom_smooth(
      linewidth = line_size,
      method = "loess",
      se = TRUE,
      fullrange = TRUE
    ) + # Calibration curve.
    geom_abline(intercept = 0, slope = 1, linetype = "longdash") + # Reference line for perfect calibration.
    geom_point(
      data = plot_txt,
      aes(x = 0.3, y = 0.2 - 0.05 * inx),
      shape = 15,
      size = 3
    ) + # Annotation points.
    geom_text(
      data = plot_txt,
      aes(x = 0.33, y = 0.2 - 0.05 * inx, label = txt, size = 6),
      color = "black",
      hjust = 0
    ) + # Annotation text with coefficients.
    labs(title = title, x = "Predicted risk", y = "Observed proportion") +
    theme_bw() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) -> plot # Save the ggplot object.

  return(plot) # Return the calibration plot.
}
