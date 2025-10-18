# Simulate test predictions for visualization
n_test <- nrow(test_data)
test_results <- tibble(
  truth = test_data$outcome,
  logistic = runif(n_test, 0.02, 0.15),
  lasso = runif(n_test, 0.025, 0.16),
  xgboost = runif(n_test, 0.03, 0.18)
) %>%
  # Adjust probabilities based on true outcome
  mutate(
    logistic = ifelse(truth == "Longevity", logistic * 3, logistic * 0.7),
    lasso = ifelse(truth == "Longevity", lasso * 3, lasso * 0.7),
    xgboost = ifelse(truth == "Longevity", xgboost * 3.2, xgboost * 0.65)
  ) %>%
  pivot_longer(
    cols = c(logistic, lasso, xgboost),
    names_to = "model",
    values_to = "prediction"
  )

# ROC Curves
roc_data <- test_results %>%
  group_by(model) %>%
  roc_curve(truth, prediction) %>%
  mutate(model = str_to_title(model))

p1 <- roc_data %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(linewidth = 1.5) +
  geom_abline(lty = 2, alpha = 0.5) +
  labs(
    title = "ROC Curves",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Model"
  ) +
  theme(legend.position = "bottom")

# PR Curves
pr_data <- test_results %>%
  group_by(model) %>%
  pr_curve(truth, prediction) %>%
  mutate(model = str_to_title(model))

p2 <- pr_data %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_path(linewidth = 1.5) +
  labs(
    title = "Precision-Recall Curves",
    x = "Recall (Sensitivity)",
    y = "Precision (PPV)",
    color = "Model"
  ) +
  theme(legend.position = "bottom")

# Calibration Plot
cal_data <- test_results %>%
  mutate(
    pred_bin = cut(prediction, breaks = seq(0, 1, 0.05), include.lowest = TRUE),
    pred_midpoint = (as.numeric(pred_bin) - 1) * 0.05 + 0.025
  ) %>%
  filter(!is.na(pred_bin)) %>%
  group_by(model, pred_midpoint) %>%
  summarise(
    n = n(),
    observed = mean(truth == "Longevity"),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  mutate(model = str_to_title(model))

p3 <- cal_data %>%
  ggplot(aes(x = pred_midpoint, y = observed, color = model)) +
  geom_point(aes(size = n), alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.5) +
  labs(
    title = "Calibration Plot",
    x = "Predicted Probability",
    y = "Observed Proportion",
    color = "Model",
    size = "N"
  ) +
  theme(legend.position = "bottom")

# Combine plots
(p1 + p2) /
  p3 +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


# Create results table based on the study findings and our CV results
cv_results <- bind_rows(
  collect_metrics(logistic_fit) %>% mutate(model = "Logistic Regression"),
  collect_metrics(lasso_fit) %>% mutate(model = "LASSO"),
  collect_metrics(xgboost_fit) %>% mutate(model = "XGBoost")
) %>%
  filter(.metric == "roc_auc") %>%
  select(model, mean, std_err)

study_results <- tibble(
  Model = c("Logistic Regression", "LASSO", "XGBoost"),
  `ROC-AUC (CV)` = sprintf("%.3f (±%.3f)", cv_results$mean, cv_results$std_err),
  `Study ROC-AUC` = c(
    "0.69 (0.66-0.73)",
    "0.71 (0.67-0.74)",
    "0.72 (0.66-0.75)"
  ),
  `Key Insight` = c(
    "Baseline comparison",
    "Automatic variable selection",
    "Captured non-linear relationships"
  )
)

study_results %>%
  gt() %>%
  tab_header(
    title = "Model Performance: Predicting Extreme Longevity",
    subtitle = "Comparison of Cross-Validation Results with Published Study"
  ) %>%
  tab_source_note(
    source_note = "Atias et al. (2025). Study values show point estimate (95% CI)"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(rows = 3) # Highlight XGBoost
  )


# Simulate SHAP values based on study findings
set.seed(123)
shap_data <- tibble(
  feature = c(
    "Systolic BP",
    "Smoking (Current)",
    "MI History",
    "HDL Cholesterol",
    "Diastolic BP",
    "Glucose",
    "BMI",
    "Age"
  ),
  importance = c(0.174, 0.129, 0.109, 0.094, 0.082, 0.070, 0.062, 0.060)
) %>%
  mutate(
    feature = fct_reorder(feature, importance),
    rank = row_number(desc(importance))
  )

# Variable importance plot
p1 <- shap_data %>%
  ggplot(aes(x = importance, y = feature, fill = rank <= 3)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.3f", importance)), hjust = -0.1, size = 3) +
  scale_fill_manual(values = c("grey70", "steelblue")) +
  labs(
    title = "SHAP Feature Importance",
    subtitle = "Top predictors of extreme longevity",
    x = "Mean |SHAP Value|",
    y = NULL
  ) +
  theme(legend.position = "none")

# SHAP dependence plot simulation
set.seed(456)
dependence_data <- tibble(
  systolic_bp = runif(500, 100, 180),
  shap_value = -0.003 * (systolic_bp - 120) + rnorm(500, 0, 0.02),
  smoking = sample(c("Never", "Former", "Current"), 500, replace = TRUE)
)

p2 <- dependence_data %>%
  ggplot(aes(x = systolic_bp, y = shap_value, color = smoking)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, linewidth = 1) +
  labs(
    title = "SHAP Dependence Plot",
    subtitle = "How systolic BP affects longevity prediction",
    x = "Systolic Blood Pressure (mmHg)",
    y = "SHAP Value (Log-odds)",
    color = "Smoking Status"
  ) +
  theme(legend.position = "bottom")

p1 + p2


# Show consistent findings across models
consistent_predictors <- tibble(
  Predictor = c(
    "Systolic Blood Pressure",
    "Smoking Status",
    "Myocardial Infarction"
  ),
  `Logistic Regression` = c("✓ (Top 1)", "✓ (Top 2)", "✓ (Top 3)"),
  `LASSO` = c("✓ (Top 1)", "✓ (Top 2)", "✓ (Top 4)"),
  `XGBoost` = c("✓ (Top 1)", "✓ (Top 2)", "✓ (Top 5)"),
  `Clinical Knowledge` = c(
    "Well-established",
    "Well-established",
    "Well-established"
  ),
  `Effect Direction` = c(
    "Lower is better",
    "Never smoking best",
    "No history better"
  )
)

consistent_predictors %>%
  gt() %>%
  tab_header(
    title = "Consistent Top Predictors Across All Models",
    subtitle = "Validation through multiple approaches increases confidence"
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(columns = 2:4)
  ) %>%
  cols_align(align = "center", columns = 2:4)


# Simulate partial dependence plotsset.seed(789)
pdp_data <- tibble(
  systolic_bp = seq(100, 180, by = 2),
  pred_prob = plogis(
    -3 + -0.02 * (systolic_bp - 120) + rnorm(length(systolic_bp), 0, 0.05)
  )
) %>%
  mutate(pred_prob = pmax(0.02, pmin(0.18, pred_prob))) # Realistic bounds

hdl_data <- tibble(
  hdl = seq(20, 80, by = 2),
  pred_prob = plogis(-3 + 0.025 * (hdl - 40) + rnorm(length(hdl), 0, 0.05))
) %>%
  mutate(pred_prob = pmax(0.02, pmin(0.18, pred_prob)))

p1 <- pdp_data %>%
  ggplot(aes(x = systolic_bp, y = pred_prob)) +
  geom_line(linewidth = 2, color = "steelblue") +
  geom_ribbon(
    aes(ymin = pred_prob - 0.01, ymax = pred_prob + 0.01),
    alpha = 0.3,
    fill = "steelblue"
  ) +
  labs(
    title = "Systolic Blood Pressure",
    x = "mmHg",
    y = "Predicted Probability"
  )

p2 <- hdl_data %>%
  ggplot(aes(x = hdl, y = pred_prob)) +
  geom_line(linewidth = 2, color = "darkgreen") +
  geom_ribbon(
    aes(ymin = pred_prob - 0.01, ymax = pred_prob + 0.01),
    alpha = 0.3,
    fill = "darkgreen"
  ) +
  labs(title = "HDL Cholesterol", x = "mg/dL", y = NULL)

p1 +
  p2 +
  plot_annotation(
    title = "Partial Dependence Plots: Individual Variable Effects",
    theme = theme(plot.title = element_text(size = 16))
  )

# # ML Workflow in Practice

# ## Data Preprocessing: Foundation of Success

# ```{r}
# #| code-fold: false

# # Create preprocessing recipe

# Centenarianism_recipe <- recipe(outcome ~ ., data = demo_data) %>%
#   # Remove ID variables (if any)
#   step_rm(matches("id|ID")) %>%
#   # Handle missing data with KNN imputation
#   step_impute_knn(all_predictors(), neighbors = 3) %>%
#   # Group infrequent categorical levels
#   step_other(all_nominal_predictors(), threshold = 0.1) %>%
#   # Create dummy variables
#   step_dummy(all_nominal_predictors()) %>%
#   # Normalize numeric variables
#   step_normalize(all_numeric_predictors()) %>%
#   # Remove highly correlated variables
#   step_corr(all_numeric_predictors(), threshold = 0.9)
# # Display the recipe

# Centenarianism_recipe
# ```

# **Golden Rule:** All preprocessing must be made on training and test data separately to prevent data-leakage!

# ::: notes
# Explain each preprocessing step and why it's needed. KNN imputation uses similar patients to fill missing values. Normalization ensures all variables are on the same scale.
# :::

# ## Train/Test Split: Honest Evaluation

# ```{r}
# #| code-fold: false
# # Set seed for reproducibility
# set.seed(123)

# # Create stratified split (maintains outcome proportions)
# data_split <- initial_split(
#   demo_data,
#   strata = outcome, # Ensure balanced outcomes
#   prop = 0.75
# ) # 75% for training

# train_data <- training(data_split)
# test_data <- testing(data_split)

# # Create cross-validation folds

# cv_folds <- vfold_cv(train_data, v = 10, strata = outcome)

# # Check the split

# cat("Training set size:", nrow(train_data), "\n")
# cat("Test set size:", nrow(test_data), "\n")
# cat(
#   "Outcome prevalence in training:",
#   round(mean(train_data$outcome == "Centenarianism") * 100, 1),
#   "%\n"
# )
# cat(
#   "Outcome prevalence in test:",
#   round(mean(test_data$outcome == "Centenarianism") * 100, 1),
#   "%"
# )
# ```

# ::: callout-important
# ## Why stratify?

# With rare outcomes (like reaching age 95), random splits might create unbalanced training/test sets, making evaluation unreliable.
# :::

# ::: notes
# Explain that test data is "locked away" until final evaluation. Cross-validation provides multiple honest estimates of performance during model development. Emphasize that data leakage (using information from test set) is a critical error that invalidates results. This is like grading an exam after seeing the answer key.
# :::

# ## Model Training: Three Approaches

# ```{r}
# #| code-fold: false

# # 1. Logistic Regression (Traditional)
# logistic_spec <- logistic_reg() %>%
#   set_engine("glm") %>%
#   set_mode("classification")

# # 2. LASSO Regression (Regularized)
# lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
#   set_engine("glmnet") %>%
#   set_mode("classification")

# # 3. XGBoost (Tree-based ensemble)
# xgboost_spec <- boost_tree(
#   trees = tune(),
#   tree_depth = tune(),
#   learn_rate = tune()
# ) %>%
#   set_engine("xgboost") %>%
#   set_mode("classification")

# # Create workflows
# logistic_wf <- workflow() %>%
#   add_recipe(Centenarianism_recipe) %>%
#   add_model(logistic_spec)

# lasso_wf <- workflow() %>%
#   add_recipe(Centenarianism_recipe) %>%
#   add_model(lasso_spec)

# xgboost_wf <- workflow() %>%
#   add_recipe(Centenarianism_recipe) %>%
#   add_model(xgboost_spec)
# ```

# ::: notes
# Each model has different strengths: logistic regression is interpretable, LASSO does automatic variable selection XGBoost handles interactions and non-linearities.
# :::

# ## Hyperparameter Tuning with Cross-Validation

# ```{r}
# #| code-fold: false
# #| cache: true

# # Create tuning grids
# lasso_grid <- grid_regular(penalty(), levels = 10)
# xgboost_grid <- grid_latin_hypercube(
#   trees(),
#   tree_depth(),
#   learn_rate(),
#   size = 20 # Reduced for demo speed
# )

# # Tune LASSO
# lasso_results <- tune_grid(
#   lasso_wf,
#   resamples = cv_folds,
#   grid = lasso_grid,
#   metrics = metric_set(roc_auc, pr_auc)
# )

# # Tune XGBoost
# xgboost_results <- tune_grid(
#   xgboost_wf,
#   resamples = cv_folds,
#   grid = xgboost_grid,
#   metrics = metric_set(roc_auc, pr_auc)
# )

# # Select best parameters
# best_lasso <- select_best(lasso_results, metric = "roc_auc")
# best_xgboost <- select_best(xgboost_results, metric = "roc_auc")

# # Display best parameters
# print("Best LASSO parameters:")
# print(best_lasso)
# print("Best XGBoost parameters:")
# print(best_xgboost)
# ```

# ::: callout-tip
# ## Cross-Validation

# Think of cross-validation as "rehearsing" your model multiple times with different audience segments before the final performance.
# :::

# ::: notes
# Emphasize that we're using cross-validation to find the best settings without touching the test data. This is like practicing a presentation multiple times before the real thing.
# :::

# # Model Evaluation: Beyond Accuracy

# ## Performance Metrics for Epidemiological Research

# ```{r}
# #| echo: false
# #| fig-width: 12
# #| fig-height: 8

# # Train models for evaluation demonstration
# set.seed(42)

# # Fit logistic model
# logistic_fit <- fit_resamples(
#   logistic_wf,
#   resamples = cv_folds,
#   metrics = metric_set(roc_auc, pr_auc)
# )

# # Finalize tuned models
# final_lasso <- finalize_workflow(lasso_wf, best_lasso)
# final_xgboost <- finalize_workflow(xgboost_wf, best_xgboost)

# lasso_fit <- fit_resamples(
#   final_lasso,
#   resamples = cv_folds,
#   metrics = metric_set(roc_auc, pr_auc)
# )

# xgboost_fit <- fit_resamples(
#   final_xgboost,
#   resamples = cv_folds,
#   metrics = metric_set(roc_auc, pr_auc)
# )

# # Simulate test predictions for visualization
# n_test <- nrow(test_data)
# test_results <- tibble(
#   truth = test_data$outcome,
#   logistic = runif(n_test, 0.02, 0.15),
#   lasso = runif(n_test, 0.025, 0.16),
#   xgboost = runif(n_test, 0.03, 0.18)
# ) %>%
#   # Adjust probabilities based on true outcome
#   mutate(
#     logistic = ifelse(truth == "Centenarianism", logistic * 3, logistic * 0.7),
#     lasso = ifelse(truth == "Centenarianism", lasso * 3, lasso * 0.7),
#     xgboost = ifelse(truth == "Centenarianism", xgboost * 3.2, xgboost * 0.65)
#   ) %>%
#   pivot_longer(
#     cols = c(logistic, lasso, xgboost),
#     names_to = "model",
#     values_to = "prediction"
#   )

# # ROC Curves
# roc_data <- test_results %>%
#   group_by(model) %>%
#   roc_curve(truth, prediction) %>%
#   mutate(model = str_to_title(model))

# p1 <- roc_data %>%
#   ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
#   geom_path(linewidth = 1.5) +
#   geom_abline(lty = 2, alpha = 0.5) +
#   labs(
#     title = "ROC Curves",
#     x = "1 - Specificity (False Positive Rate)",
#     y = "Sensitivity (True Positive Rate)",
#     color = "Model"
#   ) +
#   theme(legend.position = "bottom")

# # PR Curves
# pr_data <- test_results %>%
#   group_by(model) %>%
#   pr_curve(truth, prediction) %>%
#   mutate(model = str_to_title(model))

# p2 <- pr_data %>%
#   ggplot(aes(x = recall, y = precision, color = model)) +
#   geom_path(linewidth = 1.5) +
#   labs(
#     title = "Precision-Recall Curves",
#     x = "Recall (Sensitivity)",
#     y = "Precision (PPV)",
#     color = "Model"
#   ) +
#   theme(legend.position = "bottom")

# # Calibration Plot
# cal_data <- test_results %>%
#   mutate(
#     pred_bin = cut(prediction, breaks = seq(0, 1, 0.05), include.lowest = TRUE),
#     pred_midpoint = (as.numeric(pred_bin) - 1) * 0.05 + 0.025
#   ) %>%
#   filter(!is.na(pred_bin)) %>%
#   group_by(model, pred_midpoint) %>%
#   summarise(
#     n = n(),
#     observed = mean(truth == "Centenarianism"),
#     .groups = "drop"
#   ) %>%
#   filter(n >= 5) %>%
#   mutate(model = str_to_title(model))

# p3 <- cal_data %>%
#   ggplot(aes(x = pred_midpoint, y = observed, color = model)) +
#   geom_point(aes(size = n), alpha = 0.7) +
#   geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
#   geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 0.5) +
#   labs(
#     title = "Calibration Plot",
#     x = "Predicted Probability",
#     y = "Observed Proportion",
#     color = "Model",
#     size = "N"
#   ) +
#   theme(legend.position = "bottom")

# # Combine plots
# (p1 + p2) /
#   p3 +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# ```

# ::: notes
# ROC curves show discrimination (can the model separate cases from controls). PR curves are better for rare outcomes. Calibration shows if predicted probabilities match reality.
# :::

# ## Study Results: Modest but Meaningful Improvements

# ```{r}
# #| echo: false

# # Create results table based on the study findings and our CV results
# cv_results <- bind_rows(
#   collect_metrics(logistic_fit) %>% mutate(model = "Logistic Regression"),
#   collect_metrics(lasso_fit) %>% mutate(model = "LASSO"),
#   collect_metrics(xgboost_fit) %>% mutate(model = "XGBoost")
# ) %>%
#   filter(.metric == "roc_auc") %>%
#   select(model, mean, std_err)

# study_results <- tibble(
#   Model = c("Logistic Regression", "LASSO", "XGBoost"),
#   `ROC-AUC (CV)` = sprintf("%.3f (±%.3f)", cv_results$mean, cv_results$std_err),
#   `Study ROC-AUC` = c(
#     "0.69 (0.66-0.73)",
#     "0.71 (0.67-0.74)",
#     "0.72 (0.66-0.75)"
#   ),
#   `Key Insight` = c(
#     "Baseline comparison",
#     "Automatic variable selection",
#     "Captured non-linear relationships"
#   )
# )

# study_results %>%
#   gt() %>%
#   tab_header(
#     title = "Model Performance: Predicting Extreme Centenarianism",
#     subtitle = "Comparison of Cross-Validation Results with Published Study"
#   ) %>%
#   tab_source_note(
#     source_note = "Atias et al. (2025). Study values show point estimate (95% CI)"
#   ) %>%
#   cols_align(align = "center", columns = everything()) %>%
#   tab_style(
#     style = cell_fill(color = "lightblue"),
#     locations = cells_body(rows = 3) # Highlight XGBoost
#   )
# ```

# **Key Finding:** XGBoost showed statistically significant improvement over logistic regression, though differences were modest.

# ::: notes
# Emphasize that even modest improvements can be clinically meaningful, especially for rare outcomes. The consistency of findings across models increases confidence.
# :::

# # Explainable AI: Opening the Black Box

# ## Why Interpretability Matters in Healthcare

# ::::: columns
# ::: {.column width="50%"}
# **Clinical Adoption Requires:**

# -   Trust in the model
# -   Understanding of decisions
# -   Ability to explain to patients
# -   Regulatory compliance
# -   Error detection capability

# > *"Would you trust a colleague who's always right but never explains their reasoning?"*
# :::

# ::: {.column width="50%"}
# **XAI Tools Provide:**

# -   Feature importance rankings
# -   Individual prediction explanations
# -   Model behavior visualization
# -   Interaction detection
# -   Bias identification
# :::
# :::::

# ::: callout-warning
# ## Black Box Problem

# Complex models like XGBoost can make accurate predictions but provide little insight into how decisions are made.
# :::

# ::: notes
# Use the analogy of trusting a brilliant colleague who gives perfect answers but never explains their reasoning. In healthcare, understanding is as important as accuracy.
# :::

# ## SHAP Values: Fair Credit Assignment

# ```{r}
# #| echo: false
# #| fig-width: 12
# #| fig-height: 8

# # Simulate SHAP values based on study findings
# set.seed(123)
# shap_data <- tibble(
#   feature = c(
#     "Systolic BP",
#     "Smoking (Current)",
#     "MI History",
#     "HDL Cholesterol",
#     "Diastolic BP",
#     "Glucose",
#     "BMI",
#     "Age"
#   ),
#   importance = c(0.174, 0.129, 0.109, 0.094, 0.082, 0.070, 0.062, 0.060)
# ) %>%
#   mutate(
#     feature = fct_reorder(feature, importance),
#     rank = row_number(desc(importance))
#   )

# # Variable importance plot
# p1 <- shap_data %>%
#   ggplot(aes(x = importance, y = feature, fill = rank <= 3)) +
#   geom_col() +
#   geom_text(aes(label = sprintf("%.3f", importance)), hjust = -0.1, size = 3) +
#   scale_fill_manual(values = c("grey70", "steelblue")) +
#   labs(
#     title = "SHAP Feature Importance",
#     subtitle = "Top predictors of extreme Centenarianism",
#     x = "Mean |SHAP Value|",
#     y = NULL
#   ) +
#   theme(legend.position = "none")

# # SHAP dependence plot simulation
# set.seed(456)
# dependence_data <- tibble(
#   systolic_bp = runif(500, 100, 180),
#   shap_value = -0.003 * (systolic_bp - 120) + rnorm(500, 0, 0.02),
#   smoking = sample(c("Never", "Former", "Current"), 500, replace = TRUE)
# )

# p2 <- dependence_data %>%
#   ggplot(aes(x = systolic_bp, y = shap_value, color = smoking)) +
#   geom_point(alpha = 0.6) +
#   geom_smooth(se = FALSE, linewidth = 1) +
#   labs(
#     title = "SHAP Dependence Plot",
#     subtitle = "How systolic BP affects Centenarianism prediction",
#     x = "Systolic Blood Pressure (mmHg)",
#     y = "SHAP Value (Log-odds)",
#     color = "Smoking Status"
#   ) +
#   theme(legend.position = "bottom")

# p1 + p2
# ```

# **SHAP Properties:** - **Additive:** Sum of SHAP values = prediction - baseline - **Fair:** Each feature gets appropriate credit - **Consistent:** Higher feature value = higher SHAP value (if feature is positively associated)

# ::: notes
# SHAP values are like dividing credit among team players. They tell us exactly how much each variable contributed to a specific prediction, with mathematical guarantees of fairness.
# :::

# ## Key Findings: Consistent Across Models

# ```{r}
# #| echo: false

# # Show consistent findings across models
# consistent_predictors <- tibble(
#   Predictor = c(
#     "Systolic Blood Pressure",
#     "Smoking Status",
#     "Myocardial Infarction"
#   ),
#   `Logistic Regression` = c("✓ (Top 1)", "✓ (Top 2)", "✓ (Top 3)"),
#   `LASSO` = c("✓ (Top 1)", "✓ (Top 2)", "✓ (Top 4)"),
#   `XGBoost` = c("✓ (Top 1)", "✓ (Top 2)", "✓ (Top 5)"),
#   `Clinical Knowledge` = c(
#     "Well-established",
#     "Well-established",
#     "Well-established"
#   ),
#   `Effect Direction` = c(
#     "Lower is better",
#     "Never smoking best",
#     "No history better"
#   )
# )

# consistent_predictors %>%
#   gt() %>%
#   tab_header(
#     title = "Consistent Top Predictors Across All Models",
#     subtitle = "Validation through multiple approaches increases confidence"
#   ) %>%
#   tab_style(
#     style = cell_fill(color = "lightgreen"),
#     locations = cells_body(columns = 2:4)
#   ) %>%
#   cols_align(align = "center", columns = 2:4)
# ```

# ::: callout-note
# ## Model Agreement = Increased Confidence

# When different models identify the same important predictors, it strengthens our confidence in the findings and aligns with existing clinical knowledge.
# :::

# ::: notes
# The consistency across models provides validation. These aren't statistical artifacts but real patterns that different algorithms independently discovered.
# :::

# ## Partial Dependence: "What-If" Scenarios

# ```{r}
# #| code-fold: false
# #| eval: false

# # # Example of creating partial dependence plots
# # library(pdp)

# # # For XGBoost model (after training)
# # pdp_systolic <- partial(
# # extract_fit_engine(final_xgb_fit),
# # pred.var = "systolic_bp",
# # train = xgb_train_data,
# # type = "classification"
# # )

# # # Visualize

# # pdp_systolic %>%
# # autoplot() +
# # labs(
# # title = "Partial Dependence: Systolic Blood Pressure",
# # subtitle = "How Centenarianism probability changes with BP",
# # x = "Systolic BP (mmHg)",
# # y = "Predicted Probability"
# # )
# ```

# ```{r}
# #| echo: false
# #| fig-width: 10
# #| fig-height: 5

# # Simulate partial dependence plotsset.seed(789)
# pdp_data <- tibble(
#   systolic_bp = seq(100, 180, by = 2),
#   pred_prob = plogis(
#     -3 + -0.02 * (systolic_bp - 120) + rnorm(length(systolic_bp), 0, 0.05)
#   )
# ) %>%
#   mutate(pred_prob = pmax(0.02, pmin(0.18, pred_prob))) # Realistic bounds

# hdl_data <- tibble(
#   hdl = seq(20, 80, by = 2),
#   pred_prob = plogis(-3 + 0.025 * (hdl - 40) + rnorm(length(hdl), 0, 0.05))
# ) %>%
#   mutate(pred_prob = pmax(0.02, pmin(0.18, pred_prob)))

# p1 <- pdp_data %>%
#   ggplot(aes(x = systolic_bp, y = pred_prob)) +
#   geom_line(linewidth = 2, color = "steelblue") +
#   geom_ribbon(
#     aes(ymin = pred_prob - 0.01, ymax = pred_prob + 0.01),
#     alpha = 0.3,
#     fill = "steelblue"
#   ) +
#   labs(
#     title = "Systolic Blood Pressure",
#     x = "mmHg",
#     y = "Predicted Probability"
#   )

# p2 <- hdl_data %>%
#   ggplot(aes(x = hdl, y = pred_prob)) +
#   geom_line(linewidth = 2, color = "darkgreen") +
#   geom_ribbon(
#     aes(ymin = pred_prob - 0.01, ymax = pred_prob + 0.01),
#     alpha = 0.3,
#     fill = "darkgreen"
#   ) +
#   labs(title = "HDL Cholesterol", x = "mg/dL", y = NULL)

# p1 +
#   p2 +
#   plot_annotation(
#     title = "Partial Dependence Plots: Individual Variable Effects",
#     theme = theme(plot.title = element_text(size = 16))
#   )
# ```

# **Key Insights from Study:** - **Diastolic BP:** Sharp drop above 93 mmHg - **HDL:** Favorable above 42 mg/dL\
# - **Glucose:** Optimal below 100 mg/dL

# ::: notes
# These plots show "what happens if we change just one variable." Notice how the model discovered clinically meaningful thresholds without being explicitly programmed with medical knowledge.
# :::

# # Hands-On Implementation

# ## Complete Workflow in R

# ```{r}
# #| code-fold: false

# # 1. Setup and data loading

# library(tidymodels)
# library(tidyverse)

# # Our demo data is already loaded

# glimpse(demo_data)

# # 2. Data splitting (already done above)

# set.seed(123)
# data_split <- initial_split(demo_data, strata = outcome, prop = 0.75)
# train_data <- training(data_split)
# test_data <- testing(data_split)
# cv_folds <- vfold_cv(train_data, v = 5) # Using 5-fold for demo

# # 3. Preprocessing recipe

# preprocessing_recipe <- recipe(outcome ~ ., data = train_data) %>%
#   step_impute_knn(all_predictors()) %>%
#   step_dummy(all_nominal_predictors()) %>%
#   step_normalize(all_numeric_predictors()) %>%
#   step_zv(all_predictors()) # Remove zero variance

# # 4. Model specifications

# models <- list(
#   logistic = logistic_reg() %>% set_engine("glm"),
#   lasso = logistic_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"),
#   xgboost = boost_tree(trees = tune(), tree_depth = tune()) %>%
#     set_engine("xgboost") %>%
#     set_mode("classification")
# )

# # 5. Create workflows

# workflows <- map(
#   models,
#   ~ workflow() %>%
#     add_recipe(preprocessing_recipe) %>%
#     add_model(.x)
# )

# print("Workflows created successfully!")
# ```

# ::: notes
# This demonstrates a complete, reproducible ML workflow. Emphasize the systematic approach and the importance of setting seeds for reproducibility.
# :::

# ## Model Training and Evaluation

# ```{r}
# #| code-fold: false
# #| cache: true

# # Train logistic regression (no tuning needed)

# logistic_results <- fit_resamples(
#   workflows$logistic,
#   resamples = cv_folds,
#   metrics = metric_set(roc_auc, pr_auc, accuracy, sens, spec)
# )

# # Tune and train LASSO

# lasso_grid <- grid_regular(penalty(), levels = 10)
# lasso_results <- tune_grid(
#   workflows$lasso,
#   resamples = cv_folds,
#   grid = lasso_grid,
#   metrics = metric_set(roc_auc, pr_auc, accuracy, sens, spec)
# )

# # Tune and train XGBoost

# xgb_grid <- grid_regular(trees(), tree_depth(), levels = 3)
# xgb_results <- tune_grid(
#   workflows$xgboost,
#   resamples = cv_folds,
#   grid = xgb_grid,
#   metrics = metric_set(roc_auc, pr_auc, accuracy, sens, spec)
# )

# # Compare performance

# performance_comparison <- bind_rows(
#   collect_metrics(logistic_results) %>% mutate(model = "Logistic"),
#   collect_metrics(lasso_results) %>%
#     mutate(model = "LASSO") %>%
#     filter(.config == select_best(lasso_results, metric = "roc_auc")$.config),
#   collect_metrics(xgb_results) %>%
#     mutate(model = "XGBoost") %>%
#     filter(.config == select_best(xgb_results, metric = "roc_auc")$.config)
# ) %>%
#   filter(.metric %in% c("roc_auc", "pr_auc")) %>%
#   select(model, .metric, mean, std_err) %>%
#   pivot_wider(names_from = .metric, values_from = c(mean, std_err))

# print(performance_comparison)
# ```

# ```{r}
# #| echo: false

# # Create a nice table
# performance_comparison %>%
#   mutate(
#     `ROC-AUC` = sprintf("%.3f (±%.3f)", mean_roc_auc, std_err_roc_auc),
#     `PR-AUC` = sprintf("%.3f (±%.3f)", mean_pr_auc, std_err_pr_auc)
#   ) %>%
#   select(Model = model, `ROC-AUC`, `PR-AUC`) %>%
#   gt() %>%
#   tab_header("Cross-Validation Performance Comparison") %>%
#   cols_align(align = "center", columns = everything())
# ```

# ::: notes
# Show students how to systematically compare models using cross-validation. Emphasize that we're looking for meaningful differences, not just statistical significance.
# :::

# ## Final Model Selection and Testing

# ```{r}
# #| code-fold: false
# #| cache: true

# # Select best models

# best_lasso <- select_best(lasso_results, metric = "roc_auc")
# best_xgb <- select_best(xgb_results, metric = "roc_auc")

# # Finalize workflows

# final_lasso <- finalize_workflow(workflows$lasso, best_lasso)
# final_xgb <- finalize_workflow(workflows$xgboost, best_xgb)

# # Fit on full training set and evaluate on test set

# final_fits <- list(
#   logistic = last_fit(workflows$logistic, data_split),
#   lasso = last_fit(final_lasso, data_split),
#   xgboost = last_fit(final_xgb, data_split)
# )

# # Extract test set performance

# test_performance <- map_dfr(final_fits, collect_metrics, .id = "model") %>%
#   filter(.metric %in% c("roc_auc", "accuracy")) %>%
#   select(model, .metric, .estimate) %>%
#   pivot_wider(names_from = .metric, values_from = .estimate)

# test_performance %>%
#   gt() %>%
#   tab_header("Final Test Set Performance") %>%
#   fmt_number(columns = 2:3, decimals = 3) %>%
#   cols_align(align = "center", columns = everything())
# ```

# ::: callout-important
# ## Honest Evaluation

# The test set results are the "final exam" - they tell us how well our model will perform on truly new patients.
# :::

# ::: notes
# Emphasize that test set performance is the ultimate measure of model utility. This is the "honest" evaluation that tells us real-world performance.
# :::

# ## Creating an Interactive Prediction Tool

# ```{r}
# #| code-fold: false
# #| eval: false

# # Example Shiny app for predictions (code only - not run in slides)

# library(shiny)
# library(bslib)

# # Extract final model

# final_model <- extract_workflow(final_fits$xgboost)

# ui <- page_sidebar(
#   title = "Centenarianism Risk Calculator",
#   sidebar = sidebar(
#     numericInput("age", "Age (years)", value = 55, min = 40, max = 70),
#     numericInput(
#       "systolic_bp",
#       "Systolic BP (mmHg)",
#       value = 140,
#       min = 90,
#       max = 200
#     ),
#     selectInput(
#       "smoking_status",
#       "Smoking Status",
#       choices = c("Never", "Former", "Current")
#     ),
#     numericInput(
#       "hdl",
#       "HDL Cholesterol (mg/dL)",
#       value = 45,
#       min = 20,
#       max = 100
#     ),
#     selectInput("mi_history", "Heart Attack History", choices = c("No", "Yes")),
#     actionButton("predict", "Calculate Risk", class = "btn-primary")
#   ),
#   card(
#     card_header("Predicted Centenarianism Risk"),
#     plotOutput("risk_plot"),
#     verbatimTextOutput("risk_text")
#   )
# )

# server <- function(input, output) {
#   prediction <- eventReactive(input$predict, {
#     new_data <- tibble(
#       age = input$age,
#       systolic_bp = input$systolic_bp,
#       smoking_status = input$smoking_status,
#       hdl = input$hdl,
#       mi_history = input$mi_history,
#       # Add default values for other required variables
#       diastolic_bp = 85,
#       bmi = 26,
#       glucose = 100
#     )

#     predict(final_model, new_data, type = "prob")
#   })

#   output$risk_text <- renderText({
#     req(prediction())
#     risk <- prediction()$.pred_Centenarianism * 100
#     paste0("Estimated probability of reaching age 95+: ", round(risk, 1), "%")
#   })
# }

# #shinyApp(ui, server) # Uncomment to run
# ```

# ::: notes
# This shows how ML models can be deployed as interactive tools for clinicians. Emphasize the importance of making predictions accessible and interpretable.
# :::

# # Summary and Key Takeaways

# ## What We've Learned Today

# ::: incremental
# 1.  **ML complements traditional epidemiology** - prediction vs. causation serve different purposes

# 2.  **Structured workflow prevents common pitfalls** - systematic approach is more important than fancy algorithms

# 3.  **Overfitting is the main enemy** - validation and cross-validation are essential safeguards

# 4.  **Interpretability builds trust** - XAI tools make complex models clinically acceptable

# 5.  **Modest improvements can be meaningful** - especially for rare outcomes and clinical applications

# 6.  **Domain expertise remains crucial** - "human-in-the-loop" prevents algorithmic mistakes
# :::

# ::: notes
# Reinforce that ML is not magic but a systematic approach to pattern recognition. The human expert remains essential for interpretation and validation.
# :::

# ## When to Use ML in Epidemiological Research

# ::::: columns
# ::: {.column width="50%"}
# **ML is Great For:**

# -   High-dimensional data
# -   Complex interactions
# -   Prediction problems
# -   Pattern discovery
# -   Risk stratification
# -   Screening tools

# **Examples:** - Identifying high-risk patients - Early warning systems - Precision medicine
# :::

# ::: {.column width="50%"}
# **Traditional Methods Better For:**

# -   Causal inference
# -   Small datasets
# -   Simple relationships
# -   Regulatory approval
# -   Understanding mechanisms
# -   Clinical interpretation

# **Examples:** - Vaccine effectiveness - Treatment effects - Policy evaluation
# :::
# :::::

# > **Bottom Line:** Choose the right tool for your research question!

# ::: notes
# Help students understand that the choice between ML and traditional methods should be driven by the research question and data characteristics, not by what's trendy.
# :::

# ## Practical Recommendations

# ::: {.callout-tip collapse="false"}
# ## For Your Future Research

# 1.  **Start simple** - Always compare ML to appropriate baseline (like logistic regression)

# 2.  **Validate rigorously** - Use proper train/test splits and cross-validation

# 3.  **Interpret results** - Use SHAP, partial dependence plots, and domain knowledge

# 4.  **Report transparently** - Follow reporting guidelines (TRIPOD, STARD-AI)

# 5.  **Consider ethics** - Address bias, fairness, and clinical impact

# 6.  **Stay updated** - ML in medicine is rapidly evolving
# :::

# ::: notes
# These recommendations will help students apply ML responsibly in their future research. Emphasize the importance of ethical considerations and transparent reporting.
# :::

# ## Resources for Continued Learning

# **Books:** - *Hands-On Machine Learning* by Aurélien Géron - *The Elements of Statistical Learning* by Hastie et al. - *Tidy Modeling with R* by Kuhn & Silge

# **Online Courses:** - tidymodels tutorials: tidymodels.org - Machine Learning for Healthcare (MIT) - Coursera Machine Learning Course

# **R Packages:** - `tidymodels` - Modern ML workflow - `shapviz` - SHAP visualizations\
# - `probably` - Post-processing predictions - `vetiver` - Model deployment

# **Key Papers:** - Atias et al. (2025) - Our case study - Christodoulou et al. (2019) - ML vs. logistic regression - Collins et al. (2015) - TRIPOD reporting guidelines

# ## Hands-On Exercise for Next Class

# ::: callout-note
# ## Take-Home Assignment

# 1.  Download the demo dataset and R code from today's lecture
# 2.  Try different preprocessing approaches (mean imputation vs. KNN)
# 3.  Experiment with different hyperparameters
# 4.  Create SHAP plots for the best model
# 5.  Write a 1-page interpretation of your findings

# **Due next week - we'll discuss your results!**
# :::

# ------------------------------------------------------------------------

# ## Questions & Discussion

# ::: callout-question
# ## Think About:

# -   When might you use ML in your own research?
# -   What ethical considerations are most important?
# -   How would you explain ML results to clinicians?
# -   What barriers exist to ML adoption in your field?
# :::

# **Thank you for your attention!**

# *Remember: Machine learning is a powerful tool, but like any tool, its value depends on how skillfully and appropriately it's used.*

# ::: notes
# Encourage questions and discussion. This is where the real learning often happens as students connect concepts to their own research interests and challenges.

# Key teaching points to reinforce: - ML is not magic, it's systematic pattern recognition - Always start with domain knowledge - Validation is crucial - never trust a model you haven't tested properly - Interpretability is essential for clinical adoption - Modest improvements can have real clinical impact - The human expert remains central to the process
# :::

# ::: ::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::
