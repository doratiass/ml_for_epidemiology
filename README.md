# Machine Learning in Epidemiology: A Practical Lecture

## Overview

This repository contains the materials for a guest lecture on the application of machine learning (ML) in epidemiology, designed for the "Advanced Statistical Methods" course for MPH and epidemiology students at Tel Aviv University (TAU).

The lecture provides a comprehensive introduction to the machine learning workflow, from study design to model interpretation, using a real-world case study. The goal is to bridge the gap between traditional epidemiological methods and modern predictive modeling techniques, equipping students with the conceptual understanding and practical R coding skills to apply ML in their own research.

## Case Study and Publication

The concepts and examples presented in this lecture are based on the following published paper:

> Atias, D., & Ashri, S. (2025). **Machine learning in epidemiology: An introduction, comparison with traditional methods, and a case study of predicting extreme longevity**. *Annals of Epidemiology*, *\[110\]*, *\[Pages 23-33\]*. [https://doi.org/10.1016/j.annepidem.2025.07.024](https://www.sciencedirect.com/science/article/pii/S1047279725001735 "null")

This work serves as the primary case study, demonstrating how ML models can be built and interpreted to predict health outcomes.

## Lecture Content

The lecture covers the following key topics:

-   **Introduction to Machine Learning:** Contrasting predictive modeling with traditional causal inference in epidemiology.

-   **The ML Workflow:** A step-by-step guide covering data preprocessing, feature engineering, model training, hyperparameter tuning, and evaluation.

-   **Core ML Models:** An intuitive overview of the concepts behind Logistic Regression, LASSO, Decision Trees, Random Forest, and XGBoost.

-   **Model Evaluation:** Selecting and interpreting appropriate performance metrics (e.g., AUC-ROC, Precision-Recall, Calibration).

-   **Explainable AI (XAI):** Using methods like SHAP (SHapley Additive exPlanations) to open the "black box" and understand model predictions.

-   **Practical Implementation:** All concepts are demonstrated with accompanying R code using the `tidymodels`framework.

## Data

**Important Note:** The dataset included in this repository is **synthetic**. It was generated using the `synthpop` package in R to mimic the statistical properties of the original dataset used in the publication, while ensuring full confidentiality and removing any patient-level information. The synthetic data is for educational and demonstration purposes only.

## Getting Started

To run the code and render the lecture presentation, you will need:

1.  R and RStudio installed on your system.

2.  Clone this repository to your local machine.

3.  Open the repository folder in RStudio \ Positron.

4.  Install the required R packages listed in the `00 - funcs.R` script.

5.  The main lecture file is `ml_for_epidemiology.qmd`, which can be rendered into a Reveal.js presentation.

## Acknowledgements

This lecture was prepared as part of the **Advanced Statistical Methods** course in the School of Public Health at Tel Aviv University.