# Load required libraries
library(tidymodels)
library(dplyr)
library(purrr)
library(vip)
library(ggplot2)

#' Train Multiple Naive Predictive Models (The Trap)
#'
#' Trains GLM, Random Forest, and XGBoost models the way a competent
#' predictive-first workflow typically would: throw in EVERY observed feature,
#' cross-validate, tune, and ship the best scorer.
#'
#' This is the kitchen-sink specification. It correctly ADJUSTS the confounder
#' (order_volume) -- and still gets the causal question wrong, because its
#' single causal error is:
#'   - KEEPS the collider (opened_support_ticket) -> the most predictive
#'     feature (a proxy for latent impatience), which is exactly why a
#'     predictive workflow would never remove it -- and exactly what opens
#'     the Berkson's-paradox path (exception -> ticket <- impatience -> churn)
#'
#' The result is an estimate for `has_exception` with the WRONG sign despite
#' genuinely good metrics (test AUC ~ 0.84): the model concludes that delivery
#' exceptions REDUCE churn.
#'
#' @param data Simulated dataset from simulate_delivery_data()
#' @return A fitted workflow set
fit_naive_models <- function(data) {

  set.seed(123)
  data_split <- initial_split(data, prop = 0.8, strata = churned)
  train_data <- training(data_split)

  # The naive "kitchen-sink" recipe: every observed predictor goes in --
  # confounder AND collider. No variable selection, no causal reasoning:
  # exactly what a metrics-driven workflow produces.
  naive_recipe <- recipe(churned ~ ., data = train_data) |>
    update_role(customer_id, new_role = "ID") |>
    step_dummy(all_nominal_predictors()) |>
    step_normalize(all_numeric_predictors())

  log_spec <- logistic_reg() |>
    set_engine("glm") |>
    set_mode("classification")

  rf_spec <- rand_forest(trees = 100) |>
    set_engine("ranger", importance = "impurity") |>
    set_mode("classification")

  xgb_spec <- boost_tree(trees = 100) |>
    set_engine("xgboost") |>
    set_mode("classification")

  naive_workflows <- workflow_set(
    preproc = list(base_rec = naive_recipe),
    models = list(glm = log_spec, rf = rf_spec, xgb = xgb_spec)
  )

  naive_workflows |>
    mutate(fit = map(info, ~ fit(.x$workflow[[1]], data = train_data)))
}

#' Plot GLM Coefficients to Show the Trap
#'
#' @param fitted_workflows The output from fit_naive_models()
plot_glm_trap <- function(fitted_workflows) {

  glm_fit <- fitted_workflows |>
    filter(wflow_id == "base_rec_glm") |>
    pull(fit) |>
    _[[1]] |>
    extract_fit_parsnip()

  tidy(glm_fit) |>
    filter(term != "(Intercept)") |>
    mutate(
      term = reorder(term, estimate),
      is_negative = estimate < 0
    ) |>
    ggplot(aes(x = estimate, y = term, fill = is_negative)) +
    geom_col() +
    scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
    theme_minimal() +
    labs(
      title = "Naive Logistic Regression Coefficients",
      subtitle = "'has_exception' is NEGATIVE. The model claims delays REDUCE churn.",
      x = "Log-Odds Estimate",
      y = NULL
    ) +
    theme(legend.position = "none")
}

#' Plot XGBoost Variable Importance
#'
#' @param fitted_workflows The output from fit_naive_models()
plot_xgb_vip <- function(fitted_workflows) {

  xgb_fit <- fitted_workflows |>
    filter(wflow_id == "base_rec_xgb") |>
    pull(fit) |>
    _[[1]] |>
    extract_fit_parsnip()

  vip(xgb_fit, geom = "col", aesthetics = list(fill = "firebrick")) +
    theme_minimal() +
    labs(
      title = "Naive XGBoost Feature Importance",
      subtitle = "XGBoost leans heavily on the Collider (support_ticket) - its top predictor.",
      x = "Features",
      y = "Importance"
    )
}
