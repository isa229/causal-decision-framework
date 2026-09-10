# Load required libraries
library(tidymodels)
library(dplyr)
library(purrr)
library(vip)
library(ggplot2)

if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  source(if (requireNamespace("here", quietly = TRUE))
    here::here("R", "00_theme_talk.R") else "R/00_theme_talk.R")
}

#' Train Multiple Causal Predictive Models (DAG-Guided)
#'
#' Trains GLM, Random Forest, and XGBoost models that strictly follow the DAG's
#' adjustment strategy:
#'   - ADD the confounder (order_volume)  -> closes the Simpson's backdoor
#'   - REMOVE the collider (opened_support_ticket) -> avoids collider bias
#'   - REMOVE noise variables (age, emails, logins)
#'   - KEEP spend as a precision covariate
#'
#' This recovers the correct POSITIVE causal direction: exceptions increase churn.
#'
#' @param data Simulated dataset from simulate_delivery_data()
#' @return A fitted workflow set
fit_causal_models <- function(data) {

  set.seed(123)
  data_split <- initial_split(data, prop = 0.8, strata = churned)
  train_data <- training(data_split)

  # The DAG-guided recipe:
  #   keep: has_exception (treatment), order_volume (confounder), monthly_spend_usd (precision)
  #   drop: opened_support_ticket (COLLIDER) + noise variables
  causal_recipe <- recipe(churned ~ ., data = train_data) |>
    update_role(customer_id, new_role = "ID") |>
    step_rm(
      opened_support_ticket,        # the collider -- the key intervention
      customer_age_years,
      marketing_emails_clicked,
      app_logins_last_7_days
    ) |>
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

  causal_workflows <- workflow_set(
    preproc = list(causal_rec = causal_recipe),
    models = list(glm = log_spec, rf = rf_spec, xgb = xgb_spec)
  )

  causal_workflows |>
    mutate(fit = map(info, ~ fit(.x$workflow[[1]], data = train_data)))
}

#' Plot Causal GLM Coefficients
#'
#' @param fitted_workflows The output from fit_causal_models()
plot_causal_glm <- function(fitted_workflows) {

  glm_fit <- fitted_workflows |>
    filter(wflow_id == "causal_rec_glm") |>
    pull(fit) |>
    _[[1]] |>
    extract_fit_parsnip()

  tidy(glm_fit) |>
    filter(term != "(Intercept)") |>
    mutate(
      term = reorder(term, estimate),
      is_positive = estimate > 0
    ) |>
    ggplot(aes(x = estimate, y = term, fill = is_positive)) +
    geom_col() +
    scale_fill_manual(values = c("TRUE" = ix_green, "FALSE" = ix_red)) +
    labs(title = "DAG-Guided Model Coefficients",
         x = "Log-Odds Estimate", y = NULL) +
    theme_talk() +
    theme(legend.position = "none")
}

#' Plot Causal XGBoost Variable Importance
#'
#' @param fitted_workflows The output from fit_causal_models()
plot_causal_xgb <- function(fitted_workflows) {

  xgb_fit <- fitted_workflows |>
    filter(wflow_id == "causal_rec_xgb") |>
    pull(fit) |>
    _[[1]] |>
    extract_fit_parsnip()

  vip(xgb_fit, geom = "col", aesthetics = list(fill = ix_green)) +
    labs(title = "DAG-Guided Model Feature Importance") +
    theme_talk()
}
