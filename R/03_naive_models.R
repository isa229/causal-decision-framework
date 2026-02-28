# Load required libraries
library(tidymodels)
library(dplyr)
library(purrr)
library(vip)
library(ggplot2)

#' Train Multiple Naive Predictive Models (The Collider Trap)
#' 
#' Trains GLM, Random Forest, and XGBoost models using all available data.
#' Demonstrates that advanced algorithms filter out noise but still fall 
#' for collider bias (Bad Control)
#' 
#' @param data Simulated dataset from simulate_delivery_data()
#' @return A fitted workflow set
fit_naive_models <- function(data) {
  
  # 1. Data Splitting
  set.seed(123)
  data_split <- initial_split(data, prop = 0.8, strata = churned)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # 2. Recipe: Throw everything into the model
  naive_recipe <- recipe(churned ~ ., data = train_data) |>
    update_role(customer_id, new_role = "ID") |>
    step_dummy(all_nominal_predictors()) |>
    step_normalize(all_numeric_predictors())
  
  # 3. Model Specifications
  log_spec <- logistic_reg() |> 
    set_engine("glm") |> 
    set_mode("classification")
  
  rf_spec <- rand_forest(trees = 100) |> 
    set_engine("ranger", importance = "impurity") |> 
    set_mode("classification")
  
  xgb_spec <- boost_tree(trees = 100) |> 
    set_engine("xgboost") |> 
    set_mode("classification")
  
  # 4. Create a Workflow Set
  naive_workflows <- workflow_set(
    preproc = list(base_rec = naive_recipe),
    models = list(glm = log_spec, rf = rf_spec, xgb = xgb_spec)
  )
  
  # 5. Fit all models to the training data
  fitted_workflows <- naive_workflows |>
    mutate(fit = map(info, ~ fit(.x$workflow[[1]], data = train_data)))
  
  return(fitted_workflows)
}

#' Plot GLM Coefficients to Show the Trap
#' 
#' @param fitted_workflows The output from fit_naive_models()
plot_glm_trap <- function(fitted_workflows) {
  
  # Extract the fitted GLM model
  glm_fit <- fitted_workflows |> 
    filter(wflow_id == "base_rec_glm") |> 
    pull(fit) |> 
    _[[1]] |> 
    extract_fit_parsnip()
  
  # Plot the coefficients
  p <- tidy(glm_fit) |>
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
      title = "Logistic Regression Coefficients",
      subtitle = "Notice that 'has_exception' is negative. The model says delays reduce churn",
      x = "Log-Odds Estimate",
      y = NULL
    ) +
    theme(legend.position = "none")
  
  return(p)
}

#' Plot XGBoost Variable Importance
#' 
#' @param fitted_workflows The output from fit_naive_models()
plot_xgb_vip <- function(fitted_workflows) {
  
  # Extract the fitted XGBoost model
  xgb_fit <- fitted_workflows |> 
    filter(wflow_id == "base_rec_xgb") |> 
    pull(fit) |> 
    _[[1]] |> 
    extract_fit_parsnip()
  
  # Plot Variable Importance
  p <- vip(xgb_fit, geom = "col", aesthetics = list(fill = "darkgreen")) +
    theme_minimal() +
    labs(
      title = "XGBoost Feature Importance",
      subtitle = "XGBoost correctly ignores noise (age, emails) but relies heavily on the Collider (support_ticket).",
      x = "Features",
      y = "Importance"
    )
  
  return(p)
}