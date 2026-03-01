# Load required libraries
library(tidymodels)
library(dplyr)
library(purrr)
library(vip)
library(ggplot2)

#' Train Multiple Causal Predictive Models
#' 
#' Trains GLM, Random Forest, and XGBoost models strictly adhering to the DAG.
#' By dropping the collider (opened_support_ticket), the models recover the 
#' true causal effect of delivery exceptions.
#' 
#' @param data Simulated dataset from simulate_delivery_data()
#' @return A fitted workflow set
fit_causal_models <- function(data) {
  
  # 1. Data Splitting (Identical to Naive)
  set.seed(123)
  data_split <- initial_split(data, prop = 0.8, strata = churned)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # 2. Recipe: The Causal Intervention
  causal_recipe <- recipe(churned ~ ., data = train_data) |>
    update_role(customer_id, new_role = "ID") |>
    # --- THE DAG INTERVENTION ---
    # We explicitly remove the collider
    step_rm(opened_support_ticket) |> 
    # ----------------------------
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
  causal_workflows <- workflow_set(
    preproc = list(causal_rec = causal_recipe),
    models = list(glm = log_spec, rf = rf_spec, xgb = xgb_spec)
  )
  
  # 5. Fit all models to the training data
  fitted_workflows <- causal_workflows |>
    mutate(fit = map(info, ~ fit(.x$workflow[[1]], data = train_data)))
  
  return(fitted_workflows)
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
  
  p <- tidy(glm_fit) |>
    filter(term != "(Intercept)") |>
    mutate(
      term = reorder(term, estimate),
      is_positive = estimate > 0
    ) |>
    ggplot(aes(x = estimate, y = term, fill = is_positive)) +
    geom_col() +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "steelblue")) +
    theme_minimal() +
    labs(
      title = "Causal GLM Coefficients (DAG Guided)",
      subtitle = "By dropping the collider, the model correctly identifies that exceptions INCREASE churn.",
      x = "Log-Odds Estimate",
      y = NULL
    ) +
    theme(legend.position = "none")
  
  return(p)
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
  
  p <- vip(xgb_fit, geom = "col", aesthetics = list(fill = "darkgreen")) +
    theme_minimal() +
    labs(
      title = "Causal XGBoost Feature Importance",
      subtitle = "With the trap removed, XGBoost correctly elevates 'has_exception' and precision covariates.",
      x = "Features",
      y = "Importance"
    )
  
  return(p)
}