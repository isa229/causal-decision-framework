library(tidymodels)
library(dplyr)
library(vip) 
library(ggplot2)

#' Train Naive Predictive Model (The Causal Trap)
#' 
#' Trains a standard logistic regression model to predict churn, ignoring 
#' the causal structure (specifically the confounder: shipping volume).
#' This demonstrates how standard ML provides misleading business strategy.
#' 
#' @param data A data frame containing the simulated delivery data.
#' @return A fitted workflow and evaluation metrics.
fit_naive_churn_model <- function(data) {
  
  # 1. Data Splitting (Tidymodels standard)
  set.seed(123)
  data_split <- initial_split(data, prop = 0.8, strata = churned)
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  # 2. Recipe: Define the modeling steps
  # We purposefully omit 'shipping_volume' and 'user_segment' (the confounders)
  naive_recipe <- recipe(churned ~ has_exception + account_tenure_months + distance_from_hub_km, 
                         data = train_data) %>%
    # Convert logical/factor to dummy variables if needed (has_exception is integer 0/1 here)
    step_normalize(all_numeric_predictors()) 
  
  # 3. Model Specification
  # We use logistic regression to get clear, interpretable coefficients
  logistic_spec <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")
  
  # 4. Workflow: Bundle recipe and model
  naive_workflow <- workflow() %>%
    add_recipe(naive_recipe) %>%
    add_model(logistic_spec)
  
  # 5. Fit the model to the training data
  naive_fit <- naive_workflow %>%
    fit(data = train_data)
  
  # 6. Evaluate on Test Data (To show the model looks "good" predictively)
  test_predictions <- predict(naive_fit, new_data = test_data, type = "prob") %>%
    bind_cols(predict(naive_fit, new_data = test_data)) %>%
    bind_cols(test_data %>% select(churned))
  
  metrics_result <- test_predictions %>%
    metrics(truth = churned, estimate = .pred_class, .pred_1)
  
  # Return the fitted object and metrics
  list(
    fit = naive_fit,
    metrics = metrics_result,
    test_preds = test_predictions
  )
}

#' Extract and Plot Misleading Business Insights
#' 
#' Extracts the coefficients from the naive model to show how it 
#' recommends the wrong business action.
#' 
#' @param fitted_model The fitted tidymodels workflow.
plot_naive_insights <- function(fitted_model) {
  
  # Extract the underlying parsnip model
  model_obj <- extract_fit_parsnip(fitted_model)
  
  # Create a Variable Importance Plot based on coefficients
  p <- vip(model_obj, geom = "col", aesthetics = list(fill = "steelblue")) +
    theme_minimal() +
    labs(
      title = "Naive ML Feature Importance (Predictive)",
      subtitle = "Notice how 'has_exception' has a NEGATIVE coefficient (reduces churn risk!).\nThe model tells the business: 'Exceptions are good for retention.'",
      x = "Features",
      y = "Importance (Coefficient Magnitude & Direction)"
    )
  
  return(p)
}