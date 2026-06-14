library(dplyr)
library(broom)
library(tibble)

#' Get Ground Truth Causal Effect
#'
#' Returns the true causal effect baked into the data generating process.
#' This is the effect we embedded in the simulation (0.5 log-odds).
#' 
#' @return A list containing the true effect on different scales
get_ground_truth_ate <- function() {
  
  # From the structural equation in simulate_delivery_data():
  # log_odds_churn = -2.0 + (0.5 * has_exception) + ...
  # The coefficient 0.5 is the true causal effect (log-odds scale)
  
  true_log_odds_effect <- 0.5
  
  # Convert to odds ratio
  true_odds_ratio <- exp(true_log_odds_effect)
  
  # For interpretation: approximate risk difference (marginal effect)
  # This is context-dependent but useful for business communication
  # Calculated by simulating counterfactual outcomes
  
  list(
    log_odds = true_log_odds_effect,
    odds_ratio = true_odds_ratio,
    interpretation = paste0(
      "Delivery exceptions increase the odds of churn by ",
      round((true_odds_ratio - 1) * 100, 1),
      "% (OR = ", round(true_odds_ratio, 2), ")"
    )
  )
}

#' Extract Coefficient from Fitted Model
#'
#' Helper function to extract the has_exception coefficient from a fitted workflow
#' 
#' @param fitted_workflow A fitted tidymodels workflow
#' @param term_name The name of the coefficient to extract
#' @return A tibble with estimate and std.error
extract_exception_coefficient <- function(fitted_workflow, term_name = "has_exception") {
  
  fitted_workflow |>
    extract_fit_parsnip() |>
    tidy() |>
    filter(term == term_name) |>
    select(term, estimate, std.error)
}

#' Compare All Estimates to Ground Truth
#'
#' Creates a comprehensive comparison table showing:
#' - Ground truth (from DGP)
#' - Naive estimate (conditioning on collider)
#' - Causal estimate (DAG-guided)
#' 
#' @param naive_workflows Fitted naive models from fit_naive_models()
#' @param causal_workflows Fitted causal models from fit_causal_models()
#' @return A tibble with comparison metrics
compare_to_ground_truth <- function(naive_workflows, causal_workflows) {
  
  # Get ground truth
  truth <- get_ground_truth_ate()
  
  # Extract naive GLM coefficient (with collider)
  naive_coef <- naive_workflows |>
    filter(wflow_id == "base_rec_glm") |>
    pull(fit) |>
    _[[1]] |>
    extract_exception_coefficient()
  
  # Extract causal GLM coefficient (without collider)
  causal_coef <- causal_workflows |>
    filter(wflow_id == "causal_rec_glm") |>
    pull(fit) |>
    _[[1]] |>
    extract_exception_coefficient()
  
  # Build comparison table
  comparison <- tibble(
    Method = c("Ground Truth (DGP)", "Naive Model (w/ Collider)", "Causal Model (DAG-Guided)"),
    Estimate = c(truth$log_odds, naive_coef$estimate, causal_coef$estimate),
    Std_Error = c(NA_real_, naive_coef$std.error, causal_coef$std.error),
    Bias = c(0, naive_coef$estimate - truth$log_odds, causal_coef$estimate - truth$log_odds),
    Pct_Bias = c(0, 
                 (naive_coef$estimate - truth$log_odds) / truth$log_odds * 100,
                 (causal_coef$estimate - truth$log_odds) / truth$log_odds * 100),
    Direction = c("Positive (Increases Churn)", 
                  ifelse(naive_coef$estimate > 0, "Positive (Increases Churn)", "NEGATIVE (Decreases Churn) WARNING"),
                  ifelse(causal_coef$estimate > 0, "Positive (Increases Churn)", "Negative (Decreases Churn)"))
  )
  
  return(comparison)
}

#' Compute Marginal Treatment Effect via G-Computation
#'
#' Calculates the Average Treatment Effect by simulating counterfactual outcomes.
#' This gives us the effect on the probability scale (risk difference).
#' 
#' @param data The simulated dataset
#' @param fitted_workflow A fitted tidymodels workflow (should be causal model)
#' @return A list with ATE estimate and interpretation
compute_marginal_ate <- function(data, fitted_workflow) {
  
  # Convert churned to numeric for prediction
  data_numeric <- data |>
    mutate(churn_numeric = as.numeric(as.character(churned)))
  
  # Create counterfactual datasets
  data_treated <- data_numeric |> mutate(has_exception = 1)
  data_control <- data_numeric |> mutate(has_exception = 0)
  
  # Predict under both scenarios
  pred_treated <- predict(fitted_workflow, new_data = data_treated, type = "prob") |>
    pull(.pred_1)
  
  pred_control <- predict(fitted_workflow, new_data = data_control, type = "prob") |>
    pull(.pred_1)
  
  # Calculate Average Treatment Effect (Risk Difference)
  ate_risk_diff <- mean(pred_treated - pred_control)
  
  list(
    risk_difference = ate_risk_diff,
    interpretation = paste0(
      "Delivery exceptions increase churn probability by ",
      round(ate_risk_diff * 100, 2),
      " percentage points"
    )
  )
}

#' Bootstrap Confidence Intervals for ATE
#'
#' Computes bootstrap confidence intervals for the causal effect estimate.
#' 
#' @param data The simulated dataset
#' @param n_bootstrap Number of bootstrap samples (default: 1000)
#' @param seed Random seed for reproducibility
#' @return A tibble with estimate and confidence intervals
bootstrap_ate_ci <- function(data, n_bootstrap = 1000, seed = 2026) {
  
  set.seed(seed)
  
  # Convert churned to numeric
  data_numeric <- data |>
    mutate(churn_numeric = as.numeric(as.character(churned)))
  
  bootstrap_estimates <- replicate(n_bootstrap, {
    # Resample with replacement
    boot_sample <- data_numeric |>
      slice_sample(n = nrow(data_numeric), replace = TRUE)
    
    # Fit causal model (without collider)
    boot_model <- glm(
      churn_numeric ~ has_exception + impatience_score + 
        account_tenure_months + monthly_spend_usd,
      data = boot_sample,
      family = binomial()
    )
    
    # Extract coefficient
    coef(boot_model)["has_exception"]
  })
  
  # Calculate percentile confidence intervals
  tibble(
    Estimate = mean(bootstrap_estimates),
    CI_Lower = quantile(bootstrap_estimates, 0.025),
    CI_Upper = quantile(bootstrap_estimates, 0.975),
    Std_Error = sd(bootstrap_estimates)
  )
}
