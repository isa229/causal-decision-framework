# Load required libraries
library(dplyr)
library(tibble)

#' Inverse Logit Function
#' 
#' Helper function to convert log-odds to probabilities.
#' 
#' @param x Numeric vector of log-odds.
#' @return Numeric vector of probabilities between 0 and 1.
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

#' Simulate Delivery Customer Data (Data Generating Process)
#' 
#' Generates a synthetic dataset of delivery customers exhibiting Simpson's Paradox. 
#' The true causal effect of a delivery exception is positive (increases churn), 
#' but the observational correlation is negative due to the confounding effect 
#' of shipping volume (driven by user segment).
#' 
#' @param n_customers Integer. Number of customers to simulate. Default is 10000.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#' @return A tibble with simulated customer data ready for ML and Causal inference.
simulate_delivery_data <- function(n_customers = 10000, seed = 42) {
  
  # Ensure reproducibility
  set.seed(seed)
  
  # ----------------------------------------------------------------------------
  # 1. Structural Parameters & Causal Coefficients
  # ----------------------------------------------------------------------------
  p_power       <- 0.20   # 20% of customer base are Power Users
  lambda_casual <- 1      # Poisson lambda for casual user volume
  lambda_power  <- 50     # Poisson lambda for power user volume
  
  # Churn Log-Odds Coefficients
  beta_0      <-  0.0     # Baseline intercept
  beta_1      <- -3.5     # The "Lock-in" Effect (Power users rarely churn)
  beta_2      <-  1.8     # The TRUE Causal Effect (Exceptions cause churn)
  beta_tenure <- -0.05    # Precision Covariate (Longer tenure = lower churn)
  
  # ----------------------------------------------------------------------------
  # 2. Generate Exogenous Variables (Independent)
  # ----------------------------------------------------------------------------
  data <- tibble(
    customer_id = 1:n_customers,
    
    # U: User Segment (The Unobserved Confounder Root)
    is_power_user = rbinom(n_customers, size = 1, prob = p_power),
    
    # T: Account Tenure in months (Precision Covariate)
    account_tenure_months = round(runif(n_customers, min = 1, max = 60)),
    
    # D: Distance from Hub in km (Instrumental Variable)
    distance_from_hub_km = round(rlnorm(n_customers, meanlog = 2.0, sdlog = 0.5), 1)
  )
  
  # ----------------------------------------------------------------------------
  # 3. Generate Endogenous Variables (Mechanistic)
  # ----------------------------------------------------------------------------
  data <- data |> 
    mutate(
      # V: Shipping Volume (Driven entirely by U)
      shipping_volume = if_else(
        is_power_user == 1, 
        rpois(n(), lambda_power), 
        rpois(n(), lambda_casual)
      ),
      # Ensure minimum volume is 1 (they must be active customers)
      shipping_volume = pmax(shipping_volume, 1),
      
      # E: Delivery Exception (Treatment)
      # Base fail probability increases slightly with distance from hub
      base_fail_prob = 0.02 + (0.001 * distance_from_hub_km),
      # Probability of AT LEAST ONE exception scales with volume
      prob_exception = 1 - (1 - base_fail_prob)^shipping_volume,
      # Draw the actual event
      has_exception = rbinom(n(), size = 1, prob = prob_exception),
      
      # C: Customer Churn (The Outcome)
      # Log-odds defined by our structural equation
      log_odds_churn = beta_0 + 
                       (beta_1 * is_power_user) + 
                       (beta_2 * has_exception) + 
                       (beta_tenure * account_tenure_months),
      prob_churn = inv_logit(log_odds_churn),
      # Draw the actual event
      churned = rbinom(n(), size = 1, prob = prob_churn)
    )
  
  # ----------------------------------------------------------------------------
  # 4. Final Formatting
  # ----------------------------------------------------------------------------
  final_data <- data |> 
    select(
      customer_id,
      user_segment = is_power_user,
      account_tenure_months,
      distance_from_hub_km,
      shipping_volume,
      has_exception,
      churned
    ) |> 
    mutate(
      user_segment = if_else(user_segment == 1, "Power User", "Casual User"),
      has_exception = as.integer(has_exception), # 1/0 for easier modeling
      churned = as.factor(churned)               # Factor for classification models
    )
  
  return(final_data)
}