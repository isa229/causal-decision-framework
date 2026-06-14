library(dplyr)
library(tibble)

#' Inverse Logit Function
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

#' Simulate Delivery Customer Data (Enterprise Collider Bias)
#' 
#' Generates a synthetic dataset mimicking an enterprise database.
#' Includes the "Bad Control" (opened_support_ticket) collider trap.
#' 
#' @param n_customers Integer. Number of customers to simulate. Default is 10000.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#' @return A tibble with observed variables ready for modeling.
simulate_delivery_data <- function(n_customers = 10000, seed = 42) {
  
  set.seed(seed)
  
  # ----------------------------------------------------------------------------
  # 1. Generate The Core Causal Physics & Exogenous Variables
  # ----------------------------------------------------------------------------
  data <- tibble(
    customer_id = 1:n_customers,
    
    # --- MEASURED BEHAVIORAL TRAIT (Survey Score) ---
    # This represents customer impatience measured via survey
    # Scale: -3 (very patient) to +3 (very impatient)
    impatience_score = rnorm(n_customers, mean = 0, sd = 1),
    
    # --- THE TREATMENT (Randomly Assigned by Logistics) ---
    # Delivery exceptions occur independently of customer traits
    has_exception = rbinom(n_customers, size = 1, prob = 0.15),
    
    # --- PRECISION COVARIATES ---
    account_tenure_months = round(runif(n_customers, min = 1, max = 60)),
    monthly_spend_usd     = round(rlnorm(n_customers, meanlog = 4, sdlog = 0.5), 2),
    
    # --- PURE NOISE VARIABLES (should be filtered by models) ---
    customer_age_years       = round(rnorm(n_customers, mean = 40, sd = 12)),
    marketing_emails_clicked = rpois(n_customers, lambda = 2),
    app_logins_last_7_days   = rpois(n_customers, lambda = 3)
  )
  
  data <- data |> mutate(customer_age_years = pmax(customer_age_years, 18))
  
  # ----------------------------------------------------------------------------
  # 2. Generate the Collider (Support Ticket)
  # ----------------------------------------------------------------------------
  data <- data |>
    mutate(
      # Both variables very strongly drive opening a ticket
      # Strong effects create dramatic collider bias
      log_odds_ticket = -3.0 + (4.0 * has_exception) + (4.5 * impatience_score),
      prob_ticket = inv_logit(log_odds_ticket),
      opened_support_ticket = rbinom(n(), size = 1, prob = prob_ticket)
    )
  
  # ----------------------------------------------------------------------------
  # 3. Generate the Outcome (Churn)
  # ----------------------------------------------------------------------------
  data <- data |>
    mutate(
      # The true causal effect of exception is 0.5 log-odds
      # (This is the parameter we will try to recover with causal methods)
      # Impatience has a very strong effect, drives both tickets and churn
      # Tenure and Spend provide precision adjustments
      log_odds_churn = -1.5 + 
                       (0.5 * has_exception) +           # True causal effect
                       (4.5 * impatience_score) +        # Strong confounder
                       (-0.03 * account_tenure_months) + 
                       (-0.003 * monthly_spend_usd),
      prob_churn = inv_logit(log_odds_churn),
      churned = rbinom(n(), size = 1, prob = prob_churn)
    )
  
  # ----------------------------------------------------------------------------
  # 4. Final Formatting
  # ----------------------------------------------------------------------------
  final_data <- data |>
    select(
      customer_id,
      account_tenure_months,
      monthly_spend_usd,
      customer_age_years,
      marketing_emails_clicked,
      app_logins_last_7_days,
      impatience_score,
      has_exception,
      opened_support_ticket,
      churned
    ) |>
    mutate(
      has_exception = as.integer(has_exception),
      opened_support_ticket = as.integer(opened_support_ticket),
      churned = as.factor(churned)
    )
  
  return(final_data)
}