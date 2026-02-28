library(dplyr)
library(tibble)

#' Inverse Logit Function
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

#' Simulate Delivery Customer Data (Enterprise Collider Bias)
#' 
#' Generates a rich synthetic dataset mimicking an enterprise database.
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
    
    # --- UNOBSERVED TRAIT ---
    impatience_score = rnorm(n_customers, mean = 0, sd = 1),
    
    # --- THE TREATMENT ---
    has_exception = rbinom(n_customers, size = 1, prob = 0.15),
    
    # --- PRECISION COVARIATES ---
    account_tenure_months = round(runif(n_customers, min = 1, max = 60)),
    monthly_spend_usd     = round(rlnorm(n_customers, meanlog = 4, sdlog = 0.5), 2),
    
    # --- PURE NOISE VARIABLES ---
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
      # Both variables strongly drive opening a ticket
      log_odds_ticket = -2.5 + (3.0 * has_exception) + (3.5 * impatience_score),
      prob_ticket = inv_logit(log_odds_ticket),
      opened_support_ticket = rbinom(n(), size = 1, prob = prob_ticket)
    )
  
  # ----------------------------------------------------------------------------
  # 3. Generate the Outcome (Churn)
  # ----------------------------------------------------------------------------
  data <- data |>
    mutate(
      # The TRUE direct effect of exception is moderate
      # The effect of impatience is massive
      log_odds_churn = -2.0 + 
                       (0.4 * has_exception) + 
                       (4.0 * impatience_score) + 
                       (-0.05 * account_tenure_months) + 
                       (-0.005 * monthly_spend_usd),
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