library(testthat)
library(dplyr)
library(broom)

source("R/02_simulate_data.R")

test_that("The DGP correctly generates Collider Bias (The Support Ticket Trap)", {
  
  # 1. Generate a large dataset to ensure stable coefficients
  df <- simulate_delivery_data(n_customers = 50000, seed = 2026)
  
  # Convert churned to numeric (0 and 1) for the logistic regression models
  df <- df |>
    mutate(churn_numeric = as.numeric(as.character(churned)))
  
  # ----------------------------------------------------------------------------
  # Model 1: The Causal Truth (Dropping the Collider)
  # ----------------------------------------------------------------------------
  # We regress churn on the exception, intentionally ignoring the support ticket.
  causal_model <- glm(churn_numeric ~ has_exception, data = df, family = binomial())
  
  # Extract the coefficient for has_exception
  true_effect <- tidy(causal_model) |>
    filter(term == "has_exception") |>
    pull(estimate)
  
  # ----------------------------------------------------------------------------
  # Model 2: The ML Trap (Conditioning on the Collider)
  # ----------------------------------------------------------------------------
  # We include the 'opened_support_ticket' variable, which opens the backdoor path.
  ml_model <- glm(churn_numeric ~ has_exception + opened_support_ticket, data = df, family = binomial())
  
  # Extract the coefficient for has_exception
  biased_effect <- tidy(ml_model) |>
    filter(term == "has_exception") |>
    pull(estimate)
  
  # ----------------------------------------------------------------------------
  # ASSERTIONS
  # ----------------------------------------------------------------------------
  
  # Test 1: The true causal effect must be positive (Exceptions CAUSE churn)
  expect_gt(
    true_effect, 
    0, 
    label = "When omitting the collider, the effect of exceptions on churn must be correctly positive."
  )
  
  # Test 2: The biased effect must be negative (The Spurious Correlation)
  expect_lt(
    biased_effect, 
    0, 
    label = "When conditioning on the collider, the effect of exceptions must incorrectly appear negative."
  )
  
  # Test 3: The Support Ticket must be highly predictive of churn overall (Why ML loves it)
  ticket_effect <- tidy(ml_model) |>
    filter(term == "opened_support_ticket") |>
    pull(estimate)
  
  expect_gt(
    ticket_effect,
    1.0,
    label = "Support tickets must strongly predict churn, explaining why ML algorithms select it as a top feature."
  )
})