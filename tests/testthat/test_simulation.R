library(testthat)
library(dplyr)
library(tidyr) # Added for potential future use, though filter/pull is sufficient here

source("R/02_simulate_data.R")

test_that("The DGP correctly generates Simpson's Paradox", {
  
  # Generate a large dataset with a fixed seed for statistical stability
  df <- simulate_delivery_data(n_customers = 20000, seed = 2026)
  
  df <- df |>
    mutate(churn_numeric = as.numeric(as.character(churned)))
  
  # ----------------------------------------------------------------------------
  # Calculate Aggregate Rates (The Observational Illusion)
  # ----------------------------------------------------------------------------
  agg_rates <- df |>
    group_by(has_exception) |>
    summarise(churn_rate = mean(churn_numeric), .groups = "drop")
  
  rate_no_exc_agg <- agg_rates |> 
    filter(has_exception == 0) |> 
    pull(churn_rate)
    
  rate_yes_exc_agg <- agg_rates |> 
    filter(has_exception == 1) |> 
    pull(churn_rate)
  
  # ----------------------------------------------------------------------------
  # Calculate Stratified Rates (The Causal Reality)
  # ----------------------------------------------------------------------------
  stratified_rates <- df |>
    group_by(user_segment, has_exception) |>
    summarise(churn_rate = mean(churn_numeric), .groups = "drop")
  
  rate_no_exc_power <- stratified_rates |> 
    filter(user_segment == "Power User", has_exception == 0) |> 
    pull(churn_rate)
    
  rate_yes_exc_power <- stratified_rates |> 
    filter(user_segment == "Power User", has_exception == 1) |> 
    pull(churn_rate)
  
  rate_no_exc_casual <- stratified_rates |> 
    filter(user_segment == "Casual User", has_exception == 0) |> 
    pull(churn_rate)
    
  rate_yes_exc_casual <- stratified_rates |> 
    filter(user_segment == "Casual User", has_exception == 1) |> 
    pull(churn_rate)
  
  # ----------------------------------------------------------------------------
  # ASSERTIONS
  # ----------------------------------------------------------------------------
  
  # Test 1: At the aggregate level, exceptions appear to REDUCE churn (Spurious Correlation)
  expect_lt(
    rate_yes_exc_agg, 
    rate_no_exc_agg, 
    label = "Aggregate churn rate with exception must be lower than without exception (The Illusion)"
  )
  
  # Test 2: For Power Users, exceptions INCREASE churn (True Causal Effect)
  expect_gt(
    rate_yes_exc_power, 
    rate_no_exc_power,
    label = "Exception must increase churn for Power Users (True Causal Effect)"
  )
  
  # Test 3: For Casual Users, exceptions INCREASE churn (True Causal Effect)
  expect_gt(
    rate_yes_exc_casual, 
    rate_no_exc_casual,
    label = "Exception must increase churn for Casual Users (True Causal Effect)"
  )
  
  # Test 4: Sanity check (Power Users have a much lower baseline churn than Casual Users)
  expect_lt(
    rate_no_exc_power,
    rate_no_exc_casual,
    label = "Power users must have lower baseline churn due to lock-in"
  )
})