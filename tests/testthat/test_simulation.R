library(testthat)
library(dplyr)
library(broom)
library(here)

source(here::here("R", "02_simulate_data.R"))
source(here::here("R", "01_define_dag.R"))
source(here::here("R", "06_validate_ground_truth.R"))

inv_logit_test <- function(x) 1 / (1 + exp(-x))

# G-computation helper: marginal ATE (risk difference) from a fitted glm
gcomp_ate <- function(model, data) {
  d1 <- data |> mutate(has_exception = 1L)
  d0 <- data |> mutate(has_exception = 0L)
  mean(predict(model, d1, type = "response") - predict(model, d0, type = "response"))
}

# ==============================================================================
# DATA STRUCTURE & INTEGRITY
# ==============================================================================
test_that("Simulated data has the correct structure and hides the latent trait", {
  df <- simulate_delivery_data(n_customers = 5000, seed = 2026)

  # Latent impatience must NOT leak into the observed dataset
  expect_false("impatience" %in% names(df),
               label = "impatience is latent and must not be returned")

  # Required observed columns are present
  required <- c("customer_id", "order_volume", "monthly_spend_usd",
                "has_exception", "opened_support_ticket", "churned")
  expect_true(all(required %in% names(df)))

  # Types
  expect_s3_class(df$churned, "factor")
  expect_true(all(df$has_exception %in% c(0L, 1L)))
  expect_true(all(df$opened_support_ticket %in% c(0L, 1L)))
  expect_equal(nrow(df), 5000)
})

# ==============================================================================
# SIMPSON'S PARADOX (confounding via order_volume)
# ==============================================================================
test_that("The DGP generates a genuine Simpson's Paradox via order_volume", {
  df <- simulate_delivery_data(n_customers = 80000, seed = 2026) |>
    mutate(churn_numeric = as.numeric(as.character(churned)))

  rate_diff <- function(d) {
    mean(d$churn_numeric[d$has_exception == 1]) -
      mean(d$churn_numeric[d$has_exception == 0])
  }

  # 1. Aggregate: exceptions look like they REDUCE churn (negative, wrong sign)
  expect_lt(rate_diff(df), 0,
            label = "Aggregate churn-rate difference must be NEGATIVE (Simpson's)")

  # 2. The confounding mechanism: exposed customers cluster at HIGH order volume
  #    (the low-churn region). This is WHY the aggregate reverses.
  expect_gt(
    mean(df$order_volume[df$has_exception == 1]),
    mean(df$order_volume[df$has_exception == 0]),
    label = "Exception customers must concentrate in high-volume (low-churn) region"
  )

  # 3. Within EACH order-volume decile, the exception churn rate is HIGHER
  #    (the sign reverses back to the truth in every fine-grained stratum).
  by_decile <- df |>
    mutate(dec = cut(order_volume,
                     breaks = quantile(order_volume, seq(0, 1, 0.1)),
                     include.lowest = TRUE, labels = FALSE)) |>
    group_by(dec) |>
    summarise(d = rate_diff(pick(everything())), .groups = "drop")
  expect_true(all(by_decile$d > 0),
              label = "Within each volume decile, exceptions must INCREASE churn (sign reversal)")

  # 4. The adjusted within-stratum effect is robustly POSITIVE in every tertile
  #    (the model-based statement of the same paradox).
  adj_effects <- df |>
    mutate(grp = cut(order_volume,
                     breaks = quantile(order_volume, c(0, 1/3, 2/3, 1)),
                     include.lowest = TRUE)) |>
    group_by(grp) |>
    group_map(~ coef(glm(churn_numeric ~ has_exception + order_volume,
                         data = .x, family = binomial()))["has_exception"]) |>
    unlist()
  expect_true(all(adj_effects > 0),
              label = "Adjusted within-stratum effect must be POSITIVE in every tertile")
})

# ==============================================================================
# CONFOUNDING: adjusting for order_volume recovers the correct SIGN
# ==============================================================================
test_that("Adjusting for the confounder flips the naive sign to correct (positive)", {
  df <- simulate_delivery_data(n_customers = 80000, seed = 2026) |>
    mutate(churn_numeric = as.numeric(as.character(churned)))

  # Naive aggregate (omits confounder): wrong (negative) sign
  naive <- glm(churn_numeric ~ has_exception + monthly_spend_usd,
               data = df, family = binomial())
  naive_coef <- tidy(naive) |> filter(term == "has_exception") |> pull(estimate)
  expect_lt(naive_coef, 0,
            label = "Naive model (no confounder) must show a NEGATIVE (wrong) coefficient")

  # Causal (adjusts for confounder, drops collider): correct positive sign
  causal <- glm(churn_numeric ~ has_exception + order_volume + monthly_spend_usd,
                data = df, family = binomial())
  causal_coef <- tidy(causal) |> filter(term == "has_exception") |> pull(estimate)
  expect_gt(causal_coef, 0,
            label = "Causal model must show the correct POSITIVE coefficient")
})

# ==============================================================================
# COLLIDER BIAS (support_ticket)
# ==============================================================================
test_that("Conditioning on the collider re-introduces bias toward the null", {
  df <- simulate_delivery_data(n_customers = 80000, seed = 2026) |>
    mutate(churn_numeric = as.numeric(as.character(churned)))

  causal <- glm(churn_numeric ~ has_exception + order_volume + monthly_spend_usd,
                data = df, family = binomial())
  collider <- glm(churn_numeric ~ has_exception + order_volume + monthly_spend_usd +
                    opened_support_ticket,
                  data = df, family = binomial())

  causal_coef   <- tidy(causal)   |> filter(term == "has_exception") |> pull(estimate)
  collider_coef <- tidy(collider) |> filter(term == "has_exception") |> pull(estimate)

  # Adding the collider biases the estimate toward (and through) the null
  expect_lt(collider_coef, causal_coef,
            label = "Adding the collider must bias the estimate downward vs the causal model")

  # The collider is itself strongly predictive (why ML keeps it)
  ticket_effect <- tidy(collider) |> filter(term == "opened_support_ticket") |> pull(estimate)
  expect_gt(ticket_effect, 1.0,
            label = "Support ticket must be strongly predictive of churn")
})

# ==============================================================================
# GROUND TRUTH: marginal ATE recovery (collapsible, honest estimand)
# ==============================================================================
test_that("Causal model recovers the TRUE marginal ATE; naive gets the wrong sign", {
  df <- simulate_delivery_data(n_customers = 80000, seed = 2026) |>
    mutate(churn_numeric = as.numeric(as.character(churned)))

  truth <- get_ground_truth_ate()
  expect_gt(truth$ate_pp, 0)  # true effect is positive

  causal <- glm(churn_numeric ~ has_exception + order_volume + monthly_spend_usd,
                data = df, family = binomial())
  naive  <- glm(churn_numeric ~ has_exception + opened_support_ticket + monthly_spend_usd,
                data = df, family = binomial())

  causal_ate <- gcomp_ate(causal, df) * 100
  naive_ate  <- gcomp_ate(naive, df) * 100

  # Causal ATE recovers truth within 2 percentage points
  expect_lt(abs(causal_ate - truth$ate_pp), 2.0,
            label = "Causal marginal ATE must be within 2pp of ground truth")

  # Naive ATE is negative (wrong sign)
  expect_lt(naive_ate, 0,
            label = "Naive marginal ATE must have the WRONG (negative) sign")

  # Causal must be closer to truth than naive
  expect_lt(abs(causal_ate - truth$ate_pp), abs(naive_ate - truth$ate_pp))
})

# ==============================================================================
# DAG: dagitty's minimal sufficient adjustment set
# ==============================================================================
test_that("dagitty identifies {Order_Volume} as the adjustment set (not the collider)", {
  dag <- define_causal_dag()$dag
  adj <- get_adjustment_strategy(dag)

  adj_vars <- unlist(adj)
  expect_true("Order_Volume" %in% adj_vars,
              label = "Order_Volume must be in the adjustment set")
  expect_false("Support_Ticket" %in% adj_vars,
               label = "Support_Ticket (collider) must NOT be in the adjustment set")
})

# ==============================================================================
# E-VALUE sensitivity analysis
# ==============================================================================
test_that("E-value is computed correctly and exceeds 1", {
  ev <- compute_evalue(1.543)
  expect_gt(ev$evalue, 1)
  # VanderWeele-Ding: OR=1.543 -> E-value ~ 2.46
  expect_lt(abs(ev$evalue - 2.46), 0.05)

  # Symmetric for protective ORs
  ev_protective <- compute_evalue(1 / 1.543)
  expect_equal(ev$evalue, ev_protective$evalue, tolerance = 1e-6)
})
