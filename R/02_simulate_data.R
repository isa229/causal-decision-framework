library(dplyr)
library(tibble)

#' Inverse Logit Function
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

#' Simulate Delivery Customer Data (Simpson's Paradox + Collider Trap)
#'
#' Generates a synthetic dataset mimicking an enterprise customer database.
#'
#' The Data Generating Process (DGP) embeds TWO classic causal traps:
#'
#'   1. SIMPSON'S PARADOX (confounding via Order_Volume):
#'      High-engagement / high-volume customers place many orders, so by sheer
#'      exposure they encounter MORE delivery exceptions. Yet these loyal,
#'      high-volume customers churn far LESS. Order_Volume therefore confounds
#'      the Exception -> Churn relationship: in the AGGREGATE, exceptions look
#'      like they REDUCE churn (wrong sign), but WITHIN each volume stratum the
#'      true positive effect re-appears. The fix is to ADD (adjust for) volume.
#'
#'   2. COLLIDER BIAS (Support_Ticket):
#'      Both delivery exceptions AND (latent) customer impatience drive opening a
#'      support ticket. Support_Ticket is therefore a COLLIDER. Conditioning on
#'      it opens a spurious path and biases the estimate. The fix is to REMOVE
#'      (never adjust for) the ticket.
#'
#' Note on `impatience`: it is a LATENT trait. It is used to generate the data
#' but is deliberately NOT returned in the final dataset. It plays two roles:
#'   - a parent of the collider (Support_Ticket), and
#'   - an unmeasured cause of Churn (the basis for the E-value example).
#'
#' The TRUE structural effect of `has_exception` on churn is +0.5 on the
#' log-odds scale. Because logistic coefficients are non-collapsible, the most
#' honest headline estimand is the MARGINAL Average Treatment Effect (ATE) on
#' the probability (risk-difference) scale, recovered via g-computation.
#'
#' @param n_customers Integer. Number of customers to simulate.
#' @param seed Integer. Random seed for reproducibility.
#' @return A tibble of observed variables ready for modeling (impatience hidden).
simulate_delivery_data <- function(n_customers = 50000, seed = 2026) {

  set.seed(seed)

  # ----------------------------------------------------------------------------
  # 1. Exogenous variables (the root causes)
  # ----------------------------------------------------------------------------
  # CONFOUNDER (observed): standardized customer engagement / order-frequency
  # score. Positive = high-volume, highly-engaged, loyal customer.
  order_volume <- rnorm(n_customers, mean = 0, sd = 1)

  # LATENT TRAIT (unobserved): customer impatience. NOT returned in the data.
  impatience <- rnorm(n_customers, mean = 0, sd = 1)

  # Spend (observed precision covariate). We use a standardized version in the DGP
  monthly_spend_usd <- round(rlnorm(n_customers, meanlog = 4, sdlog = 0.5), 2)
  spend_z <- as.numeric(scale(monthly_spend_usd))

  # ----------------------------------------------------------------------------
  # 2. Treatment: delivery exceptions (driven by order volume -> CONFOUNDING)
  # ----------------------------------------------------------------------------
  # More orders => more chances for something to go wrong => more exceptions.
  log_odds_exception <- -1.0 + (1.5 * order_volume)
  has_exception <- rbinom(n_customers, size = 1, prob = inv_logit(log_odds_exception))

  # ----------------------------------------------------------------------------
  # 3. Collider: support ticket (driven by BOTH exception AND latent impatience)
  # ----------------------------------------------------------------------------
  log_odds_ticket <- -3.0 + (2.5 * has_exception) + (2.5 * impatience)
  opened_support_ticket <- rbinom(n_customers, size = 1, prob = inv_logit(log_odds_ticket))

  # ----------------------------------------------------------------------------
  # 4. Outcome: churn
  # ----------------------------------------------------------------------------
  #   +0.5  * has_exception   <- TRUE causal effect (log-odds) we will recover
  #   -2.0  * order_volume    <- loyal/high-volume customers churn much less
  #   +1.0  * impatience      <- LATENT cause of churn (to be recovered by the E-value)
  #   -0.3  * spend_z         <- precision covariate
  log_odds_churn <- -1.0 +
    (0.5 * has_exception) +
    (-2.0 * order_volume) +
    (1.0 * impatience) +
    (-0.3 * spend_z)
  churned <- rbinom(n_customers, size = 1, prob = inv_logit(log_odds_churn))

  # ----------------------------------------------------------------------------
  # 5. Pure noise variables (should be filtered out by good modeling)
  # ----------------------------------------------------------------------------
  customer_age_years       <- pmax(round(rnorm(n_customers, mean = 40, sd = 12)), 18)
  marketing_emails_clicked <- rpois(n_customers, lambda = 2)
  app_logins_last_7_days   <- rpois(n_customers, lambda = 3)

  # ----------------------------------------------------------------------------
  # 6. Final format for simulated data (without impatience)
  # ----------------------------------------------------------------------------
  tibble(
    customer_id = seq_len(n_customers),
    order_volume = order_volume,
    monthly_spend_usd = monthly_spend_usd,
    customer_age_years = customer_age_years,
    marketing_emails_clicked = marketing_emails_clicked,
    app_logins_last_7_days = app_logins_last_7_days,
    has_exception = as.integer(has_exception),
    opened_support_ticket = as.integer(opened_support_ticket),
    churned = as.factor(churned)
  )
}
