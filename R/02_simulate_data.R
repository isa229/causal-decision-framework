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
#'   1. COLLIDER BIAS / BERKSON'S PARADOX (Support_Ticket) -- THE HEADLINE:
#'      Both delivery exceptions AND (latent) customer impatience drive opening
#'      a support ticket. Support_Ticket is therefore a COLLIDER. Conditioning
#'      on it opens the spurious path
#'        exception -> ticket <- impatience -> churn,
#'      which alone flips the estimated effect of exceptions on churn to the
#'      WRONG (negative) sign -- even in a kitchen-sink model that adjusts for
#'      every confounder. The fix is to REMOVE (never adjust for) the ticket.
#'
#'   2. SIMPSON'S PARADOX (confounding via Order_Volume) -- present in the
#'      DATA as a diagnostic: high-volume customers encounter MORE exceptions
#'      yet churn LESS, so the AGGREGATE comparison reverses even though every
#'      volume stratum shows the true positive effect. The fix is to ADD
#'      (adjust for) volume.
#'
#' CALIBRATION NOTE (design of the in-silico lab): the collider equation below
#' is CALIBRATED (strength 4.5 on both parents, latent impatience effect 1.5
#' on churn, ticket rate held at a realistic ~25%) so that collider bias
#' EXCEEDS the true +0.5 log-odds effect. The naive kitchen-sink model
#' therefore has genuinely good metrics (test AUC ~ 0.84) AND the wrong
#' causal sign -- the headline of the talk. The full grid search, contract
#' checks and drift guard live in R/08_dgp_calibration.R; re-run it whenever
#' these constants change.
#'
#' Note on `impatience`: it is a LATENT trait. It is used to generate the data
#' but is deliberately NOT returned in the final dataset. It plays two roles:
#'   - a parent of the collider (Support_Ticket), and
#'   - an unmeasured cause of Churn (the basis for the E-value backup slide).
#'   The ticket is, in effect, an observed PROXY for this hidden driver --
#'   which is exactly why it buys AUC while breaking the causal answer.
#'
#' The TRUE structural effect of `has_exception` on churn is +0.5 on the
#' log-odds scale. Because logistic coefficients are non-collapsible, the most
#' honest headline estimand is the MARGINAL Average Treatment Effect (ATE) on
#' the probability (risk-difference) scale, recovered via g-computation.
#'
#' @param n_customers Integer. Number of customers to simulate. Default 50000.
#' @param seed Integer. Random seed for reproducibility. Default 2026.
#' @return A tibble of OBSERVED variables ready for modeling (impatience hidden).
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

  # Spend (observed precision covariate). Standardized version drives the DGP.
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
  # CALIBRATED design (see R/08_dgp_calibration.R): strength 4.5 on both
  # parents makes the ticket a strong proxy for the latent impatience, so the
  # collider bias it induces exceeds the true +0.5 effect. The intercept
  # keeps the observed ticket rate at a realistic ~25%.
  log_odds_ticket <- -5.1 + (4.5 * has_exception) + (4.5 * impatience)
  opened_support_ticket <- rbinom(n_customers, size = 1, prob = inv_logit(log_odds_ticket))

  # ----------------------------------------------------------------------------
  # 4. Outcome: churn
  # ----------------------------------------------------------------------------
  #   +0.5  * has_exception   <- TRUE causal effect (log-odds) we will recover
  #   -2.0  * order_volume    <- loyal/high-volume customers churn much less
  #   +1.5  * impatience      <- LATENT cause of churn (powers the E-value
  #                              story and the collider's AUC advantage)
  #   -0.3  * spend_z         <- precision covariate
  log_odds_churn <- -1.0 +
    (0.5 * has_exception) +
    (-2.0 * order_volume) +
    (1.5 * impatience) +
    (-0.3 * spend_z)
  churned <- rbinom(n_customers, size = 1, prob = inv_logit(log_odds_churn))

  # ----------------------------------------------------------------------------
  # 5. Pure noise variables (should be filtered out by good modeling)
  # ----------------------------------------------------------------------------
  customer_age_years       <- pmax(round(rnorm(n_customers, mean = 40, sd = 12)), 18)
  marketing_emails_clicked <- rpois(n_customers, lambda = 2)
  app_logins_last_7_days   <- rpois(n_customers, lambda = 3)

  # ----------------------------------------------------------------------------
  # 6. Final formatting (NOTE: `impatience` is intentionally NOT included)
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
