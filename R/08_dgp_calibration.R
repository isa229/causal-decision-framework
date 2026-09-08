#!/usr/bin/env Rscript
# ==============================================================================
# 08_dgp_calibration.R -- design document for the in-silico laboratory
# ==============================================================================
# WHY THIS SCRIPT EXISTS
#
#   The talk's headline is a CONTRACT between the narrative and the data
#   generating process (DGP) in R/02_simulate_data.R:
#
#     "The naive model -- every observed feature thrown in -- has genuinely
#      good metrics (test AUC ~ 0.84) yet estimates the effect of delivery
#      exceptions on churn with the WRONG (negative) sign, because it
#      conditions on the collider (opened_support_ticket). Dropping the
#      collider recovers the truth."
#
#   That is not an accident: the collider equation is CALIBRATED so that
#   collider bias exceeds the true +0.5 log-odds effect. This script is the
#   calibration, kept as code so the design is reproducible, auditable and
#   guarded against drift. Re-run it whenever R/02 changes.
#
# THE HEADLINE CONTRACT (all must hold simultaneously)
#
#   H1  The true effect is positive and business-meaningful (ATE >= +4 pp)
#   H2  The naive (kitchen-sink) model has GENUINELY GOOD metrics
#       (test AUC >= 0.80)
#   H3  The naive model gets the WRONG sign, decisively: has_exception
#       coefficient < 0, z <= -5, g-computation ATE < 0 pp
#   H4  The DAG-guided causal model recovers the truth (positive, within
#       2 pp of truth, test AUC >= 0.75)
#   H5  COLLIDER ISOLATION: the same model WITHOUT the ticket is
#       sign-correct. Because the naive model already adjusts the
#       confounder, this proves the sign flip is caused by the collider
#       ALONE (Berkson's paradox), not by residual confounding.
#   H6  Realism anchors: churn rate in [25%, 50%], ticket rate in
#       [10%, 35%], exception rate in [15%, 45%]
#
#   Diagnostics (reported, not gating): the aggregate Simpson's reversal
#   remains a property of the data (order_volume confounding) even though
#   the headline attributes the naive model's failure to the collider only.
#
# DESIGN KNOBS (everything else is fixed, mirroring R/02)
#
#   collider_strength  : both parents load the ticket equation equally
#                        (symmetry is a parsimony choice, not a necessity)
#   impatience_effect  : how strongly the LATENT trait drives churn (the
#                        hidden driver the collider proxies for)
#   ticket_intercept   : auto-calibrated per cell so the OBSERVED ticket
#                        rate stays at a realistic ~25%. The knob is the
#                        collider's STRENGTH, never its prevalence.
#
# SELECTION RULE: among passing cells pick the smallest collider_strength,
#   then the smallest impatience_effect -- the WEAKEST collider that still
#   breaks the headline, so the demo never relies on an absurd feature.
#
# OUTPUT
#   - Console: grid report, chosen design, production-scale verification,
#     and a MATCH/DRIFT check against R/02's actual constants.
#   - figures/00_dgp_calibration.png: the design trade-off made visible --
#     a stronger collider buys AUC and deepens the wrong sign at once.

library(here)

# ------------------------------------------------------------------------------
# 0. Fixed design constants (seeds mirror the pipeline conventions)
# ------------------------------------------------------------------------------
seed_data <- 2026    # data seed, as in R/02 and the Quarto document
seed_split <- 123    # split seed, as in R/03 / R/04
n_grid <- 20000      # grid-screening sample size per cell
n_verify <- 50000    # production-scale verification (pipeline size)
n_truth <- 300000    # ground-truth ATE sample (mirrors R/06)
target_ticket_rate <- 0.25

grid_strength <- seq(2.5, 6.5, by = 0.5)
grid_impatience <- c(1.0, 1.25, 1.5)

#' Simulate From the Parametric Mirror of the DGP
#'
#' Mirrors R/02_simulate_data.R (same structural equations and draw order)
#' but exposes the two collider-design knobs. Keeping the seed fixed across
#' grid cells gives common random numbers, so cells differ only by design.
#'
#' @param n_customers Number of customers to simulate.
#' @param seed Random seed.
#' @param collider_strength Coefficient of both parents in the ticket
#'   equation (symmetric by parsimony).
#' @param impatience_effect Coefficient of the latent trait in the churn
#'   equation.
#' @param ticket_intercept Fixed intercept, or NULL to auto-calibrate to
#'   `target_ticket_rate`.
#' @return A list: `observed` (analyst-visible tibble), `truth_ate_pp`
#'   (finite-sample true marginal ATE, percentage points) and
#'   `ticket_intercept` (the calibrated intercept).
#' @examples
#' lab <- simulate_core(
#'   n_customers = 1000, seed = 2026,
#'   collider_strength = 5.5, impatience_effect = 1.25
#' )
#' mean(lab$observed$opened_support_ticket)
simulate_core <- function(n_customers, seed, collider_strength,
                          impatience_effect, ticket_intercept = NULL) {
  set.seed(seed)

  order_volume <- stats::rnorm(n_customers)
  impatience <- stats::rnorm(n_customers)
  monthly_spend_usd <- round(stats::rlnorm(n_customers, 4, 0.5), 2)
  spend_scaled <- as.numeric(scale(monthly_spend_usd))
  has_exception <- stats::rbinom(
    n_customers, 1, stats::plogis(-1.0 + 1.5 * order_volume)
  )

  # The collider: the intercept is calibrated to keep prevalence realistic,
  # so only the STRENGTH varies across the grid.
  ticket_prob <- function(intercept) {
    stats::plogis(
      intercept + collider_strength * has_exception +
        collider_strength * impatience
    )
  }
  if (is.null(ticket_intercept)) {
    ticket_intercept <- stats::uniroot(
      function(intercept) {
        mean(ticket_prob(intercept)) - target_ticket_rate
      },
      lower = -15, upper = 5
    )$root
  }
  opened_support_ticket <- stats::rbinom(
    n_customers, 1, ticket_prob(ticket_intercept)
  )

  # The true effect of exceptions is +0.5 log-odds -- fixed, never a knob.
  churn_linear <- function(exception) {
    -1.0 + 0.5 * exception - 2.0 * order_volume +
      impatience_effect * impatience - 0.3 * spend_scaled
  }
  churned <- factor(
    stats::rbinom(n_customers, 1, stats::plogis(churn_linear(has_exception))),
    levels = c("0", "1")
  )

  observed <- tibble::tibble(
    customer_id = seq_len(n_customers),
    order_volume = order_volume,
    monthly_spend_usd = monthly_spend_usd,
    customer_age_years = pmax(round(stats::rnorm(n_customers, 40, 12)), 18),
    marketing_emails_clicked = stats::rpois(n_customers, 2),
    app_logins_last_7_days = stats::rpois(n_customers, 3),
    has_exception = as.integer(has_exception),
    opened_support_ticket = as.integer(opened_support_ticket),
    churned = churned
  )

  truth_ate_pp <- 100 * (
    mean(stats::plogis(churn_linear(1))) -
      mean(stats::plogis(churn_linear(0)))
  )

  list(
    observed = observed,
    truth_ate_pp = truth_ate_pp,
    ticket_intercept = ticket_intercept
  )
}

#' Make a Stratified Train/Test Split
#'
#' Dependency-light mirror of the pipeline's convention (seed 123, 80/20,
#' stratified on churn) so calibration numbers compare directly with the
#' pipeline's test-set metrics.
#'
#' @param customers Simulated tibble with a `churned` factor column.
#' @param prop Training fraction.
#' @param seed Split seed.
#' @return A list with `train` and `test` tibbles.
make_stratified_split <- function(customers, prop = 0.8, seed = seed_split) {
  set.seed(seed)
  churn_positive <- which(customers$churned == "1")
  churn_negative <- which(customers$churned == "0")
  train_idx <- c(
    sample(churn_positive, floor(prop * length(churn_positive))),
    sample(churn_negative, floor(prop * length(churn_negative)))
  )
  test_idx <- setdiff(seq_len(nrow(customers)), train_idx)
  list(
    train = customers[sort(train_idx), ],
    test = customers[test_idx, ]
  )
}

#' Compute AUC via the Mann-Whitney Formulation
#'
#' Equivalent to yardstick::roc_auc() with event_level = "second" but
#' without the tidymodels dependency, keeping the calibration script fast
#' and isolated from the modelling stack it is used to design.
#'
#' @param churn_truth Numeric 0/1 outcomes.
#' @param predicted_prob Predicted churn probabilities.
#' @return The AUC (a single number).
compute_auc <- function(churn_truth, predicted_prob) {
  n_pos <- sum(churn_truth == 1)
  n_neg <- sum(churn_truth == 0)
  rank_all <- rank(predicted_prob)
  (sum(rank_all[churn_truth == 1]) - n_pos * (n_pos + 1) / 2) /
    (n_pos * n_neg)
}

#' Compute the Marginal ATE by G-Computation
#'
#' @param model A fitted binomial stats::glm.
#' @param customers The dataset to standardise over.
#' @return The ATE in percentage points.
compute_gcomp_ate_pp <- function(model, customers) {
  treated <- transform(customers, has_exception = 1L)
  control <- transform(customers, has_exception = 0L)
  100 * mean(
    stats::predict(model, treated, type = "response") -
      stats::predict(model, control, type = "response")
  )
}

#' Fit One Logistic Specification and Summarise It
#'
#' @param formula A model formula that includes has_exception.
#' @param train Training tibble.
#' @param test Held-out test tibble.
#' @param customers Full dataset for the g-computation ATE.
#' @return A list: `coefficient`, `z_value`, `test_auc`, `ate_pp`.
fit_specification <- function(formula, train, test, customers) {
  model <- stats::glm(formula, data = train, family = stats::binomial())
  coef_row <- summary(model)$coefficients["has_exception", ]
  predicted <- stats::predict(model, newdata = test, type = "response")
  churn_test <- as.numeric(as.character(test$churned))
  list(
    coefficient = unname(coef_row[1]),
    z_value = unname(coef_row[3]),
    test_auc = compute_auc(churn_test, predicted),
    ate_pp = compute_gcomp_ate_pp(model, customers)
  )
}

#' Evaluate One Design Cell Against the Estimation Stack
#'
#' Fits the five specifications the talk needs and returns every number
#' the headline contract is checked against.
#'
#' @param collider_strength Collider design knob.
#' @param impatience_effect Latent-trait design knob.
#' @param n_customers Sample size for the cell.
#' @param seed Data seed (constant across the grid by design).
#' @param ticket_intercept Optional fixed intercept for verification runs.
#' @return A one-row tibble of cell metrics.
#' @examples
#' \dontrun{evaluate_cell(5.5, 1.25)}
evaluate_cell <- function(collider_strength, impatience_effect,
                          n_customers = n_grid, seed = seed_data,
                          ticket_intercept = NULL) {
  lab <- simulate_core(
    n_customers, seed, collider_strength, impatience_effect,
    ticket_intercept
  )
  customers <- dplyr::mutate(
    lab$observed,
    churn_numeric = as.numeric(as.character(churned))
  )
  data_split <- make_stratified_split(customers)

  naive_formula <- churn_numeric ~ has_exception + order_volume +
    monthly_spend_usd + opened_support_ticket + customer_age_years +
    marketing_emails_clicked + app_logins_last_7_days
  no_collider_formula <- churn_numeric ~ has_exception + order_volume +
    monthly_spend_usd + customer_age_years +
    marketing_emails_clicked + app_logins_last_7_days
  causal_formula <- churn_numeric ~ has_exception + order_volume +
    monthly_spend_usd
  overadjusted_formula <- churn_numeric ~ has_exception + order_volume +
    monthly_spend_usd + opened_support_ticket
  aggregate_formula <- churn_numeric ~ has_exception

  naive <- fit_specification(
    naive_formula, data_split$train, data_split$test, customers
  )
  no_collider <- fit_specification(
    no_collider_formula, data_split$train, data_split$test, customers
  )
  causal <- fit_specification(
    causal_formula, data_split$train, data_split$test, customers
  )
  overadjusted <- fit_specification(
    overadjusted_formula, data_split$train, data_split$test, customers
  )
  aggregate <- fit_specification(
    aggregate_formula, data_split$train, data_split$test, customers
  )

  marginal_rate_diff_pp <- 100 * (
    mean(customers$churn_numeric[customers$has_exception == 1]) -
      mean(customers$churn_numeric[customers$has_exception == 0])
  )

  tibble::tibble(
    collider_strength = collider_strength,
    impatience_effect = impatience_effect,
    ticket_intercept = round(lab$ticket_intercept, 2),
    truth_ate_pp = lab$truth_ate_pp,
    naive_test_auc = naive$test_auc,
    naive_coefficient = naive$coefficient,
    naive_z_value = naive$z_value,
    naive_ate_pp = naive$ate_pp,
    no_collider_coefficient = no_collider$coefficient,
    no_collider_ate_pp = no_collider$ate_pp,
    causal_test_auc = causal$test_auc,
    causal_ate_pp = causal$ate_pp,
    overadjusted_coefficient = overadjusted$coefficient,
    overadjusted_ate_pp = overadjusted$ate_pp,
    aggregate_ate_pp = aggregate$ate_pp,
    marginal_rate_diff_pp = marginal_rate_diff_pp,
    churn_rate = mean(customers$churn_numeric),
    ticket_rate = mean(customers$opened_support_ticket),
    exception_rate = mean(customers$has_exception)
  )
}

#' Score a Calibration Grid Against the Headline Contract
#'
#' @param grid A tibble produced by evaluate_cell().
#' @return The grid plus one logical column per contract criterion and a
#'   `passes_all` summary.
apply_contract <- function(grid) {
  scored <- dplyr::mutate(grid,
    h1_truth_positive = truth_ate_pp >= 4,
    h2_naive_metrics = naive_test_auc >= 0.80,
    h3_naive_wrong_sign = naive_coefficient < 0 &
      naive_z_value <= -5 & naive_ate_pp < 0,
    h4_causal_recovers = causal_ate_pp > 0 &
      abs(causal_ate_pp - truth_ate_pp) <= 2 & causal_test_auc >= 0.75,
    h5_collider_isolated = no_collider_coefficient > 0 &
      naive_coefficient < no_collider_coefficient,
    h6_realism = churn_rate >= 0.25 & churn_rate <= 0.50 &
      ticket_rate >= 0.10 & ticket_rate <= 0.35 &
      exception_rate >= 0.15 & exception_rate <= 0.45
  )
  dplyr::mutate(scored,
    passes_all = h1_truth_positive & h2_naive_metrics &
      h3_naive_wrong_sign & h4_causal_recovers & h5_collider_isolated &
      h6_realism
  )
}

#' Check Whether R/02 Still Matches the Calibrated Design
#'
#' Re-reads the constants actually coded in R/02_simulate_data.R and
#' compares them with the chosen design, so the laboratory cannot silently
#' drift away from its calibration.
#'
#' @param collider_strength Chosen collider strength.
#' @param impatience_effect Chosen latent-trait effect.
#' @param ticket_intercept Chosen ticket intercept.
#' @return TRUE if R/02 matches the design, FALSE otherwise (with a
#'   message describing the required constants).
check_design_drift <- function(collider_strength, impatience_effect,
                               ticket_intercept) {
  source_lines <- readLines(here::here("R", "02_simulate_data.R"))
  # Only CODE lines: docstring comments also mention "* impatience" and
  # would be picked up ahead of the real equation.
  code_lines <- source_lines[!grepl("^\\s*#", source_lines)]
  ticket_line <- code_lines[grepl("log_odds_ticket <-", code_lines)]
  churn_lines <- code_lines[grepl("\\* impatience", code_lines)]
  churn_line <- churn_lines[!grepl("log_odds_ticket", churn_lines)]

  extract_numbers <- function(line) {
    as.numeric(regmatches(line, gregexpr("-?[0-9]+\\.?[0-9]*", line))[[1]])
  }
  is_close_to <- function(actual, designed) {
    if (length(actual) == 0 || abs(actual - designed) > 0.051) {
      return(FALSE)
    }
    TRUE
  }

  if (length(ticket_line) == 0 || length(churn_line) == 0) {
    message("DRIFT: could not locate the collider lines in R/02.")
    return(FALSE)
  }

  ticket_numbers <- extract_numbers(ticket_line[1])
  impatience_match <- regmatches(
    churn_line[1],
    regexpr("-?[0-9]+\\.?[0-9]* \\* impatience", churn_line[1])
  )
  if (length(impatience_match) == 0) {
    message("DRIFT: could not parse the impatience coefficient in R/02.")
    return(FALSE)
  }
  impatience_number <- extract_numbers(impatience_match)[1]

  all_ok <- length(ticket_numbers) >= 3 &&
    is_close_to(ticket_numbers[1], ticket_intercept) &&
    is_close_to(ticket_numbers[2], collider_strength) &&
    is_close_to(ticket_numbers[3], collider_strength) &&
    is_close_to(impatience_number, impatience_effect)

  if (!all_ok) {
    message("DRIFT detected -- R/02 must use these constants:")
    message(sprintf(
      paste0(
        "  log_odds_ticket <- %.1f + (%.1f * has_exception) + ",
        "(%.1f * impatience)"
      ),
      ticket_intercept, collider_strength, collider_strength
    ))
    message(sprintf(
      "  churn equation: (... + %.2f * impatience ...)", impatience_effect
    ))
    return(FALSE)
  }
  TRUE
}

# ------------------------------------------------------------------------------
# 1. Grid search (common random numbers: one seed across all cells)
# ------------------------------------------------------------------------------
cat(strrep("=", 80), "\n", sep = "")
cat("DGP CALIBRATION -- designing the in-silico laboratory\n")
cat(strrep("=", 80), "\n\n", sep = "")

cat(sprintf(
  "Grid: %d collider strengths x %d impatience effects (n = %d per cell)\n\n",
  length(grid_strength), length(grid_impatience), n_grid
))

calibration_grid <- purrr::map_dfr(grid_impatience, function(effect) {
  purrr::map_dfr(grid_strength, function(strength) {
    evaluate_cell(
      collider_strength = strength, impatience_effect = effect
    )
  })
})

scored_grid <- apply_contract(calibration_grid)

cat("Cells passing the FULL headline contract:\n")
cat(strrep("-", 80), "\n", sep = "")
passing_cells <- dplyr::filter(scored_grid, passes_all)
print(as.data.frame(dplyr::select(
  passing_cells,
  collider_strength, impatience_effect, ticket_intercept, truth_ate_pp,
  naive_test_auc, naive_coefficient, naive_ate_pp, no_collider_coefficient,
  causal_ate_pp, causal_test_auc
)))
cat(strrep("-", 80), "\n\n", sep = "")

if (nrow(passing_cells) == 0) {
  stop("No design cell satisfies the headline contract -- widen the grid.")
}

chosen_design <- dplyr::slice(
  dplyr::arrange(passing_cells, collider_strength, impatience_effect), 1
)
cat(sprintf(
  paste0(
    "CHOSEN DESIGN (weakest collider that breaks the headline):\n",
    "  collider_strength = %.1f, impatience_effect = %.2f\n\n"
  ),
  chosen_design$collider_strength, chosen_design$impatience_effect
))

# ------------------------------------------------------------------------------
# 2. Round the design and verify at production scale
# ------------------------------------------------------------------------------
chosen_strength <- chosen_design$collider_strength
chosen_effect <- chosen_design$impatience_effect
chosen_intercept <- round(chosen_design$ticket_intercept, 1)

cat(sprintf(
  "Rounded constants for R/02 (verified below at n = %d):\n", n_verify
))
cat(sprintf(
  "  log_odds_ticket <- %.1f + (%.1f * has_exception) + (%.1f * impatience)\n",
  chosen_intercept, chosen_strength, chosen_strength
))
cat(sprintf(
  "  churn equation:  (... + %.2f * impatience ...)\n\n", chosen_effect
))

verification <- apply_contract(
  evaluate_cell(
    collider_strength = chosen_strength,
    impatience_effect = chosen_effect,
    n_customers = n_verify,
    ticket_intercept = chosen_intercept
  )
)

cat("PRODUCTION-SCALE VERIFICATION (the pipeline's expected numbers):\n")
cat(strrep("-", 80), "\n", sep = "")
print(as.data.frame(dplyr::select(
  verification,
  truth_ate_pp, naive_test_auc, naive_coefficient, naive_z_value,
  naive_ate_pp, no_collider_coefficient, no_collider_ate_pp,
  causal_test_auc, causal_ate_pp, overadjusted_coefficient,
  aggregate_ate_pp, marginal_rate_diff_pp, churn_rate, ticket_rate,
  exception_rate, passes_all
)))
cat(strrep("-", 80), "\n", sep = "")

if (!verification$passes_all) {
  stop("Rounded design FAILED production verification -- widen the grid.")
}
cat("All 6 headline criteria: PASS\n\n")

# Ground truth at R/06's scale, mirroring get_ground_truth_ate()
set.seed(seed_data)
gt_volume <- stats::rnorm(n_truth)
gt_impatience <- stats::rnorm(n_truth)
gt_spend <- as.numeric(scale(round(stats::rlnorm(n_truth, 4, 0.5), 2)))
gt_linear <- function(exception) {
  -1.0 + 0.5 * exception - 2.0 * gt_volume +
    chosen_effect * gt_impatience - 0.3 * gt_spend
}
gt_ate_pp <- 100 * (mean(stats::plogis(gt_linear(1))) -
  mean(stats::plogis(gt_linear(0))))
cat(sprintf(
  "True marginal ATE (n = %d, mirrors get_ground_truth_ate()): %+.2f pp\n\n",
  n_truth, gt_ate_pp
))

# ------------------------------------------------------------------------------
# 3. Design figure: the trade-off the talk leans on
# ------------------------------------------------------------------------------
panel_auc <- ggplot2::ggplot(
  dplyr::filter(scored_grid, impatience_effect == chosen_effect),
  ggplot2::aes(collider_strength, naive_test_auc)
) +
  ggplot2::geom_line(color = "#1565C0", linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(color = passes_all), size = 2.5) +
  ggplot2::geom_hline(
    yintercept = 0.80, linetype = "dashed", color = "gray50"
  ) +
  ggplot2::geom_vline(
    xintercept = chosen_strength, linetype = "dotted", color = "gray40"
  ) +
  ggplot2::scale_color_manual(
    values = c("TRUE" = "#2E7D32", "FALSE" = "gray70"), guide = "none"
  ) +
  ggplot2::labs(
    title = "A Stronger Collider Buys Better Metrics",
    subtitle = "Naive test AUC vs collider strength (dashed = 0.80 bar)",
    x = "Collider strength", y = "Naive test AUC"
  ) +
  ggplot2::theme_minimal(base_size = 12)

panel_coef <- ggplot2::ggplot(
  dplyr::filter(scored_grid, impatience_effect == chosen_effect),
  ggplot2::aes(collider_strength, naive_coefficient)
) +
  ggplot2::geom_hline(yintercept = 0, color = "gray40") +
  ggplot2::geom_line(color = "#C62828", linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(color = passes_all), size = 2.5) +
  ggplot2::geom_vline(
    xintercept = chosen_strength, linetype = "dotted", color = "gray40"
  ) +
  ggplot2::scale_color_manual(
    values = c("TRUE" = "#2E7D32", "FALSE" = "gray70"), guide = "none"
  ) +
  ggplot2::labs(
    title = "...and a More WRONG Causal Answer",
    subtitle = paste0(
      "Naive has_exception coefficient vs collider strength\n",
      "(true effect = +0.5; below zero = wrong sign)"
    ),
    x = "Collider strength", y = "has_exception coefficient (log-odds)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  here::here("figures", "00_dgp_calibration.png"),
  patchwork::wrap_plots(panel_auc, panel_coef, ncol = 1),
  width = 10, height = 7, dpi = 300, bg = "white"
)
cat("Saved: figures/00_dgp_calibration.png\n\n")

# ------------------------------------------------------------------------------
# 4. Drift guard: does R/02 still match this design?
# ------------------------------------------------------------------------------
cat(strrep("=", 80), "\n", sep = "")
if (check_design_drift(chosen_strength, chosen_effect, chosen_intercept)) {
  cat("R/02_simulate_data.R MATCHES the calibrated design. In spec.\n")
} else {
  cat("Update R/02 with the constants printed above, then re-run.\n")
}
cat(strrep("=", 80), "\n", sep = "")

