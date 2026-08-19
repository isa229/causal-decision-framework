library(dplyr)
library(broom)
library(tibble)

#' Get Ground Truth Causal Effect
#'
#' Returns the true causal effect baked into the data generating process.
#'
#' Two estimands are reported:
#'   - log_odds: the structural coefficient (+0.5) on the log-odds scale. NOTE:
#'     logistic coefficients are NON-COLLAPSIBLE, so a correctly-specified causal
#'     model estimates a slightly attenuated value (~0.43) when a strong latent
#'     cause of the outcome (impatience) is unmeasured. The SIGN and the marginal
#'     effect are what matter for decisions.
#'   - ate_pp: the TRUE marginal Average Treatment Effect (risk difference),
#'     computed via g-computation on the DGP. This is collapsible and is the
#'     honest, business-facing headline number (~+6.8 percentage points).
#'
#' @return A list with the true effect on different scales.
get_ground_truth_ate <- function() {

  true_log_odds_effect <- 0.5
  true_odds_ratio <- exp(true_log_odds_effect)

  # True marginal ATE recovered by g-computation on the DGP (see verify scripts).
  # Re-derived here so the number is reproducible from the structural equations.
  inv_logit <- function(x) 1 / (1 + exp(-x))
  set.seed(2026)
  n <- 300000
  order_volume <- rnorm(n)
  impatience   <- rnorm(n)
  spend_z      <- as.numeric(scale(round(rlnorm(n, 4, 0.5), 2)))
  lin <- function(exc) {
    -1.0 + 0.5 * exc + (-2.0) * order_volume + 1.0 * impatience + (-0.3) * spend_z
  }
  true_ate <- mean(inv_logit(lin(1)) - inv_logit(lin(0)))

  list(
    log_odds = true_log_odds_effect,
    odds_ratio = true_odds_ratio,
    ate_pp = true_ate * 100,
    interpretation = paste0(
      "Delivery exceptions causally INCREASE churn by ~",
      round(true_ate * 100, 1),
      " percentage points (true marginal ATE)."
    )
  )
}

#' Extract Coefficient from Fitted Model
#'
#' @param fitted_workflow A fitted tidymodels workflow
#' @param term_name The coefficient to extract
#' @return A tibble with estimate and std.error
extract_exception_coefficient <- function(fitted_workflow, term_name = "has_exception") {
  fitted_workflow |>
    extract_fit_parsnip() |>
    tidy() |>
    filter(term == term_name) |>
    select(term, estimate, std.error)
}

#' Compute Marginal ATE via G-Computation
#'
#' Calculates the Average Treatment Effect (risk difference) by simulating the
#' counterfactual world where everyone / no one experiences an exception.
#' This is the collapsible, decision-relevant estimand.
#'
#' @param data The simulated dataset
#' @param fitted_workflow A fitted tidymodels workflow
#' @return A list with the ATE estimate (proportion + percentage points)
compute_marginal_ate <- function(data, fitted_workflow) {

  data_treated <- data |> mutate(has_exception = 1L)
  data_control <- data |> mutate(has_exception = 0L)

  pred_treated <- predict(fitted_workflow, new_data = data_treated, type = "prob") |>
    pull(.pred_1)
  pred_control <- predict(fitted_workflow, new_data = data_control, type = "prob") |>
    pull(.pred_1)

  ate <- mean(pred_treated - pred_control)

  list(
    risk_difference = ate,
    ate_pp = ate * 100,
    interpretation = paste0(
      "Estimated effect: ",
      round(ate * 100, 2),
      " percentage points on churn probability."
    )
  )
}

#' Compare All Estimates to Ground Truth (Marginal ATE, percentage points)
#'
#' Builds the headline comparison table on the risk-difference scale, which is
#' both collapsible and business-interpretable.
#'
#' @param data The simulated dataset
#' @param naive_workflows Fitted naive models from fit_naive_models()
#' @param causal_workflows Fitted causal models from fit_causal_models()
#' @return A tibble with comparison metrics on the ATE (pp) scale
compare_to_ground_truth <- function(data, naive_workflows, causal_workflows) {

  truth <- get_ground_truth_ate()

  naive_glm <- naive_workflows |>
    filter(wflow_id == "base_rec_glm") |>
    pull(fit) |>
    _[[1]]

  causal_glm <- causal_workflows |>
    filter(wflow_id == "causal_rec_glm") |>
    pull(fit) |>
    _[[1]]

  naive_ate <- compute_marginal_ate(data, naive_glm)$ate_pp
  causal_ate <- compute_marginal_ate(data, causal_glm)$ate_pp
  true_ate <- truth$ate_pp

  tibble(
    Method = c("Ground Truth (DGP)", "Naive Model (Simpson + Collider)", "Causal Model (DAG-Guided)"),
    ATE_pp = c(true_ate, naive_ate, causal_ate),
    Bias_pp = c(0, naive_ate - true_ate, causal_ate - true_ate),
    Direction = c(
      "Positive (Increases Churn)",
      ifelse(naive_ate > 0, "Positive", "NEGATIVE (Decreases Churn) -- WRONG"),
      ifelse(causal_ate > 0, "Positive (Increases Churn)", "Negative")
    )
  )
}

#' Evaluate Predictive Performance (Test-Set AUC)
#'
#' Fits three logistic specifications on a training split and reports ROC AUC on
#' the held-out test split. 
#'
#' The three specifications tell the option-(c) story:
#'   - "Kitchen-Sink"  : everything, including the collider (opened_support_ticket)
#'                       and the confounder (order_volume). The collider is a
#'                       strong predictor, so this usually has the HIGHEST AUC --
#'                       yet its causal estimate for `has_exception` is biased
#'                       toward the null (the SILENT failure).
#'   - "Aggregate"     : churn ~ exception only. Omitting the confounder produces
#'                       the WRONG SIGN (the LOUD failure).
#'   - "Causal"        : DAG-guided (add confounder, drop collider).
#'
#' @param data The simulated dataset
#' @param seed Random seed for the split (kept consistent with the model fits)
#' @return A tibble with AUC per specification
evaluate_predictive_performance <- function(data, seed = 123) {

  set.seed(seed)
  split <- initial_split(data, prop = 0.8, strata = churned)
  train <- training(split) |>
    mutate(churn_f = factor(churned, levels = c("0", "1")))
  test  <- testing(split) |>
    mutate(churn_f = factor(churned, levels = c("0", "1")))

  specs <- list(
    "Kitchen-Sink (keeps collider)" =
      churn_f ~ has_exception + order_volume + monthly_spend_usd +
        opened_support_ticket + customer_age_years +
        marketing_emails_clicked + app_logins_last_7_days,
    "Aggregate (omits confounder)" =
      churn_f ~ has_exception,
    "Causal (DAG-guided)" =
      churn_f ~ has_exception + order_volume + monthly_spend_usd
  )

  purrr::map_dfr(names(specs), function(nm) {
    m <- glm(specs[[nm]], data = train, family = binomial())
    test_pred <- test |>
      mutate(.pred_1 = predict(m, newdata = test, type = "response"))
    auc <- yardstick::roc_auc(test_pred, truth = churn_f, .pred_1,
                              event_level = "second")$.estimate
    tibble(Specification = nm, Test_AUC = auc)
  })
}

#' Bootstrap Confidence Interval for the Causal ATE

#'
#' Percentile bootstrap CI for the DAG-guided marginal ATE (percentage points).
#'
#' @param data The simulated dataset
#' @param n_bootstrap Number of bootstrap resamples (default 1000)
#' @param seed Random seed
#' @return A tibble with the ATE estimate and 95% CI (percentage points)
bootstrap_ate_ci <- function(data, n_bootstrap = 1000, seed = 2026) {

  set.seed(seed)
  data_numeric <- data |>
    mutate(churn_numeric = as.numeric(as.character(churned)))

  estimates <- replicate(n_bootstrap, {
    boot <- data_numeric |> slice_sample(n = nrow(data_numeric), replace = TRUE)
    m <- glm(
      churn_numeric ~ has_exception + order_volume + monthly_spend_usd,
      data = boot, family = binomial()
    )
    d1 <- boot |> mutate(has_exception = 1L)
    d0 <- boot |> mutate(has_exception = 0L)
    mean(predict(m, d1, type = "response") - predict(m, d0, type = "response")) * 100
  })

  tibble(
    ATE_pp = mean(estimates),
    CI_Lower = quantile(estimates, 0.025),
    CI_Upper = quantile(estimates, 0.975),
    Std_Error = sd(estimates)
  )
}

#' E-value Sensitivity Analysis (Backup Slide)
#'
#' Quantifies how strong an UNMEASURED confounder would need to be -- associated
#' with both treatment and outcome -- to fully explain away the causal estimate.
#' Uses the VanderWeele & Ding (2017) formula.
#'
#' IMPORTANT (common vs rare outcome): the E-value formula is defined on the RISK
#' RATIO scale. An odds ratio only approximates the risk ratio when the outcome is
#' RARE. Churn here is COMMON (base rate ~25-30%), so using the OR directly would
#' OVERSTATE the E-value. Following VanderWeele & Ding (2017), for a common outcome
#' we first convert the OR to an approximate RR via RR ~ sqrt(OR) before applying
#' the E-value formula. Set `rare_outcome = TRUE` to skip this conversion.
#'
#' In this scenario the latent `impatience` is exactly such an unmeasured cause:
#' the E-value tells us how much robustness the finding has against it.
#'
#' @param odds_ratio The causal odds ratio (exp of the causal log-odds estimate)
#' @param rare_outcome Logical. If FALSE (default), converts OR -> RR via sqrt(OR)
#'   because the outcome is common. If TRUE, treats the OR as the RR directly.
#' @return A list with the E-value for the point estimate
compute_evalue <- function(odds_ratio, rare_outcome = FALSE) {
  or <- ifelse(odds_ratio < 1, 1 / odds_ratio, odds_ratio)

  # Convert OR to an approximate RR for a common outcome (VanderWeele & Ding 2017).
  rr <- if (rare_outcome) or else sqrt(or)

  evalue <- rr + sqrt(rr * (rr - 1))
  list(
    odds_ratio = odds_ratio,
    risk_ratio_approx = rr,
    rare_outcome = rare_outcome,
    evalue = evalue,
    interpretation = paste0(
      "An unmeasured confounder would need an association of at least ",
      round(evalue, 2),
      "x (on the risk-ratio scale) with BOTH exception and churn (beyond measured ",
      "covariates) to nullify the effect."
    )
  )
}

