#!/usr/bin/env Rscript
# Generate test-set ROC curve figures for BOTH DAG-guided models (GLM and XGBoost).
# The DAG-guided models follow the DAG's adjustment strategy (R/04: confounder
# kept, collider dropped, noise removed, spend kept as precision covariate) --
# slightly lower AUC, but the CORRECT causal sign. Uses fit_causal_models() and
# the SAME 80/20 split (set.seed(123), stratified on churned) as the naive-model
# figures (R/09), so the test set matches both the fits and the comparison.

library(ggplot2)
library(dplyr)
library(tibble)
library(purrr)
library(rsample)
library(yardstick)

if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  source(if (requireNamespace("here", quietly = TRUE))
    here::here("R", "00_theme_talk.R") else "R/00_theme_talk.R")
}

source("R/02_simulate_data.R")
source("R/04_causal_models.R")

cat("Generating DAG-guided model ROC curve figures...\n")

# ------------------------------------------------------------------------------
# 1. Simulate data (identical to the main pipeline)
# ------------------------------------------------------------------------------
set.seed(2026)
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

# ------------------------------------------------------------------------------
# 2. Fit the DAG-guided models and recover the SAME held-out test split
#    (fit_causal_models() re-seeds 123 internally, so this split is identical
#     to the naive-model figures -- a fair AUC comparison)
# ------------------------------------------------------------------------------
causal_models <- fit_causal_models(df)

set.seed(123)
data_split <- initial_split(df, prop = 0.8, strata = churned)
test <- testing(data_split)

# ------------------------------------------------------------------------------
# 3. Helper: test-set ROC curve + AUC for one fitted workflow
# ------------------------------------------------------------------------------
causal_roc <- function(fitted_workflows, wflow_id) {

  fit <- fitted_workflows |>
    filter(wflow_id == {{ wflow_id }}) |>
    pull(fit) |>
    _[[1]]

  preds <- test |>
    mutate(.pred_1 = predict(fit, new_data = test, type = "prob")$.pred_1)

  list(
    curve = roc_curve(preds, churned, .pred_1, event_level = "second"),
    auc   = roc_auc(preds, churned, .pred_1, event_level = "second")$.estimate
  )
}

plot_causal_roc <- function(roc, title) {

  ggplot(roc$curve, aes(x = 1 - specificity, y = sensitivity)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = ix_faint, linewidth = 0.6) +
    geom_line(color = ix_white, linewidth = 1.4) +
    annotate(
      "text", x = 0.95, y = 0.10, hjust = 1, size = 6.5, fontface = "bold",
      colour = ix_white, family = ix_font,
      label = sprintf("Test AUC = %.2f", roc$auc)
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    coord_equal() +
    labs(
      title = title,
      x = "1 - Specificity",
      y = "Sensitivity"
    ) +
    theme_talk()
}

# ------------------------------------------------------------------------------
# 4. DAG-guided GLM: ROC curve figure
# ------------------------------------------------------------------------------
glm_roc <- causal_roc(causal_models, "causal_rec_glm")
cat(sprintf("  Causal GLM     test AUC: %.3f\n", glm_roc$auc))

ggsave("figures/05b_causal_glm_roc.png",
       plot_causal_roc(glm_roc, title = "DAG-Guided GLM: ROC Curve"),
       width = 8, height = 6, dpi = 300, bg = ix_black)
cat("Saved: figures/05b_causal_glm_roc.png\n")

# ------------------------------------------------------------------------------
# 5. DAG-guided XGBoost: ROC curve figure
# ------------------------------------------------------------------------------
xgb_roc <- causal_roc(causal_models, "causal_rec_xgb")
cat(sprintf("  Causal XGBoost test AUC: %.3f\n", xgb_roc$auc))

ggsave("figures/06b_causal_xgb_roc.png",
       plot_causal_roc(xgb_roc, title = "DAG-Guided XGBoost: ROC Curve"),
       width = 8, height = 6, dpi = 300, bg = ix_black)
cat("Saved: figures/06b_causal_xgb_roc.png\n")

cat("Done.\n")