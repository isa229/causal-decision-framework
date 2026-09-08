#!/usr/bin/env Rscript
# Generate test-set ROC curve figures for BOTH naive models (GLM and XGBoost).
# The naive models are the KITCHEN-SINK specification (R/03: every observed
# feature, confounder adjusted, collider kept) -- genuinely good metrics,
# wrong causal sign. Uses fit_naive_models() and the SAME 80/20 split
# (set.seed(123), stratified on churned) so the test set matches the fits.

library(ggplot2)
library(dplyr)
library(tibble)
library(purrr)
library(rsample)
library(yardstick)

source("R/02_simulate_data.R")
source("R/03_naive_models.R")

cat("Generating naive-model ROC curve figures...\n")

# ------------------------------------------------------------------------------
# 1. Simulate data (identical to the main pipeline)
# ------------------------------------------------------------------------------
set.seed(2026)
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

# ------------------------------------------------------------------------------
# 2. Fit the naive models and recover the SAME held-out test split
#    (fit_naive_models() re-seeds 123 internally, so this split is identical)
# ------------------------------------------------------------------------------
naive_models <- fit_naive_models(df)

set.seed(123)
data_split <- initial_split(df, prop = 0.8, strata = churned)
test <- testing(data_split)

# ------------------------------------------------------------------------------
# 3. Helper: test-set ROC curve + AUC for one fitted workflow
# ------------------------------------------------------------------------------
naive_roc <- function(fitted_workflows, wflow_id) {

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

plot_naive_roc <- function(roc, title, subtitle) {

  ggplot(roc$curve, aes(x = 1 - specificity, y = sensitivity)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "gray60", linewidth = 0.6) +
    geom_line(color = "#C62828", linewidth = 1.2) +
    annotate(
      "text", x = 0.4, y = 0.92, size = 6.5, fontface = "bold", color = "#C62828",
      label = sprintf("Test AUC = %.3f", roc$auc)
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    coord_equal() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      caption = paste(
        "Held-out test set (20% of 50,000 customers, stratified split, seed 123).",
        "Excellent discrimination does NOT mean the causal estimate is right."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
      panel.grid.minor = element_blank()
    )
}

# ------------------------------------------------------------------------------
# 4. Naive GLM: ROC curve figure
# ------------------------------------------------------------------------------
glm_roc <- naive_roc(naive_models, "base_rec_glm")
cat(sprintf("  Naive GLM     test AUC: %.3f\n", glm_roc$auc))

ggsave("figures/03b_naive_glm_roc.png",
       plot_naive_roc(
         glm_roc,
         title = "Naive GLM: ROC Curve (The Metrics Look Great, the Advice Is Wrong)",
         subtitle = paste(
           "Kitchen-sink spec: confounder adjusted, collider kept.",
           "Excellent discrimination, yet the `has_exception` coefficient",
           "lands on the WRONG sign (collider bias).",
           sep = "\n"
         )
       ),
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: figures/03b_naive_glm_roc.png\n")

# ------------------------------------------------------------------------------
# 5. Naive XGBoost: ROC curve figure
# ------------------------------------------------------------------------------
xgb_roc <- naive_roc(naive_models, "base_rec_xgb")
cat(sprintf("  Naive XGBoost test AUC: %.3f\n", xgb_roc$auc))

ggsave("figures/04b_naive_xgb_roc.png",
       plot_naive_roc(
         xgb_roc,
         title = "Naive XGBoost: ROC Curve (The Collider Buys AUC, Not Truth)",
         subtitle = paste(
           "What discrimination it has comes from leaning on the collider (support_ticket)",
           "-- the most predictive, most misleading feature. No AUC will reveal that.",
           sep = "\n"
         )
       ),
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: figures/04b_naive_xgb_roc.png\n")

cat("Done.\n")
