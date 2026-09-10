#!/usr/bin/env Rscript
# ==============================================================================
# 10_auc_comparison_plot.R -- the AUC trade-off figure
# ==============================================================================
# Bar chart of test-set AUC across the three headline specifications, computed
# live by evaluate_predictive_performance() in R/06_validate_ground_truth.R
# (same stratified 80/20 split, seed 123, for all three).
#
# The punchline: the naive kitchen-sink model is the BEST predictor of the
# three -- and it is the one giving the wrong causal advice. The DAG-guided
# model gives up a little AUC to get the decision right.

library(ggplot2)
library(dplyr)

if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  source(if (requireNamespace("here", quietly = TRUE))
    here::here("R", "00_theme_talk.R") else "R/00_theme_talk.R")
}

source("R/02_simulate_data.R")
source("R/06_validate_ground_truth.R")

cat("Generating the AUC comparison figure...\n")

# ------------------------------------------------------------------------------
# 1. Simulate data (identical to the main pipeline) and evaluate
# ------------------------------------------------------------------------------
set.seed(2026)
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

auc_table <- evaluate_predictive_performance(df)
print(auc_table)

# ------------------------------------------------------------------------------
# 2. Plot
# ------------------------------------------------------------------------------
plot_data <- auc_table |>
  mutate(
    Specification = factor(
      Specification,
      levels = c(
        "Aggregate (exception only)",
        "Causal (DAG-guided)",
        "Kitchen-Sink (keeps collider)"
      )
    ),
    Color = case_when(
      Specification == "Causal (DAG-guided)" ~ "correct",
      Specification == "Kitchen-Sink (keeps collider)" ~ "wrong",
      TRUE ~ "neutral"
    ),
    Label = sprintf("%.3f", Test_AUC)
  )

p <- ggplot(plot_data, aes(x = Specification, y = Test_AUC, fill = Color)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = ix_faint) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = Label, y = Test_AUC + 0.015), size = 5,
            fontface = "bold", colour = ix_white, family = ix_font) +
  scale_fill_manual(
    values = c(
      "correct" = ix_blue, "wrong" = ix_red, "neutral" = ix_purple
    ),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "The Best Predictor Gives the Worst Advice",
    x = NULL,
    y = "Test AUC"
  ) +
  theme_talk()

ggsave("figures/09_auc_comparison.png", p,
       width = 10, height = 6, dpi = 300, bg = ix_black)
cat("Saved: figures/09_auc_comparison.png\n")
