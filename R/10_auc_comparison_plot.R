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
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray50") +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = Label, y = Test_AUC + 0.015), size = 5,
            fontface = "bold") +
  scale_fill_manual(
    values = c(
      "correct" = "#1565C0", "wrong" = "#C62828", "neutral" = "gray60"
    ),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "The Best Predictor Gives the Worst Advice",
    subtitle = paste(
      "Test-set AUC by specification. The naive kitchen-sink model (keeps the",
      "collider) is the best predictor -- and the only one whose causal",
      "estimate has the wrong sign."
    ),
    x = NULL,
    y = "Test AUC (held-out 20%)",
    caption = paste(
      "Same stratified 80/20 split (seed 123) for all three specifications.",
      "Dotted line = chance (AUC 0.5)."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
  )

ggsave("figures/09_auc_comparison.png", p,
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: figures/09_auc_comparison.png\n")
