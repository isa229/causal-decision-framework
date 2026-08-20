#!/usr/bin/env Rscript
# Generate a standalone AUC comparison figure
# Uses evaluate_predictive_performance() from R/06_validate_ground_truth.R

library(ggplot2)
library(dplyr)
library(tibble)
library(rsample)
library(yardstick)
library(purrr)

source("R/02_simulate_data.R")
source("R/06_validate_ground_truth.R")

cat("Generating AUC comparison figure...\n")

# Simulate data
set.seed(2026)
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

# Get AUC table
auc_tbl <- evaluate_predictive_performance(df)

# Add causal-validity annotation and ordering
auc_plot_data <- auc_tbl |>
  mutate(
    Specification = factor(Specification, levels = c(
      "Kitchen-Sink (keeps collider)",
      "Causal (DAG-guided)",
      "Aggregate (omits confounder)"
    )),
    Verdict = case_when(
      grepl("Kitchen-Sink", Specification) ~ "Best prediction,\nworst advice (ATE −28 pp)",
      grepl("Causal", Specification)       ~ "Right decision\n(ATE +5.8 pp ≈ truth)",
      TRUE                                 ~ "Wrong sign\n(ATE −23 pp)"
    ),
    Color = case_when(
      grepl("Kitchen-Sink", Specification) ~ "wrong",
      grepl("Causal", Specification)       ~ "correct",
      TRUE                                 ~ "wrong"
    )
  )

p <- ggplot(auc_plot_data, aes(x = Specification, y = Test_AUC, fill = Color)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.3f", Test_AUC)),
            vjust = -0.5, size = 5, fontface = "bold") +
  geom_text(aes(label = Verdict, y = Test_AUC / 2),
            size = 3.2, color = "white", fontface = "italic", lineheight = 0.9) +
  scale_fill_manual(
    values = c("correct" = "#1565C0", "wrong" = "#C62828"),
    guide = "none"
  ) +
  scale_x_discrete(labels = function(x) gsub(" \\(", "\n(", x)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Predictive Accuracy (AUC) vs. Causal Validity: A Different Goal",
    subtitle = paste(
      "The kitchen-sink model predicts BEST — yet gives the WRONG causal answer.",
      "The causal model gives up 0.015 AUC and gets the decision RIGHT.",
      sep = "\n"
    ),
    x = NULL,
    y = "Test-Set ROC AUC",
    caption = "AUC measures prediction only. Do not equate high AUC with correct causal estimates."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
  )

ggsave("figures/09_auc_comparison.png", p, width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: figures/09_auc_comparison.png\n")
