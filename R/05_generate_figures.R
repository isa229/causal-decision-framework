# Load required libraries
library(ggplot2)
library(here)
source(here::here("R", "01_define_dag.R"))
source(here::here("R", "02_simulate_data.R"))
source(here::here("R", "03_naive_models.R"))
source(here::here("R", "04_causal_models.R"))
source(here::here("R", "06_validate_ground_truth.R"))
source(here::here("R", "07_comparison_plots.R"))

# ------------------------------------------------------------------------------
# 1. Setup Environment
# ------------------------------------------------------------------------------
figures_dir <- here::here("figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Created 'figures/' directory.\n")
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("Figure generation for Causal framework validation\n")
cat(strrep("=", 80), "\n")
cat("\n")

# ------------------------------------------------------------------------------
# 2. Generate and Save the DAG (The Assumptions)
# ------------------------------------------------------------------------------
cat("\nStep 1/4: Generating Causal DAG...\n")
dag_results <- define_causal_dag()

ggsave(
  filename = here::here("figures", "01_causal_dag.png"),
  plot = dag_results$plot,
  width = 9, height = 6, dpi = 300, bg = "white"
)

# ------------------------------------------------------------------------------
# 3. Simulate the Data (The in silico laboratory)
# ------------------------------------------------------------------------------
cat("Step 2/7: Simulating 50,000 enterprise customers...\n")
cat("Ground truth effect: 0.5 log-odds (baked into DGP)\n")
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)
cat("Data generated with collider structure\n\n")

# ------------------------------------------------------------------------------
# 4. Fit Naive Models (The Trap)
# ------------------------------------------------------------------------------
cat("Step 3/7: Fitting Naive ML Models (Conditioning on Collider)...\n")
naive_models <- fit_naive_models(df)
cat("GLM, Random Forest, XGBoost fitted\n\n")

ggsave(
  filename = here::here("figures", "02_naive_glm_trap.png"),
  plot = plot_glm_trap(naive_models),
  width = 8, height = 6, dpi = 300, bg = "white"
)

ggsave(
  filename = here::here("figures", "03_naive_xgb_vip.png"),
  plot = plot_xgb_vip(naive_models),
  width = 8, height = 6, dpi = 300, bg = "white"
)

# ------------------------------------------------------------------------------
# 5. Fit Causal Models (The Solution)
# ------------------------------------------------------------------------------
cat("Step 4/7: Fitting Causal ML Models (DAG-Guided)...\n")
causal_models <- fit_causal_models(df)
cat("Models fitted with proper adjustment set\n\n")

ggsave(
  filename = here::here("figures", "04_causal_glm_solution.png"),
  plot = plot_causal_glm(causal_models),
  width = 8, height = 6, dpi = 300, bg = "white"
)

ggsave(
  filename = here::here("figures", "05_causal_xgb_vip.png"),
  plot = plot_causal_xgb(causal_models),
  width = 8, height = 6, dpi = 300, bg = "white"
)

# ------------------------------------------------------------------------------
# 6. Validate Against Ground Truth
# ------------------------------------------------------------------------------
cat("Step 5/7: Validating estimates against ground truth...\n")
comparison <- compare_to_ground_truth(naive_models, causal_models)

# Print comparison table to console
cat("\n")
cat("Validation results:\n")
cat(strrep("-", 80), "\n")
print(comparison, n = Inf)
cat(strrep("-", 80), "\n\n")

# Ground truth comparison
ggsave(
  filename = here::here("figures", "06_ground_truth_comparison.png"),
  plot = plot_ground_truth_comparison(comparison),
  width = 10, height = 7, dpi = 300, bg = "white"
)
cat("Ground truth comparison saved\n")


# ------------------------------------------------------------------------------
# 7. Generate Coefficient Trajectory Plot
# ------------------------------------------------------------------------------
cat("Step 6/7: Creating coefficient trajectory analysis...\n")
ggsave(
  filename = here::here("figures", "07_coefficient_trajectory.png"),
  plot = plot_coefficient_trajectory(df),
  width = 10, height = 6, dpi = 300, bg = "white"
)
cat("Coefficient trajectory saved\n\n")


# ------------------------------------------------------------------------------
# 8. Bootstrap Confidence Intervals
# ------------------------------------------------------------------------------
cat("Step 7/7: Computing bootstrap confidence intervals...\n")
boot_ci <- bootstrap_ate_ci(df, n_bootstrap = 1000, seed = 2026)
cat("\n")
cat("BOOTSTRAP 95% CI:\n")
cat(strrep("-", 80), "\n")
print(boot_ci)
cat(strrep("-", 80), "\n\n")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 80), "\n")
cat("Complete! All figures generated successfully.\n")
cat(strrep("=", 80), "\n")
cat("\n")
cat("Key Results:\n")
cat("- Ground truth effect:  0.500 log-odds\n")
cat("- Naive model estimate: ", sprintf("%.3f", comparison$Estimate[2]), " log-odds (Wrong direction)\n")
cat("- Causal model estimate:", sprintf("%.3f", comparison$Estimate[3]), " log-odds (Recovers truth)\n")
cat("- Bias reduction:       ", sprintf("%.1f", abs(comparison$Pct_Bias[2]) - abs(comparison$Pct_Bias[3])), "%%\n")
cat("\n")
cat("Generated figures:\n")
cat("  1. 01_causal_dag.png: The assumptions (DAG)\n")
cat("  2. 02_naive_glm_trap.png: Wrong coefficient sign\n")
cat("  3. 03_naive_xgb_vip.png: ML reliance on collider\n")
cat("  4. 04_causal_glm_solution.png: Correct coefficient sign\n")
cat("  5. 05_causal_xgb_vip.png: DAG-guided features\n")
cat("  6. 06_ground_truth_comparison.png: )\n")
cat("  7. 07_coefficient_trajectory.png: Specification sensitivity\n")

