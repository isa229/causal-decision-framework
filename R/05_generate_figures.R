# Load required libraries
library(ggplot2)
library(here)
source(here::here("R", "00_theme_talk.R"))
source(here::here("R", "01_define_dag.R"))
source(here::here("R", "02_simulate_data.R"))
source(here::here("R", "03_naive_models.R"))
source(here::here("R", "04_causal_models.R"))
source(here::here("R", "06_validate_ground_truth.R"))
source(here::here("R", "07_comparison_plots.R"))

# ------------------------------------------------------------------------------
# 1. Setup
# ------------------------------------------------------------------------------
figures_dir <- here::here("figures")
if (!dir.exists(figures_dir)) dir.create(figures_dir)

cat("\n", strrep("=", 80), "\n", sep = "")
cat("Causal framework: figure generation & validation\n")
cat(strrep("=", 80), "\n\n", sep = "")

# ------------------------------------------------------------------------------
# 2. The DAG (assumptions) + the mathematical adjustment set
# ------------------------------------------------------------------------------
cat("Step 1/8: Generating Causal DAG and adjustment set...\n")
dag_results <- define_causal_dag()
adj <- get_adjustment_strategy(dag_results$dag)
cat("  dagitty minimal sufficient adjustment set: { ",
    paste(unlist(adj), collapse = ", "), " }\n", sep = "")

ggsave(here::here("figures", "01_causal_dag.png"),
       dag_results$plot, width = 9, height = 6, dpi = 300, bg = ix_black)

# ------------------------------------------------------------------------------
# 3. The in silico laboratory
# ------------------------------------------------------------------------------
cat("Step 2/8: Simulating 50,000 customers (true effect baked in)...\n")
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

# ------------------------------------------------------------------------------
# 4. The Simpson's Paradox figure (the signature visual)
# ------------------------------------------------------------------------------
cat("Step 3/8: Building the Simpson's Paradox figure...\n")
ggsave(here::here("figures", "02_simpsons_paradox.png"),
       plot_simpsons_paradox(df), width = 10, height = 6, dpi = 300, bg = ix_black)

# ------------------------------------------------------------------------------
# 5. Naive models (the trap)
# ------------------------------------------------------------------------------
cat("Step 4/8: Fitting naive kitchen-sink models (every feature; keeps the collider)...\n")
naive_models <- fit_naive_models(df)
ggsave(here::here("figures", "03_naive_glm_trap.png"),
       plot_glm_trap(naive_models), width = 8, height = 6, dpi = 300, bg = ix_black)
ggsave(here::here("figures", "04_naive_xgb_vip.png"),
       plot_xgb_vip(naive_models), width = 8, height = 6, dpi = 300, bg = ix_black)

# ------------------------------------------------------------------------------
# 6. Causal models (the solution)
# ------------------------------------------------------------------------------
cat("Step 5/8: Fitting causal models (DAG-guided adjustment)...\n")
causal_models <- fit_causal_models(df)
ggsave(here::here("figures", "05_causal_glm_solution.png"),
       plot_causal_glm(causal_models), width = 8, height = 6, dpi = 300, bg = ix_black)
ggsave(here::here("figures", "06_causal_xgb_vip.png"),
       plot_causal_xgb(causal_models), width = 8, height = 6, dpi = 300, bg = ix_black)

# ------------------------------------------------------------------------------
# 7. Validation against ground truth (marginal ATE, percentage points)
# ------------------------------------------------------------------------------
cat("Step 6/8: Validating estimates against ground truth (marginal ATE)...\n")
comparison <- compare_to_ground_truth(df, naive_models, causal_models)
cat("\nValidation results (ATE, percentage points):\n")
cat(strrep("-", 80), "\n")
print(comparison, n = Inf)
cat(strrep("-", 80), "\n\n")

ggsave(here::here("figures", "07_ground_truth_comparison.png"),
       plot_ground_truth_comparison(comparison), width = 10, height = 7, dpi = 300, bg = ix_black)

ggsave(here::here("figures", "08_coefficient_trajectory.png"),
       plot_coefficient_trajectory(df), width = 10, height = 6, dpi = 300, bg = ix_black)

# ------------------------------------------------------------------------------
# 8. Bootstrap CI for the causal ATE
# ------------------------------------------------------------------------------
cat("Step 7/8: Bootstrap 95% CI for the causal ATE...\n")
boot_ci <- bootstrap_ate_ci(df, n_bootstrap = 1000, seed = 2026)
print(boot_ci)
cat("\n")

# ------------------------------------------------------------------------------
# 9. E-value sensitivity analysis (backup slide)
# ------------------------------------------------------------------------------
cat("Step 8/8: E-value sensitivity analysis (robustness to latent impatience)...\n")
# Fit the DAG-guided specification on the RAW scale: the tidymodels workflow
# normalizes predictors per-SD, which would rescale the odds ratio (and
# understate the E-value). The per-unit log-odds here matches the
# coefficient-trajectory figure and the bootstrap CI (both raw-scale glm).
causal_glm_data <- df |>
  dplyr::mutate(churn_numeric = as.numeric(as.character(churned)))
causal_glm_base <- glm(
  churn_numeric ~ has_exception + order_volume + monthly_spend_usd,
  data = causal_glm_data, family = binomial()
)
causal_logodds <- unname(coef(summary(causal_glm_base))["has_exception", "Estimate"])
ev <- compute_evalue(exp(causal_logodds))
cat(sprintf("  Causal OR = %.3f  ->  E-value = %.2f\n", ev$odds_ratio, ev$evalue))
cat("  ", ev$interpretation, "\n", sep = "")

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 80), "\n", sep = "")
cat("Complete. Key results:\n")
cat(sprintf("  True marginal ATE      : %+6.2f pp\n", comparison$ATE_pp[1]))
cat(sprintf("  Naive model ATE        : %+6.2f pp (wrong sign)\n", comparison$ATE_pp[2]))
cat(sprintf("  Causal model ATE       : %+6.2f pp (recovers truth)\n", comparison$ATE_pp[3]))
cat(sprintf("  Bootstrap 95%% CI       : [%.2f, %.2f] pp\n", boot_ci$CI_Lower, boot_ci$CI_Upper))
cat(sprintf("  E-value                : %.2f\n", ev$evalue))
cat(strrep("=", 80), "\n")
