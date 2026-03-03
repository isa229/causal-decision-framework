# Load required libraries
library(ggplot2)
library(here)
source(here::here("R", "01_define_dag.R"))
source(here::here("R", "02_simulate_data.R"))
source(here::here("R", "03_naive_models.R"))
source(here::here("R", "04_causal_models.R"))

# ------------------------------------------------------------------------------
# 1. Setup Environment
# ------------------------------------------------------------------------------
figures_dir <- here::here("figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Created 'figures/' directory.\n")
}

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
# 3. Simulate the Data (The Engine)
# ------------------------------------------------------------------------------
cat("Step 2/4: Simulating 50,000 enterprise customers...\n")
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

# ------------------------------------------------------------------------------
# 4. Fit Naive Models (The Trap)
# ------------------------------------------------------------------------------
cat("Step 3/4: Fitting Naive ML Models (Conditioning on Collider)...\n")
naive_models <- fit_naive_models(df)

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
cat("Step 4/4: Fitting Causal ML Models (Dropping the Collider)...\n")
causal_models <- fit_causal_models(df)

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

cat("\n Success! All 5 figures are ready in the 'figures/' folder.\n")