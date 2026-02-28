# Load required libraries
library(ggplot2)
library(dplyr)
library(here)
source(here::here("R", "02_simulate_data.R")) 
source(here::here("R", "03_naive_models.R"))

# Ensure the output directory exists
figures_dir <- here::here("figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Created 'figures/' directory.\n")
}

# Execute the pipeline
cat("1/3: Generating simulated customer data...\n")
df <- simulate_delivery_data(n_customers = 50000, seed = 2026)

cat("2/3: Fitting naive ML workflows (GLM, RF, XGBoost)...\n")
my_models <- fit_naive_models(df)

cat("3/3: Generating and saving plots...\n")

# Generate the plots using custom functions
glm_plot <- plot_glm_trap(my_models)
xgb_plot <- plot_xgb_vip(my_models)

# Save pngs
ggsave(
  filename = here::here("figures", "01_glm_trap_coefficients.png"), 
  plot = glm_plot, 
  width = 8, 
  height = 6, 
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = here::here("figures", "02_xgb_feature_importance.png"), 
  plot = xgb_plot, 
  width = 8, 
  height = 6, 
  dpi = 300,
  bg = "white"
)

cat("All methodological trap figures have been saved to the 'figures/' folder.\n")