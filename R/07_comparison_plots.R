library(ggplot2)
library(dplyr)
library(patchwork)

#' Plot Ground Truth Comparison
#'
#' Creates a side-by-side comparison showing:
#' - Ground truth from DGP
#' - Naive estimate (wrong direction)
#' - Causal estimate (recovers truth)
#' 
#' 
#' @param comparison_table Output from compare_to_ground_truth()
#' @return A ggplot object
plot_ground_truth_comparison <- function(comparison_table) {
  
  # Add visual indicators
  plot_data <- comparison_table |>
    mutate(
      Method = factor(Method, levels = c("Ground Truth (DGP)", 
                                         "Naive Model (w/ Collider)", 
                                         "Causal Model (DAG-Guided)")),
      Color = case_when(
        Method == "Ground Truth (DGP)" ~ "truth",
        Method == "Naive Model (w/ Collider)" ~ "wrong",
        Method == "Causal Model (DAG-Guided)" ~ "correct"
      ),
      Label = paste0(round(Estimate, 3), 
                     ifelse(!is.na(Std_Error), 
                            paste0("\n(SE: ", round(Std_Error, 3), ")"),
                            ""))
    )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Method, y = Estimate, fill = Color)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_hline(yintercept = 0.5, linetype = "solid", color = "darkgreen", 
               linewidth = 0.8, alpha = 0.6) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_errorbar(aes(ymin = Estimate - 1.96 * Std_Error, 
                      ymax = Estimate + 1.96 * Std_Error),
                  width = 0.2, linewidth = 0.8, na.rm = TRUE) +
    geom_text(aes(label = Label, y = Estimate + sign(Estimate) * 0.15), 
              size = 3.5, fontface = "bold") +
    scale_fill_manual(
      values = c("truth" = "#2E7D32", "wrong" = "#C62828", "correct" = "#1565C0"),
      guide = "none"
    ) +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
    labs(
      title = "Validation Against Ground Truth: Causal Framework Recovers True Effect",
      subtitle = "The naive model (conditioning on collider) gives the WRONG direction.\nThe DAG-guided model recovers the true causal effect (green line = 0.5).",
      x = NULL,
      y = "Effect of Delivery Exceptions on Churn (Log-Odds)",
      caption = "Error bars show 95% confidence intervals. Ground truth is from data generating process."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.text.x = element_text(size = 10, face = "bold"),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  
  return(p)
}



#' Plot Bias Comparison
#'
#' Shows the magnitude of bias for each method
#' 
#' @param comparison_table Output from compare_to_ground_truth()
#' @return A ggplot object
plot_bias_comparison <- function(comparison_table) {
  
  plot_data <- comparison_table |>
    filter(Method != "Ground Truth (DGP)") |>
    mutate(
      Method = gsub(" \\(.*\\)", "", Method),
      Abs_Pct_Bias = abs(Pct_Bias),
      Label = paste0(round(Pct_Bias, 1), "%")
    )
  
  p <- ggplot(plot_data, aes(x = reorder(Method, -Abs_Pct_Bias), y = Pct_Bias)) +
    geom_col(aes(fill = Abs_Pct_Bias > 50), width = 0.6, alpha = 0.9) +
    geom_text(aes(label = Label), vjust = -0.5, fontface = "bold", size = 4) +
    scale_fill_manual(
      values = c("TRUE" = "#C62828", "FALSE" = "#1565C0"),
      guide = "none"
    ) +
    labs(
      title = "Percent Bias Relative to Ground Truth",
      subtitle = "Naive model has massive bias; DAG-guided model is nearly unbiased",
      x = NULL,
      y = "Percent Bias (%)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}


#' Create Comprehensive Validation Dashboard
#'
#' Combines multiple diagnostic plots into a single figure
#' 
#' @param comparison_table Output from compare_to_ground_truth()
#' @param naive_workflows Fitted naive models
#' @param causal_workflows Fitted causal models
#' @return A patchwork composite plot
create_validation_dashboard <- function(comparison_table, naive_workflows, causal_workflows) {
  
  # Main comparison
  p_main <- plot_ground_truth_comparison(comparison_table)
  
  # Create a summary table plot
  table_data <- comparison_table |>
    mutate(
      Estimate = round(Estimate, 3),
      Std_Error = round(Std_Error, 3),
      Bias = round(Bias, 3),
      Pct_Bias = paste0(round(Pct_Bias, 1), "%")
    ) |>
    select(Method, Estimate, Std_Error, Bias, Pct_Bias, Direction)
  
  # Create table grob
  p_table <- gridExtra::tableGrob(
    table_data,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core = list(fg_params = list(hjust = 0, x = 0.1, fontsize = 9)),
      colhead = list(fg_params = list(fontface = "bold", fontsize = 10))
    )
  )
  
  # Combine with patchwork
  combined <- p_main / 
    patchwork::wrap_elements(p_table) +
    plot_layout(heights = c(3, 1.5))
  
  return(combined)
}


#' Plot Coefficient Trajectories Across Methods
#'
#' Shows how the coefficient estimate changes as we add/remove variables
#' 
#' @param data The simulated dataset
#' @return A ggplot object showing coefficient paths
plot_coefficient_trajectory <- function(data) {
  
  # Convert churned to numeric
  data_numeric <- data |>
    mutate(churn_numeric = as.numeric(as.character(churned)))
  
  # Fit a series of models with different adjustments
  models <- list(
    "Unadjusted" = glm(churn_numeric ~ has_exception, 
                       data = data_numeric, family = binomial()),
    
    "With Collider" = glm(churn_numeric ~ has_exception + opened_support_ticket, 
                          data = data_numeric, family = binomial()),
    
    "DAG-Guided" = glm(churn_numeric ~ has_exception + impatience_score + 
                        account_tenure_months + monthly_spend_usd,
                      data = data_numeric, family = binomial()),
    
    "Over-Adjusted" = glm(churn_numeric ~ has_exception + opened_support_ticket + 
                           impatience_score + account_tenure_months + monthly_spend_usd,
                         data = data_numeric, family = binomial())
  )
  
  # Extract coefficients
  coefs <- purrr::map_dfr(names(models), function(name) {
    broom::tidy(models[[name]]) |>
      filter(term == "has_exception") |>
      mutate(Model = name)
  }) |>
    mutate(
      Model = factor(Model, levels = c("Unadjusted", "With Collider", 
                                       "DAG-Guided", "Over-Adjusted")),
      Color = case_when(
        Model == "DAG-Guided" ~ "correct",
        Model == "With Collider" ~ "wrong",
        Model == "Over-Adjusted" ~ "wrong",
        TRUE ~ "neutral"
      )
    )
  
  # Create plot
  p <- ggplot(coefs, aes(x = Model, y = estimate, color = Color, group = 1)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    geom_line(linewidth = 1.2, color = "gray40") +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2, linewidth = 1) +
    scale_color_manual(
      values = c("correct" = "#1565C0", "wrong" = "#C62828", "neutral" = "#FFA726"),
      guide = "none"
    ) +
    labs(
      title = "Effect Estimate Across Different Model Specifications",
      subtitle = "Only the DAG-guided model (adjusting for confounders, avoiding collider) recovers truth",
      x = NULL,
      y = "Effect of Exceptions on Churn (Log-Odds)",
      caption = "Green dashed line shows ground truth (0.5). Error bars are 95% CIs."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}
