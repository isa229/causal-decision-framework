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