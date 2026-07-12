library(ggplot2)
library(dplyr)
library(patchwork)

#' Plot Ground Truth Comparison (Marginal ATE scale)
#'
#' Side-by-side comparison of the true ATE, the naive estimate (wrong sign), and
#' the DAG-guided causal estimate (recovers truth) -- all in percentage points.
#'
#' @param comparison_table Output from compare_to_ground_truth()
#' @return A ggplot object
plot_ground_truth_comparison <- function(comparison_table) {

  truth_val <- comparison_table$ATE_pp[comparison_table$Method == "Ground Truth (DGP)"]

  plot_data <- comparison_table |>
    mutate(
      Method = factor(Method, levels = c(
        "Ground Truth (DGP)",
        "Naive Model (Simpson + Collider)",
        "Causal Model (DAG-Guided)"
      )),
      Color = case_when(
        grepl("Ground Truth", Method) ~ "truth",
        grepl("Naive", Method) ~ "wrong",
        TRUE ~ "correct"
      ),
      Label = paste0(round(ATE_pp, 1), " pp")
    )

  ggplot(plot_data, aes(x = Method, y = ATE_pp, fill = Color)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_hline(yintercept = truth_val, linetype = "solid", color = "darkgreen",
               linewidth = 0.8, alpha = 0.6) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = Label, y = ATE_pp + sign(ATE_pp) * 1.5),
              size = 4, fontface = "bold") +
    scale_fill_manual(
      values = c("truth" = "#2E7D32", "wrong" = "#C62828", "correct" = "#1565C0"),
      guide = "none"
    ) +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
    labs(
      title = "Validation Against Ground Truth: Causal Framework Recovers True Effect",
      subtitle = paste(
        "Naive model (omits the confounder, keeps the collider) gets the WRONG SIGN.",
        "DAG-guided model recovers the true marginal effect (green line).",
        sep = "\n"
      ),
      x = NULL,
      y = "Effect of Delivery Exceptions on Churn (percentage points)",
      caption = "Ground truth is the marginal ATE computed from the data generating process."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.text.x = element_text(size = 10, face = "bold"),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
}

#' Plot Coefficient Trajectories Across Model Specifications (Log-Odds)
#'
#' Shows how the `has_exception` estimate moves as we change the adjustment set.
#' Demonstrates that ONLY the DAG-guided specification (add confounder, drop
#' collider) recovers the correct positive sign.
#'
#' Specifications (impatience is LATENT and never available):
#'   - "Aggregate"        : churn ~ exception                 (Simpson's: wrong sign)
#'   - "Naive ML"         : + support_ticket + spend          (collider too: wrong sign)
#'   - "DAG-Guided"       : + order_volume + spend, NO ticket (correct sign)
#'   - "Over-Adjusted"    : DAG-guided + support_ticket       (re-introduces collider bias)
#'
#' @param data The simulated dataset
#' @return A ggplot object
plot_coefficient_trajectory <- function(data) {

  data_numeric <- data |>
    mutate(churn_numeric = as.numeric(as.character(churned)))

  models <- list(
    "Aggregate" = glm(churn_numeric ~ has_exception,
                      data = data_numeric, family = binomial()),

    "Naive ML" = glm(churn_numeric ~ has_exception + opened_support_ticket + monthly_spend_usd,
                     data = data_numeric, family = binomial()),

    "DAG-Guided" = glm(churn_numeric ~ has_exception + order_volume + monthly_spend_usd,
                       data = data_numeric, family = binomial()),

    "Over-Adjusted" = glm(churn_numeric ~ has_exception + order_volume + monthly_spend_usd + opened_support_ticket,
                          data = data_numeric, family = binomial())
  )

  coefs <- purrr::map_dfr(names(models), function(name) {
    broom::tidy(models[[name]]) |>
      filter(term == "has_exception") |>
      mutate(Model = name)
  }) |>
    mutate(
      Model = factor(Model, levels = c("Aggregate", "Naive ML", "DAG-Guided", "Over-Adjusted")),
      Color = case_when(
        Model == "DAG-Guided" ~ "correct",
        TRUE ~ "wrong"
      )
    )

  ggplot(coefs, aes(x = Model, y = estimate, color = Color, group = 1)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    geom_line(linewidth = 1.2, color = "gray40") +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2, linewidth = 1) +
    scale_color_manual(
      values = c("correct" = "#1565C0", "wrong" = "#C62828"),
      guide = "none"
    ) +
    labs(
      title = "Effect Estimate Across Model Specifications",
      subtitle = "Only the DAG-guided model (add confounder, drop collider) recovers the truth.",
      x = NULL,
      y = "Effect of Exceptions on Churn (Log-Odds)",
      caption = "Green dashed line = true structural coefficient (0.5). Error bars are 95% CIs."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 15, hjust = 1, face = "bold"),
      panel.grid.major.x = element_blank()
    )
}

#' Plot Simpson's Paradox (Two-Line Decile Plot)
#'
#' THE signature figure. Customers are binned into order-volume deciles. For each
#' decile we plot the churn rate for customers WITH vs WITHOUT a delivery
#' exception. Two facts coexist (the definition of Simpson's Paradox):
#'
#'   1. WITHIN every decile, the "exception" line sits ABOVE the "no exception"
#'      line -> exceptions INCREASE churn (the truth).
#'   2. Exception customers cluster in the high-volume / low-churn deciles, so the
#'      MARGINAL comparison reverses -> exceptions look protective (the illusion).
#'
#' Point size encodes how many exception customers fall in each decile, making
#' the confounding (where the exposed mass lives) visually obvious.
#'
#' @param data The simulated dataset
#' @return A ggplot object
plot_simpsons_paradox <- function(data) {

  d <- data |>
    mutate(
      churn_numeric = as.numeric(as.character(churned)),
      volume_decile = cut(
        order_volume,
        breaks = quantile(order_volume, seq(0, 1, 0.1)),
        include.lowest = TRUE,
        labels = FALSE
      )
    )

  by_decile <- d |>
    group_by(volume_decile, has_exception) |>
    summarise(churn_rate = mean(churn_numeric) * 100, n = dplyr::n(), .groups = "drop") |>
    mutate(Exception = factor(
      has_exception, levels = c(0, 1),
      labels = c("No Exception", "Delivery Exception")
    ))

  agg <- d |>
    group_by(has_exception) |>
    summarise(churn_rate = mean(churn_numeric) * 100, .groups = "drop")
  agg_diff <- agg$churn_rate[agg$has_exception == 1] - agg$churn_rate[agg$has_exception == 0]

  ggplot(by_decile, aes(x = volume_decile, y = churn_rate, color = Exception)) +
    geom_line(linewidth = 1.2) +
    geom_point(aes(size = n), alpha = 0.85) +
    scale_x_continuous(breaks = 1:10) +
    scale_color_manual(values = c("No Exception" = "#1565C0", "Delivery Exception" = "#C62828")) +
    scale_size_continuous(range = c(2, 8), guide = "none") +
    labs(
      title = "Simpson's Paradox: Within Every Group, Exceptions Increase Churn",
      subtitle = paste0(
        "WITHIN each volume decile the red (exception) line is ABOVE blue -> exceptions raise churn.\n",
        "But exception customers cluster in high-volume/low-churn deciles, so the AGGREGATE reverses (",
        round(agg_diff, 1), " pp)."
      ),
      x = "Order Volume Decile (1 = lowest, 10 = highest)",
      y = "Churn Rate (%)",
      color = NULL,
      caption = "Point size = number of customers. Order volume is the confounder; adjusting for it reveals the truth."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}
