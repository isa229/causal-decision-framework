library(ggplot2)
library(dplyr)
library(patchwork)


if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  source(if (requireNamespace("here", quietly = TRUE))
    here::here("R", "00_theme_talk.R") else "R/00_theme_talk.R")
}

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
        "Naive Model",
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
    geom_hline(yintercept = 0, linetype = "dashed", color = ix_faint, linewidth = 0.5) +
    geom_hline(yintercept = truth_val, linetype = "solid", color = ix_green,
               linewidth = 0.8, alpha = 0.6) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = Label, y = ATE_pp + sign(ATE_pp) * 1.5),
              colour = ix_white, family = ix_font, size = 5, fontface = "bold") +
    scale_fill_manual(
      values = c("truth" = ix_green, "wrong" = ix_red, "correct" = ix_blue),
      guide = "none"
    ) +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
    labs(
      title = "DAG-Guided Model Recovers the True Causal Effect",
      x = NULL,
      y = "Percentage Points"
    ) +
    theme_talk()
}

#' Plot Coefficient Trajectories Across Model Specifications (Log-Odds)
#'
#' Shows how the `has_exception` estimate moves as we change the adjustment set.
#' Demonstrates that ONLY the DAG-guided specification (add confounder, drop
#' collider) recovers the correct positive sign.
#'
#' Specifications (impatience is LATENT and never available):
#'   - "Aggregate"        : churn ~ exception                 (Simpson's: wrong sign)
#'   - "Naive ML"         : every observed feature            (kitchen-sink; the
#'                         collider is its only causal error: wrong sign)
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

    "Naive ML" = glm(churn_numeric ~ has_exception + order_volume +
                       monthly_spend_usd + opened_support_ticket +
                       customer_age_years + marketing_emails_clicked +
                       app_logins_last_7_days,
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
    geom_hline(yintercept = 0.5, linetype = "dashed", color = ix_green, linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = ix_faint) +
    geom_line(linewidth = 1.2, color = ix_faint) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2, linewidth = 1) +
    scale_color_manual(
      values = c("correct" = ix_blue, "wrong" = ix_red),
      guide = "none"
    ) +
    labs(
      title = "Exception Effect Across Specifications",
      x = NULL,
      y = "Log-Odds"
    ) +
    theme_talk() +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
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
    scale_color_manual(values = c("No Exception" = ix_blue, "Delivery Exception" = ix_red)) +
    scale_size_continuous(range = c(2, 8), guide = "none") +
    labs(
      title = "Churn Rate by Order-Volume Decile",
      x = "Order Volume Decile",
      y = "Churn Rate (%)",
      color = NULL
    ) +
    theme_talk()
}
