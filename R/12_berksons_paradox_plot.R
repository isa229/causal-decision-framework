#!/usr/bin/env Rscript
# Generate the Berkson's Paradox (collider bias) figure.
#
# ONE panel, TWO lines -- the only difference between them is conditioning on
# the collider (opened_support_ticket):
#
#   Blue line -- "All customers": exception rate by impatience decile.
#     FLAT: exceptions and impatience are marginally INDEPENDENT in the DGP
#     (exceptions depend only on order volume). This is the "even when they
#     are not" of the talk.
#
#   Red line -- "Opened a ticket": the same rate among ticket-openers only.
#     STEEP DECLINE: filtering on the collider manufactures an association
#     between its two causes. Calm ticket-openers almost always had an
#     exception; impatient ticket-openers mostly did not -- because according
#     to the DAG, that is "the only other way into the ticket".
#
# NOTE: this figure is only possible because the simulation can return the
# LATENT `impatience` (include_latent = TRUE). In a real dataset this picture
# is invisible -- which is exactly why the collider is silent.

library(ggplot2)
library(dplyr)

if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  source(if (requireNamespace("here", quietly = TRUE))
    here::here("R", "00_theme_talk.R") else "R/00_theme_talk.R")
}

source("R/02_simulate_data.R")

cat("Generating Berkson's Paradox (collider bias) figure...\n")

# ------------------------------------------------------------------------------
# 1. Simulate data with the latent trait made visible (figure-only option)
# ------------------------------------------------------------------------------
df <- simulate_delivery_data(n_customers = 50000, seed = 2026,
                             include_latent = TRUE)

# ------------------------------------------------------------------------------
# 2. Exception rate by impatience decile, with and without the collider filter
# ------------------------------------------------------------------------------
exception_rate_by_decile <- function(data) {
  data |>
    mutate(imp_d = ntile(impatience, 10)) |>
    group_by(imp_d) |>
    summarise(rate = mean(has_exception), .groups = "drop")
}

all_customers  <- exception_rate_by_decile(df) |>
  mutate(group = "All customers")
ticket_openers <- exception_rate_by_decile(
  df |> filter(opened_support_ticket == 1)) |>
  mutate(group = "Opened a ticket")

combined <- bind_rows(all_customers, ticket_openers) |>
  mutate(group = factor(group, levels = c("All customers", "Opened a ticket")))

cat("  All customers   exception rate by impatience decile:\n")
cat("   ", paste(sprintf("D%02d: %.3f", all_customers$imp_d,
                         all_customers$rate), collapse = "  "), "\n")
cat("  Ticket-openers  exception rate by impatience decile:\n")
cat("   ", paste(sprintf("D%02d: %.3f", ticket_openers$imp_d,
                         ticket_openers$rate), collapse = "  "), "\n")

# ------------------------------------------------------------------------------
# 3. Single-panel figure: two lines; the gap between them IS the paradox
# ------------------------------------------------------------------------------
# Label only the red line's endpoints (first above the point, last below) so
# the collapse reads numerically without cluttering the trend.
endpoints <- combined |>
  filter(group == "Opened a ticket", imp_d %in% c(1, 10)) |>
  mutate(label_y = rate + ifelse(imp_d == 1, 0.05, -0.07))

fig <- ggplot(combined, aes(x = imp_d, y = rate, colour = group)) +
  geom_line(linewidth = 1.6) +
  geom_point(size = 3.4) +
  geom_text(
    data = endpoints,
    aes(x = imp_d, y = label_y, label = sprintf("%.0f%%", 100 * rate)),
    colour = ix_white, family = ix_font, size = 4.5, fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("All customers" = ix_blue, "Opened a ticket" = ix_red)
  ) +
  scale_x_continuous(breaks = 1:10, expand = expansion(mult = 0.03)) +
  scale_y_continuous(limits = c(0, 1.02),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Berkson's Paradox: Filter on the Ticket, Its Causes Look Related",
    x = "Impatience (calm \u2192 impatient)",
    y = "Exception rate"
  ) +
  theme_talk()

ggsave("figures/02b_berksons_paradox.png", fig,
       width = 10, height = 6, dpi = 300, bg = ix_black)
cat("Saved: figures/02b_berksons_paradox.png\n")
cat("Done.\n")
