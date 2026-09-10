library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(broom)

if (!identical(getOption("ix_talk_theme_loaded"), TRUE)) {
  source(if (requireNamespace("here", quietly = TRUE))
    here::here("R", "00_theme_talk.R") else "R/00_theme_talk.R")
}

#' Define and Plot the Causal DAG
#'
#' Formalizes the business assumptions about the Data Generating Process.
#'
#' The DAG encodes two classic traps:
#'   - SIMPSON'S PARADOX: Order_Volume -> Exception and Order_Volume -> Churn,
#'     so Order_Volume is a CONFOUNDER (a backdoor path). It MUST be adjusted for.
#'   - COLLIDER BIAS: Exception -> Support_Ticket <- Impatience, so Support_Ticket
#'     is a COLLIDER. It must NOT be adjusted for.
#'
#' Impatience is LATENT (unobserved). It is drawn dashed/conceptually because it
#' is a parent of the collider and an unmeasured cause of churn.
#'
#' @return A list containing the dagitty object and a ggplot diagram.
define_causal_dag <- function() {

  delivery_dag <- dagitty('dag {
    "Order_Volume" [pos="0,1"]
    "Exception" [exposure, pos="1,2"]
    "Churn" [outcome, pos="4,2"]
    "Support_Ticket" [pos="2.5,1"]
    "Impatience" [latent, pos="2.5,3"]
    "Spend" [pos="1,0"]

    "Order_Volume" -> "Exception"
    "Order_Volume" -> "Churn"
    "Exception" -> "Churn"
    "Exception" -> "Support_Ticket"
    "Impatience" -> "Support_Ticket"
    "Impatience" -> "Churn"
    "Spend" -> "Churn"
  }')

  # Dark-slide render: white Lato node labels on the #1B1B1B canvas, faint
  # edges. Built from tidy_dagitty() directly so every color and font is under
  # our control (the pre-baked ggdag() defaults assume a white background).
  td <- tidy_dagitty(delivery_dag)

  p <- ggplot(td, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(edge_colour = ix_faint) +
    geom_dag_text(aes(label = name), colour = ix_white,
                  family = ix_font, size = 3.8) +
    theme_dag_blank() +
    labs(title = "Causal DAG") +
    theme(
      plot.background = element_rect(fill = ix_black, colour = ix_black),
      plot.title = element_text(family = ix_font_title, colour = ix_white,
                                size = 22, hjust = 0.5,
                                margin = margin(b = 10, t = 6))
    )

  list(dag = delivery_dag, plot = p)
}

#' Get the Mathematical Adjustment Set
#'
#' Asks dagitty which variables we MUST include to identify the true causal
#' effect of Exception on Churn. For this DAG the minimal sufficient adjustment
#' set is { Order_Volume } -- and notably it does NOT include Support_Ticket.
#'
#' @param dag_obj The dagitty object from define_causal_dag()
get_adjustment_strategy <- function(dag_obj) {
  adjustmentSets(dag_obj, exposure = "Exception", outcome = "Churn")
}
