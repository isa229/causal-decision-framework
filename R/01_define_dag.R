library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(broom)

#' Define and Plot the Causal DAG
#'
#' Formalizes the business assumptions about the Data Generating Process
#'
#' The DAG encodes two classic traps:
#'   - SIMPSON'S PARADOX: Order_Volume -> Exception and Order_Volume -> Churn,
#'     so Order_Volume is a CONFOUNDER (a backdoor path). It MUST be adjusted for.
#'   - COLLIDER BIAS: Exception -> Support_Ticket <- Impatience, so Support_Ticket
#'     is a COLLIDER. It must NOT be adjusted for.
#'
#' Impatience is LATENT (unobserved). For this case, it is drawn dashed/conceptually because it
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

  p <- ggdag(delivery_dag, node = FALSE, text_col = "black", text_size = 3.6) +
    theme_dag_blank() +
    labs(
      title = "Causal DAG: Two Traps in One Business Question",
      subtitle = paste(
        "Order_Volume is a CONFOUNDER (must ADD it) -> Simpson's Paradox.",
        "Support_Ticket is a COLLIDER (must REMOVE it) -> spurious correlation.",
        sep = "\n"
      )
    )

  list(dag = delivery_dag, plot = p)
}

#' Get the Mathematical Adjustment Set
#'
#' Asks dagitty which variables we MUST include to identify the true causal
#' effect of Exception on Churn. For this DAG the minimal sufficient adjustment
#' set is { Order_Volume }, and notably it does NOT include Support_Ticket.
#'
#' @param dag_obj The dagitty object from define_causal_dag()
get_adjustment_strategy <- function(dag_obj) {
  adjustmentSets(dag_obj, exposure = "Exception", outcome = "Churn")
}
