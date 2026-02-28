library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(broom)

#' Define and Plot the Causal DAG
#'
#' Formalizes our business assumptions about the Data Generating Process.
#' 
#' @return A list containing the dagitty object and a ggplot diagram.
define_causal_dag <- function() {
  
  # Define the structural equations
  delivery_dag <- dagitty('dag {
    "Exception" [exposure, pos="1,2"]
    "Churn" [outcome, pos="4,2"]
    "Support_Ticket" [pos="2.5,1"]
    "U_Impatience" [unobserved, pos="2.5,3"]
    "Tenure" [pos="1.5,2.7"] 
    
    "Exception" -> "Support_Ticket"
    "Exception" -> "Churn"
    "U_Impatience" -> "Support_Ticket"
    "U_Impatience" -> "Churn"
    "Tenure" -> "Churn"
  }')
  
  # Plot the DAG
  p <- ggdag(delivery_dag, node = FALSE, text_col = "black", text_size = 4.5) +
    theme_dag_blank() +
    labs(
      title = "Causal DAG: The Support Ticket Trap",
      subtitle = "Support_Ticket is a Collider. Conditioning on it opens a backdoor path."
    )
  
  return(list(dag = delivery_dag, plot = p))
}

#' Get the Mathematical Adjustment Set
#'
#' Asks dagitty what variables we MUST include to find the true causal effect.
#' 
#' @param dag_obj The dagitty object from define_causal_dag()
get_adjustment_strategy <- function(dag_obj) {

  adjustment_sets <- adjustmentSets(dag_obj, exposure = "Exception", outcome = "Churn")
  
  return(adjustment_sets)
}