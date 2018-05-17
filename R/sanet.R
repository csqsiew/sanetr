#' This function takes in a dataframe with 'node' and 'activation' columns, and
#' calls the spread.parallel() function for a specific number of times to simulate the spread of
#' activation in a network.
#'
#' @param start_run A non-empty dataframe with 'node' and 'activation' columns. Must be specified.
#' @param decay Proportion of activation that is lost at each time step. Ranges from 0 to 1. Default is 0.2.
#' @param retention Proportion of activation that remains in the node from which activation is being spread from. Ranges from 0 to 1. Default is 0.8.
#' @param suppress Suppress nodes with a final activation of < x units at each time step to an activation value of 0. Suggested value of x is 0.1 percent of total initial activation at t = 0 in order to speed up simulations substantially. Default is 0.1.
#' @param network Network where the spreading occurs. Must be specified. Must be an igraph object with a "name" property.
#' @param time Number of time steps to run the spread() function for. Default is 10.
#' @return A compiled dataframe with 'node', 'activation' and 'time' columns showing the spread of activation in the network over time.
#' @examples
#' See Vignette for examples.

sanet <- function(start_run, decay = 0.2, retention = 0.8, suppress = 0.1, network, time = 10) {
  # start_run = a non-empty dataframe with 'node' and 'activation' columns
  # decay = proportion of activation that is lost over time, ranges from 0 to 1
  # decay value default = 0.2
  # retention = proportion of activation that remains in the originator node, ranges from 0 to 1
  # retention value default = 0.8
  # suppress = nodes with activation less than the suppress value will be suppressed to activation of 0 at each time step
  # suppress value default = 0.1 (0.1 percent of 100 units)
  # network = network where the spread of activation occurs in
  # time = number of time steps to run spread() for
  # time value default = 10

  # check if start_run is in the correct format
  if (is.data.frame(start_run) == F || colnames(start_run) != c('node', 'activation')) {
    stop('Initial activation dataframe is not in the correct format. Must be a dataframe with -node-
         and -activation- columns.')
  }

  # check if the node column in start_run is a factor (if so, it must be converted to character class)
  if (is.factor(start_run$node) == T) {
    start_run$node <- as.character(start_run$node)
  }

  # check if decay is a number from 0 to 1
  if (decay < 0 || decay > 1) {
    stop('Decay value is not a number from 0 to 1.')
  }

  # check if retention is a number from 0 to 1
  if (retention < 0 || retention > 1) {
    stop('Retention value is not a number from 0 to 1.')
  }

  # check if time is a non-negative number
  if (time < 0 || is.numeric(time) == F) {
    stop('Something is off with the time value.')
  }

  # check if network is an igraph object
  if (igraph::is.igraph(network) == F) {
    stop('Network is not an igraph object.')
  }

  # check if the igraph object has a name attribute
  if (is.null(igraph::V(network)$name) == T) {
    stop('Network does not have a "name" attribute.')
  }

  # create an empty dataframe to store output
  output <- data.frame(node = vector(), activation = vector(), time = vector(),
                       stringsAsFactors=FALSE)

  for (t in 1:time) {

    updated <- spread.parallel(start_run, decay, retention, suppress, network)

    if (nrow(updated) > 0) {
      # if updated is not empty, save the updated output
      updated$time <- t
      output <- rbind(output, updated)
      # updated is now the new input (start_run)
      start_run <- updated
    } else {
      print('Spread terminated due to low activations.')
      return(output)
    }
  }
  return(output)
  }

