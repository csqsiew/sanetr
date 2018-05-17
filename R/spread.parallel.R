#' This function takes in a dataframe with 'node' and 'activation' columns, and
#' simulates the spread of activation of nodes listed in the 'node' column to its
#' neighbors in the specified network.
#'
#' @param start_run A non-empty dataframe with 'node' and 'activation' columns.
#' @param decay Proportion of activation that is lost at each time step. Ranges from 0 to 1.
#' @param retention Proportion of activation that remains in the node from which activation is being spread from. Ranges from 0 to 1.
#' @param suppress Suppress nodes with a final activation of < x units at each time step to an activation value of 0. Suggested value of x is 0.1 percent of total initial activation at t = 0 in order to speed up simulations substantially.
#' @param network Network where the spreading occurs.
#' @return A updated dataframe with 'node' and 'activation' columns with new activation values.
#' @examples
#' See Vignette for examples.

spread.parallel <- function(start_run, decay, retention, suppress, network) {
  # start_run = a non-empty dataframe with 'node' and 'activation' columns
  # start_run dataframe must be specified in the butter.parallel() function
  # decay = proportion of activation that is lost at each time step, ranges from 0 to 1
  # decay value must be specified in the butter.parallel() function
  # retention = proportion of activation that remains in the node, ranges from 0 to 1
  # retention value must be specified in the butter.parallel() function
  # suppress = nodes with activation less than the suppress value will have their activations suppressed to 0
  # suppress value must be specified in the butter.parallel() function
  # network = network where the spreading occurs
  # network must be specified in the butter.parallel() function

  ### START PARALLEL CODE

  # initialize
  # Create cluster with desired number of cores
  ncores <- parallel::detectCores()
  cl <- parallel::makeCluster(ncores)
  # Register cluster
  doParallel::registerDoParallel(cl)

  # create parallel functions
  x <- foreach::foreach(i = 1:nrow(start_run), .combine = rbind,
               .packages = c('dplyr', 'samr', 'igraph', 'foreach')) %dopar% {
  # create an empty dataframe to store output
  end_run <- data.frame(node = character(), activation = numeric(), stringsAsFactors=FALSE)

  # get a list of neighbors of originator node from the network
  neighborlist <- igraph::neighbors(network, start_run$node[i])$name

  if(length(neighborlist) == 0) {
    stop('Error: Word has no neighbors/not found in network.') # this should NOT ever happen
  }

  # update the end_run dataframe with new activations

  # if retention > 0, the originator node will keep a proportion of the activation
  if (retention > 0) { # if retention = 0, do not run this chunk of code as originator node does not keep any activation
    # for the target/originator node:
    output <- c(as.character(start_run$node[i]), start_run$activation[i]*retention)
    # new activation = original activation * retention
    if (nrow(end_run) == 0) { # if end_run is an empty df
      end_run[1, ] <- output  # add to the first row
    } else {                  # if end_run is a non-empty df
      end_run <- rbind(end_run, output)  # just rbind as usual
    }
  }

  # for the neighbors:
  for (j in 1:length(neighborlist)) { # for each neighbor of the target/originator node
    output <- c(as.character(neighborlist[j]),
                (start_run$activation[i]*(1-retention))/length(neighborlist))
    # new activation = activation given up by the target node / number of neighbors to share activation with
    if (nrow(end_run) == 0) { # if end_run is an empty df
      end_run[1, ] <- output  # add to the first row
    } else {                  # if end_run is a non-empty df
      if (output[2] != 0) { # this code is needed on the off-chance that retention = 1 and the activation received by the neighbor node would be 0. so if output[2] == 0, do nothing...
        end_run <- rbind(end_run, output) } # just rbind as usual
    }
  }
  end_run
               }

  # close cluster
  parallel::stopCluster(cl)

  end_run <- x

  ### END PARALLEL CODE

  # after all nodes in start_run have 'spread' their activation,
  # clean up and consolidate end_run df
  end_run <- aggregate(as.numeric(activation) ~ node , data = end_run , FUN = sum)
  ## there used to be a bug here where aggregate doesn't work when nrow = 0, i.e., when words do not have activation (although theoretically this shouldn't ever happen)
  colnames(end_run) <- c('node', 'activation')

  # allow entire end_run dataframe to 'decay' based on decay value
  end_run$activation <- end_run$activation*(1 - decay)

  # suppress outputs that are less than a specified value to speed up the process
  end_run <- dplyr::filter(end_run, activation > suppress)

  # return the output
  return(as.data.frame(end_run))
}
