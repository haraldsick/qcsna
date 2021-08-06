#' @title create_qcadata
#'
#' @description Function to create a basic data set of cases and outcome.
#'
#' @param network An object of class 'network'
#' @param attr_case The node-level attributes that are the cases (usually vertex.names)
#' @param attr_outcome The node_level attributes that are defined as the outcome
#' @param b2 Use b2 if the cases and the outcome are on the second level (sometimes referred as the
#' event level) of a bipartite network. Otherwise leave empty.
#'
#' @return Creates a data set (tibble) with two columns ('Cases' and the corresponding 'Outcome')
#'
#' @details Creating this data set and defining cases and outcome is always the first step.
#' Other variables or network-indices can be added in subsequent steps.
#' Please note that the tibble format is used due to convenience. After adding the desired variables
#' the qcadata-tibble can be finalized/converted with the finalize_qcadata-function!
#'
#' @export

create_qcadata <- function(network, attr_case, attr_outcome, b2 = FALSE) {
  if (b2 == TRUE) {
    for (i in 1:length(get.vertex.attribute(network, attr_case))) {
      b <- 1:i
    }
    b <- b[-c(1:get.network.attribute(network, "bipartite"))]
    b2 <- get.vertex.attribute(network, attr_case)
    b2 <- b2[b]
    tibble(
      Cases = b2,
      Outcome = get.vertex.attribute(network, attr_outcome)[b]
    )
  } else {
    for (i in 1:length(get.vertex.attribute(network, attr_case))) {
      a <- 1:i
    }
    a <- a[1:get.network.attribute(network, "bipartite")]
    b1 <- get.vertex.attribute(network, attr_case)
    b1 <- b1[a]
    tibble(
      Cases = b1,
      Outcome = get.vertex.attribute(network, attr_outcome)[a]
    )
  }
}
