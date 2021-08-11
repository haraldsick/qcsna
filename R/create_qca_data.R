#' @title create_qca_data
#'
#' @description Function to create a basic dataset of cases and outcome.
#'
#' @param network An object of class 'network'.
#' @param attr_case The node-level attributes that are the cases (usually vertex.names).
#' @param attr_outcome The node_level attributes that are defined as the outcome.
#' @param bipartite If the network is bipartite, select the level the cases and the
#' outcome are on. If they are on the first level (sometimes referred as the
#' actor level) of a bipartite network, choose 'b1', if they are on the second level
#' (sometimes referred as the event level), choose 'b2'.
#'
#' @return Creates a data set (tibble) with two columns ('Cases' and the corresponding 'Outcome').
#'
#' @details Creating this data set and defining cases and outcome is always the first step.
#' Other variables or network-indices can be added in subsequent steps.
#' Please note that the tibble format is used due to convenience. After adding the desired variables,
#' the qcadata-tibble can be finalized/converted with the finalize_qcadata-function!
#'
#' @examples qca_data <- create_qca_data(network, attr_cases = "vertex.names", attr_outcome = "Type", bipartite = "b2")
#'
#'
#' @importFrom network get.vertex.attribute
#' @importFrom network get.network.attribute
#' @importFrom tibble tibble
#'
#' @export

create_qca_data <- function(network, attr_case, attr_outcome, bipartite = NULL) {
  if(is.null(bipartite)){
    tibble(
      Cases = network::get.vertex.attribute(network, attr_case),
      Outcome = network::get.vertex.attribute(network, attr_outcome)
    )
  }
  else if (bipartite == "b2") {
    for (i in 1:length(network::get.vertex.attribute(network, attr_case))) {
      b <- 1:i
    }
    b <- b[-c(1:network::get.network.attribute(network, "bipartite"))]
    b2 <- network::get.vertex.attribute(network, attr_case)
    b2 <- b2[b]
    tibble(
      Cases = b2,
      Outcome = network::get.vertex.attribute(network, attr_outcome)[b]
    )
  }
  else if (bipartite == "b1") {
    for (i in 1:length(network::get.vertex.attribute(network, attr_case))) {
      a <- 1:i
    }
    a <- a[1:network::get.network.attribute(network, "bipartite")]
    b1 <- network::get.vertex.attribute(network, attr_case)
    b1 <- b1[a]
    tibble(
      Cases = b1,
      Outcome = network::get.vertex.attribute(network, attr_outcome)[a]
    )
  }
  else {
    print("No valid network-level selected! If the network is bipartite, select 'b1' or 'b2', otherwise leave empty")
  }
}
