#' @title create_qca_data
#'
#' @description Function to create a basic dataset of cases and outcome.
#'
#' @param network An object of class 'network'.
#' @param attr_case The node-level attributes that are the cases (usually 'vertex.names').
#' @param attr_outcome This argument is optional. If the outcome is a node_level attribute, it can be defined as the outcome already.
#' A according column named 'Output' will be added to the dataset. If an other output, like an node-level indice or another network
#' configuration shall be the outcome, just add them with one of the other functions of this package (or manually) and define them
#' as the outcome in the QCA package. Otherwise omit it.
#' @param bipartite If the network is bipartite, select the level the cases and the
#' outcome are on. If they are on the first level (sometimes referred as the
#' actor level) of a bipartite network, choose 'bipartite = "b1"', if they are on the second level
#' (sometimes referred as the event level), choose 'bipartite = "b2"'. Otherwise omit it.
#'
#' @return Creates a data set (tibble) with two columns ('Cases' and the corresponding 'Outcome').
#'
#' @details Creating this data set and defining cases and outcome is always the first step.
#' Other variables or network-indices can be added in subsequent steps.
#' Please note that the tibble format is used due to convenience. After adding the desired variables,
#' the QCA-data-tibble can be finalized/converted with the finalize_qca_data-function!
#'
#' @examples qca_data <- create_qca_data(network, attr_cases = "vertex.names", attr_outcome = "Type", bipartite = "b2")
#'
#'
#' @importFrom network get.vertex.attribute
#' @importFrom network get.network.attribute
#' @importFrom network is.bipartite
#' @importFrom tibble tibble
#'
#' @references Dusa, Adrian (2019). QCA with R. A Comprehensive Resource. Springer International Publishing.
#'
#' @export


create_qca_data <- function(network,
                            attr_case,
                            attr_outcome = NULL,
                            bipartite = NULL) {
  if (!network::is.bipartite(network)) {
    if (is.null(attr_outcome)) {
      tibble(Cases = network::get.vertex.attribute(network, attr_case))
    } else {
      tibble::tibble(
        Cases = network::get.vertex.attribute(network, attr_case),
        Outcome = network::get.vertex.attribute(network, attr_outcome)
      )
    }
  } else {
    if (is.null(bipartite)) {
      print ("Network is bipartite. Please select a level with 'b1' or 'b2'")
    }
    else if (bipartite == "b2") {
      if (is.null(attr_outcome)) {
        for (i in 1:length(network::get.vertex.attribute(network, attr_case))) {
          b <- 1:i
        }
        b <-
          b[-c(1:network::get.network.attribute(network, "bipartite"))]
        b2 <- network::get.vertex.attribute(network, attr_case)
        b2 <- b2[b]
        tibble::tibble(Cases = b2)
      } else {
        for (i in 1:length(network::get.vertex.attribute(network, attr_case))) {
          b <- 1:i
        }
        b <-
          b[-c(1:network::get.network.attribute(network, "bipartite"))]
        b2 <- network::get.vertex.attribute(network, attr_case)
        b2 <- b2[b]
        tibble::tibble(
          Cases = b2,
          Outcome = network::get.vertex.attribute(network, attr_outcome)[b]
        )
      }
    }
    else if (bipartite == "b1") {
      if (is.null(attr_outcome)) {
        for (i in 1:length(network::get.vertex.attribute(network, attr_case))) {
          a <- 1:i
        }
        a <-
          a[1:network::get.network.attribute(network, "bipartite")]
        b1 <- network::get.vertex.attribute(network, attr_case)
        b1 <- b1[a]
        tibble::tibble(Cases = b1)
      } else {
        for (i in 1:length(network::get.vertex.attribute(network, attr_case))) {
          a <- 1:i
        }
        a <-
          a[1:network::get.network.attribute(network, "bipartite")]
        b1 <- network::get.vertex.attribute(network, attr_case)
        b1 <- b1[a]
        tibble::tibble(
          Cases = b1,
          Outcome = network::get.vertex.attribute(network, attr_outcome)[a]
        )
      }
    }
    else {
      print(
        "No valid network-level selected! If bipartite select 'b1' or 'b2', otherwise leave empty."
      )
    }
  }
}
