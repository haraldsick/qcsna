#' @title add_node_indice
#'
#' @description Adds a sna node-level indice to the qca data-set
#'
#' @param network An object of class 'network'.
#' @param qca_data The qca-dataset you want to extend.
#' @param bipartite If the network is bipartite, select the level the cases and the
#' outcome are on. If they are on the first level (sometimes referred as the
#' actor level) of a bipartite network, choose 'b1', if they are on the second level
#' (sometimes referred as the event level), choose 'b2'.
#' @param indice The sna node-level indice you want to add to the qca-dataset as a variable
#' and its parameters (e.g. degree, cmode = "indegree"). For details, see the sna package.
#'
#' @details With the add_node_indice function you can add the specific node-level metrics
#' for each of the cases to the qca-dataset. The 'indice' parameter is basically the command
#' for the sna package to calculate the indice you like to add.
#'
#' Right now, it should be compatible with betweenness, closeness, degree, stresscent,
#' graphcent, and evcent. For the specific commands and more options, please refer to the
#' reference manual of the sna package or the help file for the specific functions
#' (e.g. \code{\link[sna]{degree}}).
#'
#' @importFrom dplyr "%>%"
#' @importFrom stringr str_to_title
#' @importFrom network get.vertex.attribute
#' @importFrom network get.network.attribute
#' @importFrom tibble add_column
#'
#' @return The preexisting qca-dataset enhanced with a new column consisting of the
#' specified sna node-level indice.
#'
#' @examples qca_data <- add_node_indice(network, qca_data, "b2", indice = degree, cmode = "indegree")
#'
#' @references Carter T. Butts (2020). sna: Tools for Social Network Analysis. R package
#' version 2.6. \url{https://CRAN.R-project.org/package=sna}
#'
#' @export

add_node_indice <- function(network,
                            qca_data,
                            bipartite = NULL,
                            indice = NULL,
                            ...) {
  if (is.null(bipartite)) {
    if (is.null(indice)) {
      stop("Please provide a sna node-level indice")
    } else {
      colname <- stringr::str_to_title(as.character(substitute(indice)))
      indice <- indice(network, ...)
      qca_data %>% tibble::add_column(!!colname := indice, .before = "Outcome")
    }
  }
  else if (bipartite == "b2") {
    if (is.null(indice)) {
      stop("Please provide a sna node-level indice")
    } else {
      for (i in 1:length(network::network.vertex.names(network))) {
        b <- 1:i
      }
      b <- b[-c(1:get.network.attribute(network, "bipartite"))]
      colname <-
        stringr::str_to_title(as.character(substitute(indice)))
      indice <- indice(network, ...)
      indice <- indice[b]
      qca_data %>% tibble::add_column(!!colname := indice, .before = "Outcome")
    }
  }
  else if (bipartite == "b1") {
    if (is.null(indice)) {
      stop("Please provide a sna node-level indice")
    } else {
      for (i in 1:length(network::network.vertex.names(network))) {
        a <- 1:i
      }
      a <- a[1:network::get.network.attribute(network, "bipartite")]
      colname <-
        stringr::str_to_title(as.character(substitute(indice)))
      indice <- indice(network, ...)
      indice <- indice[a]
      qca_data %>% tibble::add_column(!!colname := indice, .before = "Outcome")
    }
  } else {
    print("No valid network-level selected! If the network is bipartite, select 'b1' or 'b2', otherwise leave empty")
  }
}
