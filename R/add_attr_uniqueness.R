#' @title add_attr_uniqueness
#'
#' @param network An object of class 'network'.
#' @param qca_data The qca-dataset you want to extend.
#' @param attr The node-level attribute that you would like to calculate an uniqueness-
#' score. Note that this function works only with node-attributes of type 'character'!
#' @param bipartite If the network is bipartite, select the level the cases and the
#' outcome are on. If they are on the first level (sometimes referred as the
#' actor level) of a bipartite network, choose 'b1', if they are on the second level
#' (sometimes referred as the event level), choose 'b2'. Otherwise omit it.
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr right_join
#' @importFrom network get.vertex.attribute
#' @importFrom network get.network.attribute
#' @importFrom network is.bipartite
#' @importFrom tibble add_column
#'
#' @example
#'

add_attr_uniqueness <- function(network,
                                qca_data,
                                attr = NULL,
                                bipartite = NULL) {
  if (!network::is.bipartite(network)) {
    if (is.null(attr)) {
      stop("Please provide all necessary arguments")
    } else {
      attrs1 <-
        tibble::as_tibble(network::get.vertex.attribute(network, attr))
      attrs2 <-
        tibble::as_tibble(table(network::get.vertex.attribute(network, attr)),
                  .name_repair = ~ c("value", "n"))
      uniqueness_tibble <- dplyr::right_join(attrs1, attrs2)
      uniqueness_tibble <- 1 / uniqueness_tibble$n
      colname <- paste(attr, "uniqueness", sep = "-")
      qca_data %>% tibble::add_column(!!colname := uniqueness_tibble, .after = "Cases")
    }
  }
  else if (network::is.bipartite(network) && missing(bipartite)) {
    stop("The network is bipartite, please select a level with 'b1' or 'b2'")
  }
  else if (network::is.bipartite(network) && bipartite == "b2") {
    if (is.null(attr)) {
      stop("Please provide all necessary arguments")
    }
    else {
      for (i in 1:length(network::network.vertex.names(network))) {
        b <- 1:i
      }
      b <- b[-c(1:get.network.attribute(network, "bipartite"))]
      attrs1 <-
        tibble::as_tibble(network::get.vertex.attribute(network, attr))
      attrs2 <-
        tibble::as_tibble(table(network::get.vertex.attribute(network, attr)),
                  .name_repair = ~ c("value", "n"))
      uniqueness_tibble <- dplyr::right_join(attrs1, attrs2)
      uniqueness_tibble <- 1 / uniqueness_tibble$n[b]
      colname <- paste(attr, "uniqueness", sep = "-")
      qca_data %>% tibble::add_column(!!colname := uniqueness_tibble, .after = "Cases")
    }
  }
  else if (network::is.bipartite(network) && bipartite == "b1") {
    if (is.null(attr)) {
      stop("Please provide all necessary arguments")
    } else {
      for (i in 1:length(network::network.vertex.names(network))) {
        a <- 1:i
      }
      a <-
        a[1:network::get.network.attribute(network, "bipartite")]
      attrs1 <-
        tibble::as_tibble(network::get.vertex.attribute(network, attr))
      attrs2 <-
        tibble::as_tibble(table(network::get.vertex.attribute(network, attr)),
                  .name_repair = ~ c("value", "n"))
      uniqueness_tibble <- dplyr::right_join(attrs1, attrs2)
      uniqueness_tibble <- 1 / uniqueness_tibble$n[a]
      colname <- paste(attr, "uniqueness", sep = "-")
      qca_data %>% tibble::add_column(!!colname := uniqueness_tibble, .after = "Cases")
    }
  }
  else {
    stop(
      "The network is bipatite, but no valid level selected. Please select a level with 'b1' or 'b2'"
    )
  }
}
