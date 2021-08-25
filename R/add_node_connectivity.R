#' @title add_node_connectivity
#'
#' @description Adds counts for the connectivity of the cases (nodes).
#'
#' @details Sometimes it might be interesting to know how well the the nodes that are the cases for the QCA
#' are connected within the network and if that this connectivity could be a condition for the outcome that
#' you want to examine. This function adds a new column to your QCA-dataset with the number of threepaths
#' and/or fourcycles that the individual cases/nodes are a member of.
#'
#' @param network An object of class 'network'.
#' @param qca_data The QCA-dataset you want to extend.
#' @param count The connectivity-count you want to add to the QCA-dataset as a variable.
#' Right now, threepaths and fourcycles are available. Select them with e.g. 'count = "Fourcycles"'
#' @param bipartite If the network is bipartite, select the level the cases and the
#' outcome are on. If they are on the first level (sometimes referred as the
#' actor level) of a bipartite network, choose 'b1', if they are on the second level
#' (sometimes referred as the event level), choose 'b2'. Otherwise omit it.
#'
#' @return The preexisting QCA-dataset enhanced with a new column with the threepaths or fourcycles
#' the cases (or nodes) are a member of.
#'
#'
#' @importFrom dplyr "%>%"
#' @importFrom network get.vertex.attribute
#' @importFrom network get.network.attribute
#' @importFrom network network.vertex.names
#' @importFrom network is.bipartite
#' @importFrom sna kcycle.census
#' @importFrom sna kpath.census
#' @importFrom rlang :=
#' @importFrom tibble add_column
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#'
#' @examples \dontrun{qca_data <- add_node_connectivity(qca_data, network, count = "Fourcycles", "b2")}
#'
#' @references
#'
#' Butts, Carter T. (2008). network: a Package for Managing Relational Data in R. Journal of Statistical Software, 24(2).
#' \url{https://www.jstatsoft.org/v24/i02/paper}.
#'
#' Butts, Carter T. (2020). sna: Tools for Social Network Analysis. R package
#' version 2.6. \url{https://CRAN.R-project.org/package=sna}.
#'
#' Kirill Müller and Hadley Wickham (2021). tibble: Simple Data Frames. R package
#' version 3.1.4. https://CRAN.R-project.org/package=tibble
#'
#' @export


add_node_connectivity <- function(network,
                                  qca_data,
                                  count = NULL,
                                  bipartite = NULL) {
  if (!network::is.bipartite(network)) {
    if (is.null(count)) {
      stop("Please select a valid count! Right now, 'Fourcycles' and 'Threepaths' are possible")
    } else
      if (count == "Fourcycles") {
        n <- 1:length(network::network.vertex.names(network)) + 1
        fourcycles <- tibble::as_tibble(sna::kcycle.census(network, maxlen = 4)$cycle.count[3,][n])
        qca_data %>% tibble::add_column(Fourcycles = fourcycles$value, .after = "Cases")
      } else
        if (count == "Threepaths") {
          n <- 1:length(network::network.vertex.names(network)) + 1
          threepath <- tibble::tibble(sna::kpath.census(network, maxlen = 3)$path.count[3,][n])
          qca_data %>% tibble::add_column(Threepaths = threepath$value, .after = "Cases", .name_repair = "minimal")
        }
    else {
      stop("No valid count, please select 'Fourcycles' or 'Threepaths'")
    }
  }
  else if (network::is.bipartite(network) && missing(bipartite)) {
    stop("The network is bipartite, please select a level with 'b1' or 'b2'; or did you forget another argument?")
  }
  else if (network::is.bipartite(network) && bipartite == "b2") {
    if (is.null(count)) {
      stop("Please select a valid count! Right now, 'Fourcycles' and 'Threepaths' are possible")
    } else
      if (count == "Fourcycles") {
        b <- 1:length(network::network.vertex.names(network))
        b <- b[-c(1:network::get.network.attribute(network, "bipartite"))]
        b <- b + 1 # the census-functions of sna add an Agg-count in the first count, so we need a + 1
        fourcycles <- tibble::as_tibble(sna::kcycle.census(network, maxlen = 4)$cycle.count[3,][b])
        qca_data %>% tibble::add_column(Fourcycles = fourcycles$value, .after = "Cases")
      } else
        if (count == "Threepaths") {
          b <- 1:length(network::network.vertex.names(network))
          b <- b[-c(1:network::get.network.attribute(network, "bipartite"))]
          b <- b + 1
          threepath <- tibble::as_tibble(sna::kpath.census(network, maxlen = 3)$path.count[3,][b])
          qca_data %>% tibble::add_column(Threepaths = threepath$value, .after = "Cases")
        }
    else {
      stop("No valid count, please select 'Fourcycles' or 'Threepaths'")
    }
  }
  else if (network::is.bipartite(network) && bipartite == "b1") {
    if (is.null(count)) {
      stop("Please select a valid count! Right now, 'Fourcycles' and 'Threepaths' are possible")
    } else
      if (count == "Fourcycles") {
        a <- 1:(network::get.network.attribute(network, "bipartite"))
        a <- a + 1 # the census-functions of sna add an Agg-count in the first count, so we need a + 1
        fourcycles <- tibble::as_tibble(sna::kcycle.census(network, maxlen = 4)$cycle.count[3,][a])
        qca_data %>% tibble::add_column(Fourcycles = fourcycles$value, .after = "Cases")
      } else
        if (count == "Threepaths") {
          a <- 1:(network::get.network.attribute(network, "bipartite"))
          a <- a + 1
          threepath <- tibble::as_tibble(sna::kpath.census(network, maxlen = 3)$path.count[3,][a])
          qca_data %>% tibble::add_column(Threepaths = threepath$value, .after = "Cases")
        }
    else {
      stop("No valid count, please select 'Fourcycles' or 'Threepaths'")
    }
  }
  else {
    stop(
      "The network is bipatite, but no valid level selected. Please select a level with 'b1' or 'b2'"
    )
  }
}
