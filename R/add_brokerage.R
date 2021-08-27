#' @title add_brokerage
#'
#' @description Performs a brokerage analysis and adds a user-defined brokerage-score for each case (node) to the QCA data-set.
#'
#' @param network An object of class 'network'.
#' @param qca_data The QCA-dataset you want to extend.
#' @param attr The node-level attribute that defines the group membership of the vertex and its alters
#' you would like to calculate the brokerage-scores for.
#' @param role Defines the brokerage role, you would like to calculate the score for. Choose between 'Coordinator',
#' 'Itinerant', 'Gatekeeper', 'Representative', 'Liaison' or 'Cumulative' (see the details-section for more information).
#' @param bipartite If the network is bipartite, select the level the cases and the
#' outcome are on. If they are on the first level (sometimes referred as the
#' actor level) of a bipartite network, choose 'b1', if they are on the second level
#' (sometimes referred as the event level), choose 'b2'. Otherwise omit it.
#' @param normalization Set TRUE, if you would like to include normalized brokerage-scores.
#'
#' @details This function relies on Butts (2020) brokerage function within his great sna package. Butts (2020),
#' following Gould and Fernandez (1989), implemented a bunch predefined brokerage roles.
#' They analyze the role a specific vertex plays between two vertex groups by calculating scores for the
#' brokerage role of interest. The group membership can be chosen by defining the the attr argument.
#' Butts (2008) describes the brokerage roles as following:
#'
#' \itemize{
#'     \item{Coordinator role; the broker mediates contact between two individuals from his or her own group. Two-path structure: A -> A -> A}
#'     \item{Itinerant broker role; the broker mediates contact between two individuals from a single group to which he or she does not belong. Two-path structure: A -> B -> A}
#'     \item{Gatekeeper role; the broker mediates an incoming contact from an out-group member to an in-group member. Two-path structure: A -> B -> B}
#'     \item{Representative role; the broker mediates an outgoing contact from an in-group member to an out-group member. Two-path structure: A -> A -> B}
#'     \item{Liaison role; the broker mediates contact between two individuals from different groups, neither of which is the group to which he or she belongs. Two-path structure: A -> B -> C}
#'     \item{Total (cumulative) brokerage role occupancy. (Any of the above two-paths.)}
#' }
#'
#' For more detail, see the description of the sna package and its brokerage-function! Please keep in mind, that the roles for bipartite networks can be
#' defined differently, since e.g. actors and events might have different vertex attributes and are probably not members of the same group by definition.
#'
#' @return The preexisting QCA-dataset enhanced with a new column with the brokerage-scores of interest.
#'
#' @importFrom dplyr "%>%"
#' @importFrom network get.vertex.attribute
#' @importFrom network get.network.attribute
#' @importFrom network is.bipartite
#' @importFrom network network.vertex.names
#' @importFrom sna brokerage
#' @importFrom rlang :=
#' @importFrom tibble add_column
#'
#' @examples
#' \dontrun{qca_data <- add_brokerage(network, qca_data, attr = "Type", role = "Itinerant", bipartite = "b2", normalization = TRUE)}
#'
#' @references
#'
#' Butts, Carter T. (2008). network: a Package for Managing Relational Data in R. Journal of Statistical Software, 24(2).
#' \url{https://www.jstatsoft.org/v24/i02/paper}.
#'
#' Butts, Carter T. (2020). sna: Tools for Social Network Analysis. R package
#' version 2.6. \url{https://CRAN.R-project.org/package=sna}.
#'
#' Gould, R.V. and Fernandez, R.M. (1989). Structures of Mediation: A Formal Approach to Brokerage in Transaction Networks.
#' Sociological Methodology, 19: 89-126.
#'
#' Müller, Kirill and Wickham, Hadley (2021). tibble: Simple Data Frames. R package
#' version 3.1.4. \url{https://CRAN.R-project.org/package=tibble}.
#'
#' @export

add_brokerage <- function(network,
                           qca_data,
                           attr = NULL,
                           role = NULL,
                           bipartite = NULL,
                           normalization = FALSE) {
  if (!network::is.bipartite(network)) {
    n <- 1:length(network::network.vertex.names(network))
    if (is.null(attr)) {
      stop("Please provide a vertex attribute")
    }
    if (is.null(role)) {
      stop("Please provide a brokerage role")
    } else
      if (normalization == FALSE) {
        if (role == "Coordinator") {
          coordinator <-
            tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[n, 1])
          qca_data %>% tibble::add_column(Coordinator = coordinator$value, .after = "Cases")
        } else
          if (role == "Itinerant") {
            itinerant <-
              tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[n, 2])
            qca_data %>% tibble::add_column(Itinerant = itinerant$value, .after = "Cases")
          } else
            if (role == "Gatekeeper") {
              gatekeeper <-
                tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[n, 3])
              qca_data %>% tibble::add_column(Gatekeeper = gatekeeper$value, .after = "Cases")
            } else
              if (role == "Representative") {
                representative <-
                  tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[n, 4])
                qca_data %>% tibble::add_column(Representative = representative$value, .after = "Cases")
              } else
                if (role == "Liaison") {
                  liaison <-
                    tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[n, 5])
                  qca_data %>% tibble::add_column(Liaison = liaison$value, .after = "Cases")
                } else
                  if (role == "Cumulative") {
                    cumulative <-
                      tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[n, 6])
                    qca_data %>% tibble::add_column(Cumulative.Brokerage = cumulative$value, .after = "Cases")
                  } else {
                    stop(
                      "No valid role, please select 'Coordinator', 'Itinerant', 'Gatekeeper',
                      'Representative', 'Liaison' or 'Cumulative'"
                    )
                  }
      } else
        if (normalization == TRUE) {
          if (role == "Coordinator") {
            coordinator <-
              tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[n, 1])
            qca_data %>% tibble::add_column(Coordinator = coordinator$value, .after = "Cases")
          } else
            if (role == "Itinerant") {
              itinerant <-
                tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[n, 2])
              qca_data %>% tibble::add_column(Itinerant = itinerant$value, .after = "Cases")
            } else
              if (role == "Gatekeeper") {
                gatekeeper <-
                  tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[n, 3])
                qca_data %>% tibble::add_column(Gatekeeper = gatekeeper$value, .after = "Cases")
              } else
                if (role == "Representative") {
                  representative <-
                    tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[n, 4])
                  qca_data %>% tibble::add_column(Representative = representative$value, .after = "Cases")
                } else
                  if (role == "Liaison") {
                    liaison <-
                      tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[n, 5])
                    qca_data %>% tibble::add_column(Liaison = liaison$value, .after = "Cases")
                  } else
                    if (role == "Cumulative") {
                      cumulative <-
                        tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[n, 6])
                      qca_data %>% tibble::add_column(Cumulative.Brokerage = cumulative$value, .after = "Cases")
                    } else {
                      stop(
                        "No valid role, please select 'Coordinator', 'Itinerant', 'Gatekeeper',
                        'Representative', 'Liaison' or 'Cumulative'"
                      )
                    }
        }
  }
  else if (network::is.bipartite(network) && missing(bipartite)) {
    stop(
      "The network is bipartite, please select a level with 'b1' or 'b2'; or did you forget another argument?"
    )
  }
  else if (network::is.bipartite(network) && bipartite == "b2") {
    b <- 1:length(network::network.vertex.names(network))
    b <- b[-c(1:network::get.network.attribute(network, "bipartite"))]
    if (is.null(attr)) {
      stop("Please provide a vertex attribute")
    }
    if (is.null(role)) {
      stop("Please provide a brokerage role")
    } else
      if (normalization == FALSE) {
        if (role == "Coordinator") {
          coordinator <-
            tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[b, 1])
          qca_data %>% tibble::add_column(Coordinator = coordinator$value, .after = "Cases")
        } else
          if (role == "Itinerant") {
            itinerant <-
              tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[b, 2])
            qca_data %>% tibble::add_column(Itinerant = itinerant$value, .after = "Cases")
          } else
            if (role == "Gatekeeper") {
              gatekeeper <-
                tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[b, 3])
              qca_data %>% tibble::add_column(Gatekeeper = gatekeeper$value, .after = "Cases")
            } else
              if (role == "Representative") {
                representative <-
                  tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[b, 4])
                qca_data %>% tibble::add_column(Representative = representative$value, .after = "Cases")
              } else
                if (role == "Liaison") {
                  liaison <-
                    tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[b, 5])
                  qca_data %>% tibble::add_column(Liaison = liaison$value, .after = "Cases")
                } else
                  if (role == "Cumulative") {
                    cumulative <-
                      tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[b, 6])
                    qca_data %>% tibble::add_column(Cumulative.Brokerage = cumulative$value, .after = "Cases")
                  } else {
                    stop(
                      "No valid role, please select 'Coordinator', 'Itinerant', 'Gatekeeper',
                      'Representative', 'Liaison' or 'Cumulative'"
                    )
                  }
      } else
        if (normalization == TRUE) {
          if (role == "Coordinator") {
            coordinator <-
              tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[b, 1])
            qca_data %>% tibble::add_column(Coordinator = coordinator$value, .after = "Cases")
          } else
            if (role == "Itinerant") {
              itinerant <-
                tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[b, 2])
              qca_data %>% tibble::add_column(Itinerant = itinerant$value, .after = "Cases")
            } else
              if (role == "Gatekeeper") {
                gatekeeper <-
                  tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[b, 3])
                qca_data %>% tibble::add_column(Gatekeeper = gatekeeper$value, .after = "Cases")
              } else
                if (role == "Representative") {
                  representative <-
                    tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[b, 4])
                  qca_data %>% tibble::add_column(Representative = representative$value, .after = "Cases")
                } else
                  if (role == "Liaison") {
                    liaison <-
                      tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[b, 5])
                    qca_data %>% tibble::add_column(Liaison = liaison$value, .after = "Cases")
                  } else
                    if (role == "Cumulative") {
                      cumulative <-
                        tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[b, 6])
                      qca_data %>% tibble::add_column(Cumulative.Brokerage = cumulative$value, .after = "Cases")
                    } else {
                      stop(
                        "No valid role, please select 'Coordinator', 'Itinerant', 'Gatekeeper',
                        'Representative', 'Liaison' or 'Cumulative'"
                      )
                    }
        }
  }
  else if (network::is.bipartite(network) && bipartite == "b1") {
    a <- 1:(network::get.network.attribute(network, "bipartite"))
    if (is.null(attr)) {
      stop("Please provide a vertex attribute")
    }
    if (is.null(role)) {
      stop("Please provide a brokerage role")
    } else
      if (normalization == FALSE) {
        if (role == "Coordinator") {
          coordinator <-
            tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[a, 1])
          qca_data %>% tibble::add_column(Coordinator = coordinator$value, .after = "Cases")
        } else
          if (role == "Itinerant") {
            itinerant <-
              tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[a, 2])
            qca_data %>% tibble::add_column(Itinerant = itinerant$value, .after = "Cases")
          } else
            if (role == "Gatekeeper") {
              gatekeeper <-
                tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[a, 3])
              qca_data %>% tibble::add_column(Gatekeeper = gatekeeper$value, .after = "Cases")
            } else
              if (role == "Representative") {
                representative <-
                  tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[a, 4])
                qca_data %>% tibble::add_column(Representative = representative$value, .after = "Cases")
              } else
                if (role == "Liaison") {
                  liaison <-
                    tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[a, 5])
                  qca_data %>% tibble::add_column(Liaison = liaison$value, .after = "Cases")
                } else
                  if (role == "Cumulative") {
                    cumulative <-
                      tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$raw.nli[a, 6])
                    qca_data %>% tibble::add_column(Cumulative.Brokerage = cumulative$value, .after = "Cases")
                  } else {
                    stop(
                      "No valid role, please select 'Coordinator', 'Itinerant', 'Gatekeeper',
                      'Representative', 'Liaison' or 'Cumulative'"
                    )
                  }
      } else
        if (normalization == TRUE) {
          if (role == "Coordinator") {
            coordinator <-
              tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[a, 1])
            qca_data %>% tibble::add_column(Coordinator = coordinator$value, .after = "Cases")
          } else
            if (role == "Itinerant") {
              itinerant <-
                tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[a, 2])
              qca_data %>% tibble::add_column(Itinerant = itinerant$value, .after = "Cases")
            } else
              if (role == "Gatekeeper") {
                gatekeeper <-
                  tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[a, 3])
                qca_data %>% tibble::add_column(Gatekeeper = gatekeeper$value, .after = "Cases")
              } else
                if (role == "Representative") {
                  representative <-
                    tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[a, 4])
                  qca_data %>% tibble::add_column(Representative = representative$value, .after = "Cases")
                } else
                  if (role == "Liaison") {
                    liaison <-
                      tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[a, 5])
                    qca_data %>% tibble::add_column(Liaison = liaison$value, .after = "Cases")
                  } else
                    if (role == "Cumulative") {
                      cumulative <-
                        tibble::as_tibble(sna::brokerage(network, network::get.vertex.attribute(network, attr))$z.nli[a, 6])
                      qca_data %>% tibble::add_column(Cumulative.Brokerage = cumulative$value, .after = "Cases")
                    } else {
                      stop(
                        "No valid role, please select 'Coordinator', 'Itinerant', 'Gatekeeper',
                        'Representative', 'Liaison' or 'Cumulative'"
                      )
                    }
        }
  }
}
