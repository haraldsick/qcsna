#' vehicle_categories_dn
#'
#' Small illustrative discourse network showing the debate whether to include vans (N1 vehicles)
#' into the proposed efforts to reduce CO2 emissions of cars. The network is bipartite, including
#' an actor level and a concept level that shows their claims during the debate.
#' While the initial draft of the EU Commission suggested to include N1 vehicles or to introduce at least
#' separate targets for this vehicle category, all eight actors representing the automotive industry in
#' the debate favored an exclusion of N1 vehicles. Despite strong initial support by environmentalist NGOs
#' and political decision makers for the inclusion of N1 vehicles, the concept was ultimately
#' dismissed and the automotive industry's concept was included in the final legislation.
#'
#' @docType data
#'
#' @usage data(vehicle_categories_dn)
#'
#' @format An bipartite network of class \code{"network"}.
#'
#' @keywords datasets
#'
#' @references Sick, Harald (2021). With a little help from their friends -- Lobbying efforts to regulate
#' CO2 emissions of cars in the EU. Manuscript submitted for publication.
#'
#' @examples
#' \dontrun{qca_data <- create_qca_data(vehicle_categories_dn, attr_cases = "vertex.names", attr_outcome = "Type", bipartite = "b2")
#' qca_data <- add_node_indice(vehicle_categories_dn, qca_data, indice = degree, cmode = "indegree", "b2")
#' qca_data}
"vehicle_categories_dn"
