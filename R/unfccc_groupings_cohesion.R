#' UNFCCC Negotiating Group Membership
#'
#' Assignment of unfccc parties to negotiating groups based on a "cohesion" model. When is a country is a member of multiple groups,
#' it is assigned membership of the group with the smallest number of members. e.g China is assigned to BASIC, rather than
#'
#' @format ## unfccc_groupings
#' A data frame with 221 rows and 6 columns:
#' \describe{
#'   \item{country.name.en}{Country name}
#'   \item{country}{3 letter ISO country codes}
#'   \item{current}{2022 CO2 emissions in PgC/year}
#'   \item{current}{1851-2022 cumulative CO2 emissions in PgC}
#'   \item{grouping}{grouping code}
#'   \item{n_group}{The number of members of the group (may be greater than the assigned number)}
#' }
#' @source <https://unfccc.int/process-and-meetings/parties-non-party-stakeholders/parties/party-groupings>
"unfccc_groupings_cohesion"
