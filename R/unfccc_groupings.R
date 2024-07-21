#' UNFCCC Negotiating Group Membership
#'
#' The assignment of unfccc parties to negotiating groups used by allocateR
#'
#' @format ## unfccc_groupings
#' A data frame with 221 rows and 6 columns:
#' \describe{
#'   \item{country.name.en}{Country name}
#'   \item{country}{3 letter ISO country codes}
#'   \item{current}{2022 CO2 emissions in PgC/year}
#'   \item{current}{1851-2022 cumulative CO2 emissions in PgC}
#'   \item{grouping}{grouping code}
#'   \item{group_current}{2022 PgC emissions from negotiating group member countries}
#' }
#' @source <https://unfccc.int/process-and-meetings/parties-non-party-stakeholders/parties/party-groupings>
"unfccc_groupings"
