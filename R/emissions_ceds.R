#' CEDS emissions data at country level
#'
#' Annual 1851-2022 emissions of FFI-CO2, CH4, N2O, BC, OC, SO2, CO, NOX, NMVOC, NH3
#' LULUCF emissions are not included.
#'
#' CEDS data for CH4 and N2O do not extend for 1970 so these are generated using global estimates.
#'
#'
#' @format ## unfccc_groupings
#' A data frame with 38120 rows and 5 columns:
#' \describe{
#'   \item{country}{iso code}
#'   \item{year}{year}
#'   \item{variable}{climate forcer}
#'   \item{value}{emissions amount}
#'   \item{units}{units}
#' }
#' @source <https://zenodo.org/records/10904361>
"emissions_ceds"
