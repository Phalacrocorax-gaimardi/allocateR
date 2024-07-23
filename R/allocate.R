# Load necessary libraries
#library(dplyr)
#library(parallel)


#' group_emissions
#'
#' Takes a country level historical emissions dataset and converts to a grouped emissions dataset
#'
#' @param emissions historical emissions at country level in Hector format
#' @param groupings groupings dataframe coontaining country and grouping codes
#'
#' @return emissions dataframe at grouping level in Hector format
#' @export
#'
#' @examples "group_emissions(emissions_ceds,unfccc_groupings_cohesion)"
group_emissions <- function(emissions,groupings){
  #
  emissions <- emissions %>% dplyr::left_join(groupings)
  emissions <- emissions %>% dplyr::group_by(year,variable, grouping) %>% dplyr::summarise(value=sum(value),units=units[1])
  emissions <- emissions %>% dplyr::rename("country"=grouping) %>% dplyr::select(country,year,variable,value,units)
  return(emissions)
}

#grouped_emissions <- group_emissions(emissions,groupings)


#' get_coalitions
#'
#' Helper function that creates a list of all coalitions from a list of countries or country groupings. If there are N groups the number of coalitions is 2^N including
#' the null coalition.
#'
#' @param groups a vector of countries or negotiating groups
#'
#' @return a list of length 2^N
#' @export
#'
#' @examples
get_coalitions <- function(groups){

  coalitions <- unlist(lapply(0:length(groups),utils::combn, x=groups,simplify = FALSE), recursive = FALSE)
  return(coalitions)

}


#' make_null_core
#'
#' A hector model instance (core) with zeroed anthropogenic and volcanic sulfur emissions
#'
#' @param natural_n2o Tuned natural N2O emissions
#' @param natural_ch4 Tuned natural CH4 emissions
#'
#' @return a hector core
#' @export
#'
#' @examples
make_null_core <- function(natural_n2o=9.96,natural_ch4=337.7725){

   ssp126_ini <- system.file("input/hector_ssp126.ini", package = "hector")
   nullcore <- hector::newcore(ssp126_ini, name="Zeroed Anthrogenic and Volanic emissions")
   years <- 1745:2022
   var_emissions <- hector::fxntable %>% dplyr::filter(stringr::str_detect(string,"emissions")) %>% dplyr::pull(string)
   var_emissions <- var_emissions[var_emissions != "N2O_natural_emissions"]
   no_emissions <- hector::fetchvars(nullcore,years,var_emissions)
   no_uptake <- hector::fetchvars(nullcore,years,c(hector::LUC_UPTAKE(),hector::DACCS_UPTAKE()))
   no_albedo <- hector::fetchvars(nullcore,years,hector::RF_ALBEDO())
   no_impact <- dplyr::bind_rows(no_emissions,no_uptake,no_albedo) %>% dplyr::mutate(value = 0)

   #natural_n2o <- 9.959 #Hector has 9.7
   #natural_n2o <- 9.7
   #natural_ch4 <- 337.75 #vs Hector 338
   hector::setvar(nullcore,years,"SV", rep(0,length(years)), unit="W/m2")
   hector::setvar(nullcore,years,hector::NAT_EMISSIONS_N2O(), rep(natural_n2o,length(years)), unit="Tg N")
   hector::setvar(nullcore,NA,hector::NATURAL_CH4(), natural_ch4, unit="Tg CH4")
   hector::setvar(nullcore,years,hector::RF_ALBEDO(), rep(0,length(years)), unit="W/m2")

#vars0 <- no_impact$variable %>% unique()

   for(forcer in var_emissions)
     hector::setvar(nullcore,years,forcer, no_impact %>% dplyr::filter(variable==forcer) %>% dplyr::pull(value), unit=no_impact %>% dplyr::filter(variable==forcer) %>% dplyr::pull(units) %>% magrittr::extract2(1))

   hector::run(nullcore,runtodate = 2022)

   #forcings <- fxntable %>% filter(str_detect(string,"RF")) %>% pull(string)
   #forcings <- c("FCH4",forcings)
   #forcings <- forcings[forcings != "RF_tot_constrain"]
#fetchvars(core,years,forcings) %>% filter(year==2019) %>% arrange(-abs(value))
#fetchvars(core,years,CONCENTRATIONS_CH4()) %>% ggplot(aes(year,value))+geom_line()
#fetchvars(core,years,EMISSIONS_BC()) %>% ggplot(aes(year,value))+geom_line()
#fetchvars(core,NA,NATURAL_CH4())
   #print("nullcore created")
   print(paste("GSAT 2022", hector::fetchvars(nullcore,2022,hector::GLOBAL_TAS())$value))
   nullcore %>% return()
}

#usage: nullcore <- make_null_core()


#' get_coalition_emissions
#'
#' helper function to combine emissions in a coalition
#'
#' @param emissions emissions of individual countrie/groups in a coalition
#'
#' @return emissions of the coalition
#' @export
#'
#' @examples
get_coalition_emissions <- function(emissions){
  #take a subset
  #emissions %>% group_by(year,variable) %>% summarise(value=sum(value),units=units[1]) # %>% arrange(year,variable)
  emissions_DT <- data.table::setDT(emissions)
  emissions_DT[,.(value=sum(value)), by=.(year,variable)]
}

#' get_gsat
#'
#' Returns the 1851-2022 warming contribution from a coalition
#'
#'
#' @param coalition_emissions coalition emissions
#' @param start_year usually 1851
#' @param evaluation_year usually 2022
#'
#' @return a coalition evaluation_year warming (GSAT) in degC
#' @export
#'
#' @examples
get_gsat <- function(coalition_emissions, start_year=1851, evaluation_year=2022){
  #step 1:first ensure that all emissions are set to zero
  #step 2:add emissions for ceds database
  #compute gsat from a ceds emissions
  #zero volcanic aerosol forcing
  #vars <- coalition_emissions$variable %>% unique()
  #coalition_emissions <- coalition_emissions %>% as.data.table()
  #coalition_emissions <- setDT(coalition_emissions)
  core <- nullcore
  data.table::setkey(coalition_emissions,"variable") #%>% system.time()
  years <- start_year:2022
  #1
  forcer <- "ffi_emissions"
  hector::setvar(core,years,forcer,coalition_emissions[.(forcer),]$value, unit="Pg C/yr")
  #2
  forcer <- "BC_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer,coalition_emissions[.(forcer),]$value, unit="Tg")
  #3
  forcer <- "OC_emissions"
  values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer,coalition_emissions[.(forcer),]$value, unit="Tg")
  #4
  forcer <- "CO_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer, coalition_emissions[.(forcer),]$value, unit="Tg CO")
  #5
  forcer <- "NOX_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer,coalition_emissions[.(forcer),]$value, unit="Tg N")
  #6
  forcer <- "NMVOC_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer, coalition_emissions[.(forcer),]$value, unit="Tg NMVOC")
  #7
  forcer <- "SO2_emissions"
  hector::setvar(core,years,forcer,coalition_emissions[.(forcer),]$value, unit="Gg S")
  #8
  forcer <- "NH3_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer, coalition_emissions[.(forcer),]$value, unit="Tg")
  #9
  forcer <- "CH4_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer, coalition_emissions[.(forcer),]$value, unit="Tg CH4")
  #10
  forcer <- "N2O_emissions"
  #values <- coalition_emissions[.(forcer),]$value
  hector::setvar(core,years,forcer, coalition_emissions[.(forcer),]$value, unit="Tg N")
  #
  hector::reset(core,1850)
  hector::run(core, runtodate = 2022)
  gsat <- hector::fetchvars(core,evaluation_year,hector::GLOBAL_TAS()) #%>% as.data.table()
  #hector::shutdown(core)
  gsat$value %>% return()


}


# Define the contrib_shap function
#' contrib_shap
#'
#' evaluate the reasonable allocation GSAT of country or grouping given an emissions dataset and a list of all possible coalitions
#'
#' @param emissions the emissions dataset for all countries or groupings
#' @param country the country (iso3) or grouping code
#' @param coalitions the list of all coalitions of length 2^N where N is the number of countries or groupings
#'
#' @return the reasonable gsat allocation for the country
#' @export
#'
#' @examples
contrib_shap <- function(emissions, country, coalitions) {
  # Get the total number of coalitions
  N <- length(coalitions[[length(coalitions)]])

  # Separate coalitions with and without the country
  coalitions_without <- coalitions[!sapply(coalitions, function(coal) country %in% coal)]
  coalitions_with <- lapply(coalitions_without, function(coal) c(country, coal))
  N_s <- sapply(coalitions_without, length)

  # Define a helper function to compute gsat difference
  compute_gsat_diff <- function(i) {
    gsats1 <- emissions %>% dplyr::filter(country %in% coalitions_with[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    gsats2 <- emissions %>% dplyr::filter(country %in% coalitions_without[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    return(gsats1 - gsats2)
  }

  # Use parallel processing to compute gsat differences

  #num_cores <- parallel::detectCores() - 1  # Use all available cores minus one
  #gsat_diffs <- parallel::mclapply(1:length(coalitions_without), compute_gsat_diff, mc.cores = num_cores)
  gsat_diffs <- lapply(1:length(coalitions_without), compute_gsat_diff)


  # Compute the Shapley value
  shap <- sapply(1:length(coalitions_without), function(i) {
    factorial(N_s[i]) * factorial(N - N_s[i] - 1) * gsat_diffs[[i]]
  }) %>% sum()

  return(shap / factorial(N))
}


# Define the contrib_shap function
#' contrib_shap_p
#'
#' evaluate the reasonable allocation GSAT of country or grouping given an emissions dataset and a list of all possible coalitions.
#' contrib_shap_p is the parallel version of contrib_shap using mclapply
#'
#' @param emissions the emissions dataset for all countries or groupings
#' @param country the country (iso3) or grouping code
#' @param coalitions the list of all coalitions of length 2^N where N is the number of countries or groupings
#'
#' @return the reasonable gsat allocation for the country
#' @export
#'
#' @examples
contrib_shap_p <- function(emissions, country, coalitions) {
  # Get the total number of coalitions
  N <- length(coalitions[[length(coalitions)]])

  # Separate coalitions with and without the country
  coalitions_without <- coalitions[!sapply(coalitions, function(coal) country %in% coal)]
  coalitions_with <- lapply(coalitions_without, function(coal) c(country, coal))
  N_s <- sapply(coalitions_without, length)

  # Define a helper function to compute gsat difference
  compute_gsat_diff <- function(i) {
    gsats1 <- emissions %>% dplyr::filter(country %in% coalitions_with[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    gsats2 <- emissions %>% dplyr::filter(country %in% coalitions_without[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    return(gsats1 - gsats2)
  }

  # Use parallel processing to compute gsat differences

  num_cores <- parallel::detectCores() - 1  # Use all available cores minus one
  gsat_diffs <- parallel::mclapply(1:length(coalitions_without), compute_gsat_diff, mc.cores = num_cores)

  # Compute the Shapley value
  shap <- sapply(1:length(coalitions_without), function(i) {
    factorial(N_s[i]) * factorial(N - N_s[i] - 1) * gsat_diffs[[i]]
  }) %>% sum()

  return(shap / factorial(N))
}

