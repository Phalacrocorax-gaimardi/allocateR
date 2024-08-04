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
#' A hector model instance (core) with zero-ed anthropogenic and volcanic sulfur emissions. Natural methane and nitrous oxide emissions
#' values (in units Tg CH4 and Tg N) are chosen to produce a 1750-2022 warming not more than one millionth degC.This allows "leave one in" contributions
#' of small countries to be evaluated in Hector.
#'
#' @param natural_n2o Tuned natural N2O emissions
#' @param natural_ch4 Tuned natural CH4 emissions
#' @param alpha the aerosol scale factor (default 1 unitless)
#' @param S Equilibrium climate sensitivity (default 3 degC)
#' @param diff ocean heat diffusivity (default 2.38 cm^2/s)
#' @param q10_rh Q10 respiration temperature dependence parameter (1.76 unitless)
#' @param npp_flux0 pre-industrial NPP (default 56.2 Pg C y-1)
#' @param beta carbon fertilisation (default 0.53 unitless)
#'
#' @return a hector core
#' @export
#'
#' @examples
make_null_core <- function(natural_n2o=9.96,natural_ch4=337.7725,alpha=1,S=3,diff=2.38,q10_rh=1.76,npp_flux0=56.2,beta=0.53){

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
   #set paramaters
   hector::setvar(nullcore,NA,"alpha",alpha,"(unitless)")
   hector::setvar(nullcore,NA,"S",S,"degC")
   hector::setvar(nullcore,NA,"diff",diff,"cm2/s")
   hector::setvar(nullcore,NA,"q10_rh",q10_rh,"(unitless)")
   hector::setvar(nullcore,NA,"beta",beta,"(unitless)")


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
  emissions_DT[,.(value=sum(value)), by=.(year,variable,units)]
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
#' @param group a country (iso3) or grouping code
#'
#' @return the reasonable gsat allocation for the country
#' @export
#'
#' @examples
contrib_shap <- function(emissions, group){
  # Get the total number of coalitions
  groups <- emissions %>% dplyr::pull(grouping) %>% unique()
  if(!(group %in% groups)) stop("country or grouping not present in emissions grouping")
  coalitions <- get_coalitions(groups)

  N <- length(coalitions[[length(coalitions)]])

  # Separate coalitions with and without the country
  coalitions_without <- coalitions[!sapply(coalitions, function(coal) group %in% coal)]
  coalitions_with <- lapply(coalitions_without, function(coal) c(group, coal))
  N_s <- sapply(coalitions_without, length)

  # Define a helper function to compute gsat difference
  compute_gsat_diff <- function(i) {
    gsats1 <- emissions %>% dplyr::filter(grouping %in% coalitions_with[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    gsats2 <- emissions %>% dplyr::filter(grouping %in% coalitions_without[[i]]) %>% get_coalition_emissions() %>% get_gsat()
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
#' contrib_shap_p is the parallel version of contrib_shap using mclapply for use Linux systems. Does not work on Windows.
#'
#' @param emissions a grouped emissions dataset such as that produced by allocateR::regroup_emissions()
#' @param group a country (iso3) or unfccc group code
#'
#' @return a reasonable gsat allocation value.
#' @export
#'
#' @examples
contrib_shap_p <- function(emissions, group) {
  # Get the total number of coalitions
  groups <- emissions %>% dplyr::pull(grouping) %>% unique()
  if(!(group %in% groups)) stop("country or grouping not present in emissions grouping")
  coalitions <- get_coalitions(groups)

  N <- length(coalitions[[length(coalitions)]])

  # Separate coalitions with and without the country
  coalitions_without <- coalitions[!sapply(coalitions, function(coal) group %in% coal)]
  coalitions_with <- lapply(coalitions_without, function(coal) c(group, coal))
  N_s <- sapply(coalitions_without, length)

  # Define a helper function to compute gsat difference
  compute_gsat_diff <- function(i) {
    gsats1 <- emissions %>% dplyr::filter(grouping %in% coalitions_with[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    gsats2 <- emissions %>% dplyr::filter(grouping %in% coalitions_without[[i]]) %>% get_coalition_emissions() %>% get_gsat()
    return(gsats1 - gsats2)
  }

  # Use parallel processing to compute gsat differences

  num_cores <- parallel::detectCores() - 1  # Use all available cores minus one
  gsat_diffs <- parallel::mclapply(1:length(coalitions_without), compute_gsat_diff, mc.cores = num_cores)
  #gsat_diffs <- lapply(1:length(coalitions_without), compute_gsat_diff)


  # Compute the Shapley value
  shap <- sapply(1:length(coalitions_without), function(i) {
    factorial(N_s[i]) * factorial(N - N_s[i] - 1) * gsat_diffs[[i]]
  }) %>% sum()

  return(shap / factorial(N))
}


#' regroup_emissions
#'
#' regroup_emissions regroups a country-level emissions dataset, allowing the warming impact of selected individual countries to be found. Groups in groupings_drop are moved into the "other" grouping category.
#' Countries in a vector of iso codes countries_add appear with group label equal to the country code. The returned dataframe
#' has emissions aggregated by the new grouping labels and can be used with using allocateR::contrib_shap.
#'
#' @param emissions country-level emissions dataset with original grouping labels (based on e.g. unfccc_groupings_cohesion)
#' @param countries_add A vector of countries to add so that their gsat contribution can be calculated.
#' @param groupings_sub A vector of groups to subtract i.e. to assign to the "other" category
#'
#' @return a regrouped emissions dataset with grouping code and no country codes
#' @export
#'
#' @examples
regroup_emissions <- function(emissions, countries_add=c("nzl","irl","ury"),groupings_sub = c("cvf","mp","g77","eig","cacam","sids","ailac","alba","rn")){

  emissions_f <- emissions %>% dplyr::mutate(grouping=ifelse(grouping %in% groupings_sub,"other",grouping))

  filter_groups <- emissions_f %>% dplyr::filter(country %in% countries_add) %>% dplyr::pull(grouping) %>% unique()
  #remove filter groups
  emissions_f <- emissions_f %>% dplyr::filter(!(grouping %in% filter_groups)) %>% dplyr::group_by(year,variable,grouping) %>% dplyr::summarise(variable=variable[1],value=sum(value),units=units[1])
  #restore filter_groups excluding add_countries
  for(i in seq_along(countries_add)){
    #
    emit <- get_coalition_emissions(emissions %>% dplyr::filter(grouping == filter_groups[i], country != countries_add[i]))
    emit$grouping <- filter_groups[i]
    emissions_f <- emissions_f %>% dplyr::bind_rows(emit)
  }
  #restore add_countries
  for(i in seq_along(countries_add))
    emissions_f <- emissions_f %>% dplyr::bind_rows(emissions %>% dplyr::filter(country == countries_add[i]) %>% dplyr::select(-grouping) %>% dplyr::rename("grouping"=country))
  print(paste("Number of groups=", dplyr::n_distinct(emissions_f$grouping)))
  emissions_f %>% dplyr::select(year,variable,value,units,grouping) %>% return()
}



#' contrib_approx
#'
#' Approximation to country or group level contribution to global warming based on LOO and LOI values for the entity.
#'
#' Set type=country if working with a country-level emissions dataset. Set type=group if working with a grouped emissions
#' dataset created by allocateR::regroup_emissions. This allows comparison with output produced by allocateR::contrib_shap which
#' requires grouped data
#'
#' @param emissions country or group level emissions dataset
#' @param type country or group
#' @param entity iso3 country or unfccc group code
#'
#' @return a one-row data-frame giving gsat_loo, gsat_loi and gsat_approx
#' @export
#'
#' @examples
contrib_approx <- function(emissions, type="country",entity){
  #get the contribution of country_a belonging to the group of country present in emissions
  if(type=="country"){
  if(!(entity %in% emissions$country %>% unique())) stop("country not in emissions")
  emissions_a <- emissions %>% dplyr::filter(country==entity) %>% get_coalition_emissions()
  emissions_b <- emissions %>% dplyr::filter(country!=entity) %>% get_coalition_emissions()
  emissions_ab <- emissions %>% get_coalition_emissions()
  }
  if(type=="group"){
    if(!(entity %in% emissions$grouping %>% unique())) stop("grouping not in emissions")
    emissions_a <- emissions %>% dplyr::filter(grouping==entity) %>% get_coalition_emissions()
    emissions_b <- emissions %>% dplyr::filter(grouping!=entity) %>% get_coalition_emissions()
    emissions_ab <- emissions %>% get_coalition_emissions()
  }
  gsat_loi <- get_gsat(emissions_a)
  gsat_loo <- get_gsat(emissions_ab)-get_gsat(emissions_b)
  return(tibble::tibble(entity=entity,gsat_loo=gsat_loo,gsat_loi=gsat_loi, gsat_approx=(gsat_loo + gsat_loi)/2))
}

#sample usage



