


##############################################
# Use the matilda package to generate ensembles
################################################
library(matilda)
ini <- system.file("input/hector_ssp126.ini", package = "hector")
c_ssp126 <- newcore(ini)
#setvar(c_ssp126,NA,VOLCANIC_SCALE(),0,"(unitless)")
#set ceds variables to ceds values, leave other variables at ssp126 values
ceds <- get_coalition_emissions(emissions_ceds) %>% as_tibble()
allvars <- fxntable %>% filter(str_detect(string,"emissions")) %>% pull(string)
cedsvars <- (ceds$variable %>% unique())
non_cedsvars <- allvars[!(allvars %in% cedsvars) ]
for(var in cedsvars){
  
  setvar(c_ssp126,1851:2022,var, ceds %>% filter(variable==var) %>% pull(value),unitstable %>% filter(variable==var) %>% pull(units))
}
run(c_ssp126)
fetchvars(c_ssp126,2022,GLOBAL_TAS())
#
param_values <- generate_params(c_ssp126, draws = 100)
param_values$scenario <- 1:dim(param_values)[1]
apply(param_values,2, mean)
#check no negative params
apply(param_values,2, min)
apply(param_values,2, max)
#perfect agreement with 
emissions <- get_coalition_emissions(emissions_ceds)
gsat <- tibble()
for(i in 1:dim(param_values)[1]){
  print(i)
  setvar(c_ssp126,NA,AERO_SCALE(), param_values[i,"AERO_SCALE"], "(unitless)")
  setvar(c_ssp126,NA,ECS(),param_values[i,"ECS"],"degC")
  setvar(c_ssp126,NA,BETA(),param_values[i,"BETA"],"(unitless)")
  setvar(c_ssp126,NA,Q10_RH(),param_values[i,"Q10_RH"],"(unitless)")
  setvar(c_ssp126,NA,DIFFUSIVITY(),param_values[i,"DIFFUSIVITY"],"cm2/s")
  setvar(c_ssp126,NA,NPP_FLUX0(),param_values[i,"NPP_FLUX0"], "Pg C/yr" )
  run(c_ssp126)
  gsat <- gsat %>% bind_rows(fetchvars(c_ssp126,2003:2022,GLOBAL_TAS(),scenario=i))
}
results <- param_values %>% inner_join(gsat)
results <- results %>% group_by(scenario) %>% summarise(gsat_2003_2022 = mean(value)) %>% inner_join(param_values)
#
#
#2003-2022 GMST 1.03 [0.87-1.13] 90% confidence interval
#
pnorm(0.87,mean=1., sd=0.08)
pnorm(1.13,mean=1., sd=0.08)
results <- results %>% mutate(score = dnorm(gsat_2003_2022,mean=1,sd=0.08)/dnorm(1,mean=1,sd=0.08))

results_screened <- results %>% sample_frac(0.5,weight=score) #%>% pull(score) %>% mean()

results_screened$gsat_2003_2022 %>% sd()

