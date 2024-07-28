
# allocateR

<!-- badges: start -->
<!-- badges: end -->

allocateR finds a country's contribution to global warming. 

## Background

Country level warming contributions are computed based on a concept from cooperative game theory i.e. as weighted sums of marginal contributions over all possible coalitions of other countries (Shapley value). In practice, this is feasible for about $\approx$ 20 countries or country groupings. 

## Installation

You can install the development version of allocateR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Phalacrocorax-gaimardi/allocateR")
```

## Simple Climate Model

allocateR uses (and installs) the Hector Simple Climate Model v3.2. The model can be run with default parameters or with adjusted values.


## Data

allocateRv0.1.x uses country level data from the Community Emissions Data System (CEDS). This dataset and excludes LULUCF and halogenated gas emissions.

As there are more than 200 National entities, a choice of country groupings must be made. A natural approach is to use UNFCCC Negotiating Groups. However, countries may be members of multiple UNFCCC Negotiating Groups and therefore a method is needed to assign each country to a unique group. The package includes 18 UNFCCC Negotiating Groups, with non-group members and international shipping and aviation forming an additional two groups. Two potential potential groupings included in the package are:
```
unfccc_groupings_power
```
This assigns each country to the group with the largest 2022 emissions. On the other hand,
```
unfccc_groupings_cohesion
```
assigns each country to the group with the smallest membership. Other alternatives are possible.

## Example

Evaluate the contribution of USA to global warming, considering only emissions from the Umbrella Group of countries

``` r
library(allocateR)
library(tidyverse)
#Countries of the Umbrella Group
ug_countries <- unfccc_groupings_cohesion %>% filter(grouping=="ug") %>% arrange(-cumulative) %>% pull(country)
#CEDS emissions dataset combined with group membership
emissions <- emissions_ceds %>% inner_join(unfccc_groupings_cohesion %>% select(country,grouping))
#Ubrella Group emissions
emissions_ug <- emissions %>% filter(grouping=="ug")
#list all possible coalitions ($2^N$), including the null coalition:
coalitions_ug <- get_coalitions(ug_countries)
#A core object with zero anthropogenic emissions, zero volcanic emissions and natural CH4 and N2O emissions only. A reduced aerosol scaling factor of 0.75 is used in this example
nullcore <- make_null_core(alpha=0.75)
#calculate the "reasonable contribution"" of USA to the warming caused by the Umbrella Group countries.
contrib_shap(emissions_ug,"usa",coalitions_ug)
```





