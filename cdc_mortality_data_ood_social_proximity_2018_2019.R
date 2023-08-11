library(tidyverse)
library(igraph)
library(zipcodeR)
library(tidyverse)
library(tigris) # for fips_codes data
library(rvest) #for web scrapping
library(zipcodeR) #for ZIP codes
library(fuzzyjoin)
library(tidycensus)
library(geodist)
library(haven)
library(scales)
#### mortality data ###########
cdc_2018_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2018_drugoverdose.sas7bdat')
cdc_2019_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2019_drugoverdose.sas7bdat')
cdc_combined_mort_data_2018_2019 <- rbind(cdc_2018_mort_data,cdc_2019_mort_data)

cdc_2018_2019_mort_data_pa <- cdc_combined_mort_data_2018_2019 %>% filter(stposto=='PA')
### fitering the T CODES FOR OPIOID RELATED DEATHS PRIMARY USE CASES THROUGH
#ICD CODES X40-X44 and R-axis T400-T404, T406, T409########
cdc_2018_2019_mort_data_pa <- cdc_2018_2019_mort_data_pa %>% 
  filter(
    RAXIS2 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS3 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS4 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS5 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS6 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS7 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS8 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS9 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS10 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS11 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS12 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS13 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS14 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS15 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS16 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS17 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS18 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS19 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409') |
      RAXIS20 %in% c('T400','T401', 'T402', 'T403', 'T404','T406', 'T409')
  )
cdc_mort_data_fips_wise_death_certificates <- cdc_2018_2019_mort_data_pa %>% group_by(cntfipso) %>% summarise(total_count = n())