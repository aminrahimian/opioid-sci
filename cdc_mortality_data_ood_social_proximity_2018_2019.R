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
library(tidycensus)
census_api_key("e6460566746931aed6c241abe8a6e2425aa9c699", install = TRUE, overwrite = TRUE)

### census data ####
# race_vars <- c(
#   White = "B03002_003",
#   Black = "B03002_004",
#   Native = "B03002_005",
#   Asian = "B03002_006",
#   HIPI = "B03002_007",
#   Hispanic = "B03002_012"
# )
# 
# pa_race <- get_acs(
#   geography = "county",
#   state = "PA",
#   variables = race_vars,
#   summary_var = "B03002_001",
#   year = 2019
# ) 
# 
# pa_race_percent <- pa_race %>% mutate(percent = 100 * (estimate / summary_est)) 
# pa_race_percent <- pa_race_percent %>% dplyr::select(NAME, variable, percent)
# pa_race_percent <- pa_race_percent %>% mutate(NAME=str_replace_all(pa_race_percent$NAME,"County, Pennsylvania",""))
# 
# 
# asian <- pa_race_percent %>% dplyr::filter(variable=="Asian")
# asian <- asian %>% mutate(perecent=percent/100)
# asian <- asian[,-3]
# 
# black <- pa_race_percent %>% dplyr::filter(variable=="Black")
# black <- black %>% mutate(perecent=percent/100)
# black <- black[,-3]
# 
# hipi <- pa_race_percent %>% dplyr::filter(variable=="HIPI")
# hipi <- hipi %>% mutate(perecent=percent/100)
# hipi <- hipi[,-3]
# 
# hispanic <- pa_race_percent %>% dplyr::filter(variable=="Hispanic")
# hispanic <- hispanic %>%  mutate(perecent=percent/100)
# hispanic <- hispanic[,-3]
# 
# native <-pa_race_percent %>% dplyr::filter(variable=="Native")
# native <- native %>% mutate(perecent=percent/100)
# native <- native[,-3]
# 
# white <- pa_race_percent %>% dplyr::filter(variable=="White")
# white <- white %>% mutate(perecent=percent/100)
# white <- white[,-3]
# 
# pa_hh_income <- get_acs(
#   geography = "county",
#   table = "B19001",
#   state = "PA",
#   year = 2019
# )
# 
# pa_hh_income <- pa_hh_income %>% dplyr::filter(pa_hh_income$variable == "B19001_001")
# colnames(pa_hh_income)[3] <- "hh_income"
# pa_hh_income <- pa_hh_income %>% mutate(NAME=str_replace_all(pa_hh_income$NAME,"County, Pennsylvania",""))
# pa_hh_income <- pa_hh_income %>% dplyr::select(c(GEOID,NAME,estimate))
# 
# Soc.2019 <- get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),state="PA", survey="acs5") %>% mutate(Year = "2017")
# Soc.2019 <- Soc.2019 %>% mutate(NAME=str_replace_all(Soc.2017$NAME,"County, Pennsylvania",""))
# Soc.2019 <- Soc.2019[,-c(3,5,6)]
# colnames(Soc.2019)[3] <- "population"
# Soc.2019
# census_data_frame <- merge(Soc.2019,pa_hh_income,by="NAME")
#### mortality data ###########
cdc_2018_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2018_drugoverdose.sas7bdat')
cdc_2019_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2019_drugoverdose.sas7bdat')
cdc_combined_mort_data_2018_2019 <- rbind(cdc_2018_mort_data,cdc_2019_mort_data)

cdc_2018_2019_mort_data_east_america <- cdc_combined_mort_data_2018_2019 %>% filter(stposto==c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "NJ", "PA", "DE", 
                                                                                     "MD", "DC", "MI", "OH", "IN", "IL", "WI", "WV", "VA", "NC", 
                                                                                     "TN", "KY", "SC", "GA", "AL", "MS", "FL"))

#### getting fips related information for the eastern states using tidycensus ###
# Get FIPS codes for all states
all_fips <- fips_codes("state")

# Specify the states you are interested in
interested_states <- c("AL", "CT", "DC", "DE", "FL", "GA", "IL", "IN", "KY", "MA", 
                       "MD", "ME", "MI", "MS", "NC", "NH", "NJ", "NY", "OH", "PA", 
                       "RI", "SC", "TN", "VA", "VT", "WI", "WV")

# Filter FIPS codes for the interested states
state_fips <- all_fips[all_fips$abbr %in% interested_states, ]

# Print the FIPS codes
print(state_fips[, c("name", "fips", "abbr")])




# total_County_available <- cdc_2018_2019_mort_data_east_america %>% group_by(cntfipso) %>% summarise(total_count = n())
### fitering the T CODES FOR OPIOID RELATED DEATHS PRIMARY USE CASES THROUGH
#ICD CODES X40-X44 and R-axis T400-T404, T406, T409########
cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>% 
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
### to get the count of deaths in each county ###
cdc_mort_data_fips_wise_death_certificates <-cdc_2018_2019_mort_data_east_america  %>% group_by(stposto,cntfipso) %>% summarise(total_count = n())
## the filtered data show OOD counts for 62 counties we have missing fips data for 5 counties ####
## first padding the 42 infront###
cdc_mort_data_fips_wise_death_certificates$cntfipso <- paste(42, cdc_mort_data_fips_wise_death_certificates$cntfipso)
cdc_mort_data_fips_wise_death_certificates$cntfipso <-gsub(" ", "", cdc_mort_data_fips_wise_death_certificates$cntfipso)
pa_counties <- counties(state = "PA", cb = TRUE, year = 2020) %>% 
  dplyr::select(GEOID, NAME)
missing_counties <- setdiff(pa_counties$GEOID,cdc_mort_data_fips_wise_death_certificates$cntfipso)
#### missing counties contain 42067-Juniata, 42005-Armstrong 
#### 42023-Cameron, 42053-Forst, 42105-Potter
### two options pad them with zero or use the previous overdose deaths information from pennsylvania stastics department##
cntfipso <- c('42067','42005', '42023','42053', '42105')
total_count <- c(0,0,0,0,0)
missing_county_df <- data.frame(cntfipso,total_count)
## adding the missing county data in the cdc mort data fips certificates data frame ###
cdc_mort_data_fips_wise_death_certificates <- rbind(cdc_mort_data_fips_wise_death_certificates,missing_county_df)
#### calculating death per capita in each county ###
cdc_mort_data_fips_wise_death_certificates$cntfipso <- as.numeric(cdc_mort_data_fips_wise_death_certificates$cntfipso)
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>% 
  arrange(cdc_mort_data_fips_wise_death_certificates$cntfipso)
cdc_mort_data_fips_wise_death_certificates$population <- census_data_frame$population
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>% 
  mutate(deaths_per_capita= total_count/population)
## changing colnames of fipso to fr_loc to filter the data ####
cdc_mort_data_fips_wise_death_certificates$cntfipso <- as.character(cdc_mort_data_fips_wise_death_certificates$cntfipso)
colnames(cdc_mort_data_fips_wise_death_certificates)[1] <- "fr_loc"
############ social proximity #########
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
df_1 <- df_0 %>% dplyr::filter(user_loc %in% cdc_mort_data_fips_wise_death_certificates$fr_loc & fr_loc %in% cdc_mort_data_fips_wise_death_certificates$fr_loc)
df_1$user_loc <- as.numeric(as.character(df_1$user_loc))
df_1$fr_loc <- as.numeric(as.character(df_1$fr_loc))
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc)))) ##unique pairs + self loops
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
nodes <- df_1 %>% distinct(fr_loc)
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- census_data_frame$population
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
v <- aggregated_ood_per_capita_events_in_each_county$deaths_per_capita
for(i in 1:ncol(cumulative_sci_weighted_test)){
  cumulative_sci_weighted_test[,i] <- cumulative_sci_weighted_test[,i] * v[i]
}
sci_proximity_county_2013_2017 <- rowSums(cumulative_sci_weighted_test)
