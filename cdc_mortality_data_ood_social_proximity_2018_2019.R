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
library(tigris)### for lat and lng
library(sf)### for lat and lng 

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
eastern_states <- c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "NJ", "PA", "DE", 
                    "MD", "DC", "MI", "OH", "IN", "IL", "WI", "WV", "VA", "NC", 
                    "TN", "KY", "SC", "GA", "AL", "MS", "FL")
Soc.2019 <- get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
                    state=eastern_states, survey="acs5", geometry = FALSE)

Soc.2019<- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")

fips_code <- tidycensus::fips_codes
fips_code <- fips_code %>% filter(state %in%  c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "NJ", "PA", "DE", 
                                             "MD", "DC", "MI", "OH", "IN", "IL", "WI", "WV", "VA", "NC", 
                                             "TN", "KY", "SC", "GA", "AL", "MS", "FL"))
fips_code$GEOID <- paste0(fips_code$state_code, fips_code$county_code)

Soc.2019 <- merge(fips_code,Soc.2019, by="GEOID")
### extracting state code and fips code ###
st_code_st_abb <- Soc.2019 %>% select(state,state_code)
st_code_st_abb <- unique(st_code_st_abb)
colnames(st_code_st_abb)[1] <- "stposto"
### merging the state_fips_code with cdc_2018_2019_to_get_5_digits_fips_code###

cdc_2018_2019_mort_data_east_america <- left_join(cdc_2018_2019_mort_data_east_america,st_code_st_abb,
                                              by = "stposto")
cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>% 
  mutate(FullFIPS = paste0(state_code, cntfipso))
### reordering the columns###
cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>%
  select(1, 93, 94, 2:92)
colnames(cdc_2018_2019_mort_data_east_america)[3] <- "GEOID"

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
cdc_mort_data_fips_wise_death_certificates <- cdc_2018_2019_mort_data_east_america  %>% group_by(stnchsxo,GEOID) %>% summarise(total_count = n())
## the filtered data show OOD counts for 62 counties we have missing fips data for 5 counties ####
# Find GEOIDs that are in soc.2019 but not in cdc_mort_data_fips_wise_death_certificates

# Now try the setdiff() function again
missing_GEOIDs <- setdiff(Soc.2019$GEOID, cdc_mort_data_fips_wise_death_certificates$GEOID)

# Create a new data frame with the missing GEOIDs and a total_count of 0
missing_data <- data.frame(GEOID = missing_GEOIDs, total_count = 0)
missing_data <- missing_data %>%
  left_join(select(Soc.2019, GEOID, state), by = "GEOID")

colnames(missing_data)[3] <- "stnchsxo"
missing_data <- missing_data[,c(3,1,2)]
# Add the missing data to the original data frame
cdc_mort_data_fips_wise_death_certificates <- bind_rows(cdc_mort_data_fips_wise_death_certificates, 
                                                        missing_data)

### adding the population to the cdc_mort_data_fips_wise_death_certificates #####
geoid_population <- Soc.2019 %>% select(GEOID,estimate)
### using the left join to merge it ###
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,geoid_population, by="GEOID")
colnames(cdc_mort_data_fips_wise_death_certificates)[4] <- "population"
### getting the lat and lng for counties###
### getting the shapefile for US counties using SF package####
counties <- counties(cb = TRUE, class = "sf")
east_american_fips_vector <- cdc_mort_data_fips_wise_death_certificates[order(cdc_mort_data_fips_wise_death_certificates$GEOID),]
east_american_fips_vector <- east_american_fips_vector$GEOID
### filtering the east_american_fips_Vector from counties###
selected_counties <- counties[counties$GEOID %in% east_american_fips_vector, ]
#### getting the centroids ###
centroids <- st_centroid(selected_counties)
centroids <- centroids[order(centroids$GEOID ),]
coords <- st_coordinates(centroids)
### getting the lat lng ###
geoid_lat_lng<- data.frame(
  GEOID = east_american_fips_vector,
  Longitude = coords[,1],
  Latitude = coords[,2]
)
### adding lat and lng information to the county information ####
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,
                                                        geoid_lat_lng, by="GEOID")
### calculating deaths per capita using mutate function #####
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>% 
  mutate(deaths_per_capita=total_count/population)

#### calculating deaths social proximity #####
### first creating a data frame that only contains geoid,population and deaths_per_capita###
social_df <- cdc_mort_data_fips_wise_death_certificates[,c(2,4,7)]
colnames(social_df )[1] <- "fr_loc"
colnames(social_df)[3] <- "deaths_per_capita"
social_df <- social_df[order(social_df$fr_loc),]
#### calculating sci_proximity####
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
df_1 <- df_0 %>% dplyr::filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc)
#df_1$user_loc <- as.numeric(as.character(df_1$user_loc))
#df_1$fr_loc <- as.numeric(as.character(df_1$fr_loc))
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc)))) ##unique pairs + self loops
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
nodes <- df_1 %>% distinct(fr_loc)
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- social_df$population
### numerator of w_i_j: population_i*cumulative_sci_weighted_i
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
v <- social_df$deaths_per_capita
for(i in 1:ncol(cumulative_sci_weighted_test)){
  cumulative_sci_weighted_test[,i] <- cumulative_sci_weighted_test[,i] * v[i]
}
s_minus_i <- rowSums(cumulative_sci_weighted_test)
################### spatial proximity ##########
#### physical proximity ###
### we first create the spatial df that contains GEOID, LAT AND LNG
spatial_df <- cdc_mort_data_fips_wise_death_certificates %>% select(GEOID,Longitude,Latitude)
### dropping the state name###
spatial_df <- spatial_df[,-1]
### ordering it so the geoid are alligned from 01001
spatial_df <- spatial_df[order(spatial_df$GEOID),]
###calculating the distance between location
distance_matrix <-  geodist(spatial_df, measure = 'geodesic') / 1000 # converting it to km
### adding 1 so the distances are not very small and still computable ### to the distance 
distance_matrix <- 1 + distance_matrix
### inverse distance
distance_matrix <- distance_matrix**(-1)
### to insure the distance with itself is zero
diag(distance_matrix) <- 0
### providing the matrix row name and column name
colnames(distance_matrix) <- spatial_df$GEOID
rownames(distance_matrix) <- spatial_df$GEOID
### ordering the cdc_mort_data_fips for population so each geoid in the spatial df
###has respecitve population associated with it##
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates[order(cdc_mort_data_fips_wise_death_certificates$GEOID),]
### y stores the deaths per capita ###
y <- cdc_mort_data_fips_wise_death_certificates$deaths_per_capita
### a_i_j distance matrix and again ensuring that digonals are zero##
a_i_j <- data.frame(distance_matrix)
diag(a_i_j) <- 0
a_i_j <- as.matrix(a_i_j)
# Normalize a_i_j
normalised_scale <- rowSums(a_i_j)
a_i_j <- a_i_j / normalised_scale

# Perform matrix multiplication for deaths in spatial proximity ###
d_minus_i <- a_i_j %*% y

#### now adding s_minus_i and d_minus_i in the cdc_mort_data_fips_wise_death_certificates#### 
cdc_mort_data_fips_wise_death_certificates <- cbind(cdc_mort_data_fips_wise_death_certificates,s_minus_i)
colnames(cdc_mort_data_fips_wise_death_certificates)[8] <- "deaths_in_social_proximity"
cdc_mort_data_fips_wise_death_certificates <- cbind(cdc_mort_data_fips_wise_death_certificates,d_minus_i)
colnames(cdc_mort_data_fips_wise_death_certificates)[9] <- "deaths_in_spatial_proximity"







