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
library(readxl)
library(spdep)
library(lmtest)
library(sandwich)
census_api_key("e6460566746931aed6c241abe8a6e2425aa9c699", install = TRUE, overwrite = TRUE)

#### mortality data ###########
cdc_2018_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2018_drugoverdose.sas7bdat')

cdc_2019_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2019_drugoverdose.sas7bdat')
cdc_combined_mort_data_2018_2019 <- rbind(cdc_2018_mort_data,cdc_2019_mort_data)

###filtering counties except Hawai and Alaska
cdc_2018_2019_mort_data_entire_united_states <- cdc_combined_mort_data_2018_2019 %>% filter(stposto %in% c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                                                                                                           "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                                                                                                           "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                                                                                                           "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                                                                                                           "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))


#### getting fips related information for the eastern states using tidycensus ###
# Get FIPS codes for all states
united_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                   "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                   "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                   "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                   "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

Soc.2019 <- get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
                    state=united_states, survey="acs5", geometry = FALSE)

Soc.2019<- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")

fips_code <- tidycensus::fips_codes
fips_code <- fips_code %>% filter(state %in% c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                                               "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                                               "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                                               "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                                               "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))
fips_code$GEOID <- paste0(fips_code$state_code, fips_code$county_code)

Soc.2019 <- merge(fips_code,Soc.2019, by="GEOID")
### extracting state code and fips code ###
st_code_st_abb <- Soc.2019 %>% dplyr::select(state,state_code)
st_code_st_abb <- unique(st_code_st_abb)
colnames(st_code_st_abb)[1] <- "stposto"
### merging the state_fips_code with cdc_2018_2019_to_get_5_digits_fips_code###

cdc_2018_2019_mort_data_entire_united_states <- left_join(cdc_2018_2019_mort_data_entire_united_states,st_code_st_abb,
                                                          by = "stposto")
cdc_2018_2019_mort_data_entire_united_states <- cdc_2018_2019_mort_data_entire_united_states %>% 
  mutate(FullFIPS = paste0(state_code, cntfipso))
### reordering the columns###
cdc_2018_2019_mort_data_entire_united_states <- cdc_2018_2019_mort_data_entire_united_states %>%
  dplyr::select(1, 93, 94, 2:92)
colnames(cdc_2018_2019_mort_data_entire_united_states)[3] <- "GEOID"

cdc_2018_2019_mort_data_entire_united_states <- cdc_2018_2019_mort_data_entire_united_states %>% 
  filter(icd10_3 %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                        "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))

# total_County_available <- cdc_2018_2019_mort_data_east_america %>% group_by(cntfipso) %>% summarise(total_count = n())
### fitering the T CODES FOR OPIOID RELATED DEATHS PRIMARY USE CASES THROUGH
#ICD CODES X40-X44 and R-axis T400-T404, T406, T409########
cdc_2018_2019_mort_data_entire_united_states <- cdc_2018_2019_mort_data_entire_united_states %>% 
  filter(
    RAXIS2 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS3 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS4 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS5 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS6 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS7 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS8 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS9 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS10 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS11 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS12 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS13 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS14 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS15 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS16 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS17 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS18 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS19 %in% c('T400','T401', 'T402', 'T403', 'T404','T406') |
      RAXIS20 %in% c('T400','T401', 'T402', 'T403', 'T404','T406')
  )
cdc_mort_data_fips_wise_death_certificates <- cdc_2018_2019_mort_data_entire_united_states  %>% 
  group_by(stnchsxo,GEOID, year) %>% summarise(total_count = n())
##### for year 2018 ####
oods_2018 <- cdc_mort_data_fips_wise_death_certificates %>% 
  filter(year == 2018)
oods_in_county_46102 <- oods_2018 %>% 
  dplyr::filter(GEOID == "46102")
oods_in_county_46113 <- oods_2018 %>% 
  dplyr::filter(GEOID == "46113")

#### updating it by the count OF 46113 for 46102@@@
oods_2018 <- oods_2018 %>%  
  mutate(GEOID = replace(GEOID, GEOID == "46113", "46102"))
zero_death_count_fips_2018 <-  setdiff(Soc.2019$GEOID, oods_2018$GEOID)

missing_data_2018 <- data.frame(GEOID = zero_death_count_fips_2018, total_count = 0)
missing_data_2018 <- missing_data_2018 %>%
  left_join(dplyr::select(Soc.2019, GEOID, state), by = "GEOID")

colnames(missing_data_2018)[3] <- "stnchsxo"
missing_data_2018 <- missing_data_2018[,c(3,1,2)]
missing_data_2018$year <- "2018"
missing_data_2018 <- missing_data_2018[,c(1,2,4,3)]

oods_2018  <- bind_rows(oods_2018, 
                        missing_data_2018)
#### for  year 2019 ### 
oods_2019 <- cdc_mort_data_fips_wise_death_certificates %>% 
  filter(year == 2019)
zero_death_count_fips_2019 <-  setdiff(Soc.2019$GEOID, oods_2019$GEOID)

missing_data_2019 <- data.frame(GEOID = zero_death_count_fips_2019, total_count = 0)
missing_data_2019 <- missing_data_2019 %>%
  left_join(dplyr::select(Soc.2019, GEOID, state), by = "GEOID")

colnames(missing_data_2019)[3] <- "stnchsxo"
missing_data_2019 <- missing_data_2019[,c(3,1,2)]
missing_data_2019$year <- "2019"
missing_data_2019 <- missing_data_2019[,c(1,2,4,3)]
oods_2019  <- bind_rows(oods_2019, 
                        missing_data_2019)
### adding population ###
geoid_population <- Soc.2019 %>% dplyr::select(GEOID,estimate)
### using the left join to merge the population for oods_2018 and oods_2019 ###
oods_2018 <- left_join(oods_2018,geoid_population, by="GEOID")
colnames(oods_2018)[5] <- "population"
oods_2019 <- left_join(oods_2019,geoid_population, by="GEOID")
colnames(oods_2019)[5] <- "population"
#### social proximity function 2018###
counties <- counties(cb = TRUE, class = "sf")
entire_american_fips_vector <- oods_2018[order(oods_2018$GEOID),]
entire_american_fips_vector <- entire_american_fips_vector$GEOID
### filtering the american_fips_Vector from counties###
selected_counties <- counties[counties$GEOID %in% entire_american_fips_vector, ]
#### getting the centroids ###
centroids <- st_centroid(selected_counties)
centroids <- centroids[order(centroids$GEOID ),]
coords <- st_coordinates(centroids)
### getting the lat lng ###
geoid_lat_lng<- data.frame(
  GEOID = entire_american_fips_vector,
  Longitude = coords[,1],
  Latitude = coords[,2]
)
### adding lat and lng information to the county information ####
oods_2018 <- left_join(oods_2018,geoid_lat_lng, by="GEOID")
oods_2019 <- left_join(oods_2019,geoid_lat_lng, by="GEOID")
### calculating deaths per capita using mutate function #####
oods_2018 <- oods_2018 %>% 
  mutate(deaths_per_capita=total_count/population)
oods_2019 <- oods_2019 %>% 
  mutate(deaths_per_capita=total_count/population)


### deaths in social proximity ###
calculateSCI <- function(oods_df, file_path) {
  # Subset and rename columns
  social_df <- oods_df[, c(2, 5, 8)]
  colnames(social_df)[1] <- "fr_loc"
  colnames(social_df)[3] <- "deaths_per_capita"
  social_df <- social_df[order(social_df$fr_loc),]
  
  # Reading and processing the TSV file
  df_0 <- read_tsv(file_path)
  df_1 <- df_0 %>%
    dplyr::filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc) %>%
    filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
  
  df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc, fr_loc, scaled_sci))
  nodes <- df_1 %>% distinct(fr_loc)
  k <- graph.data.frame(df_for_matrix_weights, directed = FALSE, vertices = nodes)
  cumulative_sci_weighted <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE)
  cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
  diag(cumulative_sci_weighted) <- 0
  
  # Adjusting weights by population
  population <- social_df$population
  for (i in 1:ncol(cumulative_sci_weighted)) {
    cumulative_sci_weighted[, i] <- cumulative_sci_weighted[, i] * population[i]
  }
  
  row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
  cumulative_sci_weighted_test <- cumulative_sci_weighted / row_sums_cumulative_sci_weighted
  
  # Storing the matrix weight spatial format
  w_i_j <- as.matrix(cumulative_sci_weighted_test)
  diag(w_i_j) <- 0
  lw_1 <- mat2listw(w_i_j, style = 'W')
  
  # Further calculations
  v <- social_df$deaths_per_capita
  for (i in 1:ncol(cumulative_sci_weighted_test)) {
    cumulative_sci_weighted_test[, i] <- cumulative_sci_weighted_test[, i] * v[i]
  }
  
  s_minus_i <- rowSums(cumulative_sci_weighted_test)
  
  # Returning the results
  return(list(s_minus_i = s_minus_i, lw_1 = lw_1))
}

## social_proximity_results ###########
result_2018 <- calculateSCI(oods_2018, 'C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
result_2019 <- calculateSCI(oods_2019, 'C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')

########## spatial proximity results #########
calculateSpatialProximity <- function(oods_df) {
  # Selecting and preparing spatial data
  spatial_df <- oods_df %>% 
    dplyr::select(GEOID, Longitude, Latitude) %>%
    arrange(GEOID)
  
  # Calculating distance matrix
  distance_matrix <- geodist::geodist(spatial_df, measure = "geodesic") / 1000  # converting to km
  distance_matrix <- 1 + distance_matrix  # Adjusting distances
  distance_matrix <- distance_matrix ** (-1)  # Inverse distance
  diag(distance_matrix) <- 0  # Setting diagonal to zero
  
  # Setting matrix row and column names
  colnames(distance_matrix) <- spatial_df$GEOID
  rownames(distance_matrix) <- spatial_df$GEOID
  
  # Ordering oods data and calculating deaths per capita
  oods_df <- oods_df %>% arrange(GEOID)
  y <- oods_df$deaths_per_capita
  
  # Preparing a_i_j matrix
  a_i_j <- as.data.frame(distance_matrix)
  diag(a_i_j) <- 0
  a_i_j <- as.matrix(a_i_j)
  
  # Normalizing a_i_j
  normalised_scale <- rowSums(a_i_j)
  a_i_j <- a_i_j / normalised_scale
  
  # Calculating deaths in spatial proximity
  d_minus_i <- a_i_j %*% y
  
  return(d_minus_i)
}

# deaths in spatial proximity 2019 #### 
d_minus_i_2018 <- calculateSpatialProximity(oods_2018)
d_minus_i_2019 <- calculateSpatialProximity(oods_2019)




