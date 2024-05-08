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

Soc.2019 <- get_estimates(geography = "county", year=2019, product = "population",state=united_states, geometry = TRUE )
# get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
#state=eastern_states, survey="acs5", geometry = TRUE)
Soc.2019 <- Soc.2019 %>% filter(variable=="POP")
Soc.2019 <- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")

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
geoid_population <- Soc.2019 %>% dplyr::select(GEOID,value)
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

#### adding social and spatial df to the ood_s data_frame ####
oods_2018 <- oods_2018[order(oods_2018$GEOID),]
oods_2019 <- oods_2019[order(oods_2019$GEOID),]
oods_2018 <- cbind(oods_2018,result_2018$s_minus_i)
colnames(oods_2018)[9] <- "deaths_in_social_proximity"
oods_2018 <- cbind(oods_2018,d_minus_i_2018)
colnames(oods_2018)[10] <- "deaths_in_spatial_proximity"

oods_2019 <-cbind(oods_2019,result_2019$s_minus_i)
oods_2019 <- cbind(oods_2019,d_minus_i_2019)
colnames(oods_2019)[9] <- "deaths_in_social_proximity"
colnames(oods_2019)[10] <- "deaths_in_spatial_proximity"


#### clinical covariates ###
#### naloxone ####
naloxone_2018 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2018.sas7bdat")
naloxone_2019 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2019.sas7bdat")
total_naloxone <- rbind(naloxone_2018, naloxone_2019)
total_naloxone <- total_naloxone %>% filter(state_and_county_fip  %in% oods_2018$GEOID )
total_naloxone <- total_naloxone %>% group_by(county_nm,state_and_county_fip,year) %>% 
  summarise(avg_total_rx = mean(total_rx, na.rm = TRUE))

### total naloxone subsetting geoid avg_total_rx ####
total_naloxone <- total_naloxone[,-1]
colnames(total_naloxone)[1] <- "GEOID"
#total_naloxone$avg_total_rx <- rescale(total_naloxone$avg_total_rx,to=c(0,1))
total_naloxone <- total_naloxone[order(total_naloxone$GEOID),]
#### total_naloxone #####
total_naloxone_2018 <- total_naloxone %>% group_by(total_naloxone$year) %>% filter(year == 2018)
total_naloxone_2018 <- total_naloxone_2018[,-4]
zero_naloxone_2018 <- setdiff(oods_2018$GEOID,total_naloxone_2018$GEOID)
df_zero_naloxone_2018 <- data.frame(GEOID=zero_naloxone_2018,year= rep(2018),avg_total_rx=rep(0))
total_naloxone_2018 <- rbind(total_naloxone_2018,df_zero_naloxone_2018)
total_naloxone_2018 <- total_naloxone_2018[order(total_naloxone_2018$GEOID),]
####### total_naloxone 2019 #######
total_naloxone_2019 <- total_naloxone %>% group_by(total_naloxone$year) %>% filter(year == 2019)
total_naloxone_2019 <- total_naloxone_2019[,-4]
zero_naloxone_2019 <- setdiff(oods_2019$GEOID,total_naloxone_2019$GEOID)
df_zero_naloxone_2019 <- data.frame(GEOID=zero_naloxone_2019, year= rep(2019), avg_total_rx=rep(0))
total_naloxone_2019 <- rbind(total_naloxone_2019,df_zero_naloxone_2019)
total_naloxone_2019 <- total_naloxone_2019[order(total_naloxone_2019$GEOID),]
#### adding naloxone to oods_df### 
oods_2018 <- left_join(oods_2018,total_naloxone_2018,by="GEOID")
oods_2018 <- oods_2018[,-11]
oods_2018 <- oods_2018 %>%
  mutate(avg_total_rx=avg_total_rx/population)
oods_2019 <- left_join(oods_2019,total_naloxone_2019,by="GEOID")
oods_2019 <- oods_2019[,-11]
oods_2019 <- oods_2019 %>%
  mutate(avg_total_rx=avg_total_rx/population)
#### clinical covariates odr #####
opioids_2018<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2018.sas7bdat")
opioids_2019 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2019.sas7bdat")
total_opioids <- rbind(opioids_2018,opioids_2019)
total_opioids <- total_opioids %>% filter(state_fip_county_fip %in% 
                                            Soc.2019$GEOID )
total_opioids <- total_opioids %>% group_by(county_nm, state_fip_county_fip,year) %>% 
  summarise( cumulative_total_dose=sum(total_dose))

total_opioids <- total_opioids[,-1]
colnames(total_opioids)[1] <- "GEOID"

## odr 2018 ###
total_opioids_2018 <- total_opioids %>% group_by(year) %>% filter(year==2018)
zero_odr_2018 <- setdiff(Soc.2019$GEOID, total_opioids_2018$GEOID)
df_zero_odr_2018 <- data.frame(GEOID=zero_odr_2018, year= rep(2018), cumulative_total_dose=rep(0))
total_opioids_2018 <- rbind(total_opioids_2018,df_zero_odr_2018)

### odr 2019 ######
total_opioids_2019 <- total_opioids %>% group_by(year) %>% filter(year==2019)
zero_odr_2019 <- setdiff(Soc.2019$GEOID, total_opioids_2019$GEOID)
df_zero_odr_2019 <- data.frame(GEOID=zero_odr_2019, year= rep(2019), cumulative_total_dose=rep(0))
total_opioids_2019 <- rbind(total_opioids_2019,df_zero_odr_2019)
### joining odr_2018 and oder_2019 with oods_df###
oods_2018 <- left_join(oods_2018,total_opioids_2018, by="GEOID")
oods_2019 <- left_join(oods_2019, total_opioids_2019, by="GEOID")
oods_2018 <- oods_2018[,-12]
oods_2019 <- oods_2019[,-12]
oods_2018 <- oods_2018 %>%
  mutate(cumulative_total_dose=cumulative_total_dose/population)
oods_2019 <- oods_2019 %>%
  mutate(cumulative_total_dose=cumulative_total_dose/population)
#### beupromorphne ###
beupromorphine_2018 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2018.sas7bdat")
beupromorphine_2019 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2019.sas7bdat")
total_beupromorphine <- rbind(beupromorphine_2018,beupromorphine_2019)
total_beupromorphine <- total_beupromorphine %>% filter(state_fip_county_fip %in% 
                                                          Soc.2019$GEOID )
total_beupromorphine  <- total_beupromorphine  %>% group_by(county_nm, state_fip_county_fip, year) %>% 
  summarise( cumulative_total_beupromorphine=mean(total_rx))
total_beupromorphine  <- total_beupromorphine [,-1]
colnames(total_beupromorphine )[1] <- "GEOID"
#### beupromorphine 2018###
total_beupromorphine_2018 <- total_beupromorphine %>% filter(year==2018 )
zero_beupromorphine_2018 <- setdiff(Soc.2019$GEOID,total_beupromorphine_2018$GEOID)
df_zero_beupromorphine_2018 <- data.frame(GEOID=zero_beupromorphine_2018 , year= rep(2018), cumulative_total_beupromorphine=rep(0))
total_beupromorphine_2018  <- rbind(total_beupromorphine_2018 ,df_zero_beupromorphine_2018 )
#### beupromorphine 2019###
total_beupromorphine_2019 <- total_beupromorphine %>% filter(year == 2019)
zero_beupromorphine_2019 <- setdiff(Soc.2019$GEOID, total_beupromorphine_2019$GEOID)
df_zero_beupromorphine_2019 <- data.frame(GEOID = zero_beupromorphine_2019, year = rep(2019), cumulative_total_beupromorphine = rep(0))
total_beupromorphine_2019 <- rbind(total_beupromorphine_2019, df_zero_beupromorphine_2019)
### adding beupromorphine 2018 and 2019 to oods_df ####
oods_2018 <- left_join(oods_2018,total_beupromorphine_2018, by="GEOID")
oods_2019 <- left_join(oods_2019,total_beupromorphine_2019, by="GEOID")
oods_2018 <- oods_2018[,-13]
oods_2019 <- oods_2019[,-13]
oods_2018 <- oods_2018 %>%
  mutate(cumulative_total_dose=cumulative_total_beupromorphine/population)
oods_2019 <- oods_2019 %>%
  mutate(cumulative_total_dose=cumulative_total_beupromorphine/population)
#### fentanyl ####
fentanyl_2018 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NFLIS Data tables/NFLIS Data tables/2018NFLISWebsiteTable3_for_analysis.csv')
### slicing the data to get states and the related drugs
fentanyl_2018_a  <- fentanyl_2018 %>% slice(1:49)
## removing column 1 ##
####
fentanyl_2018_a[, 2:14] <- lapply(fentanyl_2018_a[, 2:14], as.numeric)
fentanyl_2018_a[is.na(fentanyl_2018_a)] <- 0

cumulative_fentanyl_2018_a <- data.frame(
  State = colnames(fentanyl_2018_a)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2018_a[-1])
)
rownames(cumulative_fentanyl_2018_a) <- 1:nrow(cumulative_fentanyl_2018_a)



### fentanyl_a, fentanyl_b and so these represents different state wise enteries for fentanyl
### since the initial data frame was not clean we had to slice it and extract them as independent enteries
### we repeated the steps done for fentanyl_a for fentanyl_b ##
fentanyl_2018_b <- fentanyl_2018 %>% slice(50:99)
colnames(fentanyl_2018_b) <- fentanyl_2018_b[1,]
colnames(fentanyl_2018_b)[1] <- "Drug"
fentanyl_2018_b <- fentanyl_2018_b[-1,]
fentanyl_2018_b[, 2:14] <- lapply(fentanyl_2018_b[, 2:14], as.numeric)
fentanyl_2018_b[is.na(fentanyl_2018_b)] <- 0

cumulative_fentanyl_2018_b <- data.frame(
  State = colnames(fentanyl_2018_b)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2018_b[-1])
)
rownames(cumulative_fentanyl_2018_b) <- 1:nrow(cumulative_fentanyl_2018_b)

## similarly for fentanyl_c ##
fentanyl_2018_c <- fentanyl_2018 %>% slice(100:149)
colnames(fentanyl_2018_c) <- fentanyl_2018_c[1,]
colnames(fentanyl_2018_c)[1] <- "Drug"
fentanyl_2018_c <- fentanyl_2018_c[-1,]
fentanyl_2018_c[, 2:14] <- lapply(fentanyl_2018_c[, 2:14], as.numeric)
fentanyl_2018_c[is.na(fentanyl_2018_c)] <- 0

cumulative_fentanyl_2018_c <- data.frame(
  State = colnames(fentanyl_2018_c)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2018_c[-1])
)
rownames(cumulative_fentanyl_2018_c) <- 1:nrow(cumulative_fentanyl_2018_c)


### similarly for fentanyl_d
fentanyl_2018_d <- fentanyl_2018 %>% slice(150:199)
colnames(fentanyl_2018_d) <- fentanyl_2018_d[1,]
colnames(fentanyl_2018_d)[1] <- "Drug"
fentanyl_2018_d <- fentanyl_2018_d[,-14]
fentanyl_2018_d <- fentanyl_2018_d[-1,]
fentanyl_2018_d[, 2:13] <- lapply(fentanyl_2018_d[, 2:13], as.numeric)
fentanyl_2018_d[is.na(fentanyl_2018_d)] <- 0

cumulative_fentanyl_2018_d <- data.frame(
  State = colnames(fentanyl_2018_d)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2018_d[-1])
)
rownames(cumulative_fentanyl_2018_d) <- 1:nrow(cumulative_fentanyl_2018_d)

### cumulative fentanyl count for entire us 2018 #

total_fentanyl_2018_us <- rbind(cumulative_fentanyl_2018_a,cumulative_fentanyl_2018_b,cumulative_fentanyl_2018_c,cumulative_fentanyl_2018_d)
colnames(total_fentanyl_2018_us)[2] <- "2018_total_cumulative_total_fentanyl"
### now we will repeat if for 2019 ###
fentanyl_2019 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NFLIS Data tables/NFLIS Data tables/NFLISPublicData_2019_Table3_for_analysis.csv')
fentanyl_2019_a  <- fentanyl_2019 %>% slice(1:41)
fentanyl_2019_a[, 2:14] <- lapply(fentanyl_2019_a[, 2:14], as.numeric)
fentanyl_2019_a[is.na(fentanyl_2019_a)] <- 0

cumulative_fentanyl_2019_a <- data.frame(
  State = colnames(fentanyl_2019_a)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2019_a[-1])
)
rownames(cumulative_fentanyl_2019_a) <- 1:nrow(cumulative_fentanyl_2019_a)

#### fentanyl_2019_b ###
fentanyl_2019_b  <- fentanyl_2019 %>% slice(42:83)
colnames(fentanyl_2019_b) <- fentanyl_2019_b[1,]
fentanyl_2019_b <- fentanyl_2019_b[-1,]
fentanyl_2019_b[, 2:14] <- lapply(fentanyl_2019_b[, 2:14], as.numeric)
fentanyl_2019_b[is.na(fentanyl_2019_b)] <- 0

cumulative_fentanyl_2019_b <- data.frame(
  State = colnames(fentanyl_2019_b)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2019_b[-1])
)
rownames(cumulative_fentanyl_2019_b) <- 1:nrow(cumulative_fentanyl_2019_b)

#### fentanyl_2019_c
fentanyl_2019_c  <- fentanyl_2019 %>% slice(84:125)
colnames(fentanyl_2019_c) <- fentanyl_2019_c[1,]
fentanyl_2019_c <- fentanyl_2019_c[-1,]
fentanyl_2019_c[, 2:14] <- lapply(fentanyl_2019_c[, 2:14], as.numeric)
fentanyl_2019_c[is.na(fentanyl_2019_c)] <- 0

cumulative_fentanyl_2019_c <- data.frame(
  State = colnames(fentanyl_2019_c)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2019_c[-1])
)
rownames(cumulative_fentanyl_2019_c) <- 1:nrow(cumulative_fentanyl_2019_c)
#### fentanyl 2019_d ####
fentanyl_2019_d  <- fentanyl_2019 %>% slice(126:167)
colnames(fentanyl_2019_d) <- fentanyl_2019_d[1,]
fentanyl_2019_d <- fentanyl_2019_d[-1,]
fentanyl_2019_d <- fentanyl_2019_d[,-14]
fentanyl_2019_d[, 2:13] <- lapply(fentanyl_2019_d[, 2:13], as.numeric)
fentanyl_2019_d[is.na(fentanyl_2019_d)] <- 0

cumulative_fentanyl_2019_d <- data.frame(
  State = colnames(fentanyl_2019_d)[-1],  # Exclude the 'Drug' column
  Total_Sum = colSums(fentanyl_2019_d[-1])
)
rownames(cumulative_fentanyl_2019_d) <- 1:nrow(cumulative_fentanyl_2019_d)

total_fentanyl_2019_us <- rbind(cumulative_fentanyl_2019_a,cumulative_fentanyl_2019_b,cumulative_fentanyl_2019_c,cumulative_fentanyl_2019_d)
colnames(total_fentanyl_2019_us)[2] <- "2019_total_cumulative_total_fentanyl"
#### aggregate fentanyl 2018-2019 US ###
total_fentanyl_2018_us  <- total_fentanyl_2018_us[-c(2,12),]
total_fentanyl_2019_us  <- total_fentanyl_2019_us[-c(2,12),]

### population ####
state_population <- Soc.2019 %>% group_by(state_name) %>% summarise(total_population=sum(value))
colnames(state_population)[1] <- "State"

#### adding population to the data set ####
total_fentanyl_2018_us[8,1] <- "District of Columbia"
total_fentanyl_2019_us[8,1] <- "District of Columbia"
total_fentanyl_2018_us <- left_join(total_fentanyl_2018_us,state_population,by="State")
total_fentanyl_2019_us <- left_join(total_fentanyl_2019_us,state_population,by="State")


### mutate to get state wise fentanyl count per capita ###
colnames(total_fentanyl_2018_us)[2] <- "fentany_state"
colnames(total_fentanyl_2019_us)[2] <- "fentany_state"

state_name_and_abrevations <- fips_code %>% distinct(state, state_name)
colnames(state_name_and_abrevations)[2] <- "State"

total_fentanyl_2018_us <- total_fentanyl_2018_us %>% mutate(St_count_illicit_opioid_reported=
                                                              fentany_state/total_population 
)
total_fentanyl_2019_us <- total_fentanyl_2019_us %>% mutate(St_count_illicit_opioid_reported=
                                                              fentany_state/total_population 
)

state_fentanyl_count_per_capita <- left_join(total_fentanyl_2018_us, 
                                             state_name_and_abrevations, by="State")

state_fentanyl_count_per_capita_2019 <- left_join(total_fentanyl_2019_us, 
                                             state_name_and_abrevations, by="State")

total_fentanyl_2018_us <- state_fentanyl_count_per_capita[,c(4,5)]
total_fentanyl_2019_us <- state_fentanyl_count_per_capita_2019[,c(4,5)]

colnames(total_fentanyl_2018_us)[2] <- "stnchsxo"
colnames(total_fentanyl_2019_us)[2] <- "stnchsxo"

### adding fentanyl to oods_df ####
# FIPS codes to update
fips_to_update <- c("36005", "36047", "36061", "36081", "36085")

# Update stnchsxo to "NY" and St_count_illicit_opioid_reported to 4.179372e-05 for the specified FIPS codes
oods_2018$stnchsxo[oods_2018$GEOID %in% fips_to_update] <- "NY"
oods_2019$stnchsxo[oods_2019$GEOID %in% fips_to_update] <- "NY"

oods_2018 <- left_join(oods_2018,total_fentanyl_2018_us, "stnchsxo")
oods_2019 <- left_join(oods_2019,total_fentanyl_2019_us, "stnchsxo")
##### ADDING SDOH COVARIATES ######
sdoh_2018 <- read_excel("C:/Users/kusha/Desktop/Data for Paper/SDOH_COUNTY_2019_AHRQ/SDOH_2018_COUNTY.xlsx")
health_determinant_2018 <- sdoh_2018 %>% filter(COUNTYFIPS %in% oods_2018$GEOID)
selected_variables <- c("COUNTYFIPS", 'ACS_PCT_UNEMPLOY', 
                                              'ACS_PCT_HU_NO_VEH', 'POS_MEAN_DIST_ALC', 
                                              'ACS_PCT_LT_HS',
                                              'AHRF_TOT_COM_HEALTH_GRANT',
                                              'ACS_MEDIAN_HH_INC','CCBP_BWLSTORES_RATE','AMFAR_MHFAC_RATE', 
                                              'ACS_MEDIAN_AGE', 'ACS_PCT_MALE','ACS_PCT_WHITE'
                                              ,'ACS_PCT_ASIAN','ACS_PCT_AIAN','ACS_PCT_NHPI','ACS_PCT_MULT_RACE')
health_determinant_covariates_2018 <- health_determinant_2018 %>% dplyr::select(selected_variables)
health_determinant_covariates_2018 <- health_determinant_covariates_2018 %>% replace(is.na(.), 0)

health_determinant_covariates_2018 <- health_determinant_covariates_2018 %>%
  mutate(across(-1, ~rescale(.x, to = c(0, 1))))

#### SDOH 2019 ####
sdoh_2019 <- read_excel("C:/Users/kusha/Desktop/Data for Paper/SDOH_COUNTY_2019_AHRQ/SDOH_2019_COUNTY_excel.xlsx")
health_determinant <- sdoh_2019 %>% filter(COUNTYFIPS %in% oods_2019$GEOID)
selected_variables <- c("COUNTYFIPS", 'ACS_PCT_UNEMPLOY', 
                        'ACS_PCT_HU_NO_VEH', 'POS_MEAN_DIST_ALC', 
                        'ACS_PCT_LT_HS',
                        'AHRF_TOT_COM_HEALTH_GRANT',
                        'ACS_MEDIAN_HH_INC','CCBP_BWLSTORES_RATE','AMFAR_MHFAC_RATE', 
                        'ACS_MEDIAN_AGE', 'ACS_PCT_MALE','ACS_PCT_WHITE'
                        ,'ACS_PCT_ASIAN','ACS_PCT_AIAN','ACS_PCT_NHPI','ACS_PCT_MULT_RACE')
health_determinant_covariates <- health_determinant %>% dplyr::select(selected_variables)
health_determinant_covariates <- health_determinant_covariates %>% replace(is.na(.), 0)

health_determinant_covariates <- health_determinant_covariates %>%
  mutate(across(-1, ~rescale(.x, to = c(0, 1))))
### ADDING SDOH covariates to oods_df ####
colnames(health_determinant_covariates_2018)[1] <- "GEOID"
colnames(health_determinant_covariates)[1] <- "GEOID"
oods_2018 <- left_join(oods_2018, health_determinant_covariates_2018, by="GEOID")
oods_2019 <- left_join(oods_2019, health_determinant_covariates, by="GEOID")
## panel_data ###
oods_2018_2019 <- rbind(oods_2018,oods_2019)
colnames(oods_2018_2019)[3] <- "year"
colnames(oods_2018_2019)[11] <- "Naloxone_Available"
colnames(oods_2018_2019)[12] <- "ODR"
colnames(oods_2018_2019)[13] <- "Buprenorphine_Available"
colnames(oods_2018_2019)[4] <- "deaths"
colnames(oods_2018_2019)[9] <- "deaths_social_porximity"
colnames(oods_2018_2019)[10] <- "deaths_spatial_proximity"

### write.csv###

write.csv(oods_2018_2019,'entire_united_states_fixed_effect_model.csv')
#### fixed effects model ###

library(lfe)
model_felm_entire_united_states<- felm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                         ACS_PCT_HU_NO_VEH+
                                         POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                         ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                         +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ offset(log(population))
                                       +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported|GEOID+year,
                   data=oods_2018_2019,weights = oods_2018_2019$population)
summary(model_felm_entire_united_states)

library(stargazer)
stargazer(model_felm_entire_united_states, type = "latex", 
          title = "Entire United States Two way fixed effect model")



library(coefplot)
coefplot(model_felm_entire_united_states, coefficients = c("deaths_social_porximity","deaths_spatial_proximity"),color = "green",)


