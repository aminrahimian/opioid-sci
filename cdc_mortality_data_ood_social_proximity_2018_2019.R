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

cdc_2018_2019_mort_data_east_america <- cdc_combined_mort_data_2018_2019 %>% filter(stposto %in% c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "NJ", "PA", "DE", 
                                                                                     "MD", "DC", "MI", "OH", "IN", "IL", "WI", "WV", "VA", "NC", 
                                                                                     "TN", "KY", "SC", "GA", "AL", "MS", "FL"))

#### getting fips related information for the eastern states using tidycensus ###
# Get FIPS codes for all states
eastern_states <- c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "NJ", "PA", "DE", 
                    "MD", "DC", "MI", "OH", "IN", "IL", "WI", "WV", "VA", "NC", 
                    "TN", "KY", "SC", "GA", "AL", "MS", "FL")
Soc.2019 <- get_estimates(geography = "county", year=2018, product = "population",state=eastern_states, geometry = TRUE )
# get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
#state=eastern_states, survey="acs5", geometry = TRUE)
Soc.2019 <- Soc.2019 %>% filter(variable=="POP")
Soc.2019 <- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")

fips_code <- tidycensus::fips_codes
fips_code <- fips_code %>% filter(state %in%  c("ME", "NH", "VT", "NY", "MA", "RI", "CT", "NJ", "PA", "DE", 
                                              "MD", "DC", "MI", "OH", "IN", "IL", "WI", "WV", "VA", "NC", 
                                              "TN", "KY", "SC", "GA", "AL", "MS", "FL"))
fips_code$GEOID <- paste0(fips_code$state_code, fips_code$county_code)

Soc.2019 <- merge(fips_code,Soc.2019, by="GEOID")
### extracting state code and fips code ###
st_code_st_abb <- Soc.2019 %>% dplyr::select(state,state_code)
st_code_st_abb <- unique(st_code_st_abb)
colnames(st_code_st_abb)[1] <- "stposto"
### merging the state_fips_code with cdc_2018_2019_to_get_5_digits_fips_code###

cdc_2018_2019_mort_data_east_america <- left_join(cdc_2018_2019_mort_data_east_america,st_code_st_abb,
                                              by = "stposto")
cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>% 
  mutate(FullFIPS = paste0(state_code, cntfipso))
### reordering the columns###
cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>%
  dplyr::select(1, 93, 94, 2:92)
colnames(cdc_2018_2019_mort_data_east_america)[3] <- "GEOID"

cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>% 
  filter(icd10_3 %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                        "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))

# total_County_available <- cdc_2018_2019_mort_data_east_america %>% group_by(cntfipso) %>% summarise(total_count = n())
### fitering the T CODES FOR OPIOID RELATED DEATHS PRIMARY USE CASES THROUGH
#ICD CODES X40-X44 and R-axis T400-T404, T406, T409########
cdc_2018_2019_mort_data_east_america <- cdc_2018_2019_mort_data_east_america %>% 
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
### to get the count of deaths in each county ###
cdc_mort_data_fips_wise_death_certificates <- cdc_2018_2019_mort_data_east_america  %>% 
  group_by(stnchsxo,GEOID) %>% summarise(total_count = n())
## the filtered data show OOD counts for 62 counties we have missing fips data for 5 counties ####
# Find GEOIDs that are in soc.2019 but not in cdc_mort_data_fips_wise_death_certificates

# Now try the setdiff() function again
missing_GEOIDs <- setdiff(Soc.2019$GEOID, cdc_mort_data_fips_wise_death_certificates$GEOID)

# Create a new data frame with the missing GEOIDs and a total_count of 0
missing_data <- data.frame(GEOID = missing_GEOIDs, total_count = 0)
missing_data <- missing_data %>%
  left_join(dplyr::select(Soc.2019, GEOID, state), by = "GEOID")

colnames(missing_data)[3] <- "stnchsxo"
missing_data <- missing_data[,c(3,1,2)]
# Add the missing data to the original data frame
cdc_mort_data_fips_wise_death_certificates <- bind_rows(cdc_mort_data_fips_wise_death_certificates, 
                                                        missing_data)

### adding the population to the cdc_mort_data_fips_wise_death_certificates #####
geoid_population <- Soc.2019 %>% dplyr::select(GEOID,value)
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
####storing the matrix weight spatial format ###
w_i_j <- as.matrix(cumulative_sci_weighted_test)
diag(w_i_j) <- 0
lw_1_eastern_united_states <- mat2listw(w_i_j)
#### further calculating s_{-i}
v <- social_df$deaths_per_capita
for(i in 1:ncol(cumulative_sci_weighted_test)){
  cumulative_sci_weighted_test[,i] <- cumulative_sci_weighted_test[,i] * v[i]
}
s_minus_i <- rowSums(cumulative_sci_weighted_test)

################### spatial proximity ##########
#### physical proximity ###
### we first create the spatial df that contains GEOID, LAT AND LNG
spatial_df <- cdc_mort_data_fips_wise_death_certificates %>% dplyr::select(GEOID,Longitude,Latitude)
### dropping the state name###
spatial_df <- spatial_df[,-1]
### ordering it so the geoid are alligned from 01001
spatial_df <- spatial_df[order(spatial_df$GEOID),]
###calculating the distance between location
distance_matrix <- geodist::geodist(spatial_df,measure="geodesic")/ 1000 # converting it to km
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
#### SDOH countywise##########
### VARIABLES TO EXTRACT: ACS_PCT_UNEMPLOY,ACS_PCT_PERSON_INC_BELOW99,ACS _PCT_HU_NO _VEH 
##### ACS_PCT_LT_HS POS_DIST_ALC ACS_PCT_OTHER_INS
sdoh_2019 <- read_excel("C:/Users/kusha/Desktop/Data for Paper/SDOH_COUNTY_2019_AHRQ/SDOH_2019_COUNTY_excel.xlsx")
health_determinant <- sdoh_2019 %>% filter(COUNTYFIPS %in% cdc_mort_data_fips_wise_death_certificates$GEOID)
selected_variables <- c("COUNTYFIPS", 'ACS_PCT_UNEMPLOY', 
                        'ACS_PCT_HU_NO_VEH', 'POS_MEAN_DIST_ALC', 
                        'ACS_PCT_LT_HS', 'ACS_PCT_OTHER_INS',
                        'AHRF_TOT_COM_HEALTH_GRANT',
                        'ACS_MEDIAN_HH_INC','CCBP_BWLSTORES_RATE','AMFAR_MHFAC_RATE', 
                        'ACS_MEDIAN_AGE', 'ACS_PCT_MALE','ACS_PCT_WHITE'
                        ,'ACS_PCT_ASIAN','ACS_PCT_AIAN','ACS_PCT_NHPI','ACS_PCT_MULT_RACE')
health_determinant_covariates <- health_determinant %>% dplyr::select(selected_variables)
health_determinant_covariates <- health_determinant_covariates %>% replace(is.na(.), 0)

health_determinant_covariates <- health_determinant_covariates %>%
  mutate(across(-1, ~rescale(.x, to = c(0, 1))))



         

# health_determinant_covariates$ACS_PCT_UNEMPLOY <- rescale(health_determinant_covariates$ACS_PCT_UNEMPLOY, to=c(0,1))
# health_determinant_covariates$ACS_PCT_LT_HS <- rescale(health_determinant_covariates$ACS_PCT_LT_HS, to=c(0,1))
# health_determinant_covariates$ACS_PCT_PERSON_INC_BELOW99 <- rescale(health_determinant_covariates$ACS_PCT_PERSON_INC_BELOW99,to=c(0,1))
# health_determinant_covariates$ACS_PCT_HU_NO_VEH <- rescale(health_determinant_covariates$ACS_PCT_HU_NO_VEH, to=c(0,1))
# health_determinant_covariates$POS_MEAN_DIST_ALC <- rescale(health_determinant_covariates$POS_MEAN_DIST_ALC, to=c(0,1))
# health_determinant_covariates$ACS_PCT_OTHER_INS <- rescale(health_determinant_covariates$ACS_PCT_OTHER_INS, to=c(0,1))
##### adding these covariates in cdc_mort_data_fips_wise_death_certificates###

colnames(health_determinant_covariates)[1] <- "GEOID"
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,
                                                        health_determinant_covariates,by="GEOID")
#### Clinical covariates###
#### naloxone ####
naloxone_2018 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2018.sas7bdat")
naloxone_2019 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2019.sas7bdat")
total_naloxone <- rbind(naloxone_2018, naloxone_2019)
total_naloxone <- total_naloxone %>% filter(state_and_county_fip  %in% cdc_mort_data_fips_wise_death_certificates$GEOID )
total_naloxone <- total_naloxone %>% group_by(county_nm,state_and_county_fip) %>% 
  summarise(avg_total_rx = mean(total_rx, na.rm = TRUE))
### missing counties ###
total_naloxone_missing_counties <- setdiff(cdc_mort_data_fips_wise_death_certificates$GEOID,
                                           total_naloxone$state_and_county_fip)
missing_naloxone_county_data <- data.frame(GEOID = total_naloxone_missing_counties, avg_total_rx = 0)

### total naloxone subsetting geoid avg_total_rx ####
total_naloxone <- total_naloxone[,-1]
colnames(total_naloxone)[1] <- "GEOID"
total_naloxone <- rbind(total_naloxone,missing_naloxone_county_data)
#total_naloxone$avg_total_rx <- rescale(total_naloxone$avg_total_rx,to=c(0,1))
total_naloxone <- total_naloxone[order(total_naloxone$GEOID),]
#### adding naloxone to the cdc_mort_data_fips_wise_death_certificates### 
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,
                                                       total_naloxone,by="GEOID")
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>%
  mutate(avg_total_rx=avg_total_rx/population)
cdc_mort_data_fips_wise_death_certificates$avg_total_rx <- rescale(cdc_mort_data_fips_wise_death_certificates$avg_total_rx,to=c(0,1))

###### opr ######
opioids_2018<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2018.sas7bdat")
opioids_2019 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2019.sas7bdat")
total_opioids <- rbind(opioids_2018,opioids_2019)
total_opioids <- total_opioids %>% filter(state_fip_county_fip %in% 
                                            cdc_mort_data_fips_wise_death_certificates$GEOID )
total_opioids <- total_opioids %>% group_by(county_nm, state_fip_county_fip) %>% 
  summarise( cumulative_total_dose=sum(total_dose))
#total_opioids$cumulative_total_dose <- rescale(total_opioids$cumulative_total_dose,to=c(0,1))

### finding the missing counties ####
missing_opioids_counties <- setdiff(cdc_mort_data_fips_wise_death_certificates$GEOID,
                                    total_opioids$state_fip_county_fip)
missing_opioids_county_data <- data.frame(GEOID = missing_opioids_counties,cumulative_total_dose= 0)
total_opioids <- total_opioids[,-1]
colnames(total_opioids)[1] <- "GEOID"
total_opioids <- rbind(total_opioids,missing_opioids_county_data)
total_opioids <- total_opioids[order(total_opioids$GEOID),]

### adding odr ### 
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,
                                                        total_opioids,by="GEOID")
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>%
  mutate(cumulative_total_dose=cumulative_total_dose/population)
cdc_mort_data_fips_wise_death_certificates$cumulative_total_dose <- rescale(cdc_mort_data_fips_wise_death_certificates$cumulative_total_dose,
                                                                            cumulative_total_dose=c(0,1))
##### beupromorphine ####
beupromorphine_2018 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2018.sas7bdat")
beupromorphine_2019 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2019.sas7bdat")
total_beupromorphine <- rbind(beupromorphine_2018,beupromorphine_2019)
total_beupromorphine <- total_beupromorphine %>% filter(state_fip_county_fip %in% 
                                            cdc_mort_data_fips_wise_death_certificates$GEOID )
total_beupromorphine  <- total_beupromorphine  %>% group_by(county_nm, state_fip_county_fip) %>% 
  summarise( cumulative_total_beupromorphine=mean(total_rx))
### finding the counties with zero beupromorphine###
zero_beupromorphine_counties <- setdiff(cdc_mort_data_fips_wise_death_certificates$GEOID,
                                    total_beupromorphine$state_fip_county_fip)
zero_beupromorphine_counties_data <- data.frame(GEOID = zero_beupromorphine_counties,cumulative_total_beupromorphine= 0)
total_beupromorphine <- total_beupromorphine[,-1]
colnames(total_beupromorphine)[1] <- "GEOID"
total_beupromorphine <- rbind(total_beupromorphine,zero_beupromorphine_counties_data)
total_beupromorphine <- total_beupromorphine[order(total_beupromorphine$GEOID),]
### adding beupronorphine ### 
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,
                                                        total_beupromorphine,by="GEOID")
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>%
  mutate(cumulative_total_beupromorphine= cumulative_total_beupromorphine/population)
cdc_mort_data_fips_wise_death_certificates$cumulative_total_beupromorphine <- rescale(cdc_mort_data_fips_wise_death_certificates$cumulative_total_beupromorphine,
                                                                            cumulative_total_dose=c(0,1))
##### fentanyl count state wise ####

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
total_fentanyl_2018_2019_US <- left_join(total_fentanyl_2018_us,total_fentanyl_2019_us,by="State")
total_fentanyl_2018_2019_US$cumulative_2018_2019_fentanyl <- total_fentanyl_2018_2019_US[,2]+total_fentanyl_2018_2019_US[,3]
total_fentanyl_2018_2019_US <- total_fentanyl_2018_2019_US[-c(2,12),]
#### getting population for states ####
state_population <- Soc.2019 %>% group_by(state_name) %>% summarise(total_population=sum(value))
colnames(state_population)[1] <- "State"
#### adding population to the data set ####
total_fentanyl_2018_2019_US <- left_join(total_fentanyl_2018_2019_US,state_population,by="State")
total_fentanyl_2018_2019_US[8,5] <- 692683
### mutate to get state wise fentanyl count per capita ###
total_fentanyl_2018_2019_US <- total_fentanyl_2018_2019_US %>% mutate(St_count_illicit_opioid_reported=
                                                                        cumulative_2018_2019_fentanyl/total_population 
)
state_fentanyl_count_per_capita <- total_fentanyl_2018_2019_US[,c(1,6)]
state_fentanyl_count_per_capita[8,1] <- "District of Columbia"

#### getting abbrevations for each state to merge it with cdc_mort_Data##
state_name_and_abrevations <- fips_code %>% distinct(state, state_name)
colnames(state_name_and_abrevations)[2] <- "State"
state_fentanyl_count_per_capita <- left_join(state_fentanyl_count_per_capita, state_name_and_abrevations, by="State")

colnames(state_fentanyl_count_per_capita)[3] <- "stnchsxo"

state_fentanyl_count_per_capita <- state_fentanyl_count_per_capita %>%
  filter(!is.na(stnchsxo))

#### adding fentanyl count per capita to the data ##
cdc_mort_data_fips_wise_death_certificates <- left_join(cdc_mort_data_fips_wise_death_certificates,
                                                        state_fentanyl_count_per_capita, by="stnchsxo")

cdc_mort_data_fips_wise_death_certificates  <- cdc_mort_data_fips_wise_death_certificates [,c(1,29,2:30)]

cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates[,-c(30)]

#### renaming columns###
colnames(cdc_mort_data_fips_wise_death_certificates)[27] <- "Naloxone_Available"
colnames(cdc_mort_data_fips_wise_death_certificates)[28] <- "ODR"
colnames(cdc_mort_data_fips_wise_death_certificates)[29] <- "Buprenorphine_Available"
colnames(cdc_mort_data_fips_wise_death_certificates)[4] <- "deaths"
colnames(cdc_mort_data_fips_wise_death_certificates)[9] <- "deaths_social_porximity"
colnames(cdc_mort_data_fips_wise_death_certificates)[10] <- "deaths_spatial_proximity"



#### last pre processing ###
# FIPS codes to update
fips_to_update <- c("36005", "36047", "36061", "36081", "36085")

# Update stnchsxo to "NY" and St_count_illicit_opioid_reported to 4.179372e-05 for the specified FIPS codes
cdc_mort_data_fips_wise_death_certificates$stnchsxo[cdc_mort_data_fips_wise_death_certificates$GEOID %in% fips_to_update] <- "NY"
cdc_mort_data_fips_wise_death_certificates$St_count_illicit_opioid_reported[cdc_mort_data_fips_wise_death_certificates$GEOID %in% fips_to_update] <- 4.179372e-05

#### population density ####
counties <- counties(year = 2018, cb = TRUE)
# Calculate area in square kilometers
counties <- st_transform(counties, crs = 5070)  # Transform to Albers Equal Area for accurate area calculation
counties <- counties %>%
  mutate(area_sq_km = as.numeric(st_area(geometry)) / 1e6)  # Convert area to square kilometers

counties <- counties %>% filter(GEOID %in% cdc_mort_data_fips_wise_death_certificates$GEOID) %>% select(c("GEOID", "area_sq_km"))
cdc_mort_data_fips_wise_death_certificates <- merge(cdc_mort_data_fips_wise_death_certificates,counties,by="GEOID")
cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>% mutate(population_density=population/area_sq_km)

#### frequent mental health distress clinical covariate #####
### https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation/national-data-documentation-2010-2022
mental_health_distress <- read.csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2019.csv")
mental_health_distress <- mental_health_distress  %>% dplyr::select("X5.digit.FIPS.Code", "Frequent.mental.distress.raw.value")
colnames(mental_health_distress)[1] <- "GEOID"
colnames(mental_health_distress)[2] <- "frequent_mental_health_distress"
mental_health_distress <- mental_health_distress[-c(1,2),]
mental_health_distress <- mental_health_distress %>% filter(GEOID %in% cdc_mort_data_fips_wise_death_certificates$GEOID)
cdc_mort_data_fips_wise_death_certificates <- merge(cdc_mort_data_fips_wise_death_certificates,mental_health_distress,by="GEOID")
### political affilation ###

#url <- "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2016_US_County_Level_Presidential_Results.csv"

# Reading the CSV file directly from the GitHub repository
election_data <- read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2016_US_County_Level_Presidential_Results.csv")

### filtering counties in the western united states ###
# Convert county names to lowercase to avoid case sensitivity issues
# Assuming your dataset is called 'election_data' and the FIPS codes are in the 'combined_fips' column
election_data$combined_fips <- str_pad(election_data$combined_fips, width = 5, pad = "0")

### shanon county renamed Oglala Lakota County, SD new fips 46102###
election_data <- election_data %>% mutate(combined_fips = ifelse(combined_fips == 46113, 46102, combined_fips))


## filtering for western state counties ###
election_data <- election_data %>% filter(combined_fips %in% cdc_mort_data_fips_wise_death_certificates$GEOID )

## mutating 0 and 1 based on election results ###
election_data <- election_data %>% mutate(political_affiliation= ifelse(votes_gop>votes_dem,1,0))

### selecting geoid and political affiliation ###
election_data <- election_data %>% dplyr::select("combined_fips","political_affiliation")
colnames(election_data)[1] <- "GEOID"

### merge data ###
cdc_mort_data_fips_wise_death_certificates <- merge(cdc_mort_data_fips_wise_death_certificates, election_data, by="GEOID")


#### scale population####
cdc_mort_data_fips_wise_death_certificates$population <- rescale(cdc_mort_data_fips_wise_death_certificates$population, 
                                                                 to=c(0,1))

### for library'C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/mort_data_2018_2019_cdc_eastern_united_states.csv.')

write.csv(cdc_mort_data_fips_wise_death_certificates, 'mort_data_2018_2019_cdc_eastern_united_states.csv')


# #### nbr ###
# library(MASS)
# summary(nb1_eastern_us <- glm.nb(deaths ~ deaths_social_porximity + deaths_spatial_proximity+
#                                  ACS_PCT_HU_NO_VEH+
#                                    POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
#                                    ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
#                                    +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ offset(log(population))
#                       +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
#                       data = cdc_mort_data_fips_wise_death_certificates,weights=population,
#                       control = glm.control(maxit = 500)))
# 
# # Display coefficients and clustered standard errors
# nb_1_clustered_std_error_eastern_us <- coeftest(nb1_eastern_us,vcov = vcovCL,
#                                      cluster = ~ cdc_mort_data_fips_wise_death_certificates$stnchsxo)
# nb_1_clustered_std_error_eastern_us
# 
# library(stargazer)
# stargazer(nb1_eastern_us, nb_1_clustered_std_error_eastern_us, type = "latex", 
#           title = "Negative Binomial Model with and without Clustered SE")
# 
# 
# 
# 
# #### lm 1#### 
# summary(lm_model_eastern_us <- lm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
#                                     ACS_PCT_HU_NO_VEH+
#                                     POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
#                                     ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
#                                     +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
#                                   +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
#                        data = cdc_mort_data_fips_wise_death_certificates, 
#                        weights = population))
# 
# library(sandwich)
# library(lmtest)
# lm_clustered_error_eastern_us <- coeftest(lm_model_eastern_us , vcov = vcovCL, 
#                                cluster = ~ cdc_mort_data_fips_wise_death_certificates$stnchsxo)
# lm_clustered_error_eastern_us
# stargazer(lm_model_eastern_us, lm_clustered_error_eastern_us, type = "latex", 
#           title = "Linear Regression with and without Clustered SE")
# 
# 
# 
# ################# spatial reg #####
# library(spdep)
# library(spatialreg)
# network_autocorrelation <- errorsarlm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
#                                         ACS_PCT_HU_NO_VEH+
#                                         POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
#                                         ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
#                                         +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
#                                         +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported,
#            data=cdc_mort_data_fips_wise_death_certificates,
#            listw = lw_1_eastern_united_states,
#            zero.policy = TRUE,
#            na.action = na.omit,
#            tol.solve = 1*exp(-50)
# )
# 
# 
# 
# ####storing the distance matrix weight spatial format ###
# diag(a_i_j) <- 0
# lw_2_eastern_united_states <- mat2listw(a_i_j,style='W')
# saveRDS(lw_1_eastern_united_states, file="lw_1_eastern_us.rds")
# saveRDS(lw_2_eastern_united_states, file="lw_2_eastern_us.rds")
# 
# 
# 
# summary(spatial_autocorrelation <- errorsarlm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
#                                                 ACS_PCT_HU_NO_VEH+
#                                                 POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
#                                                 ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
#                                                 +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
#                                                 +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported,
#                                               data=cdc_mort_data_fips_wise_death_certificates,
#                                               listw = lw_2,
#                                               zero.policy = TRUE,
#                                               na.action = na.omit,
#                                               tol.solve = 1*exp(-50)
# ))
# 
# 
# stargazer(network_autocorrelation, spatial_autocorrelation, type = "latex", 
#           title = "Autocorrelation Models for the counties in the Eastern United States")
# 
# 
# #######################
# # Load your dataset
# # You should replace the below line with the actual loading of your dataset
# # cdc_mort_data_fips_wise_death_certificates <- read.csv("path_to_your_data.csv")
# 
# # Extract the variables for correlation plot
# selected_vars <- cdc_mort_data_fips_wise_death_certificates[, c("deaths_per_capita", 
#                                                                 "deaths_social_porximity", 
#                                                                 "deaths_spatial_proximity", 
#                                                                 "ACS_PCT_UNEMPLOY", 
#                                                                 "ACS_PCT_LT_HS", 
#                                                                 "ACS_PCT_PERSON_INC_BELOW99", 
#                                                                 "ACS_PCT_HU_NO_VEH", 
#                                                                 "POS_MEAN_DIST_ALC", 
#                                                                 "ACS_PCT_OTHER_INS", 
#                                                                 "ODR", 
#                                                                 "Naloxone_Available")]
# 
# library(PerformanceAnalytics)
# 
# chart.Correlation(selected_vars, histogram = TRUE, method = "pearson")
# 
# 
# 
# 
# write.csv(cdc_mort_data_fips_wise_death_certificates, 'mort_easter_united_states_2018_2019.csv')
# 
# write.csv(w_i_j,'w_i_j_eastern.csv')
# write.csv(a_i_j,'a_i_j_eastern.csv')
# 
# 

