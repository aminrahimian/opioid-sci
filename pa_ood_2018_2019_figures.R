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

cdc_mort_data_fips_wise_death_certificates <- cdc_mort_data_fips_wise_death_certificates %>% filter(stnchsxo=="PA")

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
### figures ###
pa_ood_2018_2019 <- cdc_mort_data_fips_wise_death_certificates %>%  filter(stnchsxo=="PA")
####### social adjacency map for phili and allegheny###############
library(spatialreg)
library(spdep)
library(igraph)
library(tidyverse)
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
nvss_county <- pa_ood_2018_2019$GEOID
df_1 <- df_0 %>% dplyr::filter(user_loc %in% nvss_county & fr_loc %in% nvss_county)
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
df_1$fr_loc <- as.numeric(df_1$fr_loc)
nodes <- df_1 %>% distinct(fr_loc)
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- pa_ood_2018_2019$population
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
social_adjacency <- as.data.frame(cumulative_sci_weighted_test )
social_adjacency_phili <-  social_adjacency$`42101`
social_adjacency_phili_geoid <- row.names(social_adjacency)
phili_social_adjacency_data <- data.frame(social_adjacency_phili_geoid, social_adjacency_phili)
colnames(phili_social_adjacency_data)[1] <- "GEOID"
phili_social_adjacency_data$GEOID <- as.numeric(phili_social_adjacency_data$GEOID)

social_adjacency_alle <-  social_adjacency$`42003`
social_adjacency_alle_geoid <- row.names(social_adjacency)
alle_social_adjacency_data <- data.frame(social_adjacency_alle_geoid, social_adjacency_alle)
colnames(alle_social_adjacency_data)[1] <- "GEOID"

#### plotting the social adjacency map for phili and allegheny######
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
phili_social_adjacency_data$GEOID <- as.numeric(phili_social_adjacency_data$GEOID)
phili_counties_merged <- left_join(penn_counties, phili_social_adjacency_data, by = "GEOID")

ggplot(data = phili_counties_merged) +
  geom_sf(aes(fill = social_adjacency_phili)) +
  scale_fill_viridis_c(option = "viridis", trans="sqrt",direction = 1) +
  theme_minimal() +
  ggtitle("Social Adjacency in Philidelphia County")

alle_social_adjacency_data$GEOID <- as.numeric(alle_social_adjacency_data$GEOID)
alle_counties_merged <- left_join(penn_counties, alle_social_adjacency_data, by = "GEOID")

ggplot(data = alle_counties_merged) +
  geom_sf(aes(fill = social_adjacency_alle)) +
  scale_fill_viridis_c(option = "viridis", trans="sqrt",direction = 1) +
  theme_minimal() +
  ggtitle("Social Adjacency in Allegheny Counties")

########### spatial adjacency map ########
library(geodist)
df <- pa_ood_2018_2019 %>% dplyr::select(GEOID, Latitude, Longitude)
df <- df[,-1]
df <- df[order(df$GEOID),]
colnames(df)[2] <- "latitude"
colnames(df)[3] <- "longitude"
df <- df[, c(1, 3, 2)]

distance_matrix <- geodist(df, measure = 'geodesic') / 1000 # converting it to km
distance_matrix <- 1 + distance_matrix
distance_matrix <- distance_matrix**(-1)
diag(distance_matrix) <- 0
colnames(distance_matrix) <- df$GEOID
rownames(distance_matrix) <- df$GEOID
a_i_j <- data.frame(distance_matrix)
diag(a_i_j) <- 0
a_i_j <- as.matrix(a_i_j)

# Normalize a_i_j
normalised_scale <- rowSums(a_i_j)
a_i_j <- a_i_j / normalised_scale

spatial_adjacency_matrix <- as.data.frame(a_i_j)


spatial_adjacency_phili <-  spatial_adjacency_matrix$`X42101`
spatial_adjacency_phili_geoid <- row.names(a_i_j)
phili_spatial_adjacency_data <- data.frame(spatial_adjacency_phili_geoid, spatial_adjacency_phili)
colnames(phili_spatial_adjacency_data)[1] <- "GEOID"

spatial_adjacency_alle <-  spatial_adjacency_matrix$`X42003`
spatial_adjacency_alle_geoid <- row.names(a_i_j)
alle_spatial_adjacency_data <- data.frame(spatial_adjacency_alle_geoid, spatial_adjacency_alle)
colnames(alle_spatial_adjacency_data)[1] <- "GEOID"

### plotting the spatial adjacency map#####
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
phili_spatial_adjacency_data$GEOID <- as.numeric(phili_social_adjacency_data$GEOID)
phili_counties_merged <- left_join(penn_counties, phili_spatial_adjacency_data, by = "GEOID")

ggplot(data = phili_counties_merged) +
  geom_sf(aes(fill = spatial_adjacency_phili)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial Adjacency in Phili Counties")


alle_spatial_adjacency_data$GEOID <- as.numeric(alle_spatial_adjacency_data$GEOID)
alle_counties_merged <- left_join(penn_counties, alle_spatial_adjacency_data, by = "GEOID")

ggplot(data = alle_counties_merged) +
  geom_sf(aes(fill = spatial_adjacency_alle)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial Adjacency in Allegheny Counties")

############ map for county deaths per capita############
library(dplyr)
pa_ood_2018_2019$GEOID <- as.numeric(pa_ood_2018_2019$GEOID)
deaths_per_capita <- pa_ood_2018_2019 %>% dplyr::select(GEOID,deaths_per_capita)
deaths_per_capita$GEOID <- as.numeric(deaths_per_capita$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(deaths_per_capita, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = deaths_per_capita), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, name = "Deaths per Capita") +
  theme_minimal() +
  ggtitle("Deaths per Capita by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))
###############################################################
### getting the matrices for social proximity weights###
library(readr)
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
county_geoid <- pa_ood_2018_2019$GEOID
df_1 <- df_0 %>% dplyr::filter(user_loc %in% county_geoid & fr_loc %in% county_geoid)
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
population <- pa_ood_2018_2019$population
### numerator of w_i_j: population_i*cumulative_sci_weighted_i Calculate n_j * SCI_ij
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
### # # Calculate Σ_{k ≠ i} n_k * SCI_ik
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
# # Calculate w_{ij} = n_j * SCI_ij / Σ_{k ≠ i} n_k * SCI_ik
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
#####social proximity map####
library(dplyr)
deaths_social_proximity<- pa_ood_2018_2019 %>% dplyr::select(GEOID,deaths_in_social_proximity,deaths_per_capita)
deaths_social_proximity$GEOID <- as.numeric(deaths_social_proximity$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(deaths_social_proximity, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = deaths_in_social_proximity), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, name = "Social Proximity") +
  theme_minimal() +
  ggtitle("Deaths in Social Proximity by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))
##### spatial proximity map #######
library(dplyr)
deaths_spatial_proximity<- pa_ood_2018_2019 %>% dplyr::select(GEOID,deaths_in_spatial_proximity,deaths_per_capita)
deaths_spatial_proximity$GEOID <- as.numeric(deaths_spatial_proximity$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(deaths_spatial_proximity, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = deaths_in_spatial_proximity), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, name = "Spatial Proximity") +
  theme_minimal() +
  ggtitle("Deaths in Spatial Proximity by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))
#### social-spatial proximity #######
pa_ood_2018_2019 <- pa_ood_2018_2019 %>% 
  mutate(diff_social_spatial_proximity= 
           abs((deaths_in_social_proximity- deaths_in_spatial_proximity)))
library(dplyr)
diff_social_spatial_proximity <- pa_ood_2018_2019 %>% dplyr::select(GEOID,diff_social_spatial_proximity,deaths_per_capita)
diff_social_spatial_proximity$GEOID <- as.numeric(diff_social_spatial_proximity$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(diff_social_spatial_proximity, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = diff_social_spatial_proximity), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  theme_minimal() +
  ggtitle("Diff in Social Proximity by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))

#### correlation chart ###
library(ggplot2)
library(GGally)

colnames(pa_ood_2018_2019)[7] <- "death rate"
colnames(pa_ood_2018_2019)[8] <- "social proximity"
colnames(pa_ood_2018_2019)[9] <- "spatial proximity"
# Select the variables from the dataset
variables <- c("death rate", "social proximity", "spatial proximity")

# Create a new data frame with only the selected variables
df_selected <- pa_ood_2018_2019[variables]

# Create the conditional scatterplot matrix
ggpairs(df_selected, 
        lower = list(continuous = wrap("points", alpha = 0.8, color="red")),
        diag = list(continuous = wrap("barDiag", fill="red", bins=10)),
        upper = list(continuous = wrap("smooth", method = "lm", color="red", fullrange = TRUE)))+
  theme_minimal()
