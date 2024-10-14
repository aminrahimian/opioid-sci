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
library(sf)
library(ggplot2)
library(tigris)

#### loading data ###
cdc_mort_data_fips_wise_death_certificates_entire_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/mort_data_entire_united_cdc_2018_2019.csv', 
                                                                 check.names = FALSE, 
                                                                 colClasses = c(GEOID = "character"))
cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us[,-1]


# Get state boundaries with state FIPS and names
states <- states(cb = TRUE)  # cb = TRUE for a simplified version

# Get state names, state FIPS codes, and calculate centroids
states_info <- states %>%
  dplyr::select(STUSPS, NAME, GEOID) %>%  # STUSPS = state abbreviation, NAME = state name, GEOID = state FIPS
  mutate(centroid = st_centroid(geometry))  # Calculate centroids

# Separate centroid into latitude and longitude
states_info <- states_info %>%
  mutate(lat = st_coordinates(centroid)[,2],
         lon = st_coordinates(centroid)[,1])

# Display relevant data
states_info <- states_info %>% dplyr::select(STUSPS, NAME, GEOID, lat, lon)

### state county information ####
county_state_data <- cdc_mort_data_fips_wise_death_certificates_entire_us[,c(1,2)]

## state info 

states_info <- states_info %>% filter(STUSPS %in% county_state_data$stnchsxo)

colnames(states_info)[1] <- "stnchsxo"
colnames(states_info)[3] <- "stateid"

### county_State_data 
county_state_data <- merge(county_state_data, states_info, by="stnchsxo")

### loading the weights ###
w_i_j_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/w_i_j_entire_us.csv',check.names = FALSE)
w_i_j_us <- w_i_j_us [,-1]
w_i_j_us <- as.matrix(w_i_j_us)
rownames(w_i_j_us) <- colnames(w_i_j_us)


# Ensure row and column names of w_i_j_us are GEOIDs
rownames(w_i_j_us) <- county_state_data$GEOID
colnames(w_i_j_us) <- county_state_data$GEOID

# Create mapping from GEOID to state ID
county_to_state <- setNames(county_state_data$stnchsxo, county_state_data$GEOID)

## Get unique state abbreviations
state_ids <- unique(county_state_data$stnchsxo)

# Initialize w_sk_matrix with zeros and name rows and columns with state abbreviations
w_sk_matrix <- matrix(
  0, 
  nrow = length(state_ids), 
  ncol = length(state_ids),
  dimnames = list(state_ids, state_ids)
)

# Loop over each state 's'
for (s in state_ids) {
  
  # Get counties in state 's'
  counties_in_s <- county_state_data$GEOID[county_state_data$stnchsxo == s]
  
  # Denominator: Sum of weights from counties in 's' to counties in all other states 'k' != 's'
  denom_states <- state_ids[state_ids != s]  # States other than 's'
  
  # Initialize denominator sum
  denom_sum <- 0
  
  # Numerator sums for each 'k' != 's'
  numerator_sums <- numeric(length(denom_states))
  names(numerator_sums) <- denom_states
  
  # Loop over each state 'k' != 's'
  for (k_prime in denom_states) {
    
    # Get counties in state 'k_prime'
    counties_in_k_prime <- county_state_data$GEOID[county_state_data$stnchsxo == k_prime]
    
    # Extract submatrix of weights w_ij where i in 's' and j in 'k_prime'
    w_ij_submatrix <- w_i_j_us[counties_in_s, counties_in_k_prime, drop = FALSE]
    
    # Sum of weights for this 'k_prime'
    sum_w_ij <- sum(w_ij_submatrix, na.rm = TRUE)
    
    # Add to denominator sum
    denom_sum <- denom_sum + sum_w_ij
    
    # Store numerator sum for this 'k_prime'
    numerator_sums[k_prime] <- sum_w_ij
  }
  
  # Handle zero denominator
  if (denom_sum == 0) {
    # Set w_sk to zero for all k != s
    w_sk_matrix[s, denom_states] <- 0
  } else {
    # Compute w_sk for each k != s
    for (k in denom_states) {
      w_sk_matrix[s, k] <- numerator_sums[k] / denom_sum
    }
  }
  
  # Set w_ss to zero since s == s is not considered
  w_sk_matrix[s, s] <- 0
}

# Verify row sums (excluding diagonal elements)
row_sums <- rowSums(w_sk_matrix, na.rm = TRUE)

# Since w_ss = 0, the row sums should be 1
print(row_sums)
### state wise death rates ###
state_death_rates <- cdc_mort_data_fips_wise_death_certificates_entire_us %>% select("stnchsxo","GEOID","deaths") 
state_death_rates$deaths<- as.numeric(state_death_rates$deaths)

### adding population to the data set ###
# Get FIPS codes for all states
united_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                   "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                   "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                   "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                   "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

Soc.2019 <-  get_estimates(geography = "county", year=2018, product = "population",state=united_states, geometry = TRUE)
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
Soc.2019 <- Soc.2019 %>% select("GEOID","value")
### population ###
state_death_rates <- merge(state_death_rates,Soc.2019)
colnames(state_death_rates)[4] <- "population"
# Proceed with grouping and summarising
state_death_rates_new <- state_death_rates %>%
  group_by(stnchsxo) %>%
  summarise(mean_death_rate = sum(deaths, na.rm = TRUE) / sum(population, na.rm = TRUE))


state_death_rates_new <- merge(state_death_rates_new, states_info, by="stnchsxo")
state_death_rates_new$mean_death_rate <- state_death_rates_new$mean_death_rate*100000

### calculating s minus i for states ###
v <- state_death_rates_new$mean_death_rate
w_sk_matrix_for_all_states <- w_sk_matrix
for(i in 1:ncol(w_sk_matrix_for_all_states)){
  w_sk_matrix_for_all_states[,i] <- w_sk_matrix_for_all_states[,i] * v[i]
}
s_minus_i_state <- rowSums(w_sk_matrix_for_all_states)

### calculating d minus i for states ####
spatial_df <- state_death_rates_new %>% dplyr::select(stnchsxo,lon,lat)
### ordering it so the geoid are alligned from 01001
spatial_df <- spatial_df[order(spatial_df$stnchsxo),]
###calculating the distance between location
distance_matrix <- geodist::geodist(spatial_df,measure="geodesic")/ 1000 # converting it to km
### adding 1 so the distances are not very small and still computable ### to the distance 
distance_matrix <- 1 + distance_matrix
### inverse distance
distance_matrix <- distance_matrix**(-1)
### to insure the distance with itself is zero
diag(distance_matrix) <- 0
### providing the matrix row name and column name
colnames(distance_matrix) <- spatial_df$stnchsxo
rownames(distance_matrix) <- spatial_df$stnchsxo
### ordering the cdc_mort_data_fips for population so each geoid in the spatial df
### y stores the deaths per capita ###
y <- state_death_rates_new$mean_death_rate
### a_i_j distance matrix and again ensuring that digonals are zero##
a_i_j <- data.frame(distance_matrix)
diag(a_i_j) <- 0
a_i_j <- as.matrix(a_i_j)
# Normalize a_i_j
normalised_scale <- rowSums(a_i_j)
a_i_j <- a_i_j / normalised_scale

# Perform matrix multiplication for deaths in spatial proximity ###
d_minus_i <- a_i_j %*% y
### appending data frame ###
state_death_rates_new$deaths_social_proximity <- s_minus_i_state
state_death_rates_new$deaths_spatial_proximity <- d_minus_i

### social adjacency for state PA ####
social_adjacency_df <- data.frame(w_sk_matrix,check.names = FALSE)
social_adjacency_PA <- social_adjacency_df$PA
state_death_rates_new_pa <- state_death_rates_new 
state_death_rates_new_pa$social_adjacency_pa <- social_adjacency_PA
state_death_rates_new_pa <- st_as_sf(state_death_rates_new_pa)
ggplot(data = state_death_rates_new_pa ) +
  geom_sf(aes(fill = social_adjacency_pa)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Social Adjacency in PA State")
## social adjacency for state CA ###
social_adjacency_CA <- social_adjacency_df$CA
state_death_rates_new_ca <- state_death_rates_new 
state_death_rates_new_ca$social_adjacency_ca <- social_adjacency_CA
state_death_rates_new_ca <- st_as_sf(state_death_rates_new_ca)
ggplot(data = state_death_rates_new_ca ) +
  geom_sf(aes(fill = social_adjacency_ca)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Social Adjacency in CA State")
### spatial adjacencyny PA
spatial_adjacency_df <- data.frame(distance_matrix,check.names = FALSE)
spatial_adjacency_PA <- spatial_adjacency_df$PA
state_death_rates_new_pa <- state_death_rates_new 
state_death_rates_new_pa$spatial_adjacency_pa <- spatial_adjacency_PA
state_death_rates_new_pa <- st_as_sf(state_death_rates_new_pa)
ggplot(data = state_death_rates_new_pa ) +
  geom_sf(aes(fill = spatial_adjacency_pa)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial Adjacency in PA State")
### spatial adjacencyny CA
spatial_adjacency_CA <- spatial_adjacency_df$CA
state_death_rates_new_ca <- state_death_rates_new 
state_death_rates_new_ca$spatial_adjacency_ca <- spatial_adjacency_CA
state_death_rates_new_ca <- st_as_sf(state_death_rates_new_ca)
ggplot(data = state_death_rates_new_ca ) +
  geom_sf(aes(fill = spatial_adjacency_ca)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial Adjacency in CA State")
### mean death rates ####
state_death_rates_new_map <- state_death_rates_new
state_death_rates_new_map <- st_as_sf(state_death_rates_new_map)
state_death_rates_new_map <- state_death_rates_new_map %>% mutate(mean_death_rate_per_hnrd= mean_death_rate)
ggplot(data = state_death_rates_new_map ) +
  geom_sf(aes(fill = mean_death_rate_per_hnrd)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Death Rates in US States")
### social proximity ###
state_death_rates_new_map <- state_death_rates_new_map %>% mutate(deaths_social_proximity_per_hndrd= deaths_social_proximity)
ggplot(data = state_death_rates_new_map ) +
  geom_sf(aes(fill = deaths_social_proximity_per_hndrd)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Social proximity in US States")
### spatial proximity ###
state_death_rates_new_map <- state_death_rates_new_map %>% mutate(deaths_spatial_proximity_per_hndrd= deaths_spatial_proximity)
ggplot(data = state_death_rates_new_map ) +
  geom_sf(aes(fill = deaths_spatial_proximity_per_hndrd)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial proximity in US States")
### social-spatial proximity ###
state_death_rates_new_map <- state_death_rates_new_map %>% mutate(social_spatial_diff=deaths_social_proximity_per_hndrd-deaths_spatial_proximity_per_hndrd)
ggplot(data = state_death_rates_new_map ) +
  geom_sf(aes(fill = social_spatial_diff)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Social-Spatial proximity in US States")
### write the csv ##
state_death_rates_new <- state_death_rates_new[,-7]
write.csv(state_death_rates_new, 'C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Reviewers Comments/state_data_2018_2019.csv')
write.csv(w_sk_matrix,'C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Reviewers Comments/network_weights.csv')
state_death_rates_new_node_information <- state_death_rates_new %>% select(stnchsxo, lat, lon)
write.csv(state_death_rates_new_node_information, 'C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Reviewers Comments/node_info.csv')
write.csv(
  w_sk_matrix,
  'C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Reviewers Comments/network_weights.csv',
  row.names = TRUE
)