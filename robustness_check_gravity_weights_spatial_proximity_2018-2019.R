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
#### global functions ####
### distance matrix function ####
calculate_distance_matrix <- function(df)
{
  spatial_df <- df %>% dplyr::select(GEOID,Longitude,Latitude)
  ### ordering it so the geoid are alligned from 01001
  spatial_df <- spatial_df[order(spatial_df$GEOID),]
  ###calculating the distance between location
  distance_matrix <- geodist::geodist(spatial_df,measure="geodesic")/ 1000 # converting it to km  
  return(distance_matrix)
}


### 1+ 1/d_i_j^(alpha) gravity weights ####
calculate_gravity_weights_global_inverse_d_ij_alpha <- function(distance_matrix, y, alpha_values = seq(1, 3, by = 1)) {
  # Function to calculate gravity weights for a given distance matrix and alpha
  
  # Internal function for calculating gravity weights based on alpha
  calculate_gravity_weights <- function(distance_matrix, alpha) {
    # Step 1: Raise the distance matrix to the power of alpha
    weighted_matrix <- 1 + (1 / (distance_matrix^alpha))
    
    # Step 2: Ensure diagonal values (self-distance) are 0
    diag(weighted_matrix) <- 0
    
    # Step 3: Normalize each row so that the sum of non-diagonal elements equals 1
    row_sums <- rowSums(weighted_matrix)
    normalized_weights <- weighted_matrix / row_sums
    
    return(normalized_weights)
  }
  
  # Initialize an empty list to store results for different alpha values
  results <- list()
  
  # Loop through each alpha value
  for (alpha in alpha_values) {
    # Calculate gravity weights with the current alpha
    gravity_weights <- calculate_gravity_weights(distance_matrix, alpha)
    
    # Initialize an empty vector to store the spatially weighted deaths per capita
    d_minus_i <- numeric(nrow(gravity_weights))
    
    # Calculate the weighted sum for each spatial unit
    for (i in 1:nrow(gravity_weights)) {
      d_minus_i[i] <- sum(gravity_weights[i, ] * y)
    }
    
    # Store the result for this alpha
    results[[paste0("alpha_", alpha)]] <- d_minus_i
  }
  
  return(results)
}


### d_ij^alpha gravity weight model ####
calculate_gravity_weights_globa_d_ij_alpha <- function(distance_matrix, y, alpha_values = seq(1, 3, by = 1)) {
  # Function to calculate gravity weights for a given distance matrix and alpha
  
  # Internal function for calculating gravity weights based on alpha
  calculate_gravity_weights <- function(distance_matrix, alpha) {
    # Amplify higher distances by raising them to the power of alpha
    weighted_matrix <- distance_matrix^alpha
    
    # Step 2: Ensure diagonal values (self-distance) are 0
    diag(weighted_matrix) <- 0
    
    # Step 3: Normalize each row so that the sum of non-diagonal elements equals 1
    row_sums <- rowSums(weighted_matrix)
    normalized_weights <- weighted_matrix / row_sums
    
    return(normalized_weights)
  }
  
  # Initialize an empty list to store results for different alpha values
  results <- list()
  
  # Loop through each alpha value
  for (alpha in alpha_values) {
    # Calculate gravity weights with the current alpha
    gravity_weights <- calculate_gravity_weights(distance_matrix, alpha)
    
    # Initialize an empty vector to store the spatially weighted values
    d_minus_i <- numeric(nrow(gravity_weights))
    
    # Calculate the weighted sum for each spatial unit
    for (i in 1:nrow(gravity_weights)) {
      d_minus_i[i] <- sum(gravity_weights[i, ] * y)
    }
    
    # Store the result for this alpha
    results[[paste0("alpha_", alpha)]] <- d_minus_i
  }
  
  return(results)
}

##### reading data ###
cdc_mort_data_fips_wise_death_certificates_entire_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/mort_data_entire_united_cdc_2018_2019.csv')
cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us[,-1]
### western and central us ####
cdc_mort_data_fips_wise_death_certificates_western_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Western United States/western_united_stated_mort_data.csv')
cdc_mort_data_fips_wise_death_certificates_western_us <- cdc_mort_data_fips_wise_death_certificates_western_us[,c(-1,-22)]
#### eastern US####
cdc_mort_data_fips_wise_death_certificates_eastern_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/mort_data_2018_2019_cdc_eastern_united_states.csv.')
cdc_mort_data_fips_wise_death_certificates_eastern_us <- cdc_mort_data_fips_wise_death_certificates_eastern_us[,-1]
################### distance matrix calculation for entire,western and eastern us#####
distance_matrix_entires_us <- calculate_distance_matrix(cdc_mort_data_fips_wise_death_certificates_entire_us)
distance_matrix_western_us <- calculate_distance_matrix(cdc_mort_data_fips_wise_death_certificates_western_us)
distance_matrix_eastern_us <- calculate_distance_matrix(cdc_mort_data_fips_wise_death_certificates_eastern_us)
#### entire us ## weight calculations ###
#### ### 1+ 1/d_i_j^(alpha) gravity weights for entire us ####
spatial_proximity_entire_us <- calculate_gravity_weights_global_inverse_d_ij_alpha(distance_matrix_entires_us,cdc_mort_data_fips_wise_death_certificates_entire_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )
### d_ij^alpha gravity weight model ####
spatial_proximity_entire_us_amplification <- calculate_gravity_weights_globa_d_ij_alpha(distance_matrix_entires_us,cdc_mort_data_fips_wise_death_certificates_entire_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )
### western us ### weight calculations ### 
#### ### 1+ 1/d_i_j^(alpha) gravity weights for western us ####
spatial_proximity_western_us <- calculate_gravity_weights_global_inverse_d_ij_alpha(distance_matrix_western_us,cdc_mort_data_fips_wise_death_certificates_western_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )
### d_ij^alpha gravity weight model ####
spatial_proximity_western_us_amplification <- calculate_gravity_weights_globa_d_ij_alpha(distance_matrix_western_us,cdc_mort_data_fips_wise_death_certificates_western_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )
### eastern us ### weight calculations ### 
#### ### 1+ 1/d_i_j^(alpha) gravity weights for eastern us ####
spatial_proximity_eastern_us <- calculate_gravity_weights_global_inverse_d_ij_alpha(distance_matrix_eastern_us,cdc_mort_data_fips_wise_death_certificates_eastern_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )
### d_ij^alpha gravity weight model ####
spatial_proximity_eastern_us_amplification <- calculate_gravity_weights_globa_d_ij_alpha(distance_matrix_eastern_us,cdc_mort_data_fips_wise_death_certificates_eastern_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )


