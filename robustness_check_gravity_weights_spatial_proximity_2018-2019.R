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
library(modelsummary)
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
calculate_gravity_weights_global_inverse_d_ij_alpha <- function(distance_matrix, y, alpha_values=0.1) {
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


# ### d_ij^alpha gravity weight model ####
# calculate_gravity_weights_globa_d_ij_alpha <- function(distance_matrix, y, alpha_values = seq(1, 3, by = 1)) {
#   # Function to calculate gravity weights for a given distance matrix and alpha
#   
#   # Internal function for calculating gravity weights based on alpha
#   calculate_gravity_weights <- function(distance_matrix, alpha) {
#     # Amplify higher distances by raising them to the power of alpha
#     weighted_matrix <- distance_matrix^alpha
#     
#     # Step 2: Ensure diagonal values (self-distance) are 0
#     diag(weighted_matrix) <- 0
#     
#     # Step 3: Normalize each row so that the sum of non-diagonal elements equals 1
#     row_sums <- rowSums(weighted_matrix)
#     normalized_weights <- weighted_matrix / row_sums
#     
#     return(normalized_weights)
#   }
#   
#   # Initialize an empty list to store results for different alpha values
#   results <- list()
#   
#   # Loop through each alpha value
#   for (alpha in alpha_values) {
#     # Calculate gravity weights with the current alpha
#     gravity_weights <- calculate_gravity_weights(distance_matrix, alpha)
#     
#     # Initialize an empty vector to store the spatially weighted values
#     d_minus_i <- numeric(nrow(gravity_weights))
#     
#     # Calculate the weighted sum for each spatial unit
#     for (i in 1:nrow(gravity_weights)) {
#       d_minus_i[i] <- sum(gravity_weights[i, ] * y)
#     }
#     
#     # Store the result for this alpha
#     results[[paste0("alpha_", alpha)]] <- d_minus_i
#   }
#   
#   return(results)
# }


# #### distance decay ##
# calculate_gravity_weights_global_distance_decay <- function(distance_matrix, y, decay_values = seq(1, 3, by = 1)) {
#   # Function to calculate gravity weights for a given distance matrix and distance decay
#   
#   # Internal function for calculating gravity weights based on distance decay
#   calculate_gravity_weights <- function(distance_matrix, decay) {
#     # Step 1: Apply the distance decay function (e.g., exponential decay)
#     # Here, we assume an exponential decay function: exp(-decay * distance)
#     weighted_matrix <- exp(-decay * distance_matrix)
#     
#     # Step 2: Ensure diagonal values (self-distance) are 0
#     diag(weighted_matrix) <- 0
#     
#     # Step 3: Normalize each row so that the sum of non-diagonal elements equals 1
#     row_sums <- rowSums(weighted_matrix)
#     normalized_weights <- weighted_matrix / row_sums
#     
#     return(normalized_weights)
#   }
#   
#   # Initialize an empty list to store results for different decay values
#   results <- list()
#   
#   # Loop through each decay value
#   for (decay in decay_values) {
#     # Calculate gravity weights with the current decay value
#     gravity_weights <- calculate_gravity_weights(distance_matrix, decay)
#     
#     # Initialize an empty vector to store the spatially weighted deaths per capita
#     d_minus_i <- numeric(nrow(gravity_weights))
#     
#     # Calculate the weighted sum for each spatial unit
#     for (i in 1:nrow(gravity_weights)) {
#       d_minus_i[i] <- sum(gravity_weights[i, ] * y)
#     }
#     
#     # Store the result for this decay value
#     results[[paste0("decay_", decay)]] <- d_minus_i
#   }
#   
#   return(results)
# }


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
spatial_proximity_entire_us <- calculate_gravity_weights_global_inverse_d_ij_alpha(distance_matrix_entires_us,cdc_mort_data_fips_wise_death_certificates_entire_us$deaths_per_capita,alpha_values = 0.1)
### western us ### weight calculations ### 
#### ### 1+ 1/d_i_j^(alpha) gravity weights for western us ####
spatial_proximity_western_us <- calculate_gravity_weights_global_inverse_d_ij_alpha(distance_matrix_western_us,cdc_mort_data_fips_wise_death_certificates_western_us$deaths_per_capita,alpha_values = 0.1 )
# ### d_ij^alpha gravity weight model ####
# spatial_proximity_western_us_amplification <- calculate_gravity_weights_globa_d_ij_alpha(distance_matrix_western_us,cdc_mort_data_fips_wise_death_certificates_western_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )
### eastern us ### weight calculations ### 
#### ### 1+ 1/d_i_j^(alpha) gravity weights for eastern us ####
spatial_proximity_eastern_us <- calculate_gravity_weights_global_inverse_d_ij_alpha(distance_matrix_eastern_us,cdc_mort_data_fips_wise_death_certificates_eastern_us$deaths_per_capita,alpha_values = 0.1 )
# ### d_ij^alpha gravity weight model ####
# spatial_proximity_eastern_us_amplification <- calculate_gravity_weights_globa_d_ij_alpha(distance_matrix_eastern_us,cdc_mort_data_fips_wise_death_certificates_eastern_us$deaths_per_capita,alpha_values = seq(1, 3, by = 1) )

#### distance decay ####




#### adding the spatial proximity in the data set ####
## eastern united states ###
cdc_mort_data_fips_wise_death_certificates_eastern_us$spatial_proximity_alpha_1 <- spatial_proximity_eastern_us[[1]]

### western united states ####
cdc_mort_data_fips_wise_death_certificates_western_us$spatial_proximity_alpha_1 <- spatial_proximity_western_us[[1]]

### entire united states ####
cdc_mort_data_fips_wise_death_certificates_entire_us$spatial_proximity_alpha_1 <- spatial_proximity_entire_us[[1]]

### regressions ###
### entire untied states ###
one_hndrd_thsnd <- 100000
summary(lm_model_entire_us_1 <- lm(deaths_per_capita*one_hndrd_thsnd ~ scale(deaths_social_porximity) + scale(spatial_proximity_alpha_1) +
                                     ODR + Naloxone_Available + Buprenorphine_Available + 
                                     St_count_illicit_opioid_reported +population_density
                                   +frequent_mental_health_distress
                                   +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                     POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                     ACS_PCT_AIAN + 
                                     ACS_PCT_NHPI, 
                                 data = cdc_mort_data_fips_wise_death_certificates_entire_us, 
                                 weights = population))


lm_clustered_error_entire_us_1 <- coeftest(lm_model_entire_us_1, vcov = vcovCL, 
                                         cluster = ~ cdc_mort_data_fips_wise_death_certificates_entire_us$stnchsxo)
lm_clustered_error_entire_us_1





#### eastern us ###
summary(lm_model_eastern_us_1 <- lm(deaths_per_capita*one_hndrd_thsnd ~ scale(deaths_social_porximity) + scale(spatial_proximity_alpha_1) +
                                      ODR + Naloxone_Available + Buprenorphine_Available + 
                                      St_count_illicit_opioid_reported +population_density
                                    +frequent_mental_health_distress
                                    +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                      POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                      ACS_PCT_AIAN + 
                                      ACS_PCT_NHPI, 
                                  data = cdc_mort_data_fips_wise_death_certificates_eastern_us, weights = population 
))

lm_clustered_error_eastern_us_1 <- coeftest(lm_model_eastern_us_1 , vcov = vcovCL, 
                                          cluster = ~ cdc_mort_data_fips_wise_death_certificates_eastern_us$stnchsxo)

lm_clustered_error_eastern_us_1 


#### western us ####
min_population_west <- min(cdc_mort_data_fips_wise_death_certificates_western_us$population)
max_population_west <- max(cdc_mort_data_fips_wise_death_certificates_western_us$population)

scaled_population_west <- (cdc_mort_data_fips_wise_death_certificates_western_us$population - min_population_west) / (max_population_west - min_population_west)


lm_western_1 <- lm(deaths_per_capita*one_hndrd_thsnd ~ scale(deaths_social_porximity) + scale(spatial_proximity_alpha_1) +
                     ODR + Naloxone_Available + Buprenorphine_Available + 
                     St_count_illicit_opioid_reported +population_density
                   +frequent_mental_health_distress
                   +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                     POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                     ACS_PCT_AIAN + 
                     ACS_PCT_NHPI,data =cdc_mort_data_fips_wise_death_certificates_western_us,
                 weight=scaled_population_west )


summary(lm_western_1)

lm_western_clustered_std_error_1 <- coeftest(lm_western_1,vcov = vcovCL,
                                           cluster = ~ cdc_mort_data_fips_wise_death_certificates_western_us$stnchsxo)
lm_western_clustered_std_error_1


# Function to run regression for each alpha
# run_regressions_for_alpha <- function(data, distance_matrix, alpha_values, region) {
#   
#   # Step 1: Calculate spatial proximity for each alpha value
#   spatial_proximity <- calculate_gravity_weights_global_inverse_d_ij_alpha(
#     distance_matrix, data$deaths_per_capita, alpha_values = alpha_values)
#   
#   # Step 2: Initialize results lists
#   results <- list()  # To store model summaries and clustered errors
#   clustered_coefficients <- list()  # To store only the coefficients from the clustered error models
#   
#   # Step 3: Loop over each alpha value and run the regression
#   for (i in seq_along(alpha_values)) {
#     alpha_val <- alpha_values[i]  # Ensure alpha_val is defined within the loop
#     
#     # Add spatial proximity for the current alpha value to the data
#     proximity_col <- paste0("spatial_proximity_alpha_", alpha_val)  # Variable name
#     data[[proximity_col]] <- spatial_proximity[[i]]  # Assign the spatial proximity to the new column
#     
#     # Dynamically create the regression formula
#     formula <- as.formula(paste0("deaths_per_capita ~ scale(deaths_social_porximity) + scale(", proximity_col, ") + 
#                                   ODR + Naloxone_Available + Buprenorphine_Available + 
#                                   St_count_illicit_opioid_reported + ACS_PCT_UNEMPLOY + 
#                                   ACS_PCT_HU_NO_VEH + POS_MEAN_DIST_ALC + ACS_PCT_OTHER_INS +
#                                   ACS_PCT_LT_HS + AHRF_TOT_COM_HEALTH_GRANT + 
#                                   CCBP_BWLSTORES_RATE + ACS_PCT_ASIAN + ACS_PCT_AIAN + ACS_PCT_NHPI"))
#     
#     # Run regression model with population weighting
#     lm_model <- lm(formula, data = data, weights = population)
#     
#     # Step 4: Compute clustered standard errors
#     clustered_errors <- coeftest(lm_model, vcov = vcovCL, cluster = ~ data$stnchsxo)
#     
#     # Step 5: Store the summary and errors in the results list
#     results[[paste0("alpha_", alpha_val, "_summary")]] <- summary(lm_model)
#     results[[paste0("alpha_", alpha_val, "_clustered_errors")]] <- clustered_errors
#     
#     # Step 6: Store the clustered coefficients in a separate list
#     clustered_coefficients[[paste0("alpha_", alpha_val, "_coefficients")]] <- clustered_errors
#   }
#   
#   # Return both the regression results and the clustered coefficients
#   return(list("results" = results, "clustered_coefficients" = clustered_coefficients))
# }
# 
# # Define alpha values
# alpha_values <- seq(0, 1, by = 0.1)
# 
# # Run regressions for entire US
# results_entire_us <- run_regressions_for_alpha(
#   data = cdc_mort_data_fips_wise_death_certificates_entire_us,
#   distance_matrix = distance_matrix_entires_us,
#   alpha_values = alpha_values,
#   region = "entire_us"
# )
# 
# # Run regressions for eastern US
# results_eastern_us <- run_regressions_for_alpha(
#   data = cdc_mort_data_fips_wise_death_certificates_eastern_us,
#   distance_matrix = distance_matrix_eastern_us,
#   alpha_values = alpha_values,
#   region = "eastern_us"
# )
# 
# # Run regressions for western US
# results_western_us <- run_regressions_for_alpha(
#   data = cdc_mort_data_fips_wise_death_certificates_western_us,
#   distance_matrix = distance_matrix_western_us,
#   alpha_values = alpha_values,
#   region = "western_us"
# )
# 
# # Accessing results for entire US
# # To access the regression summaries and errors for entire US
# entire_us_results_alpha_0_1 <- results_entire_us$results[2]
# # To access the coefficients from the clustered error models for entire US
# entire_us_coefficients_alpha_0_1 <- unlist(results_entire_us$clustered_coefficients[2])
# ##### accessing results for western us####
# western_us_results_alpha_0_1 <- results_western_us$results[2]
# western_us_coefficients_0_1 <- unlist(results_western_us$clustered_coefficients[2])
# ### accessing results for eastern us ####
# eastern_us_results_0_1 <- results_eastern_us$results[2]
# eastern_us_coeeficients_0_1 <- unlist(results_eastern_us$clustered_coefficients[2])

## coefficient plot ###
# Use modelplot to plot the selected modelss
my_plot <- modelplot(list(lm_clustered_error_entire_us_1,
                          lm_western_clustered_std_error_1,
                          lm_clustered_error_eastern_us_1),
                     coef_omit = c(-2,-3),
                     draw = TRUE)


my_plot <- my_plot + 
  theme(
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    panel.background = element_blank(),   # Remove panel background
    axis.line = element_blank(),          # Remove axis lines
    axis.ticks = element_line()           # Ensure axis ticks are visible
  ) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains

my_plot <- my_plot + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("cluster robust linear regressio",
                                "cluster_robust_std_error_lm_western_us",
                                "lm_clustered_error_eastern_us_1"),values = c("#e41a1c", "#377eb8", "#4daf4a")) # replace #colorN with actual color codes or names

# Modify the plot
my_plot <- my_plot + 
  scale_x_continuous(breaks = c(0, 5, 10, 15))
print(my_plot)
