library(sandwich)
library(lmtest)
library(spatialreg)
library(modelsummary)
library(lfe)
library(modelsummary)
library(spdep)
library(spatialreg)
library(ggplot2)
library(stargazer)
###entire united states ####
cdc_mort_data_fips_wise_death_certificates_entire_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/mort_data_entire_united_cdc_2018_2019.csv')
cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us[,-1]


# library(MASS)
# summary(nb_1_entire_us <- glm.nb(deaths ~ deaths_social_porximity + deaths_spatial_proximity +
#                                    ACS_PCT_UNEMPLOY + ODR + Naloxone_Available + Buprenorphine_Available + 
#                                    St_count_illicit_opioid_reported + ACS_PCT_HU_NO_VEH + POS_MEAN_DIST_ALC +
#                                    ACS_PCT_LT_HS + AHRF_TOT_COM_HEALTH_GRANT + ACS_MEDIAN_HH_INC + CCBP_BWLSTORES_RATE +
#                                    AMFAR_MHFAC_RATE + ACS_MEDIAN_AGE + ACS_PCT_MALE + ACS_PCT_WHITE +
#                                    ACS_PCT_ASIAN + ACS_PCT_AIAN + ACS_PCT_NHPI +ACS_PCT_MULT_RACE+ offset(log(population)), 
#                                  data = cdc_mort_data_fips_wise_death_certificates_entire_us,weights=population,
#                                  control = glm.control(maxit = 1000)))
# #### negative binomial regression ####
# nb_1_clustered_std_error_entire_us <- coeftest(nb_1_entire_us,vcov = vcovCL,
#                                                cluster = ~ cdc_mort_data_fips_wise_death_certificates_entire_us$stnchsxo)
# nb_1_clustered_std_error_entire_us
#### linear regression ####
summary(lm_model_entire_us <- lm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                   ODR + Naloxone_Available + Buprenorphine_Available + 
                                   St_count_illicit_opioid_reported +population_density
                                 +frequent_mental_health_distress
                                 +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                   POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                   ACS_PCT_AIAN + 
                                   ACS_PCT_NHPI,
                                 data = cdc_mort_data_fips_wise_death_certificates_entire_us, 
                                 weights = population))


lm_clustered_error_entire_us <- coeftest(lm_model_entire_us, vcov = vcovCL, 
                                         cluster = ~ cdc_mort_data_fips_wise_death_certificates_entire_us$stnchsxo)
lm_clustered_error_entire_us
#### autocorrelation model ###
### network ####
lw_1_entire_us <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/lw_1_entire_us.rds")
lw_2_entire_us <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/lw_2_entire_us.rds")

network_autocorrelation_entire_united_states <- errorsarlm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                                             ODR + Naloxone_Available + Buprenorphine_Available + 
                                                             St_count_illicit_opioid_reported +population_density
                                                           +frequent_mental_health_distress
                                                           +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                                             POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                                             ACS_PCT_AIAN + 
                                                             ACS_PCT_NHPI, 
                                      data=cdc_mort_data_fips_wise_death_certificates_entire_us,
                                      listw = lw_1_entire_us,
                                      zero.policy = TRUE,
                                      tol.solve = 1*exp(-50)
)
# 
summary(network_autocorrelation_entire_united_states)
# #### spatial ####
spatial_autocorrelation_entire_united_states <- errorsarlm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                                             ODR + Naloxone_Available + Buprenorphine_Available + 
                                                             St_count_illicit_opioid_reported +population_density
                                                           +frequent_mental_health_distress
                                                           +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                                             POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                                             ACS_PCT_AIAN + 
                                                             ACS_PCT_NHPI, 
                                      data=cdc_mort_data_fips_wise_death_certificates_entire_us,
                                      listw = lw_2_entire_us,
                                      zero.policy = TRUE,
                                      tol.solve = 1*exp(-50)
)
summary(spatial_autocorrelation_entire_united_states)

### fixed effect model ###
oods_2018_2019_entire_united_states <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Fixed_effect_panel_data_entire_us/entire_united_states_fixed_effect_model.csv')
model_felm_entire_united_states<- felm(deaths_per_capita ~ ODR+scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                         Naloxone_Available + Buprenorphine_Available + 
                                         St_count_illicit_opioid_reported +population_density
                                       +frequent_mental_health_distress
                                       +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                         POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                         ACS_PCT_AIAN + 
                                         ACS_PCT_NHPI|stnchsxo+year,
                                       data=oods_2018_2019_entire_united_states,weights = oods_2018_2019_entire_united_states$population)
summary(model_felm_entire_united_states)




# Assume your modelplot output is assigned to this variable
my_plot <- modelplot(list(lm_clustered_error_entire_us,
                          network_autocorrelation_entire_united_states,
                          spatial_autocorrelation_entire_united_states,
                          model_felm_entire_united_states),
                     coef_omit = c(-2,-3),
                     draw = TRUE)
# Modify the plot
my_plot <- my_plot + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),   # Remove panel background
        axis.line = element_blank(),          # Remove axis lines
        axis.ticks = element_blank()) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains


my_plot <- my_plot + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("cluster_robust_std_error_lm",
                                "network_autocorrelation",
                                "spatial_autocorrelation",
                                "two_way_fixed_effect"),values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) # replace #colorN with actual color codes or names

print(my_plot)

##### eastern united states#####
### loading eastern united states data ##
cdc_mort_data_fips_wise_death_certificates_eastern_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/mort_data_2018_2019_cdc_eastern_united_states.csv.')
cdc_mort_data_fips_wise_death_certificates_eastern_us <- cdc_mort_data_fips_wise_death_certificates_eastern_us[,-1]

###negative binomial regression ###
# summary(nb1_eastern_us <- glm.nb(deaths ~ deaths_social_porximity + deaths_spatial_proximity+
#                                    ACS_PCT_UNEMPLOY+ACS_PCT_PERSON_INC_BELOW99+ 
#                                  ACS_PCT_HU_NO_VEH+ 
#                                  ACS_PCT_OTHER_INS+ ACS_PCT_LT_HS+
#                                  CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ 
#                                  ACS_MEDIAN_AGE+ACS_PCT_MALE+ACS_PCT_FEMALE,
#                                  ACS_PCT_BLACK+ACS_PCT_ASIAN+ACS_PCT_AIAN+ACS_PCT_NHPI+ offset(log(population))
#                                  +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
#                                  data = cdc_mort_data_fips_wise_death_certificates_eastern_us,weights=population,
#                                  control = glm.control(maxit = 500)))
# 
# # Display coefficients and clustered standard errors
# nb_1_clustered_std_error_eastern_us <- coeftest(nb1_eastern_us,vcov = vcovCL,
#                                                 cluster = ~ cdc_mort_data_fips_wise_death_certificates_eastern_us$stnchsxo)
# nb_1_clustered_std_error_eastern_us

### linear model ###
summary(lm_model_eastern_us <- lm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                    ODR + Naloxone_Available + Buprenorphine_Available + 
                                    St_count_illicit_opioid_reported +population_density
                                  +frequent_mental_health_distress
                                  +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                    POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                    ACS_PCT_AIAN + 
                                    ACS_PCT_NHPI, 
                                  data = cdc_mort_data_fips_wise_death_certificates_eastern_us, weights = population 
                                  ))
        
lm_clustered_error_eastern_us <- coeftest(lm_model_eastern_us , vcov = vcovCL, 
                                          cluster = ~ cdc_mort_data_fips_wise_death_certificates_eastern_us$stnchsxo)

### network and spatial autocorrelation models###
lw_1_eastern_united_states <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/lw_1_eastern_us.rds")
lw_2_eastern_united_states <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/lw_2_eastern_us.rds")
network_autocorrelation_eastern_us <- errorsarlm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                                   ODR + Naloxone_Available + Buprenorphine_Available + 
                                                   St_count_illicit_opioid_reported +population_density
                                                 +frequent_mental_health_distress
                                                 +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                                   POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                                   ACS_PCT_AIAN + 
                                                   ACS_PCT_NHPI, 
                                      data=cdc_mort_data_fips_wise_death_certificates_eastern_us ,
                                      listw = lw_1_eastern_united_states,
                                      zero.policy = TRUE,
                                      na.action = na.omit,
                                      tol.solve = 1*exp(-50),
)

summary(spatial_autocorrelation_eastern_us <- errorsarlm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                                           ODR + Naloxone_Available + Buprenorphine_Available + 
                                                           St_count_illicit_opioid_reported +population_density
                                                         +frequent_mental_health_distress
                                                         +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                                           POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                                           ACS_PCT_AIAN + 
                                                           ACS_PCT_NHPI,
                                                         data=cdc_mort_data_fips_wise_death_certificates_eastern_us,
                                              listw = lw_2_eastern_united_states,
                                              zero.policy = TRUE,
                                              na.action = na.omit,
                                              tol.solve = 1*exp(-50)
))

#### fixed effect models eastern united states###
oods_2018_2019_eastern_united_states <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Fixed_effect_panel_data_eastern_us/eastern_united_states_fixed_effect_model.csv')
model_felm_eastern_united_states<- felm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                          ODR + Naloxone_Available + Buprenorphine_Available + 
                                          St_count_illicit_opioid_reported +population_density
                                        +frequent_mental_health_distress
                                        +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                          POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                          ACS_PCT_AIAN + 
                                          ACS_PCT_NHPI|stnchsxo+year,
                                        data=oods_2018_2019_eastern_united_states,weights = oods_2018_2019_eastern_united_states$population)
summary(model_felm_eastern_united_states)


### plotting the eastern untied states coefficeints
my_plot_2 <- modelplot(list(lm_clustered_error_eastern_us,
                            network_autocorrelation_eastern_us,
                          spatial_autocorrelation_eastern_us,
                          model_felm_eastern_united_states),
                     coef_omit = c(-2,-3))


my_plot_2 <- my_plot_2 + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),   # Remove panel background
        axis.line = element_blank(),          # Remove axis lines
        axis.ticks = element_blank()) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains


my_plot_2 <- my_plot_2 + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("cluster_robust_std_error_lm",
                                "network_autocorrelation",
                                "spatial_autocorrelation",
                                "two_way_fixed_effect"),values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) # replace #colorN with actual color codes or names

print(my_plot_2)



## g2sls 
### east vs west ####
### west ####
cdc_mort_data_fips_wise_death_certificates_western_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Western United States/western_united_stated_mort_data.csv')
cdc_mort_data_fips_wise_death_certificates_western_us <- cdc_mort_data_fips_wise_death_certificates_western_us[,c(-1,-22)]

w_i_j_western <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Western United States/w_i_j_western.csv')
w_i_j_western <- w_i_j_western[,-1]
w_i_j_western <- as.matrix(w_i_j_western)

lw_1_western_us <- mat2listw(w_i_j_western, style='W')

a_i_j_western <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Western United States/a_i_j_western.csv')
a_i_j_western <- a_i_j_western[,-1]
a_i_j_western <- as.matrix(a_i_j_western)
lw_2_western_us <- mat2listw(a_i_j_western, style='W')

### linear regression ####
min_population_west <- min(cdc_mort_data_fips_wise_death_certificates_western_us$population)
max_population_west <- max(cdc_mort_data_fips_wise_death_certificates_western_us$population)

scaled_population_west <- (cdc_mort_data_fips_wise_death_certificates_western_us$population - min_population_west) / (max_population_west - min_population_west)


lm_western <- lm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                   ODR + Naloxone_Available + Buprenorphine_Available + 
                   St_count_illicit_opioid_reported +population_density
                 +frequent_mental_health_distress
                 +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                   POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                   ACS_PCT_AIAN + 
                   ACS_PCT_NHPI,data =cdc_mort_data_fips_wise_death_certificates_western_us,
                 weight=scaled_population_west )


summary(lm_western)

lm_western_clustered_std_error <- coeftest(lm_western,vcov = vcovCL,
                                           cluster = ~ cdc_mort_data_fips_wise_death_certificates_western_us$stnchsxo)
lm_western_clustered_std_error



#### network autocorrelation ####
network_autocorrelation_western_us <- errorsarlm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                                   ODR + Naloxone_Available + Buprenorphine_Available + 
                                                   St_count_illicit_opioid_reported +population_density
                                                 +frequent_mental_health_distress
                                                 +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                                   POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                                   ACS_PCT_AIAN + 
                                                   ACS_PCT_NHPI,
                                                 data= cdc_mort_data_fips_wise_death_certificates_western_us,
                                                 listw = lw_1_western_us,
                                                 zero.policy = TRUE,
                                                 na.action = na.omit,
                                                 tol.solve = 1*exp(-50)
)

summary(network_autocorrelation_western_us)
#### spatial autocorrelation ####
summary(spatial_autocorrelation_western_us <- errorsarlm(deaths_per_capita ~ scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                                           ODR + Naloxone_Available + Buprenorphine_Available + 
                                                           St_count_illicit_opioid_reported +population_density
                                                         +frequent_mental_health_distress
                                                         +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                                           POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                                           ACS_PCT_AIAN + 
                                                           ACS_PCT_NHPI,
                                                         data= cdc_mort_data_fips_wise_death_certificates_western_us,
                                                         listw = lw_2_western_us,
                                                         zero.policy = TRUE,
                                                         na.action = na.omit,
                                                         tol.solve = 1*exp(-50)
))

stargazer(network_autocorrelation_western_us,spatial_autocorrelation_western_us, type = "latex", 
          title = "Autocorrelation")
##### two way fixed effect ####
oods_2018_2019_western_united_states <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Western United States/panel_western.csv')
model_felm_western_united_states <- felm(deaths_per_capita ~ ODR + scale(deaths_social_porximity) + scale(deaths_spatial_proximity) +
                                            Naloxone_Available + Buprenorphine_Available + 
                                           St_count_illicit_opioid_reported +population_density
                                         +frequent_mental_health_distress
                                         +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                           POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                           ACS_PCT_AIAN + 
                                           ACS_PCT_NHPI|stnchsxo+year,
                                        data=oods_2018_2019_western_united_states,weights = oods_2018_2019_western_united_states$population)
summary(model_felm_western_united_states)
stargazer(model_felm_western_united_states, type = "latex", 
          title = "two-way fixed effect western and central us")
#### g2sls##
cdc_mort_data_fips_wise_death_certificates_western_us_selected_covariates_from_lasso <- cdc_mort_data_fips_wise_death_certificates_western_us %>%
  select(
    ODR,
    Naloxone_Available,
    Buprenorphine_Available,
    St_count_illicit_opioid_reported,
    population_density,
    frequent_mental_health_distress,
    political_affiliation,
    ACS_PCT_UNEMPLOY,
    ACS_MEDIAN_HH_INC,
    POS_MEAN_DIST_ALC,
    ACS_PCT_ASIAN,
    ACS_PCT_AIAN,
)
X_n_western  <- as.matrix(cdc_mort_data_fips_wise_death_certificates_western_us_selected_covariates_from_lasso[,c(1:12)])

# Compute the matrices
W1n_squared_western <- w_i_j_western %*% w_i_j_western # This is W_{1n}^2
W2n_squared_western <- a_i_j_western %*% a_i_j_western # This is W_{2n}^2
W2n_W1n_western <- a_i_j_western %*% w_i_j_western     # This is W_{2n} W_{1n}
W1n_W2n_western <- w_i_j_western %*% a_i_j_western     # This is W_{1n} W_{2n}

# Calculate the instrument variables
IV_W1n_Xn_western <- w_i_j_western %*% X_n_western     # This is W_{1n} X_n
IV_W2n_Xn_western <- a_i_j_western %*% X_n_western     # This is W_{2n} X_n
IV_W1n_squared_Xn_western <- W1n_squared_western %*% X_n_western  # This is W_{1n}^2 X_n
IV_W2n_squared_Xn_western <- W2n_squared_western %*% X_n_western  # This is W_{2n}^2 X_n
IV_W1n_W2n_Xn_western <- W1n_W2n_western %*% X_n_western          # This is W_{1n} W_{2n} X_n
IV_W2n_W1n_Xn_western <- W2n_W1n_western %*% X_n_western          # This is W_{2n} W_{1n} X_n

# Combine all instrument variables to create the IV matrix for SARAR(2,1)
Q_n_western <- cbind(X_n_western, IV_W1n_Xn_western, IV_W2n_Xn_western, IV_W1n_squared_Xn_western, IV_W2n_squared_Xn_western, IV_W1n_W2n_Xn_western, IV_W2n_W1n_Xn_western)


library("ivreg")  # For ivreg

# Given that you already have the matrix X_n, and w_i_j (W1), a_i_j (W2) weights matrices,
# and the instrument matrix Q_n, you would proceed as follows:
# Define the formula for ivreg
# The dependent variable y is regressed on the exogenous variables (X_n),
# and the endogenous spatial lags (s_i and d_i)
# The | symbol separates the model variables from the instruments in Q_n
# Fit the SARAR(2,1) model using ivreg
# Assuming 'population' is your vector of population values
min_population_west <- min(cdc_mort_data_fips_wise_death_certificates_western_us$population)
max_population_west <- max(cdc_mort_data_fips_wise_death_certificates_western_us$population)

scaled_population_west <- (cdc_mort_data_fips_wise_death_certificates_western_us$population - min_population_west) / (max_population_west - min_population_west)

##### western plot ####

# library(MASS)
# summary(nb_1_western_us <- glm.nb(deaths ~ deaths_social_porximity + deaths_spatial_proximity+
#                                    ACS_PCT_HU_NO_VEH+
#                                    POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
#                                    ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
#                                    +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ offset(log(population))
#                                  +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
#                                  data = cdc_mort_data_fips_wise_death_certificates_western_us,weights=scaled_population_west,
#                                  control = glm.control(maxit = 1000)))
# #### negative binomial regression ####
# nb_1_clustered_std_error_western_us <- coeftest(nb_1_western_us,vcov = vcovCL,
#                                                cluster = ~ cdc_mort_data_fips_wise_death_certificates_western_us$stnchsxo)
# nb_1_clustered_std_error_western_us
# 
# stargazer(nb_1_western_us,nb_1_clustered_std_error_western_us, type = "latex", 
#           title = "NBR")
# 



#### main paper plot nbr###
my_plot_west <- modelplot(list(lm_western_clustered_std_error,
                               network_autocorrelation_western_us,
                            spatial_autocorrelation_western_us,
                            model_felm_western_united_states),
                       coef_omit = c(-2,-3))


my_plot_west <- my_plot_west + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),   # Remove panel background
        axis.line = element_blank(),          # Remove axis lines
        axis.ticks = element_blank()) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains


my_plot_west <- my_plot_west + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("cluster_robust_std_error_lm",
                                "network_autocorrelation",
                                "spatial_autocorrelation",
                                "two_way_fixed_effect"),values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) # replace #colorN with actual color codes or names

print(my_plot_west)


# # Modify the plot
# my_plot_lm_west <- my_plot_lm_west + 
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),   # Remove panel background
#         axis.line = element_blank(),          # Remove axis lines
#         axis.ticks = element_blank()) +       # Remove axis ticks
#   geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains
# 
# 
# my_plot_lm_west <- my_plot_lm_west + 
#   labs(color = "Model Type") +
#   scale_color_manual(labels = c("linear regression","cluster robust linear regression"),values = c("#e41a1c","#377eb8"))
# 
# my_plot_lm_west
# 
# stargazer(eu,wu,eu_entire_us, type = "latex", 
#           title = "G2SLS")


# Now use the scaled_population in your ivreg model
first_stage_social_wu <- ivreg(scale(cdc_mort_data_fips_wise_death_certificates_western_us$deaths_social_porximity) ~ Q_n_western, 
                                      data = cdc_mort_data_fips_wise_death_certificates_western_us)

first_stage_spatial_wu<- ivreg(scale(cdc_mort_data_fips_wise_death_certificates_western_us$deaths_spatial_proximity) ~ Q_n_western, 
                                        data =  cdc_mort_data_fips_wise_death_certificates_western_us)

cdc_mort_data_fips_wise_death_certificates_western_us$fitted_social_proximity <- fitted(first_stage_social_wu)
cdc_mort_data_fips_wise_death_certificates_western_us$fitted_spatial_proximity <- fitted(first_stage_spatial_wu)


# Second stage: Use predicted values in the main regression
second_stage_wu <- ivreg(deaths_per_capita ~ fitted_social_proximity + fitted_spatial_proximity +
                           ODR + Naloxone_Available + Buprenorphine_Available + 
                           St_count_illicit_opioid_reported + population_density
                         +frequent_mental_health_distress
                         +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                           POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                           ACS_PCT_AIAN + 
                           ACS_PCT_NHPI, 
                              data =  cdc_mort_data_fips_wise_death_certificates_western_us)
# Print the results
summary(second_stage_wu)


eu_wu <- coeftest(second_stage_wu, vcov. = vcovHAC(second_stage_wu))
eu_wu

### east ###
# Reading and preprocessing data for Eastern US
cdc_mort_data_fips_wise_death_certificates_eastern_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/mort_data_2018_2019_cdc_eastern_united_states.csv')
cdc_mort_data_fips_wise_death_certificates_eastern_us <- cdc_mort_data_fips_wise_death_certificates_eastern_us[,c(-1,-21)]

w_i_j_eastern <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/w_i_j_eastern.csv')
w_i_j_eastern <- w_i_j_eastern[,-1]
w_i_j_eastern <- as.matrix(w_i_j_eastern)

a_i_j_eastern <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/a_i_j_eastern.csv')
a_i_j_eastern <- a_i_j_eastern[,-1]
a_i_j_eastern <- as.matrix(a_i_j_eastern)

# Creating the variable matrix for Eastern US
cdc_mort_data_fips_wise_death_certificates_eastern_us_selected_covariates_from_lasso <- cdc_mort_data_fips_wise_death_certificates_eastern_us %>%
  select(
    ODR,
    Naloxone_Available,
    Buprenorphine_Available,
    St_count_illicit_opioid_reported,
    population_density,
    frequent_mental_health_distress,
    political_affiliation,
    ACS_PCT_UNEMPLOY,
    ACS_MEDIAN_HH_INC,
    POS_MEAN_DIST_ALC,
    ACS_PCT_ASIAN,
    ACS_PCT_AIAN,
  )

X_n_eastern <- as.matrix(cdc_mort_data_fips_wise_death_certificates_eastern_us_selected_covariates_from_lasso[,c(1:12)])


# Compute the matrices for Eastern US
W1n_squared_eastern <- w_i_j_eastern %*% w_i_j_eastern # This is W_{1n}^2
W2n_squared_eastern <- a_i_j_eastern %*% a_i_j_eastern # This is W_{2n}^2
W2n_W1n_eastern <- a_i_j_eastern %*% w_i_j_eastern     # This is W_{2n} W_{1n}
W1n_W2n_eastern <- w_i_j_eastern %*% a_i_j_eastern     # This is W_{1n} W_{2n}

# Calculate the instrument variables for Eastern US
IV_W1n_Xn_eastern <- w_i_j_eastern %*% X_n_eastern     # This is W_{1n} X_n
IV_W2n_Xn_eastern <- a_i_j_eastern %*% X_n_eastern     # This is W_{2n} X_n
IV_W1n_squared_Xn_eastern <- W1n_squared_eastern %*% X_n_eastern  # This is W_{1n}^2 X_n
IV_W2n_squared_Xn_eastern <- W2n_squared_eastern %*% X_n_eastern  # This is W_{2n}^2 X_n
IV_W1n_W2n_Xn_eastern <- W1n_W2n_eastern %*% X_n_eastern          # This is W_{1n} W_{2n} X_n
IV_W2n_W1n_Xn_eastern <- W2n_W1n_eastern %*% X_n_eastern          # This is W_{2n} W_{1n} X_n

# Combine all instrument variables to create the IV matrix for SARAR(2,1) for Eastern US
Q_n_eastern <- cbind(X_n_eastern, IV_W1n_Xn_eastern, IV_W2n_Xn_eastern, IV_W1n_squared_Xn_eastern, IV_W2n_squared_Xn_eastern, IV_W1n_W2n_Xn_eastern, IV_W2n_W1n_Xn_eastern)

# Define the formula for ivreg for the Eastern US dataset
# The dependent variable y is regressed on the exogenous variables (X_n_eastern),
# and the endogenous spatial lags (s_i_eastern and d_i_eastern)
# The | symbol separates the model variables from the instruments in Q_n_eastern
# Fit the SARAR(2,1) model using ivreg

# Scale the population for the Eastern US dataset
min_population_east <- min(cdc_mort_data_fips_wise_death_certificates_eastern_us$population)
max_population_east <- max(cdc_mort_data_fips_wise_death_certificates_eastern_us$population)

scaled_population_east <- (cdc_mort_data_fips_wise_death_certificates_eastern_us$population - min_population_east) / (max_population_east - min_population_east)

# Now use the scaled_population in your ivreg model for the Eastern US dataset
# Now use the scaled_population in your ivreg model
first_stage_social_eu <- ivreg(scale(cdc_mort_data_fips_wise_death_certificates_eastern_us$deaths_social_porximity) ~ Q_n_eastern, 
                               data = cdc_mort_data_fips_wise_death_certificates_eastern_us)

first_stage_spatial_eu<- ivreg(scale(cdc_mort_data_fips_wise_death_certificates_eastern_us$deaths_spatial_proximity) ~ Q_n_eastern, 
                               data =  cdc_mort_data_fips_wise_death_certificates_eastern_us)


cdc_mort_data_fips_wise_death_certificates_eastern_us$fitted_social_proximity <- fitted(first_stage_social_eu) 
cdc_mort_data_fips_wise_death_certificates_eastern_us$fitted_spatial_proximity <- fitted(first_stage_spatial_eu)


# Second stage: Use predicted values in the main regression
second_stage_eu <- ivreg(deaths_per_capita ~ fitted_social_proximity + fitted_spatial_proximity +
                           ODR + Naloxone_Available + Buprenorphine_Available + 
                           St_count_illicit_opioid_reported + population_density
                         +frequent_mental_health_distress
                         +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                           POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                           ACS_PCT_AIAN + 
                           ACS_PCT_NHPI, 
                         data =  cdc_mort_data_fips_wise_death_certificates_eastern_us)
# Print the results
summary(second_stage_eu)


eu_eu <- coeftest(second_stage_eu, vcov. = vcovHAC(second_stage_eu))
eu_eu





#### east vs west CI
# my_plot_4 <- modelplot(list(eu,wu),coef_omit =c(-2,-3),
#                      draw = TRUE)
# # Modify the plot
# my_plot_4 <- my_plot_4 + 
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),   # Remove panel background
#         axis.line = element_blank(),          # Remove axis lines
#         axis.ticks = element_blank()) +       # Remove axis ticks
#   geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains
# 
# 
# my_plot_4 <- my_plot_4 + 
#   labs(color = "Model Type") +
#   scale_color_manual(labels = c("G2SLS Eastern United States",
#                                 "G2SLS Western United States"),values = c("#e41a1c", "#377eb8")) # replace #colorN with actual color codes or names
# 
# print(my_plot_4)
### g2sls entire us ####
cdc_mort_data_fips_wise_death_certificates_entire_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/mort_data_entire_united_cdc_2018_2019.csv')
cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us[,c(-1,-21)]


w_i_j_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/w_i_j_entire_us.csv')
w_i_j_us <- w_i_j_us [,-1]
w_i_j_us <- as.matrix(w_i_j_us)

a_i_j_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/a_i_j_entire_us.csv')
a_i_j_us <- a_i_j_us[,-1]
a_i_j_us <- as.matrix(a_i_j_us)

# Creating the variable matrix for Eastern US
# Assuming the data loading part is already correctly done

# Creating the variable matrix for the entire U.S.

cdc_mort_data_fips_wise_death_certificates_entire_us_selected_covariates_from_lasso <- cdc_mort_data_fips_wise_death_certificates_entire_us %>%
  select(
    ODR,
    Naloxone_Available,
    Buprenorphine_Available,
    St_count_illicit_opioid_reported,
    population_density,
    frequent_mental_health_distress,
    political_affiliation,
    ACS_PCT_UNEMPLOY,
    ACS_MEDIAN_HH_INC,
    POS_MEAN_DIST_ALC,
    ACS_PCT_ASIAN,
    ACS_PCT_AIAN,
  )

X_n_us <- as.matrix(cdc_mort_data_fips_wise_death_certificates_entire_us_selected_covariates_from_lasso[,c(1:12)])


# Compute the matrices for the entire U.S.
W1n_squared_us <- w_i_j_us %*% w_i_j_us # This is W_{1n}^2 for the entire U.S.
W2n_squared_us <- a_i_j_us %*% a_i_j_us # This is W_{2n}^2 for the entire U.S.
W2n_W1n_us <- a_i_j_us %*% w_i_j_us     # This is W_{2n} W_{1n} for the entire U.S.
W1n_W2n_us <- w_i_j_us %*% a_i_j_us     # This is W_{1n} W_{2n} for the entire U.S.

# Calculate the instrument variables for the entire U.S.
IV_W1n_Xn_us <- w_i_j_us %*% X_n_us     # This is W_{1n} X_n for the entire U.S.
IV_W2n_Xn_us <- a_i_j_us %*% X_n_us     # This is W_{2n} X_n for the entire U.S.
IV_W1n_squared_Xn_us <- W1n_squared_us %*% X_n_us  # This is W_{1n}^2 X_n for the entire U.S.
IV_W2n_squared_Xn_us <- W2n_squared_us %*% X_n_us  # This is W_{2n}^2 X_n for the entire U.S.
IV_W1n_W2n_Xn_us <- W1n_W2n_us %*% X_n_us          # This is W_{1n} W_{2n} X_n for the entire U.S.
IV_W2n_W1n_Xn_us <- W2n_W1n_us %*% X_n_us          # This is W_{2n} W_{1n} X_n for the entire U.S.

# Combine all instrument variables to create the IV matrix for SARAR(2,1) for the entire U.S.
Q_n_us <- cbind(X_n_us, IV_W1n_Xn_us, IV_W2n_Xn_us, IV_W1n_squared_Xn_us, IV_W2n_squared_Xn_us, IV_W1n_W2n_Xn_us, IV_W2n_W1n_Xn_us)

####
# Assuming cdc_mort_data_fips_wise_death_certificates_entire_us is your dataset for the entire U.S.

# Calculate min and max population for the entire U.S.
min_population_us <- min(cdc_mort_data_fips_wise_death_certificates_entire_us$population)
max_population_us <- max(cdc_mort_data_fips_wise_death_certificates_entire_us$population)

# Scale the population for the entire U.S.
scaled_population_us <- (cdc_mort_data_fips_wise_death_certificates_entire_us$population - min_population_us) / (max_population_us - min_population_us)

# Update the ivreg model for the entire U.S. using the scaled population as weights
# First stage: Regress endogenous variables on instruments
first_stage_social_entire_us <- ivreg(scale(cdc_mort_data_fips_wise_death_certificates_entire_us$deaths_social_porximity) ~ Q_n_us, 
                            data = cdc_mort_data_fips_wise_death_certificates_entire_us)

first_stage_spatial_entires_us <- ivreg(scale(cdc_mort_data_fips_wise_death_certificates_entire_us$deaths_spatial_proximity) ~ Q_n_us, 
                             data = cdc_mort_data_fips_wise_death_certificates_entire_us)

### model plot same name data frame ###

cdc_mort_data_fips_wise_death_certificates_entire_us$fitted_social_proximity <- fitted(first_stage_social_entire_us)
cdc_mort_data_fips_wise_death_certificates_entire_us$fitted_spatial_proximity <- fitted(first_stage_spatial_entires_us)

# Second stage: Use predicted values in the main regression
second_stage_entire_us <- ivreg(deaths_per_capita ~ fitted_social_proximity + fitted_spatial_proximity +
                                  ODR + Naloxone_Available + Buprenorphine_Available + 
                                  St_count_illicit_opioid_reported + population_density
                                +frequent_mental_health_distress
                                +as.factor(political_affiliation)+ACS_PCT_UNEMPLOY + 
                                  POS_MEAN_DIST_ALC + ACS_MEDIAN_HH_INC + 
                                  ACS_PCT_AIAN + 
                                  ACS_PCT_NHPI, 
                      data =  cdc_mort_data_fips_wise_death_certificates_entire_us)
# Print the results
summary(second_stage_entire_us)


eu_entire_us <- coeftest(second_stage_entire_us, vcov. = vcovHAC(second_stage_entire_us))
eu_entire_us

### with standard error correction ### 
# my_plot_5 <- modelplot(list(eu_eu,eu_wu,eu_entire_us),coef_omit =c(-2,-3),
#                        draw = TRUE)
# # Modify the plot
# my_plot_5 <- my_plot_5 + 
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),   # Remove panel background
#         axis.line = element_blank(),          # Remove axis lines
#         axis.ticks = element_blank()) +       # Remove axis ticks
#   geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains
# 
# 
# my_plot_5 <- my_plot_5 + 
#   labs(color = "Model Type") +
#   scale_color_manual(labels = c("2SLS Eastern United States",
#                                 "2SLS Western United States", "2SLS United States"),values = c("#e41a1c", "#377eb8","#4daf4a"))
# 
# stargazer(eu_eu,eu_wu,eu_entire_us, type = "latex", 
#           title = "G2SLS")



### without standard error correction ####


my_plot_6 <- modelplot(list(second_stage_eu,second_stage_wu,second_stage_entire_us),coef_omit =c(-2,-3),
                       draw = TRUE)
# Modify the plot
my_plot_6 <- my_plot_6 + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),   # Remove panel background
        axis.line = element_blank(),          # Remove axis lines
        axis.ticks = element_blank()) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains


my_plot_6 <- my_plot_6 + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("2SLS Eastern United States",
                                "2SLS Western-Central United States", "2SLS United States"),values = c("#e41a1c", "#377eb8","#4daf4a"))

stargazer(second_stage_eu, second_stage_wu, second_stage_entire_us, type = "latex", 
          title = "G2SLS", digits = 5)
# ##### nbr ####
# my_plot_3 <- modelplot(list(nb_1_clustered_std_error_western_us,
#                           nb_1_clustered_std_error_entire_us,
#                           nb_1_clustered_std_error_eastern_us),
#                      coef_omit = c(-2,-3),
#                      draw = TRUE)
# 
# 
# my_plot_3 <- my_plot_3 + 
#   theme(panel.grid.major = element_blank(),  # Remove major grid lines
#         panel.grid.minor = element_blank(),  # Remove minor grid lines
#         panel.background = element_blank(),   # Remove panel background
#         axis.line = element_blank(),          # Remove axis lines
#         axis.ticks = element_blank()) +       # Remove axis ticks
#   geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains
# my_plot_3
# 
# my_plot_3 <- my_plot_3 + 
#   labs(color = "Model Type") +
#   scale_color_manual(labels = c("cluster-robust NBR (Western US)",
#                                 "clusterd_robust NBR (Entire US)",
#                                 "clusterd-robust NBR (Eastern US)"),values = c("#e41a1c", "#377eb8", "#4daf4a")) 
# 
# print(my_plot_3)
