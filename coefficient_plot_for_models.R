library(sandwich)
library(lmtest)
library(spatialreg)
library(modelsummary)
library(lfe)
library(modelsummary)
###entire united states ####
cdc_mort_data_fips_wise_death_certificates_entire_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/mort_data_entire_united_cdc_2018_2019.csv')
cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us[,-1]
library(MASS)
summary(nb_1_entire_us <- glm.nb(deaths ~ deaths_social_porximity + deaths_spatial_proximity+
                                   ACS_PCT_HU_NO_VEH+
                                   POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                   ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                   +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ offset(log(population))
                                 +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
                                 data = cdc_mort_data_fips_wise_death_certificates_entire_us,weights=population,
                                 control = glm.control(maxit = 1000)))
modelplot(nb_1_clustered_std_error_entire_us,c(-2,-3))
modelplot(lm_clustered_error_entire_us,c(-2,-3))
#### negative binomial regression ####
nb_1_clustered_std_error_entire_us <- coeftest(nb_1_entire_us,vcov = vcovCL,
                                               cluster = ~ cdc_mort_data_fips_wise_death_certificates_entire_us$stnchsxo)
nb_1_clustered_std_error_entire_us
#### linear regression ####
summary(lm_model_entire_us <- lm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                   ACS_PCT_HU_NO_VEH+
                                   POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                   ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                   +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE
                                 +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
                                 data = cdc_mort_data_fips_wise_death_certificates_entire_us, 
                                 weights = population))


lm_clustered_error_entire_us <- coeftest(lm_model_entire_us, vcov = vcovCL, 
                                         cluster = ~ cdc_mort_data_fips_wise_death_certificates_entire_us$stnchsxo)
lm_clustered_error_entire_us

coefplot(lm_clustered_error_entire_us,coefficients = c("deaths_social_porximity","deaths_spatial_proximity"),color = "green")
#### autocorrelation model ###
### network ####
library(spdep)
library(spatialreg)
lw_1_entire_us <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/lw_1_entire_us.rds")
lw_2_entire_us <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/lw_2_entire_us.rds")
network_autocorrelation_entire_united_states <- errorsarlm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                        ACS_PCT_HU_NO_VEH+
                                        POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                        ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                        +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE
                                      +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
                                      data=cdc_mort_data_fips_wise_death_certificates_entire_us,
                                      listw = lw_1_entire_us,
                                      zero.policy = TRUE,
                                      tol.solve = 1*exp(-50)
)
# 
summary(network_autocorrelation_entire_united_states)
modelplot(network_autocorrelation_entire_united_states,coef_omit = c(-2,-3))
# #### spatial ####
spatial_autocorrelation_entire_united_states <- errorsarlm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                        ACS_PCT_HU_NO_VEH+
                                        POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                        ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                        +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE
                                      +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
                                      data=cdc_mort_data_fips_wise_death_certificates_entire_us,
                                      listw = lw_2_entire_us,
                                      zero.policy = TRUE,
                                      tol.solve = 1*exp(-50)
)
summary(spatial_autocorrelation_entire_united_states)

### fixed effect model ###
oods_2018_2019_entire_united_states <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Fixed_effect_panel_data_entire_us/entire_united_states_fixed_effect_model.csv')

model_felm_entire_united_states<- felm(deaths_per_capita ~ ACS_PCT_HU_NO_VEH+deaths_social_porximity + deaths_spatial_proximity
                                         +
                                         POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                         ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                         +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ offset(log(population))
                                       +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported|GEOID+year,
                                       data=oods_2018_2019_entire_united_states,weights = oods_2018_2019_entire_united_states$population)
summary(model_felm_entire_united_states)

### model plot ###
modelplot(list(lm_clustered_error_entire_us,network_autocorrelation_entire_united_states,spatial_autocorrelation_entire_united_states,model_felm_entire_united_states),coef_omit = c(-2,-3))

# Assume your modelplot output is assigned to this variable
my_plot <- modelplot(list(lm_clustered_error_entire_us,
                          network_autocorrelation_entire_united_states,
                          spatial_autocorrelation_entire_united_states,
                          model_felm_entire_united_states),
                     coef_omit = c(-2,-3))

# Modify the plot
my_plot <- my_plot + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),   # Remove panel background
        axis.line = element_blank(),          # Remove axis lines
        axis.ticks = element_blank()) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains
my_plot

my_plot <- my_plot + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("robust_cluster_std_error_lm",
                                "network_autocorrelation",
                                "spatial_autocorrelation",
                                "two_way_fixed_effect"),values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) # replace #colorN with actual color codes or names

print(my_plot)

#### fixed effect models eastern united states and united states ###
oods_2018_2019_eastern_united_states <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Fixed_effect_panel_data_eastern_us/eastern_united_states_fixed_effect_model.csv')
model_felm_eastern_united_states<- felm(deaths_per_capita ~ deaths_social_porximity +deaths_spatial_proximity +
                                          ACS_PCT_HU_NO_VEH+
                                          POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                          ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                          +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE
                                        +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported|GEOID+year,
                                        data=oods_2018_2019_eastern_united_states,weights = oods_2018_2019_eastern_united_states$population)
summary(model_felm_eastern_united_states)

##### eastern united states#####



