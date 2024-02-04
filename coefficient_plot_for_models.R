library(sandwich)
library(lmtest)
library(spatialreg)
library(modelsummary)
library(lfe)
library(modelsummary)
library(spdep)
library(spatialreg)
library(ggplot2)
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
#### autocorrelation model ###
### network ####
lw_1_entire_us <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/lw_1_entire_us.rds")
lw_2_entire_us <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/lw_2_entire_us.rds")
write_dta(lw_2_entire_us, "lw_2_entire_us.dta")

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
                                         +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE
                                       +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported|GEOID+year,
                                       data=oods_2018_2019_entire_united_states,weights = oods_2018_2019_entire_united_states$population)
summary(model_felm_entire_united_states)




### model plot ###
modelplot(list(lm_clustered_error_entire_us,network_autocorrelation_entire_united_states,spatial_autocorrelation_entire_united_states,model_felm_entire_united_states),coef_omit = c(-2,-3))

# Create a list of layers to add to the modelplot
background_layers <- list(
  geom_vline(xintercept = 0, color = 'black'),
  geom_point(aes(shape = model), # Ensure 'model' is a factor in your data
             position = position_dodge(width = 0.25),
             size = 3) # Adjust size as needed
)

# Specify custom shapes for each model
shape_values <- c(16, 17, 18, 19) 

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
cdc_mort_data_fips_wise_death_certificates_eastern_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/mort_data_2018_2019_cdc_eastern_united_states.csv')
cdc_mort_data_fips_wise_death_certificates_eastern_us <- cdc_mort_data_fips_wise_death_certificates_eastern_us[,-1]

###negative binomial regression ###
summary(nb1_eastern_us <- glm.nb(deaths ~ deaths_social_porximity + deaths_spatial_proximity+
                                   ACS_PCT_HU_NO_VEH+
                                   POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                   ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                   +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+ offset(log(population))
                                 +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
                                 data = cdc_mort_data_fips_wise_death_certificates_eastern_us,weights=population,
                                 control = glm.control(maxit = 500)))

# Display coefficients and clustered standard errors
nb_1_clustered_std_error_eastern_us <- coeftest(nb1_eastern_us,vcov = vcovCL,
                                                cluster = ~ cdc_mort_data_fips_wise_death_certificates_eastern_us$stnchsxo)
nb_1_clustered_std_error_eastern_us

### linear model ###
summary(lm_model_eastern_us <- lm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                    ACS_PCT_HU_NO_VEH+
                                    POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                    ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                    +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
                                    +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported, 
                                  data = cdc_mort_data_fips_wise_death_certificates_eastern_us, 
                                  weights = population))
lm_clustered_error_eastern_us <- coeftest(lm_model_eastern_us , vcov = vcovCL, 
                                          cluster = ~ cdc_mort_data_fips_wise_death_certificates_eastern_us$stnchsxo)

### network and spatial autocorrelation models###
lw_1_eastern_united_states <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/lw_1_eastern_us.rds")
lw_2_eastern_united_states <- readRDS("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Eastern United States/lw_2_eastern_us.rds")
network_autocorrelation_eastern_us <- errorsarlm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                        ACS_PCT_HU_NO_VEH+
                                        POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                        ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                        +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
                                        +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported,
                                      data=cdc_mort_data_fips_wise_death_certificates_eastern_us ,
                                      listw = lw_1_eastern_united_states,
                                      zero.policy = TRUE,
                                      na.action = na.omit,
                                      tol.solve = 1*exp(-50)
)

summary(spatial_autocorrelation_eastern_us <- errorsarlm(deaths_per_capita ~ deaths_social_porximity + deaths_spatial_proximity+
                                                ACS_PCT_HU_NO_VEH+
                                                POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                                ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                                +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
                                                +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported,
                                              data=cdc_mort_data_fips_wise_death_certificates_eastern_us ,
                                              listw = lw_2_eastern_united_states,
                                              zero.policy = TRUE,
                                              na.action = na.omit,
                                              tol.solve = 1*exp(-50)
))

#### fixed effect models eastern united states###
oods_2018_2019_eastern_united_states <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Fixed_effect_panel_data_eastern_us/eastern_united_states_fixed_effect_model.csv')
model_felm_eastern_united_states<- felm(deaths_per_capita ~ ACS_PCT_HU_NO_VEH+deaths_social_porximity 
                                        +deaths_spatial_proximity
                                          +
                                          POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
                                          ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
                                          +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE
                                        +ODR+ Naloxone_Available +Buprenorphine_Available+St_count_illicit_opioid_reported|GEOID+year,
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



#### main paper plot nbr###
my_plot_3 <- modelplot(list(nb_1_clustered_std_error_eastern_us,nb_1_clustered_std_error_entire_us),
                       coef_omit = c(-2,-3))

# Modify the plot
my_plot_3 <- my_plot_3 + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),   # Remove panel background
        axis.line = element_blank(),          # Remove axis lines
        axis.ticks = element_blank()) +       # Remove axis ticks
  geom_vline(xintercept = 0, color = "black") # Ensure the vertical line at zero remains
my_plot_3

my_plot_3 <- my_plot_3 + 
  labs(color = "Model Type") +
  scale_color_manual(labels = c("clusterd_robust_std_error_nbr_eastern_us",
                                "clusterd_robust_std_error_nbr_entire_us",
                                "spatial_autocorrelation",
                                "two_way_fixed_effect"),values = c("#e41a1c", "#377eb8")) 

print(my_plot_3)


#### correlation chart ###
library(ggplot2)
library(GGally)

nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")

colnames(nvss_ood_county_wise_2013_2017)[6] <- "deaths per capita"
colnames(nvss_ood_county_wise_2013_2017)[7] <- "social proximity"
colnames(nvss_ood_county_wise_2013_2017)[8] <- "spatial proximity"
# Select the variables from the dataset
variables <- c("deaths per capita", "social proximity", "spatial proximity")

# Create a new data frame with only the selected variables
df_selected <- nvss_ood_county_wise_2013_2017[variables]

# Create the conditional scatterplot matrix
ggpairs(df_selected, 
        lower = list(continuous = wrap("points", alpha = 0.8, color="red")),
        diag = list(continuous = wrap("barDiag", fill="red", bins=10)),
        upper = list(continuous = wrap("smooth", method = "lm", color="red", fullrange = TRUE)))+
  theme_minimal()

