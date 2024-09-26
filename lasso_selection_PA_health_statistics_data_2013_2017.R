library(glmnet)
library(randomForest)
library(DoubleML)
library(caret)
library(tidyverse)
library(tigris)
data <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/2013_2017_PA_Analysis/nvss_ood_county_wise_2013_2017.csv')
data <- data[,-1]
data <- data %>% dplyr::select('GEOID', 'NAME', 'population', 
                        'deaths', 'deaths_per_capita', 'deaths_spatial_porximity', 'deaths_social_proximity',
                        'ODR', 'Naloxone_Available')

### adding other clinical covariates ###
cdc_mort_data_fips_wise_death_certificates_entire_us <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/Entire United States/mort_data_entire_united_cdc_2018_2019.csv')
cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us[,-1]

cdc_mort_data_fips_wise_death_certificates_entire_us <- cdc_mort_data_fips_wise_death_certificates_entire_us %>% 
                                                                          dplyr::select("GEOID",
                                                                          "Buprenorphine_Available",
                                                                          "St_count_illicit_opioid_reported")

data <- data <- merge(data, cdc_mort_data_fips_wise_death_certificates_entire_us , by="GEOID")


#### population density ####
counties <- counties(year = 2018, cb = TRUE)
# Calculate area in square kilometers
counties <- st_transform(counties, crs = 5070)  # Transform to Albers Equal Area for accurate area calculation
counties <- counties %>%
  mutate(area_sq_km = as.numeric(st_area(geometry)) / 1e6)  # Convert area to square kilometers

counties <- counties %>% filter(GEOID %in% data$GEOID) %>% select(c("GEOID", "area_sq_km"))
data <- merge(data,counties,by="GEOID")
data <- data %>% mutate(population_density=population/area_sq_km)


#### frequent mental health distress clinical covariate #####
### https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation/national-data-documentation-2010-2022
mental_health_distress <- read.csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2019.csv")
mental_health_distress <- mental_health_distress  %>% dplyr::select("X5.digit.FIPS.Code", "Frequent.mental.distress.raw.value")
colnames(mental_health_distress)[1] <- "GEOID"
colnames(mental_health_distress)[2] <- "frequent_mental_health_distress"
mental_health_distress <- mental_health_distress[-c(1,2),]
mental_health_distress <- mental_health_distress %>% filter(GEOID %in% data$GEOID)
data <- merge(data,mental_health_distress,by="GEOID")
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
election_data <- election_data %>% filter(combined_fips %in% cdc_mort_data_fips_wise_death_certificates_entire_us$GEOID )

## mutating 0 and 1 based on election results ###
election_data <- election_data %>% mutate(political_affiliation= ifelse(votes_gop>votes_dem,1,0))

### selecting geoid and political affiliation ###
election_data <- election_data %>% dplyr::select("combined_fips","political_affiliation")
colnames(election_data)[1] <- "GEOID"

### merge data ###
data <- merge(data, election_data, by="GEOID")

data <- data[,-13]

data <- data %>%
  relocate("area_sq_km" , .after = "population" )

##### health determinant ###
health_determinant <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Health Determinant County 2017-2018/SDOH_2017_COUNTY_AHRQ_covariate_Selection.csv')
health_determinant <- health_determinant %>% dplyr::filter(health_determinant$STATE== "Pennsylvania")
health_determinant <- health_determinant %>% dplyr::select(c( 'COUNTYFIPS' , 
                                                             'ACS_PCT_UNEMPLOY', 
                                                             'ACS_PCT_HU_NO_VEH', 'POS_MEAN_DIST_ALC', 
                                                             'ACS_PCT_OTHER_INS', 'ACS_PCT_LT_HS',
                                                             ,'AHRF_TOT_COM_HEALTH_GRANT', 'ACS_TOT_POP_POV',
                                                             'ACS_MEDIAN_HH_INC','CCBP_BWLSTORES_RATE','AMFAR_MHFAC_RATE', 
                                                             'ACS_MEDIAN_AGE', 'ACS_PCT_WHITE',
                                                             'ACS_PCT_BLACK','ACS_PCT_ASIAN','ACS_PCT_AIAN',
                                                             'ACS_PCT_NHPI','ACS_PCT_MULT_RACE'))
nvss_ood_county_wise_2013_2017 <- left_join(data, health_determinant, by = c("GEOID" = "COUNTYFIPS"))
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017 %>% replace(is.na(.), 0)
scaled_health_determinant <- nvss_ood_county_wise_2013_2017[,c(16:32)]
scaled_health_determinant <- scale(scaled_health_determinant)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(16:32)]
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,scaled_health_determinant)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017 %>%
  relocate("ACS_MEDIAN_HH_INC" , .after = "political_affiliation" )
nvss_ood_county_wise_2013_2017$frequent_mental_health_distress <- as.numeric(nvss_ood_county_wise_2013_2017$frequent_mental_health_distress)
nvss_ood_county_wise_2013_2017$St_count_illicit_opioid_reported <- as.numeric(nvss_ood_county_wise_2013_2017$St_count_illicit_opioid_reported)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-12]
#### 'ACS_PCT_SMARTPHONE' = Percentage of households with a smartphone with no other type of computing device 
##### ''ACS_PCT_UNEMPLOY' = Percentage of population that was unemployed (ages 16 years and over)
###       ACS_PCT_PERSON_INC99  = Percentage of population with income to poverty ratio: <1.00
### ACS_MEDIAN_HH_INCOME= Median household income (in dollars, inflation-adjusted to file data year)
### CCBP_RATE_BWLSTORES_PER_1000 = Beer, wine and liquor stores per 1,000 people
#### CHR_HOMICIDES = Deaths due to homicide per 100,000 population
###ACS_PCT_NO_VEH = Percentage of housing units with no vehicle available
#### AHRF_COMM_HLTH_CNTR = Number of community health centers, grantees only
#### AHRF_MENTL_HLTH_CNT = Total number of community mental health centers
## CHR_SMOKING ### Percentage of adults who are current smokers
### CHR_MENTAL_DISTRESS = Percentage of adults reporting 14 or more days of poor mental health per month
#####AMFAR_HIVDIAGNOSED= Number of people living with diagnosed HIV
##############################
#### laso based selection ####
##############################


# Select predictor variables (columns 9 to 32) and the response variable
x <- nvss_ood_county_wise_2013_2017[, 9:31]
y <- nvss_ood_county_wise_2013_2017$deaths_per_capita

# Split the data into training (70%) and testing (30%) sets
set.seed(123)
index <- createDataPartition(y, p = 0.5, list = FALSE)
x_train <- x[index, ]
y_train <- y[index]
x_test <- x[-index, ]
y_test <- y[-index]

# Convert to data frames
x_train <- as.data.frame(x_train)
x_test <- as.data.frame(x_test)
x_full <- as.data.frame(x)  # Full dataset for final model fitting

# 2. Ensure Consistent Factor Levels
# -----------------------------------

for (col in names(x_train)) {
  if (is.factor(x_train[[col]])) {
    levels_col <- levels(x_train[[col]])
    x_test[[col]] <- factor(x_test[[col]], levels = levels_col)
    x_full[[col]] <- factor(x_full[[col]], levels = levels_col)
  }
}

# 3. Create Model Matrices Without Intercept
# ------------------------------------------

# Create model matrices without the intercept to allow glmnet to handle it
x_train_matrix <- model.matrix(~ ., data = x_train)[, -1]
x_test_matrix <- model.matrix(~ ., data = x_test)[, -1]
x_matrix <- model.matrix(~ ., data = x_full)[, -1]

# 4. Specify Unpenalized Variables by Name
# ----------------------------------------

# Define the exact names of your first 8 variables to be unpenalized
# Replace these with your actual variable names if they differ
unpenalized_vars <- c(
  "ODR",
  "Naloxone_Available",
  "Buprenorphine_Available",
  "population_density",
  "frequent_mental_health_distress",
  "political_affiliation",
  "ACS_MEDIAN_HH_INC"
)

# Verify that all unpenalized_vars exist in the model matrix
missing_vars <- setdiff(unpenalized_vars, colnames(x_train_matrix))
if(length(missing_vars) > 0){
  stop(paste("The following unpenalized variables are missing from the model matrix:", 
             paste(missing_vars, collapse = ", ")))
}

# Initialize penalty factors: 1 for penalized variables, 0 for unpenalized
penalty_factors <- rep(1, ncol(x_train_matrix))
names(penalty_factors) <- colnames(x_train_matrix)

# Assign penalty_factor=0 to unpenalized variables and their dummy variables
for (var in unpenalized_vars) {
  # Match exact variable name or its dummy variables (for factors)
  matches <- grep(paste0("^", var, "(\\.|$)"), names(penalty_factors))
  penalty_factors[matches] <- 0
}

# 4a. Diagnostic: Check Penalty Factors
print("Penalty Factors for Variables:")
print(penalty_factors)



# 6. Perform Cross-Validated Lasso Regression
# -------------------------------------------

cv.fit <- cv.glmnet(
  x_train_matrix,
  y_train,
  alpha = 1,                     # Lasso penalty
  nfolds = 5,                    # 6-fold cross-validation
  penalty.factor = penalty_factors,
  intercept = TRUE               # Let glmnet handle the intercept
)

# Plot the cross-validation results
plot(cv.fit)
title("Cross-Validation for Lasso Regression", line = 2.5)

# Extract the best lambda value
lambda_best <- cv.fit$lambda.min
cat("Optimal Lambda:", lambda_best, "\n")

# 7. Fit the Final Model on the Full Dataset
# ------------------------------------------

fit <- glmnet(
  x_matrix,
  y,
  alpha = 1,
  penalty.factor = penalty_factors,
  lambda = lambda_best,
  intercept = TRUE
)

# 8. Extract and View Coefficients
# --------------------------------

coefficients <- coef(fit)
print(coefficients)


