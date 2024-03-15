library(glmnet)
library(randomForest)
library(DoubleML)
library(caret)
library(tidyverse)
data <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/2013_2017_PA_Analysis/nvss_ood_county_wise_2013_2017.csv')
data <- data[,-1]
health_determinant <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Health Determinant County 2017-2018/SDOH_2017_COUNTY_AHRQ_covariate_Selection.csv')
health_determinant <- health_determinant %>% dplyr::filter(health_determinant$STATE== "Pennsylvania")
health_determinant <- health_determinant %>% dplyr::select(c('COUNTY', 'COUNTYFIPS' , 
                                                             'ACS_PCT_UNEMPLOY', 'ACS_PCT_PERSON_INC_BELOW99', 
                                                             'ACS_PCT_HU_NO_VEH', 'POS_MEAN_DIST_ALC', 
                                                             'ACS_PCT_OTHER_INS', 'ACS_PCT_LT_HS',
                                                             ,'AHRF_TOT_COM_HEALTH_GRANT',
                                                             'ACS_MEDIAN_HH_INC','CCBP_BWLSTORES_RATE','AMFAR_MHFAC_RATE', 
                                                             'ACS_MEDIAN_AGE', 'ACS_PCT_MALE', 'ACS_PCT_FEMALE','ACS_PCT_WHITE',
                                                             'ACS_PCT_BLACK','ACS_PCT_ASIAN','ACS_PCT_AIAN','ACS_PCT_NHPI','ACS_PCT_MULT_RACE'))




nvss_ood_county_wise_2013_2017 <- cbind(data,health_determinant)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017 %>% replace(is.na(.), 0)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(8:20)]
scaled_health_determinant <- nvss_ood_county_wise_2013_2017[,c(9:27)]
#scaled_health_determinant <- scale(scaled_health_determinant)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

scaled_health_determinant_normalized <- as.data.frame(lapply(scaled_health_determinant, normalize))

nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(9:27)]
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,scaled_health_determinant)


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
x <- nvss_ood_county_wise_2013_2017[,c(9:27)]
x <- as.matrix(x)
y <- as.matrix(nvss_ood_county_wise_2013_2017$deaths_per_capita)
set.seed(123)
index <- createDataPartition(nvss_ood_county_wise_2013_2017$deaths_per_capita, p = 0.7, list = FALSE)
x_train <- x[index, ]
y_train <- y[index]
x_test <- x[-index, ]
y_test <- y[-index]
cv.fit <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
plot(cv.fit)
lambda_best <- cv.fit$lambda.min
fit <- glmnet(x, y, alpha = 1)
coef(fit, s = lambda_best)



