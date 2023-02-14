library(glmnet)
library(randomForest)
library(DoubleML)
data <- read.csv('nvss_ood_county_wise_2013_2017.csv')
health_determinant <- read.csv('county_2017.csv')
health_determinant <- health_determinant %>% dplyr::filter(health_determinant$STATE== "Pennsylvania")
health_determinant <- health_determinant %>% dplyr::select(c('COUNTY', 'FIPSCODE' , 'ACS_PCT_SMARTPHONE', 'ACS_PCT_UNEMPLOY', 'ACS_PCT_PERSON_INC99', 'ACS_MEDIAN_HH_INCOME', 'CCBP_RATE_BWLSTORES_PER_1000', 'CHR_HOMICIDES', 'ACS_PCT_NO_VEH', 'AHRF_COMM_HLTH_CNTR', 'AHRF_MENTL_HLTH_CNT', 'CHR_SMOKING', 'CHR_MENTAL_DISTRESS', 'AMFAR_HIVDIAGNOSED'))
nvss_ood_county_wise_2013_2017 <- cbind(data,health_determinant)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017 %>% replace(is.na(.), 0)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(16,17)]
scaled_health_determinant <- nvss_ood_county_wise_2013_2017[,c(16,17,18,19,20,21,22,23,24,25,26,27)]
scaled_health_determinant <- scale(scaled_health_determinant)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(16,17,18,19,20,21,22,23,24,25,26,27)]
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

x <- nvss_ood_county_wise_2013_2017[,c(10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27)]
x <- as.matrix(x)
y <- as.matrix(nvss_ood_county_wise_2013_2017$deaths_per_capita)
set.seed(123)
index <- createDataPartition(nvss_ood_county_wise_2013_2017$deaths_per_capita, p = 0.5, list = FALSE)
x_train <- x[index, ]
y_train <- y[index]
x_test <- x[-index, ]
y_test <- y[-index]
cv.fit <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
plot(cv.fit)
lambda_best <- cv.fit$lambda.min
fit <- glmnet(x, y, alpha = 1)
coef(fit, s = lambda_best)

