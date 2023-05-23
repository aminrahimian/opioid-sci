#R version 4.2.1
## analyzes zip-level opioid death data in PA
library(tidyverse)
library(zipcodeR)
library(readr)
library(MASS)
### demographics data is search_state('PA') using zipcodeR package####
library(zipcodeR)
zip_code_demographics <- search_state('PA')
zip_code_demographics <- zip_code_demographics[!is.na(zip_code_demographics$lat),]
zip_code_demographics$zipcode <- as.integer(zip_code_demographics$zipcode)
data_2016_2017 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_2016_2017.csv')
opioid_death_2016_2017 <- data_2016_2017 %>% filter(data_2016_2017$COD %in% c("X40","X41","X42","X43","X44"))
table(opioid_death_2016_2017$COD)
zip_codes_in_data_2016_2017 <- unique(opioid_death_2016_2017$Zip)
data_ood_2016_2017 <- subset(opioid_death_2016_2017,select = c('fileno','Zip','COD', 'State'))
data_ood_2016_2017 <- data_ood_2016_2017 %>% filter(State=="PENNSYLVANIA")
table(opioid_death_2016_2017$COD)
unique_zipcodes <- table(unique(data_ood_2016_2017$Zip))
colnames(data_ood_2016_2017)[2] <- "zipcode"
nvss_data_2016_2017_with_demographic_information <- merge.data.frame(zip_code_demographics,data_ood_2016_2017,by="zipcode")
zip_code_wise_deaths_2016_2017 <- data.frame(table(nvss_data_2016_2017_with_demographic_information$zipcode))
colnames(zip_code_wise_deaths_2016_2017)[1] <- "zipcode"
colnames(zip_code_wise_deaths_2016_2017)[2] <- "deaths"
zip_code_wise_deaths_2016_2017$zipcode <- as.integer(as.character(zip_code_wise_deaths_2016_2017$zipcode))
#### ood_data_for_zipcodes_2013_2015##
ood_data_2013_2015 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_1999_2015.csv')
library(lubridate)
ood_data_2013_2015$dod<- mdy(ood_data_2013_2015$dod)
ood_data_2013_2015$dod <- as.Date(ood_data_2013_2015$dod)
ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(between(dod,as.Date('2013-01-01'), as.Date('2015-01-01')))
ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(ood_data_2013_2015$COD %in% c("X40","X41","X42","X43","X44"))
##f_i dataframe to store zip code wise deaths from 2013-2015, n represents deaths)
zip_code_wise_deaths_2013_2015 <- count(ood_data_2013_2015,zip_new) 
colnames(zip_code_wise_deaths_2013_2015)[1] <- "zip_new"
###### missing_zip_codes data 2013-2015 and 2015-2017
missing_zip_codes_2013_2015 <- setdiff(zip_code_wise_deaths_2016_2017$zipcode,zip_code_wise_deaths_2013_2015$zip_new)
missing_zip_codes_2016_2017 <- setdiff(zip_code_wise_deaths_2013_2015$zip_new,zip_code_wise_deaths_2016_2017$zipcode)
ood_for_missing_zip_codes_2013_2015 <- zip_code_wise_deaths_2016_2017 %>% filter(zipcode %in% missing_zip_codes_2013_2015)
ood_for_missing_zip_codes_2016_2017 <- zip_code_wise_deaths_2013_2015 %>% filter(zip_new %in% missing_zip_codes_2016_2017)
colnames(ood_data_2013_2015)[1] <- "zipcode"
colnames(zip_code_wise_deaths_2013_2015)[1] <- "zipcode"

### aggregating 2013-2017 ood death ###
aggregated_ood_2013_2017_zip_codes <- merge(zip_code_wise_deaths_2016_2017,zip_code_wise_deaths_2013_2015, by="zipcode")
aggregated_ood_2013_2017_zip_codes <- aggregated_ood_2013_2017_zip_codes %>% mutate(deaths=deaths+n)
aggregated_ood_2013_2017_zip_codes <- aggregated_ood_2013_2017_zip_codes[,-c(3)]
aggregated_ood_2013_2017_zip_codes <- rbind(aggregated_ood_2013_2017_zip_codes,ood_for_missing_zip_codes_2013_2015)
colnames(ood_for_missing_zip_codes_2016_2017)[1] <- "zipcode"
colnames(ood_for_missing_zip_codes_2016_2017)[2] <- "deaths"
aggregated_ood_2013_2017_zip_codes <- rbind(aggregated_ood_2013_2017_zip_codes, ood_for_missing_zip_codes_2016_2017)
### padding zeroes for missing zipcodes###
pa_zipcodes <- zip_code_demographics$zipcode
missing_zip_codes_in_aggregted_ood_2013_2017 <- setdiff(pa_zipcodes,aggregated_ood_2013_2017_zip_codes$zipcode)
n <- length(missing_zip_codes_in_aggregted_ood_2013_2017)
deaths_in_missing_zip_codes_in_aggregted_ood_2013_2017 <- rep(0, n)
df_for_missing_zipcodes_in_aggregated_ood_2013_2017 <- data.frame(c(missing_zip_codes_in_aggregted_ood_2013_2017),c(deaths_in_missing_zip_codes_in_aggregted_ood_2013_2017))
colnames(df_for_missing_zipcodes_in_aggregated_ood_2013_2017)[1] <- 'zipcode'
colnames(df_for_missing_zipcodes_in_aggregated_ood_2013_2017)[2] <- 'deaths'
complete_data_2013_2017 <- rbind(aggregated_ood_2013_2017_zip_codes,df_for_missing_zipcodes_in_aggregated_ood_2013_2017)
#### demographic_for_nvss_data_zipcode_level###
df_ood_nvss_zipcode_level <- merge(complete_data_2013_2017 ,zip_code_demographics,by="zipcode")
df_ood_nvss_zipcode_level$deaths_per_capita <- df_ood_nvss_zipcode_level$deaths/df_ood_nvss_zipcode_level$population

#### sci_proximity###
library(readr)
library(igraph)
library(tidyverse)
#df_0 <- read_tsv ('./GitHub/opioid-sci/zcta_zcta_shard1.tsv') 
df_0<- read_tsv('C:/Users/kusha/Desktop/Data for Paper/SCI/zcta_zcta_shard1.tsv')
## The zcta_zcta_shard1.tsv file contains ZIP-level SCIs 
## for ZIP=10001 to 19979, it is contained in 
## us-zip-code-us-zip-code-fb-social-connectedness-index-october-2021.zip
## file and is downloaded from  
## https://data.humdata.org/dataset/social-connectedness-index
nvss_zipcodes <- df_ood_nvss_zipcode_level$zipcode
df_1 <- df_0 %>% dplyr::filter(user_loc %in% nvss_zipcodes & fr_loc %in% nvss_zipcodes)
left_over_zip_codes <- nvss_zipcodes[!(nvss_zipcodes %in% df_1$user_loc)]
df_ood_nvss_zipcode_level<- df_ood_nvss_zipcode_level[ ! df_ood_nvss_zipcode_level$zipcode %in% left_over_zip_codes, ]
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
df_1$fr_loc <- as.numeric(df_1$fr_loc)
nodes <- df_1 %>% distinct(fr_loc)
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- df_ood_nvss_zipcode_level$population
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted

v <- df_ood_nvss_zipcode_level$deaths_per_capita
for(i in 1:ncol(cumulative_sci_weighted_test)){
  cumulative_sci_weighted_test[,i] <- cumulative_sci_weighted_test[,i] * v[i]
}
sci_proximity_zip_nvss_2013_2017 <- rowSums(cumulative_sci_weighted_test)
deaths_sci_proximity_zip_df <- data.frame(sci_proximity_zip_nvss_2013_2017)
deaths_sci_proximity_zip_df <-deaths_sci_proximity_zip_df %>% rownames_to_column(var = "zipcode")
deaths_sci_proximity_zip_df$zipcode <- as.integer(deaths_sci_proximity_zip_df$zipcode)
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,deaths_sci_proximity_zip_df,by="zipcode")



######################## ##### deaths spatial proximity##
library(geodist)
df <- df_ood_nvss_zipcode_level %>% dplyr::select(zipcode, lat, lng)
df <- df[order(df$zipcode),]
colnames(df)[2] <- "latitude"
colnames(df)[3] <- "longitude"
df <- df[, c(1, 3, 2)]

distance_matrix <- geodist(df, measure = 'geodesic') / 1000 # converting it to km
distance_matrix <- 1 + distance_matrix
distance_matrix <- distance_matrix**(-1)
diag(distance_matrix) <- 0
colnames(distance_matrix) <- df$zipcode
rownames(distance_matrix) <- df$zipcode
v <- df_ood_nvss_zipcode_level$deaths_per_capita
a_i_j <- data.frame(distance_matrix)
diag(a_i_j) <- 0
a_i_j <- as.matrix(a_i_j)

# Normalize a_i_j
normalised_scale <- rowSums(a_i_j)
a_i_j <- a_i_j / normalised_scale

# Ensure y is a numeric vector
y <- as.numeric(df_ood_nvss_zipcode_level$deaths_per_capita)

# Perform matrix multiplication
d_minus_i <- a_i_j %*% y

deaths_physical_proximity_nvss_2013_2017 <- d_minus_i
deaths_spatial_proximity_zip_df <- data.frame(deaths_physical_proximity_nvss_2013_2017)
deaths_spatial_proximity_zip_df <-deaths_spatial_proximity_zip_df  %>% rownames_to_column(var = "zipcode")
deaths_spatial_proximity_zip_df$zipcode <- as.integer(deaths_spatial_proximity_zip_df$zipcode)
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,deaths_spatial_proximity_zip_df,by="zipcode")



###### covariates
library(tidyverse)
library(tigris) # for fips_codes data
library(rvest) #for web scrapping
library(zipcodeR) #for ZIP codes
library(fuzzyjoin) #for string join
zip_code <- unique(df_ood_nvss_zipcode_level$zipcode)
##################### ODR administered per zip code#####
download_zip_data(force = FALSE)
data_zip <- zip_code_db
data_zip_for_ODR <- df_ood_nvss_zipcode_level %>% filter(zipcode %in% zip_code)
data_zip_for_ODR<- data_zip_for_ODR %>% mutate(county = str_remove_all(county, "County"))
data_zip_for_ODR_1 <- data_zip_for_ODR %>% dplyr::select(county,state)
unique_county_for_ODR <- unique(data_zip_for_ODR_1)

data_zip$county <- as.factor(data_zip$county)
county_count <- data_zip %>% count(county)
county_count <- county_count[-1,]
fips_data <- fips_codes
state_data <- merge.data.frame(county_count,fips_data,by='county')
state_data <- state_data %>% mutate(county= gsub("(.*),.*", "\\1", state_data$county))
state_data <- state_data %>% filter(state=="PA")



########## ADI#####
#PA_ZIP <- read.delim('./GitHub/opioid-sci/PA_2015_ADI_9 Digit Zip Code_v3.1.txt', sep=',')
## The 2015 ADI data set is downloaded from UW-Madison Neighborhood Atlas:
## https://www.neighborhoodatlas.medicine.wisc.edu/download
#zips <- PA_ZIP %>% mutate(zip=str_extract(ZIPID,'\\d{5}'))
#zips <- zips[,-c(1,2,10)]
#zips_adi_Staternk <- zips[,c(9,10)]
#zips_adi_Staternk$zip <- as.integer(zips_adi_Staternk$zip)
#zips_adi_Staternk <- zips_adi_Staternk %>% filter(zip %in% zip_code )
#zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "PH")
#zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "GQ")
#zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "PH-GQ")
#zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "GQ-PH")
#zips_adi_Staternk$ADI_STATERNK <- as.integer(zips_adi_Staternk$ADI_STATERNK)
#zips_adi_Staternk <- zips_adi_Staternk %>% drop_na()
#zips_adi_Staternk$zips_factor <- as.factor(zips_adi_Staternk$zip)
#zip_wise_adi <- aggregate(zips_adi_Staternk$ADI_STATERNK, list(zips_adi_Staternk$zip), FUN=mean)
#zip_wise_adi <- zip_wise_adi %>% filter(Group.1 %in% zip_code)
#colnames(zip_wise_adi)[1] <- "zip"
#colnames(zip_wise_adi)[2] <- "adi_net_rank"
#zip_wise_adi <- zip_wise_adi %>% mutate(adi_net_rank=scale(adi_net_rank))
#colnames(zip_wise_adi)[1] <- "zipcode"
#df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,zip_wise_adi,by="zipcode")
#write.csv(df_ood_nvss_zipcode_level,'aggregated_zip_data_2013_2107_nvss_zero_padded.csv')

##### health determinant ####
#############################
#ACS_PCT_LT_HS= Percentage of population with less than high school education (ages 25 and over)
#ACS_PCT_MEDICAID_ANY=Percentage of population with any Medicaid/means-tested public health insurance coverage
#ERS_RUCA1_2010=Primary Rural-Urban Commuting Area Code 2010
#ACS_PCT_UNINSURED	Percentage of population with no health insurance coverage
#ACS_PCT_LT_HS Percentage of population with less than high school education (ages 25 and over, ZCTA level)
#POS_DIST_ALC_ZP	Distance in miles to the nearest hospital with alcohol and drug abuse inpatient care, calculated using population weighted ZIP centroids
#ACS_PCT_PERSON_INC_BELOW99_ZC	Percentage of population with an income to poverty ratio under 1.00 (ZCTA level)
#ACS_PCT_LT_HS_ZC	Percentage of population with less than high school education (ages 25 and over, ZCTA level)
#ACS_PCT_OTHER_INS_ZC	Percentage of population with other health insurance coverage combinations (ZCTA level)
#CCBP_TOT_BWLSTORES_ZP	Total number of beer, wine and liquor stores (ZIP level)
#ACS_PCT_HU_NO_VEH_ZC Percentage of housing units with no vehicle available

#CCBP_TOT_GAMBLING_ZP	Total number of gambling establishments (ZIP level)
#POS_DIST_ALC_ZP	Distance in miles to the nearest hospital with alcohol and drug abuse inpatient care, calculated using population weighted ZIP centroids
#https://www.ahrq.gov/sites/default/files/wysiwyg/sdoh/SDOH_2017_ZIPCODE_1_0.xlsx

zip_codes_in_df_ood_nvss_for_sdoh <- df_ood_nvss_zipcode_level$zipcode
health_determinant <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Health Determinant ZIPCODE 2017-2018/SDOH_ZIPCODE.csv')
health_determinant_zip <- health_determinant %>% dplyr::filter(health_determinant$STATE== "Pennsylvania")
health_determinant_zip <- health_determinant_zip %>% dplyr::select(c('ZCTA', 'STATE' , 'ACS_PCT_HH_SMARTPHONE_ZC', 'ACS_PCT_UNEMPLOY_ZC', 'ACS_PCT_PERSON_INC_BELOW99_ZC', 'CCBP_TOT_BWLSTORES_ZP', 'ACS_PCT_HU_NO_VEH_ZC', 'ACS_PCT_LT_HS_ZC', 'POS_DIST_ALC_ZP','ACS_PCT_OTHER_INS_ZC','CCBP_TOT_GAMBLING_ZP'))
health_determinant_zip <- health_determinant_zip %>% dplyr::filter(ZCTA %in% zip_codes_in_df_ood_nvss_for_sdoh)
health_determinant_zip_unique <- health_determinant_zip[!duplicated(health_determinant_zip$ZCTA),]
zctas <- c("17016", "17945", "18221", "18223", "18335", "19316")
for (zcta in zctas) {
  row <- c(zcta, rep("0", 11))
  health_determinant_zip_unique <- rbind(health_determinant_zip_unique, row)
}
health_determinant_zip_unique <- health_determinant_zip_unique %>% replace(is.na(.), 0)
health_determinant_zip_unique[, 2] <- "Pennsylvania"
health_determinant_zip_unique <- health_determinant_zip_unique %>% arrange(ZCTA)
health_determinant_zip_covariates <- health_determinant_zip_unique[,c(3:11)]
health_determinant_zip_covariates[1:9] <- lapply(health_determinant_zip_covariates[1:9], as.numeric)
health_determinant_zip_covariates <- scale(health_determinant_zip_covariates)
df_ood_nvss_zipcode_level <- cbind(df_ood_nvss_zipcode_level,health_determinant_zip_covariates)
colnames(df_ood_nvss_zipcode_level)[27] <- "deaths_sci_proximity_zip"
colnames(df_ood_nvss_zipcode_level)[28] <- "deaths_spatial_proximity_zip"
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-37]



#### opioids for total_rx and total_naloxone 2013###
library(haven)
opioids_2013<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2013.sas7bdat")
opioids_2013_PA <- opioids_2013 %>% filter(st_code=="PA")
county_wise_total_rx_2013 <- opioids_2013_PA %>% group_by(county_nm, state_fip_county_fip) %>% summarise( total_rx=sum(total_rx), total_dose=sum(total_dose))
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")
county_wise_total_rx_2013$state_fip_county_fip <- as.numeric(county_wise_total_rx$state_fip_county_fip)
setdiff(nvss_ood_county_wise_2013_2017$GEOID,county_wise_total_rx_2013$state_fip_county_fip)
### opioid back tracing for fulton ###
opioids_2014_PA_fulton <- opioids_2014_PA %>% filter(county_nm== "FULTON")
opioids_2015_PA_fulton <- opioids_2015_PA %>% filter(county_nm== "FULTON")
fulton_longitudnal_data <- rbind(opioids_2014_PA_fulton,opioids_2015_PA_fulton)
fulton_cumulative_total_dose_rx_year <- fulton_longitudnal_data %>% group_by(year) %>% summarise(total_rx =sum(total_rx), total_dose=sum(total_dose))
#### the linear model prediction##
new_data <- data.frame(year=2013)
total_dose_lm <- lm(total_dose ~ year,data=fulton_cumulative_total_dose_rx_year )
predicted_dose <- predict(total_dose_lm,newdata = new_data)
### prediction for total_rx####
total_rx_lm <- lm(total_rx ~ year, data=fulton_cumulative_total_dose_rx_year)
predicted_rx <- predict(total_rx_lm,newdata = new_data)
### getting the data in county_wise_total_rs
missing_county_data <- data.frame(county_nm = "FULTON", state_fip_county_fip = 42057, 
                                  total_rx = predicted_rx, total_dose = predicted_dose)
county_wise_total_rx_2013 <- rbind(county_wise_total_rx_2013,missing_county_data)
county_wise_total_rx_2013 <- county_wise_total_rx_2013 %>%  arrange(county_wise_total_rx_2013$state_fip_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
county_wise_total_rx_2013 <- cbind(county_wise_total_rx_2013,county_Wise_zip_codes)
county_wise_total_rx_2013 <- county_wise_total_rx_2013[,-1]
county_wise_total_rx_2013 <- county_wise_total_rx_2013 %>% mutate(total_rx_per_zip_codes <- total_rx/n)
colnames(county_wise_total_rx_2013)[5] <- "n"
colnames(county_wise_total_rx_2013)[6] <- "total_rx_per_zip_codes"
#### opioids for total_rx and total_naloxone 2014###
opioids_2014<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2014.sas7bdat")
opioids_2014_PA <- opioids_2014 %>% filter(st_code=="PA")
county_wise_total_rx_2014 <- opioids_2014_PA %>% group_by(county_nm, state_fip_county_fip) %>% summarise( total_rx=sum(total_rx), total_dose=sum(total_dose))
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")
county_wise_total_rx$state_fip_county_fip <- as.numeric(county_wise_total_rx$state_fip_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
county_wise_total_rx_2014 <- cbind(county_wise_total_rx_2014,county_Wise_zip_codes)
county_wise_total_rx_2014 <- county_wise_total_rx_2014[,-1]
county_wise_total_rx_2014 <- county_wise_total_rx_2014 %>% mutate(total_rx_per_zip_codes <- total_rx/n)
colnames(county_wise_total_rx_2014)[5] <- "n"
colnames(county_wise_total_rx_2014)[6] <- "total_rx_per_zip_codes"
#### opioids for total_rx and total_naloxone 2015###
opioids_2015<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2015.sas7bdat")
opioids_2015_PA <- opioids_2015 %>% filter(st_code=="PA")
county_wise_total_rx_2015 <- opioids_2015_PA %>% group_by(county_nm, state_fip_county_fip) %>% summarise( total_rx=sum(total_rx), total_dose=sum(total_dose))
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")
county_wise_total_rx$state_fip_county_fip <- as.numeric(county_wise_total_rx$state_fip_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
county_wise_total_rx_2015 <- cbind(county_wise_total_rx_2015,county_Wise_zip_codes)
county_wise_total_rx_2015 <- county_wise_total_rx_2015[,-1]
county_wise_total_rx_2015 <- county_wise_total_rx_2015 %>% mutate(total_rx_per_zip_codes <- total_rx/n)
colnames(county_wise_total_rx_2015)[5] <- "n"
colnames(county_wise_total_rx_2015)[6] <- "total_rx_per_zip_codes"
#### opioids for total_rx and total_naloxone 2016###
opioids_2016<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2016.sas7bdat")
opioids_2016_PA <- opioids_2016 %>% filter(st_code=="PA")
county_wise_total_rx_2016 <- opioids_2016_PA %>% group_by(county_nm, state_fip_county_fip) %>% summarise( total_rx=sum(total_rx), total_dose=sum(total_dose))
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")
county_wise_total_rx$state_fip_county_fip <- as.numeric(county_wise_total_rx$state_fip_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
county_wise_total_rx_2016 <- cbind(county_wise_total_rx_2016,county_Wise_zip_codes)
county_wise_total_rx_2016 <- county_wise_total_rx_2016[,-1]
county_wise_total_rx_2016 <- county_wise_total_rx_2016 %>% mutate(total_rx_per_zip_codes <- total_rx/n)
colnames(county_wise_total_rx_2016)[6] <- "total_rx_per_zip_codes"
#### opioids for total_rx and total_naloxone 2016###
opioids_2017<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2017.sas7bdat")
opioids_2017_PA <- opioids_2017 %>% filter(st_code=="PA")
county_wise_total_rx_2017 <- opioids_2017_PA %>% group_by(county_nm, state_fip_county_fip) %>% summarise( total_rx=sum(total_rx), total_dose=sum(total_dose))
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")
county_wise_total_rx$state_fip_county_fip <- as.numeric(county_wise_total_rx$state_fip_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
county_wise_total_rx_2017 <- cbind(county_wise_total_rx_2017,county_Wise_zip_codes)
county_wise_total_rx_2017 <- county_wise_total_rx_2017[,-1]
county_wise_total_rx_2017 <- county_wise_total_rx_2017 %>% mutate(total_rx_per_zip_codes <- total_rx/n)
colnames(county_wise_total_rx_2017)[6] <- "total_rx_per_zip_codes"
###### opioid cumulative data frame######
county_total_rx <- rbind(county_wise_total_rx_2013, county_wise_total_rx_2014, county_wise_total_rx_2015, county_wise_total_rx_2016, county_wise_total_rx_2017)
county_total_rx <- county_total_rx %>% group_by(county, state_fip_county_fip) %>% summarise( cumulative_total_dose=sum(total_dose))
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,county_total_rx, by="county")
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-37]
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% group_by(county) %>% mutate(population_proprtion = population/sum(population))
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% mutate(cumulative_total_dose_population_proprtion= cumulative_total_dose*population_proprtion)
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% ungroup(county) %>%  mutate(cumulative_total_dose_population_proprtion_per_capita=cumulative_total_dose_population_proprtion/population)
##### total naloxone available 2013 ##########
naloxone_2013 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2013.sas7bdat")
naloxone_2014 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2014.sas7bdat")
naloxone_2015 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2015.sas7bdat")
naloxone_2016 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2016.sas7bdat")
naloxone_2017 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2017.sas7bdat")
total_naloxone <- rbind(naloxone_2013, naloxone_2014, naloxone_2015, naloxone_2016, naloxone_2017)
total_naloxone <- total_naloxone %>% filter(st_code=="PA") %>%  group_by(county_nm,state_and_county_fip) %>% 
  summarise(avg_total_rx = mean(total_rx, na.rm = TRUE))
total_naloxone$state_and_county_fip <- as.numeric(total_naloxone$state_and_county_fip)
county <- unique(df_ood_nvss_zipcode_level$county)
new_row <- data.frame(county_nm = "Cameron"  , state_and_county_fip = 42023, avg_total_rx = 0)
total_naloxone <- rbind(total_naloxone,new_row)
total_naloxone <- total_naloxone %>% arrange(state_and_county_fip)
total_naloxone <- cbind(total_naloxone,county_Wise_zip_codes)
total_naloxone <- total_naloxone[,-1]
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,total_naloxone, by="county")
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-c(41,43)]
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>%
  mutate(average_naloxone_population_proprtion= avg_total_rx*population_proprtion)
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>%
  mutate(average_naloxone_population_proprtion_per_capita=average_naloxone_population_proprtion/population)
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-c(37,39,41,42)]
############# write and read the csv #######3



#### covariate selection using lasso####
library(glmnet)
library(caret)
x <- df_ood_nvss_zipcode_level[,c(31,32,33,34,35,36,37)]
x <- as.matrix(x)
y <- as.matrix(df_ood_nvss_zipcode_level$deaths_per_capita)
set.seed(123)
index <- createDataPartition(df_ood_nvss_zipcode_level$deaths_per_capita, p = 0.8, list = FALSE)
x_train <- x[index, ]
y_train <- y[index]
x_test <- x[-index, ]
y_test <- y[-index]
cv.fit <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
#plot(cv.fit)
lambda_best <- cv.fit$lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_best)

# Obtain coefficients and p-values
coef_lasso <- coef(lasso_model)
pvalues_lasso <- summary(lasso_model) # Remove the intercept term

# Print the coefficients and p-values
print(coef_lasso)
print(pvalues_lasso)

########## NEGATIVE BINOMIAL #######3
library(MASS)
summary(nb1 <- glm.nb(deaths/population ~ deaths_sci_proximity_zip  + deaths_spatial_proximity_zip
                     + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                       + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                     + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                     +cumulative_total_dose_population_proprtion_per_capita+ average_naloxone_population_proprtion_per_capita, 
                     data = df_ood_nvss_zipcode_level, weights=population))



linear_model_1 <- lm(deaths_per_capita~  deaths_sci_proximity_zip  + deaths_spatial_proximity_zip
                     + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                       + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                     + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                     +cumulative_total_dose_population_proprtion_per_capita+ 
                       average_naloxone_population_proprtion_per_capita,
                     data = df_ood_nvss_zipcode_level, weights=population)
summary(linear_model_1)


### latex code ####
library(stargazer)
stargazer::stargazer(nb1)
stargazer::stargazer(linear_model_1)
install.packages("webshot")
library(AER)
# Fit a Poisson model

pois_model <- glm(deaths/population ~  deaths_sci_proximity_zip  + deaths_spatial_proximity_zip
                  + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                    + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                  + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC +
                  cumulative_total_dose_population_proprtion_per_capita+
                    average_naloxone_population_proprtion_per_capita, data = df_ood_nvss_zipcode_level, weights=population, family=poisson)
summary(pois_model)
stargazer(pois_model)
# Test for overdispersion
dispersiontest(pois_model, trafo=1)



#####################

### library(pscl)for zero inflated regression
df_ood_nvss_zipcode_level <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% mutate(deaths=deaths_per_capita*population)
library(pscl)
summary(m4 <- zeroinfl(deaths ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                       + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                       +scale(population)+ OPR, data = df_ood_nvss_zipcode_level, dist = 'negbin'))
plot(m4$residuals)

stargazer(m4,title = "Zero Inflated Regression Results", align = TRUE, type = "latex")
stargazer(m4,zero.component = TRUE, title = "Zero Inflated Regression Results", align = TRUE, type = "latex")


summary(m4.1 <- zeroinfl(deaths ~  log(deaths_sci_proximity_zip) +log(deaths_physical_proximity)+naloxone_administered_per_zip_code+naloxone_administered_per_zip_code+ACS_PCT_SMARTPHONE+ACS_PCT_UNEMPLOY+ACS_PCT_PERSON_INC99+ACS_MEDIAN_HH_INCOME+CCBP_RATE_BWLSTORES_PER_1000+ACS_PCT_NO_VEH+scale(population),data = df_ood_nvss_zipcode_level))

#### Social ERROR REGRESSION
library(spatialreg)
library(spdep)
library(igraph)
df_0<- read_tsv('C:/Users/kusha/Desktop/Data for Paper/SCI/zcta_zcta_shard1.tsv')
nvss_zipcodes <- df_ood_nvss_zipcode_level$zipcode
df_1 <- df_0 %>% dplyr::filter(user_loc %in% nvss_zipcodes & fr_loc %in% nvss_zipcodes)
left_over_zip_codes <- nvss_zipcodes[!(nvss_zipcodes %in% df_1$user_loc)]
df_ood_nvss_zipcode_level<- df_ood_nvss_zipcode_level[ ! df_ood_nvss_zipcode_level$zipcode %in% left_over_zip_codes, ]
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
df_for_matrix_probability <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_probability
nodes <- df_1 %>% distinct(user_loc)
k <- graph.data.frame(df_for_matrix_probability, directed=F, vertices=nodes)
sci_matrix <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
sci_matrix <- as.matrix(sci_matrix)
diag(sci_matrix) <- 0 
row_sums <- rowSums(sci_matrix)
recip_row_sums <- 1/row_sums
D <- Diagonal(x = recip_row_sums)
sparse_mat_norm <- D %*% sci_matrix
sci_proximity_mat_zip <- as.matrix(sparse_mat_norm)
diag(sci_proximity_mat_zip) <- 0
lw_3 <- mat2listw(sci_proximity_mat_zip)

################ spatial proximity #####
library(geodist)
df <-  df_ood_nvss_zipcode_level%>% dplyr::select(zipcode, lat, lng)
df <- df[order(df$zipcode),]
colnames(df)[2] <- "latitude"
colnames(df)[3] <- "longitude"
df <- df[,c(1,3,2)]
distance_matrix <- geodist(df, measure = 'geodesic' )/1000 #converting it to km
distance_matrix <- (1+distance_matrix)
distance_matrix<- distance_matrix**(-1)
colnames(distance_matrix) <- df$zipcode
rownames(distance_matrix) <- df$zipcode
d_i_j<- data.frame(distance_matrix)
zip_distance_proximity<- d_i_j
zip_distance_proximity <- as.matrix(zip_distance_proximity)
colnames(zip_distance_proximity) <- df$zipcode
rownames(zip_distance_proximity) <- df$zipcode
diag(zip_distance_proximity) <- 0
row_sums_distance <- rowSums(zip_distance_proximity)
recip_row_sums_distance <- 1/row_sums_distance 
D_distance <- Diagonal(x = recip_row_sums_distance )
sparse_mat_norm_distance <- D_distance %*% zip_distance_proximity
spatial_proximity_mat_zip <- as.matrix(sparse_mat_norm_distance)
diag(spatial_proximity_mat_zip) <- 0

spaial_proximiy_mat_zip <- as.matrix(spatial_proximity_mat_zip)
lw_4 <- mat2listw(spaial_proximiy_mat_zip)

#### sem model testing morrans I##
linear_model <- lm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip 
                   + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                     + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC + total_rx_per_capita 
                   , data = df_ood_nvss_zipcode_level, weights = df_ood_nvss_zipcode_level$population)
summary(linear_model)

stargazer::stargazer(linear_model,out='zipcode_linear_model.html')
moran.test(linear_model$residuals, listw = lw_4)


#### zipcode_spatial_1.1 for the new selected covariates
df_ood_nvss_zipcode_level_test <- df_ood_nvss_zipcode_level
library(scales)

df_ood_nvss_zipcode_level_test$cumulative_total_dose_population_proprtion_per_capita <- rescale(
  df_ood_nvss_zipcode_level_test$cumulative_total_dose_population_proprtion_per_capita, 
  to = c(0, 1)
)

df_ood_nvss_zipcode_level_test$average_naloxone_population_proprtion_per_capita <- rescale(
  df_ood_nvss_zipcode_level_test$average_naloxone_population_proprtion_per_capita, 
  to = c(0, 1)
)

df_ood_nvss_zipcode_level_test$ACS_PCT_UNEMPLOY_ZC <-  rescale(df_ood_nvss_zipcode_level_test$ACS_PCT_UNEMPLOY_ZC, to = c(0, 1))
df_ood_nvss_zipcode_level_test$ACS_PCT_PERSON_INC_BELOW99_ZC <- rescale(df_ood_nvss_zipcode_level_test$ACS_PCT_PERSON_INC_BELOW99_ZC ,to = c(0, 1))
df_ood_nvss_zipcode_level_test$ACS_PCT_LT_HS_ZC <- rescale(df_ood_nvss_zipcode_level_test$ACS_PCT_LT_HS_ZC, to = c(0, 1))
df_ood_nvss_zipcode_level_test$POS_DIST_ALC_ZP <- rescale(df_ood_nvss_zipcode_level_test$POS_DIST_ALC_ZP, to = c(0, 1))
df_ood_nvss_zipcode_level_test$ACS_PCT_HU_NO_VEH_ZC <- rescale(df_ood_nvss_zipcode_level_test$ACS_PCT_HU_NO_VEH_ZC , to = c(0, 1))
df_ood_nvss_zipcode_level_test$ACS_PCT_OTHER_INS_ZC <- rescale(df_ood_nvss_zipcode_level_test$ACS_PCT_OTHER_INS_ZC  , to = c(0, 1))

ZIPCODE_SOCIAL <- errorsarlm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip
                                  + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                                    + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                                  + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC + 
                               average_naloxone_population_proprtion_per_capita + cumulative_total_dose_population_proprtion_per_capita
                            ,data=df_ood_nvss_zipcode_level_test,
                                  listw = lw_3,
                                  zero.policy = TRUE,weights = population,
                                  na.action = na.omit)
summary(ZIPCODE_SOCIAL)

### spatial autocorrelation##########
ZIPCODE_SPATIAL <- errorsarlm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip
                              + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                                + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                              + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC + 
                                average_naloxone_population_proprtion_per_capita + cumulative_total_dose_population_proprtion_per_capita
                              ,data=df_ood_nvss_zipcode_level_test,
                              listw = lw_4,
                              zero.policy = TRUE,weights = population,
                              na.action = na.omit)
summary(ZIPCODE_SPATIAL)



#### fix the csv so the results dont fluctuate ####
write.csv(df_ood_nvss_zipcode_level_test, 'df_ood')


######### correlation chart deaths per capita, social proximity, spatial proximity ##########
library(ggplot2)
library(GGally)

colnames(df_ood_nvss_zipcode_level)[26] <- "deaths per capita"
colnames(df_ood_nvss_zipcode_level)[27] <- "social proximity"
colnames(df_ood_nvss_zipcode_level)[28] <- "spatial proximity"
# Select the variables from the dataset
variables <- c("deaths per capita", "social proximity", "spatial proximity")

# Create a new data frame with only the selected variables
df_selected <- df_ood_nvss_zipcode_level[variables]

# Create the conditional scatterplot matrix
ggpairs(df_selected, 
        lower = list(continuous = wrap("points", alpha = 0.5, color="red")),
        diag = list(continuous = wrap("barDiag", fill="red")),
        upper = list(continuous = wrap("smooth", method = "lm", color="red", fullrange = TRUE)))+
  theme_minimal()

###########################################
















