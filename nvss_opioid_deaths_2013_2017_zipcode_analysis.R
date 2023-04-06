#R version 4.2.1
## analyzes zip-level opioid death data in PA
library(tidyverse)
library(zipcodeR)
library(readr)

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
s_i_2013<- rowSums(cumulative_sci_weighted_test)




#### q_i stores zipcode wise deathspercapita aggregated over temporal window from 2013-2017####
#q_i <- df_ood_nvss_zipcode_level[,c(1,26)]
#colnames(q_i)[1] <- "fr_loc"
#df_s <- merge(df_1,q_i,by ="fr_loc")
#df_s <- df_s %>% mutate(wt_sci=probabilites*deaths_per_capita)
#dataframe_for_matrix <- df_s %>% dplyr::select(c(fr_loc,user_loc,wt_sci))
#nodes <- df_s %>% distinct(user_loc)
#g <- graph.data.frame(dataframe_for_matrix, directed=T, vertices=nodes)
#sci_proximity <- as_adjacency_matrix(g,attr = "wt_sci",sparse = F)
#diag(sci_proximity) <- 0
#row_wise_Wt_sum <- rowSums(sci_proximity)
#df_for_matrix_probability <- df_s %>% dplyr::select(c(user_loc,fr_loc,probabilites))
#df_for_matrix_probability
#k <- graph.data.frame(df_for_matrix_probability, directed=F, vertices=nodes)
#cumulative_sci <- as_adjacency_matrix(k,attr="probabilites",sparse=F)
#diag(cumulative_sci) <- 0
#row_wise_sum_sci <- rowSums(cumulative_sci)
#row_wise_sum_sci
#sci_proximity_zip_nvss_2013_2017 <- row_wise_Wt_sum/row_wise_sum_sci
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

#### ODR###
url <- read_html('https://www.cdc.gov/drugoverdose/rxrate-maps/county2016.html')
sites <- url %>% html_nodes('td') %>% html_text()
sites_1 <- sites[8977:9244]
sites_2 <- data.frame(print(sites_1))
ODR <- as.data.frame(t(matrix(sites_2[,1],nrow = 4)))
colnames(ODR)[1] <- 'county'
colnames(ODR)[2] <- 'State'
colnames(ODR)[3] <- 'Fips Codes'
colnames(ODR)[4] <- 'Opioid Dispensing Rate Per 100'
ODR <- ODR %>% mutate(county= gsub("(.*),.*", "\\1", ODR$county))
county_name <- unique(data_zip_for_ODR$county)
county_name <- sort(county_name)
ODR_1 <- stringdist_join(data_zip_for_ODR, ODR, 
                         by='county', #match based on team
                         mode='left', #use left join
                         method = "jw", #use jw distance metric
                         max_dist=99,distance_col='dist')%>%
  group_by(county.x) %>%
  slice_min(order_by=dist, n=1)
ODR_1 <- ODR_1 %>% filter(zipcode %in% zip_code)
ODR_2 <- ODR_1 %>% dplyr::select("zipcode","Opioid Dispensing Rate Per 100")

df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,ODR_2, by="zipcode")
df_ood_nvss_zipcode_level<- df_ood_nvss_zipcode_level[,-29]
colnames(df_ood_nvss_zipcode_level)[29] <- "ODR"
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% mutate(ODR=scale(as.numeric(ODR)))

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
#######################################################################################
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
data_naloxone <- data_naloxone %>% dplyr::select(County.Name,Cumulative.Kits.Provided)
data_naloxone <- data_naloxone[order(data_naloxone$County.Name),]
colnames(data_naloxone)[1] <- "county"
data_naloxone$Cumulative.Kits.Provided <- as.numeric(gsub(",","",data_naloxone$Cumulative.Kits.Provided))
data_naloxone <- data_naloxone %>% mutate(county= gsub("(.*),.*", "\\1", data_naloxone$county))
data_naloxone$zip_code_contains <- state_data$n
data_naloxone <- data_naloxone %>% mutate(naloxone_administered_per_zip_code= Cumulative.Kits.Provided/as.numeric(zip_code_contains))
data_naloxone$naloxone_administered_per_zip_code <- round(data_naloxone$naloxone_administered_per_zip_code)
naloxone_administered_per_zip_code <- data_naloxone[,c(1,4)]
naloxone_administered_per_zip_code$naloxone_administered_per_zip_code <-naloxone_administered_per_zip_code$naloxone_administered_per_zip_code
df_ood_nvss_zipcode_level$county <- gsub(" County", "", df_ood_nvss_zipcode_level$county)
#df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-30]
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,naloxone_administered_per_zip_code,by="county")
df_ood_nvss_zipcode_level$naloxone_administered_per_zip_code <- scale(df_ood_nvss_zipcode_level$naloxone_administered_per_zip_code/df_ood_nvss_zipcode_level$population)
colnames(df_ood_nvss_zipcode_level)[27] <- "deaths_sci_proximity_zip"
colnames(df_ood_nvss_zipcode_level)[28] <- "deaths_spatial_proximity_zip"
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-37]

write.csv(df_ood_nvss_zipcode_level,'aggregated_zip_data_2013_2107_nvss_zero_padded.csv')

df_ood_nvss_zipcode_level <- read.csv('aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-1]
############## OPR ###########
opr <- read.csv('C:/Users/kusha/Desktop/Data for Paper/OPR/OPR.csv')
opr_2013 <- opr %>% filter(Year==2013) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2013$zip_new)
opr_2013 <- opr_2013 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2013)[1] <- "zip_new"
opr_2013 <- opr_2013 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
colnames(opr_2013)[1] <- "zipcode"
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,opr_2013, by="zipcode", all.x = TRUE)
###### for year 2014 ####
opr_2014 <- opr %>% filter(Year==2014) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2014$zip_new)
opr_2014 <- opr_2014 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2014)[1] <- "zip_new"
opr_2014 <- opr_2014 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
colnames(opr_2014)[1] <- "zipcode"
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,opr_2014, by="zipcode", all.x = TRUE)
#### for year 2015 ####
opr_2015 <- opr %>% filter(Year==2015) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2015$zip_new)
opr_2015 <- opr_2015 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2015)[1] <- "zip_new"
opr_2015 <- opr_2015 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
colnames(opr_2015)[1] <- "zipcode"
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,opr_2015, by="zipcode", all.x = TRUE)
#### for year 2016 ####
opr_2016 <- opr %>% filter(Year==2016) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2016$zip_new)
opr_2016 <- opr_2016 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2016)[1] <- "zip_new"
opr_2016 <- opr_2016 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
colnames(opr_2016)[1] <- "zipcode"
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,opr_2016, by="zipcode", all.x = TRUE)
#### for year 2017##########
opr_2017 <- opr %>% filter(Year==2017) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2017$zip_new)
opr_2017 <- opr_2017 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2017)[1] <- "zip_new"
opr_2017 <- opr_2017 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
colnames(opr_2017)[1] <- "zipcode"
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,opr_2017, by="zipcode", all.x = TRUE)
####################
df_ood_nvss_zipcode_level <- replace(df_ood_nvss_zipcode_level, is.na(df_ood_nvss_zipcode_level), 0)
df_ood_nvss_zipcode_level[,c(39:43)] <- lapply(df_ood_nvss_zipcode_level[,c(39:43)], as.numeric)

# ########## AGGREGATED OPR FOR 2013-2017######
oprs <- df_ood_nvss_zipcode_level[, 39:43]
row_mean <- rowMeans(oprs)
df_ood_nvss_zipcode_level$OPR <- row_mean
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-c(39:43)]



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




### library(pscl)for zero inflated regression
df_ood_nvss_zipcode_level <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% mutate(deaths=deaths_per_capita*population)
library(pscl)
summary(m4 <- zeroinfl(deaths ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                       + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                       +scale(population)+ OPR, data = df_ood_nvss_zipcode_level, dist = 'poisson'))
plot(m4$residuals)

stargazer(m4,title = "Zero Inflated Regression Results", align = TRUE, type = "latex")
stargazer(m4,zero.component = TRUE, title = "Zero Inflated Regression Results", align = TRUE, type = "latex")





summary(m4.1 <- zeroinfl(deaths ~  log(deaths_sci_proximity_zip) +log(deaths_physical_proximity)+naloxone_administered_per_zip_code+naloxone_administered_per_zip_code+ACS_PCT_SMARTPHONE+ACS_PCT_UNEMPLOY+ACS_PCT_PERSON_INC99+ACS_MEDIAN_HH_INCOME+CCBP_RATE_BWLSTORES_PER_1000+ACS_PCT_NO_VEH+scale(population),data = df_ood_nvss_zipcode_level))

linear_model <- lm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip+ 
                     ACS_PCT_LT_HS_ZC + POS_DIST_ALC_ZP + 
                     naloxone_administered_per_zip_code +
                   ,data=df_ood_nvss_zipcode_level,weights = df_ood_nvss_zipcode_level$population)

linear_model_1 <- lm(deaths_per_capita ~ deaths_sci_proximity_zip+ deaths_spatial_proximity_zip +
                       ACS_PCT_HH_SMARTPHONE_ZC + ACS_PCT_UNEMPLOY_ZC + ACS_PCT_PERSON_INC_BELOW99_ZC + 
                       ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP + 
                       ACS_PCT_OTHER_INS_ZC, data = df_ood_nvss_zipcode_level, weights = df_ood_nvss_zipcode_level$population)
linear_model_2  <- 


#### Social ERROR REGRESSION
library(spatialreg)
library(spdep)
df_0<- read_tsv('C:/Users/kusha/Desktop/Data for Paper/SCI/zcta_zcta_shard1.tsv')
df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000))
nvss_zipcodes <- df_ood_nvss_zipcode_level$zipcode
df_1 <- df_0 %>% dplyr::filter(user_loc %in% nvss_zipcodes & fr_loc %in% nvss_zipcodes)
df_1 <- df_1 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000))
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

#### sem model##
linear_model <- lm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip 
                   + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                     + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                   + OPR, data = df_ood_nvss_zipcode_level, weights = df_ood_nvss_zipcode_level$population)
summary(linear_model)
stargazer::stargazer(linear_model,out='zipcode_linear_model.html')
moran.test(linear_model$residuals, listw = lw_4)

ZIPCODE_SPATIAL_1 <- errorsarlm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip+ ACS_PCT_HH_SMARTPHONE_ZC + ACS_PCT_UNEMPLOY_ZC + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP ,data=df_ood_nvss_zipcode_level,
                                listw = lw_3,
                                zero.policy = TRUE,
                                weights = df_ood_nvss_zipcode_level$population,
                                na.action = na.omit)
summary(ZIPCODE_SPATIAL_1)

#### zipcode_spatial_1.1 for the new selected covariates

ZIPCODE_SPATIAL_1.1 <- errorsarlm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip 
                                  + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                                    + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                                  + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                                  + OPR ,data=df_ood_nvss_zipcode_level,
                                  listw = lw_3,
                                  zero.policy = TRUE,
                                  weights = df_ood_nvss_zipcode_level$population,
                                  na.action = na.omit)
summary(ZIPCODE_SPATIAL_1.1)



ZIPCODE_SPATIAL_2 <- errorsarlm(deaths_per_capita ~ deaths_sci_proximity_zip + deaths_spatial_proximity_zip 
                                + ACS_PCT_UNEMPLOY_ZC  + ACS_PCT_LT_HS_ZC +
                                  + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC 
                                + POS_DIST_ALC_ZP + ACS_PCT_OTHER_INS_ZC 
                                + OPR ,data=df_ood_nvss_zipcode_level,
                                listw = lw_4,
                                zero.policy = TRUE,
                                weights = df_ood_nvss_zipcode_level$population,
                                na.action = na.omit)
summary(ZIPCODE_SPATIAL_2)

