#R version 4.2.1
## analyzes zip-level opioid death data in PA
library(tidyverse)
library(zipcodeR)
library(readr)
library(lubridate)
library(zipcodeR)
library(tidycensus)
library(igraph)
library(tidycensus)
zip_code_demographics <- search_state('PA')
zip_code_demographics<- zip_code_demographics[complete.cases(zip_code_demographics[, c("lat", "lng")]), ]
####### opioid deaths in 2013 2014 2015 ###
ood_data_2013_2015 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_1999_2015.csv')
### function for getting opiodi deaths###
prepare_ood_data <- function(file_path, start_date, end_date, cod_values, year) {
  ood_data <- read.csv(file_path)
  ood_data$dod <- mdy(ood_data$dod)
  ood_data$dod <- as.Date(ood_data$dod)
  
  ood_data_filtered <- ood_data %>%
    filter(between(dod, as.Date(start_date), as.Date(end_date))) %>% filter(state_res== "PENNSYLVANIA") %>% 
    filter(COD %in% cod_values) %>% filter(zip_new %in% zip_code_demographics$zipcode)
  
  ood_data_by_zip <- count(ood_data_filtered, zip_new)
  colnames(ood_data_by_zip)[1] <- "zip_new"
  colnames(ood_data_by_zip)[2] <- paste0("deaths_in_", year)
  
  missing_zipcodes <- setdiff(zip_code_demographics$zipcode, ood_data_by_zip$zip_new)
  n_missing_zipcodes <- length(missing_zipcodes)
  deaths_in_missing_zipcodes <- rep(0, n_missing_zipcodes)
  
  df_missing_zipcodes <- data.frame(zip_new = missing_zipcodes, deaths_in_zipcodes = deaths_in_missing_zipcodes)
  colnames(df_missing_zipcodes)[1] <- "zip_new"
  colnames(df_missing_zipcodes)[2] <- paste0("deaths_in_", year)
  
  final_ood_data <- rbind(ood_data_by_zip, df_missing_zipcodes)
  
  return(final_ood_data)
}

#### getting different ood data for different year 2014 2015 2016 2017#####
# Call the function
file_path <- "C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_1999_2015.csv"
start_date <- "2013-01-01"
end_date <- "2013-12-31"
cod_values <- c("X40", "X41", "X42", "X43", "X44")
year <- 2013
ood_data_2013 <- prepare_ood_data(file_path, start_date, end_date, cod_values, year)
ood_data_2013$year <- 2013
colnames(ood_data_2013)[2] <- "deaths"
#### getting deaths for 2014 ###########
start_date <- "2014-01-01"
end_date <- "2014-12-31"
year <- 2014
ood_data_2014 <- prepare_ood_data(file_path, start_date, end_date, cod_values, year)
ood_data_2014$year <- 2014
colnames(ood_data_2014)[2] <- "deaths"
#### getting deaths for 2015 #####
start_date <- "2015-01-01"
end_date <- "2015-12-31"
year <- 2015
ood_data_2015 <- prepare_ood_data(file_path, start_date, end_date, cod_values, year)
ood_data_2015$year <- 2015
colnames(ood_data_2015)[2] <- "deaths"
################ data for 2016 2017 ###
prepare_ood_data_2016_2017 <- function(file_path, start_date, end_date, cod_values, year) {
  ood_data <- read.csv(file_path)
  ood_data$dod <- mdy(ood_data$dod)
  ood_data$dod <- as.Date(ood_data$dod)
  
  ood_data_filtered <- ood_data %>%
    filter(between(dod, as.Date(start_date), as.Date(end_date))) %>%
    filter(COD %in% cod_values) %>% filter(Zip %in% zip_code_demographics$zipcode)
  
  ood_data_by_zip <- count(ood_data_filtered, Zip)
  colnames(ood_data_by_zip)[1] <- "zip_new"
  colnames(ood_data_by_zip)[2] <- paste0("deaths_in_", year)
  
  missing_zipcodes <- setdiff(zip_code_demographics$zipcode, ood_data_by_zip$zip_new)
  n_missing_zipcodes <- length(missing_zipcodes)
  deaths_in_missing_zipcodes <- rep(0, n_missing_zipcodes)
  
  df_missing_zipcodes <- data.frame(zip_new = missing_zipcodes, deaths_in_zipcodes = deaths_in_missing_zipcodes)
  colnames(df_missing_zipcodes)[1] <- "zip_new"
  colnames(df_missing_zipcodes)[2] <- paste0("deaths_in_", year)
  
  final_ood_data <- rbind(ood_data_by_zip, df_missing_zipcodes)
  
  return(final_ood_data)
}

############## ood data for 2016#####
file_path <- "C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_2016_2017.csv"
start_date <- "2016-01-01"
end_date <- "2016-12-31"
cod_values <- c("X40", "X41", "X42", "X43", "X44")
year <- 2016
ood_data_2016 <- prepare_ood_data_2016_2017(file_path, start_date, end_date, cod_values, year)
colnames(ood_data_2016)[2] <- "deaths"
ood_data_2016$year <- 2016
#### ood data for 2017 ###
start_date <- "2017-01-01"
end_date <- "2017-12-31"
cod_values <- c("X40", "X41", "X42", "X43", "X44")
year <- 2017
ood_data_2017 <- prepare_ood_data_2016_2017(file_path, start_date, end_date, cod_values, year)
colnames(ood_data_2017)[2] <- "deaths"
ood_data_2017$year <- 2017
### population data year wise###
pop_data_2013 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population estimate
  state = "PA", # Pennsylvania
  year = 2013,
  geometry = FALSE, # Set to TRUE if you want spatial data as well
  output = "wide"
)
pop_data_2013 <- pop_data_2013 %>%
  rename(zipcode = NAME, population = B01003_001E)
pop_data_2013 <- pop_data_2013 %>% filter(pop_data_2013$GEOID %in% ood_data_2013$zip_new)
pop_data_2013 <- arrange(pop_data_2013,GEOID)
colnames(pop_data_2013)[1] <- "zip_new"
ood_data_2013 <- merge(ood_data_2013, pop_data_2013, by="zip_new")
#### Getting POPULATION for 2014###
pop_data_2014 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population estimate
  state = "PA", # Pennsylvania
  year = 2014,
  geometry = FALSE, # Set to TRUE if you want spatial data as well
  output = "wide"
)
pop_data_2014 <- pop_data_2014 %>%
  rename(zipcode = NAME, population = B01003_001E)
pop_data_2014 <- pop_data_2014 %>% filter(pop_data_2014$GEOID %in% ood_data_2014$zip_new)
pop_data_2014 <- arrange(pop_data_2014,GEOID)
colnames(pop_data_2014)[1] <- "zip_new"
ood_data_2014 <- merge(ood_data_2014, pop_data_2014, by="zip_new")
#### Getting POPULATION for 2015###
pop_data_2015 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population estimate
  state = "PA", # Pennsylvania
  year = 2015,
  geometry = FALSE, # Set to TRUE if you want spatial data as well
  output = "wide"
)
pop_data_2015 <- pop_data_2015 %>%
  rename(zipcode = NAME, population = B01003_001E)
pop_data_2015 <- pop_data_2015 %>% filter(pop_data_2015$GEOID %in% ood_data_2015$zip_new)
pop_data_2015 <- arrange(pop_data_2015,GEOID)
colnames(pop_data_2015)[1] <- "zip_new"
ood_data_2015 <- merge(ood_data_2015, pop_data_2015, by="zip_new")
### Getting POPULATION for 2016###
pop_data_2016 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population estimate
  state = "PA", # Pennsylvania
  year = 2016,
  geometry = FALSE, # Set to TRUE if you want spatial data as well
  output = "wide"
)
pop_data_2016 <- pop_data_2016 %>%
  rename(zipcode = NAME, population = B01003_001E)
pop_data_2016 <- pop_data_2016 %>% filter(pop_data_2016$GEOID %in% ood_data_2016$zip_new)
pop_data_2016 <- arrange(pop_data_2016,GEOID)
colnames(pop_data_2016)[1] <- "zip_new"
ood_data_2016 <- merge(ood_data_2016, pop_data_2016, by="zip_new")
### Getting POPULATION for 2017###
pop_data_2017 <- get_acs(
  geography = "zcta",
  variables = "B01003_001", # Total population estimate
  state = "PA", # Pennsylvania
  year = 2017,
  geometry = FALSE, # Set to TRUE if you want spatial data as well
  output = "wide"
)
pop_data_2017 <- pop_data_2017 %>%
  rename(zipcode = NAME, population = B01003_001E)
pop_data_2017 <- pop_data_2017 %>% filter(pop_data_2017$GEOID %in% ood_data_2017$zip_new)
pop_data_2017 <- arrange(pop_data_2017,GEOID)
colnames(pop_data_2017)[1] <- "zip_new"
ood_data_2017 <- merge(ood_data_2017, pop_data_2017, by="zip_new")

##### deaths per capita #####
ood_data_2013$deaths_per_capta <- ood_data_2013$deaths/ood_data_2013$population
ood_data_2014$deaths_per_capta <- ood_data_2014$deaths/ood_data_2014$population
ood_data_2015$deaths_per_capta <- ood_data_2015$deaths/ood_data_2015$population
ood_data_2016$deaths_per_capta <- ood_data_2016$deaths/ood_data_2016$population
ood_data_2017$deaths_per_capta <- ood_data_2017$deaths/ood_data_2017$population

############# social proximity 2013#################
library(readr)
library(igraph)
library(tidyverse)
df_0<- read_tsv('C:/Users/kusha/Desktop/Data for Paper/SCI/zcta_zcta_shard1.tsv')
calculate_s_values <- function(ood_data) {
  zipcodes <- ood_data$zip_new
  df_1 <- df_0 %>% dplyr::filter(user_loc %in% zipcodes & fr_loc %in% zipcodes)
  left_over_zip_codes <- zipcodes[!(zipcodes %in% df_1$user_loc)]
  ood_data <- ood_data[ ! ood_data$zip_new %in% left_over_zip_codes, ]
  df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
  df_1$fr_loc <- as.numeric(df_1$fr_loc)
  nodes <- df_1 %>% distinct(fr_loc)
  df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
  df_for_matrix_weights
  k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
  cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
  cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
  diag(cumulative_sci_weighted) <- 0
  population <- ood_data$population
  for(i in 1:ncol(cumulative_sci_weighted)){
    cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
  }
  row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
  cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted

  v <- ood_data$deaths_per_capta

  for(i in 1:ncol(cumulative_sci_weighted_test)){
    cumulative_sci_weighted_test[,i] <- cumulative_sci_weighted_test[,i] * v[i]
  }

  s_values <- rowSums(cumulative_sci_weighted_test)
  
  # Add s_values as a new column in ood_data
  ood_data$s_values <- s_values
  
  # Return both the updated ood_data and the s_values
  return(list("ood_data" = ood_data, "s_values" = s_values))
  
}


### social proximiity for 2013 ###########
ood_data_2013$deaths_per_capta[is.nan(ood_data_2013$deaths_per_capta)] <- 0
result_2013 <- calculate_s_values(ood_data_2013)
ood_data_2013 <- result_2013$ood_data



### social proximiity for 2014 ###########
ood_data_2014$deaths_per_capta[is.nan(ood_data_2014$deaths_per_capta)] <- 0
result_2014 <- calculate_s_values(ood_data_2014)
ood_data_2014 <- result_2014$ood_data


### social proximiity for 2015 ###########
ood_data_2015$deaths_per_capta[is.nan(ood_data_2015$deaths_per_capta)] <- 0
result_2015 <- calculate_s_values(ood_data_2015)
ood_data_2015 <- result_2015$ood_data
### social proximiity for 2016 ###########
ood_data_2016$deaths_per_capta[is.nan(ood_data_2016$deaths_per_capta)] <- 0
result_2016 <- calculate_s_values(ood_data_2016)
ood_data_2016 <- result_2016$ood_data

### social proximiity for 2017 ###########
ood_data_2017$deaths_per_capta[is.nan(ood_data_2017$deaths_per_capta)] <- 0
result_2017 <- calculate_s_values(ood_data_2017)
ood_data_2017 <- result_2017$ood_data
##### spatial proximity ####
######################## ##### deaths spatial proximity############
######## function ######
calculate_d_minus_i <- function(ood_data, zip_code_demographics) {
  zip_codes_coordinates <- zip_code_demographics %>% dplyr::select(zipcode, lat, lng)
  colnames(zip_codes_coordinates)[1] <- "zip_new"
  ood_data <- merge(ood_data, zip_codes_coordinates, by = "zip_new")
  
  df <- ood_data %>% dplyr::select(zip_new, lat, lng)
  df <- df[order(df$zip_new), ]
  colnames(df)[2] <- "latitude"
  colnames(df)[3] <- "longitude"
  df <- df[, c(1, 3, 2)]
  
  distance_matrix <- geodist(df, measure = 'geodesic') / 1000 # converting it to km
  distance_matrix <- 1 + distance_matrix
  distance_matrix <- distance_matrix ** (-1)
  diag(distance_matrix) <- 0
  colnames(distance_matrix) <- df$zip_new
  rownames(distance_matrix) <- df$zip_new
  
  v <- ood_data$deaths_per_capta
  a_i_j <- data.frame(distance_matrix)
  diag(a_i_j) <- 0
  a_i_j <- as.matrix(a_i_j)
  
  # Normalize a_i_j
  normalised_scale <- rowSums(a_i_j)
  a_i_j <- a_i_j / normalised_scale
  
  # Ensure y is a numeric vector
  y <- as.numeric(ood_data$deaths_per_capta)
  
  # Perform matrix multiplication
  d_minus_i_values <- a_i_j %*% y

  
  ood_data$d_values <-   d_minus_i_values
  
  # Return both the updated ood_data and the s_values
  return(list("ood_data" = ood_data, "d_values" =   d_minus_i_values))
}
#### spatial proximity 2013######
d_values_test_2013 <- calculate_d_minus_i(ood_data_2013, zip_code_demographics)
ood_data_2013 <- d_values_test_2013$ood_data
####### spatial proximity 2014 #####
d_values_test_2014 <- calculate_d_minus_i(ood_data_2014, zip_code_demographics)
ood_data_2014 <- d_values_test_2014$ood_data
####### spatial proximity 2015 #####
d_values_test_2015 <- calculate_d_minus_i(ood_data_2015, zip_code_demographics)
ood_data_2015 <- d_values_test_2015$ood_data
####### spatial proximity 2016 #####
d_values_test_2016 <- calculate_d_minus_i(ood_data_2016, zip_code_demographics)
ood_data_2016 <- d_values_test_2016$ood_data
####### spatial proximity 2017 #####
d_values_test_2017 <- calculate_d_minus_i(ood_data_2017, zip_code_demographics)
ood_data_2017 <- d_values_test_2017$ood_data
####################### sdoh#########
## no smart phone data available till 2016 ###
####ACS_PCT_HH_SMARTPHONE_ZC + ACS_PCT_UNEMPLOY_ZC + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP
### sdoh 2013###
sdoh_2013 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Health Determinant ZIPCODE 2017-2018/Fixed Effect SDOH data set/sdoh_2013.csv")
sdoh_2013 <- sdoh_2013 %>% filter(ZIPCODE %in% ood_data_2013$zip_new)
sdoh_2013 <- sdoh_2013 %>% dplyr::select(c("ZIPCODE", "ACS_PCT_UNEMPLOY_ZC"
                                    , "ACS_PCT_PERSON_INC_BELOW99_ZC" , 
                                    "ACS_PCT_HU_NO_VEH_ZC" , "POS_DIST_ALC_ZP",
                                    "ACS_PCT_LT_HS_ZC","ACS_PCT_OTHER_INS_ZC"))


colnames(sdoh_2013)[1] <- "zip_new"

ood_data_2013 <- merge(ood_data_2013, sdoh_2013, by="zip_new")
## add details of zipcode and there respective county ###
df_ood_nvss_zipcode_level <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level[,-1]
colnames(df_ood_nvss_zipcode_level)[2] <- "zip_new"
ood_data_2013$zip_new <- as.numeric(ood_data_2013$zip_new)
zip_code_county_name <- df_ood_nvss_zipcode_level[,c(1,2)]
ood_data_2013 <- merge(ood_data_2013, zip_code_county_name, by = "zip_new")
ood_data_2013 <- ood_data_2013[,c(18,1:17)]

####### sdoh 2014 #######
sdoh_2014 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Health Determinant ZIPCODE 2017-2018/Fixed Effect SDOH data set/sdoh_2014.csv")
sdoh_2014 <- sdoh_2014 %>% filter(ZIPCODE %in% ood_data_2014$zip_new)
sdoh_2014 <- sdoh_2014 %>% dplyr::select(c("ZIPCODE", "ACS_PCT_UNEMPLOY_ZC"
                                    , "ACS_PCT_PERSON_INC_BELOW99_ZC" , "ACS_PCT_HU_NO_VEH_ZC" ,
                                    "POS_DIST_ALC_ZP","ACS_PCT_LT_HS_ZC","ACS_PCT_OTHER_INS_ZC"))


colnames(sdoh_2014)[1] <- "zip_new"

ood_data_2014 <- merge(ood_data_2014, sdoh_2014, by="zip_new")
ood_data_2014 <- merge(ood_data_2014, zip_code_county_name, by = "zip_new")
ood_data_2014 <- ood_data_2014[,c(18,1:17)]
###### sdoh 2015 #######3
sdoh_2015 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Health Determinant ZIPCODE 2017-2018/Fixed Effect SDOH data set/sdoh_2015.csv")
sdoh_2015 <- sdoh_2015 %>% filter(ZIPCODE %in% ood_data_2015$zip_new)
sdoh_2015 <- sdoh_2015 %>% dplyr::select(c("ZIPCODE", "ACS_PCT_UNEMPLOY_ZC"
                                    , "ACS_PCT_PERSON_INC_BELOW99_ZC" ,
                                    "ACS_PCT_HU_NO_VEH_ZC" , "POS_DIST_ALC_ZP","ACS_PCT_LT_HS_ZC",
                                    "ACS_PCT_OTHER_INS_ZC"))

colnames(sdoh_2015)[1] <- "zip_new"

ood_data_2015 <- merge(ood_data_2015, sdoh_2015, by="zip_new")
ood_data_2015 <- merge(ood_data_2015, zip_code_county_name, by = "zip_new")
ood_data_2015 <- ood_data_2015[,c(18,1:17)]

########## sdoh 2016###########3
sdoh_2016 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Health Determinant ZIPCODE 2017-2018/Fixed Effect SDOH data set/sdoh_2016.csv")
sdoh_2016 <- sdoh_2016 %>% filter(ZIPCODE %in% ood_data_2016$zip_new)
sdoh_2016 <- sdoh_2016 %>% dplyr::select(c("ZIPCODE", "ACS_PCT_UNEMPLOY_ZC"
                                    , "ACS_PCT_PERSON_INC_BELOW99_ZC" ,
                                    "ACS_PCT_HU_NO_VEH_ZC" , "POS_DIST_ALC_ZP",
                                    "ACS_PCT_LT_HS_ZC","ACS_PCT_OTHER_INS_ZC"))


colnames(sdoh_2016)[1] <- "zip_new"
ood_data_2016 <- merge(ood_data_2016, sdoh_2016, by="zip_new")
ood_data_2016 <- merge(ood_data_2016, zip_code_county_name, by = "zip_new")
ood_data_2016 <- ood_data_2016[,c(18,1:17)]

########## sdoh 2017########
sdoh_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Health Determinant ZIPCODE 2017-2018/Fixed Effect SDOH data set/sdoh_2017.csv")
sdoh_2017 <- sdoh_2017 %>% filter(ZIPCODE %in% ood_data_2017$zip_new)
sdoh_2017 <- sdoh_2017 %>% dplyr::select(c("ZIPCODE", "ACS_PCT_UNEMPLOY_ZC"
                                    , "ACS_PCT_PERSON_INC_BELOW99_ZC" , 
                                    "ACS_PCT_HU_NO_VEH_ZC" , "POS_DIST_ALC_ZP",
                                    "ACS_PCT_LT_HS_ZC","ACS_PCT_OTHER_INS_ZC"))


colnames(sdoh_2017)[1] <- "zip_new"
ood_data_2017 <- merge(ood_data_2017, sdoh_2017, by="zip_new")
ood_data_2017 <- merge(ood_data_2017, zip_code_county_name, by = "zip_new")
ood_data_2017 <- ood_data_2017[,c(18,1:17)]
############### opioid dispensing rate #################33

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
county_wise_total_rx_2013_total_dose <- county_wise_total_rx_2013[,c(2,3,4)]
ood_data_2013 <- merge(ood_data_2013,county_wise_total_rx_2013_total_dose, by="county")
ood_data_2013 <- ood_data_2013 %>% group_by(county) %>% mutate(population_proprtion = population/sum(population))
ood_data_2013 <- ood_data_2013 %>% mutate(total_dose_population_proprtion= total_dose*population_proprtion)
ood_data_2013 <- ood_data_2013 %>% ungroup(county) %>%  mutate(total_dose_population_proprtion_per_capita=total_dose_population_proprtion/population)

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
county_wise_total_rx_2014_total_dose <- county_wise_total_rx_2014[,c(2,3,4)]
ood_data_2014 <- merge(ood_data_2014,county_wise_total_rx_2014_total_dose, by="county")
ood_data_2014 <- ood_data_2014 %>% group_by(county) %>% mutate(population_proprtion = population/sum(population))
ood_data_2014 <- ood_data_2014 %>% mutate(total_dose_population_proprtion= total_dose*population_proprtion)
ood_data_2014 <- ood_data_2014 %>% ungroup(county) %>%  mutate(total_dose_population_proprtion_per_capita=total_dose_population_proprtion/population)

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
county_wise_total_rx_2015_total_dose <- county_wise_total_rx_2015[,c(2,3,4)]
ood_data_2015 <- merge(ood_data_2015,county_wise_total_rx_2015_total_dose, by="county")
ood_data_2015 <- ood_data_2015 %>% group_by(county) %>% mutate(population_proprtion = population/sum(population))
ood_data_2015 <- ood_data_2015 %>% mutate(total_dose_population_proprtion= total_dose*population_proprtion)
ood_data_2015 <- ood_data_2015 %>% ungroup(county) %>%  mutate(total_dose_population_proprtion_per_capita=total_dose_population_proprtion/population)

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
county_wise_total_rx_2016_total_dose <- county_wise_total_rx_2016[,c(2,3,4)]
ood_data_2016 <- merge(ood_data_2016,county_wise_total_rx_2016_total_dose, by="county")
ood_data_2016 <- ood_data_2016 %>% group_by(county) %>% mutate(population_proprtion = population/sum(population))
ood_data_2016 <- ood_data_2016 %>% mutate(total_dose_population_proprtion= total_dose*population_proprtion)
ood_data_2016 <- ood_data_2016 %>% ungroup(county) %>%  mutate(total_dose_population_proprtion_per_capita=total_dose_population_proprtion/population)
#### opioids for total_rx and total_naloxone 2017###
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

county_wise_total_rx_2017_total_dose <- county_wise_total_rx_2017[,c(2,3,4)]
ood_data_2017 <- merge(ood_data_2017,county_wise_total_rx_2017_total_dose, by="county")
ood_data_2017 <- ood_data_2017 %>% group_by(county) %>% mutate(population_proprtion = population/sum(population))
ood_data_2017 <- ood_data_2017 %>% mutate(total_dose_population_proprtion= total_dose*population_proprtion)
ood_data_2017 <- ood_data_2017 %>% ungroup(county) %>% 
  mutate(total_dose_population_proprtion_per_capita=total_dose_population_proprtion/population)








#### opioid prescription rate ####
# https://data.cms.gov/summary-statistics-on-use-and-payments/medicare-medicaid-opioid-prescribing-rates/medicare-part-d-opioid-prescribing-rates-by-geography
### for year 2013 ####
opr <- read.csv('C:/Users/kusha/Desktop/Data for Paper/OPR/OPR.csv')
opr_2013 <- opr %>% filter(Year==2013) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2013$zip_new)
opr_2013 <- opr_2013 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2013)[1] <- "zip_new"
opr_2013 <- opr_2013 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
ood_data_2013 <- merge(ood_data_2013, opr_2013, by="zip_new", all.x = TRUE)

###### for year 2014 ####
opr_2014 <- opr %>% filter(Year==2014) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2014$zip_new)
opr_2014 <- opr_2014 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2014)[1] <- "zip_new"
opr_2014 <- opr_2014 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
ood_data_2014 <- merge(ood_data_2014, opr_2014, by="zip_new", all.x = TRUE)
#### for year 2015 ####
opr_2015 <- opr %>% filter(Year==2015) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2015$zip_new)
opr_2015 <- opr_2015 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2015)[1] <- "zip_new"
opr_2015 <- opr_2015 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
ood_data_2015 <- merge(ood_data_2015, opr_2015, by="zip_new", all.x = TRUE)


#### for year 2016 ####
opr_2016 <- opr %>% filter(Year==2016) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2016$zip_new)
opr_2016 <- opr_2016 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2016)[1] <- "zip_new"
opr_2016 <- opr_2016 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
ood_data_2016 <- merge(ood_data_2016, opr_2016, by="zip_new", all.x = TRUE)


#### for year 2017 ####
opr_2017 <- opr %>% filter(Year==2017) %>%  filter(Prscrbr_Geo_Lvl== "ZIP") %>%  filter(Prscrbr_Geo_Cd %in% ood_data_2017$zip_new)
opr_2017 <- opr_2017 %>% select(Prscrbr_Geo_Cd, Opioid_Prscrbng_Rate )
colnames(opr_2017)[1] <- "zip_new"
opr_2017 <- opr_2017 %>% group_by(zip_new) %>% summarise(mean_OPR = mean(Opioid_Prscrbng_Rate ))
ood_data_2017 <- merge(ood_data_2017, opr_2017, by="zip_new", all.x = TRUE)


########## fixed effect data ###
fixed_effect_data <- rbind(ood_data_2013,ood_data_2014,ood_data_2015,ood_data_2016,ood_data_2017)
fixed_effect_data <- replace(fixed_effect_data, is.na(fixed_effect_data), 0)


return_data <- df_ood_nvss_zipcode_level[,c()]

library(PerformanceAnalytics)
chart.Correlation(df_ood_nvss_zipcode_level[] )




library(lfe)
# Assuming your data is in a dataframe called 'data'
model_felm <- felm(deaths_per_capta ~ s_values +  ACS_PCT_UNEMPLOY_ZC + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP +mean_OPR | zip_new + year, data = fixed_effect_data_1)
summary(model_felm)
library(fixest)
MODEL <- feols(deaths_per_capta ~ s_values + d_values + ACS_PCT_UNEMPLOY_ZC + ACS_PCT_PERSON_INC_BELOW99_ZC + ACS_PCT_HU_NO_VEH_ZC + POS_DIST_ALC_ZP +ACS_PCT_LT_HS_ZC+ACS_PCT_OTHER_INS_ZC+mean_OPR | zip_new + year, data = fixed_effect_data)
summary(MODEL)
stargazer(MODEL)
fixef(MODEL)
library(stargazer)

# Assuming your model object is named MODEL
stargazer(MODEL, type = "text", title = "Fixed Effects Model", digits = 4,
          covariate.labels = c("s_values", "d_values", "ACS_PCT_UNEMPLOY_ZC", "ACS_PCT_PERSON_INC_BELOW99_ZC",
                               "ACS_PCT_HU_NO_VEH_ZC", "POS_DIST_ALC_ZP", "ACS_PCT_LT_HS_ZC", "ACS_PCT_OTHER_INS_ZC", "mean_OPR"),
          omit.stat = c("f", "ser"), align = TRUE)
library(texreg)

# Assuming your model object is named MODEL
texreg(MODEL, custom.model.names = "Fixed Effects Model",
          custom.coef.names = c("s_values", "d_values", "ACS_PCT_UNEMPLOY_ZC", "ACS_PCT_PERSON_INC_BELOW99_ZC",
                                "ACS_PCT_HU_NO_VEH_ZC", "POS_DIST_ALC_ZP", "ACS_PCT_LT_HS_ZC", "ACS_PCT_OTHER_INS_ZC", "mean_OPR"),
          digits = 4)
texreg(, custom.model.names = "Fixed Effects Model",
       custom.coef.names = c("s_values", "d_values", "ACS_PCT_UNEMPLOY_ZC", "ACS_PCT_PERSON_INC_BELOW99_ZC",
                             "ACS_PCT_HU_NO_VEH_ZC", "POS_DIST_ALC_ZP", "ACS_PCT_LT_HS_ZC", "ACS_PCT_OTHER_INS_ZC", "mean_OPR"),
       digits = 4)