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
ood_data_2013$deaths_per_capta[is.nan(ood_data_2013$deaths_per_capta)] <- 0
library(readr)
library(igraph)
library(tidyverse)
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
  return(s_values)
}
s_2013 <- calculate_s_values(ood_data_2013)
zip_codes_2013 <- row.names(cumulative_sci_weighted_test)
s_2013_df <- data.frame(zip_codes_2013,s_2013)
colnames(s_2013_df)[1] <- "Zip_new"

### social proximiity for 2014 ###########
ood_data_2014$deaths_per_capta[is.nan(ood_data_2014$deaths_per_capta)] <- 0
s_2014 <- calculate_s_values(ood_data_2014)
### social proximiity for 2015 ###########
ood_data_2015$deaths_per_capta[is.nan(ood_data_2015$deaths_per_capta)] <- 0
s_2015 <- calculate_s_values(ood_data_2015)
### social proximiity for 2016 ###########
ood_data_2016$deaths_per_capta[is.nan(ood_data_2016$deaths_per_capta)] <- 0
s_2016 <- calculate_s_values(ood_data_2016)
### social proximiity for 2017 ###########
ood_data_2017$deaths_per_capta[is.nan(ood_data_2017$deaths_per_capta)] <- 0
s_2017 <- calculate_s_values(ood_data_2017)
