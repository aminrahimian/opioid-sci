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
data_2016_2017 <- read.csv('Combined_Drug_2016_2017.csv')
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
ood_data_2013_2015 <- read.csv('Combined_Drug_1999_2015.csv')
library(lubridate)
ood_data_2013_2015$dod<- mdy(ood_data_2013_2015$dod)
ood_data_2013_2015$dod <- as.Date(ood_data_2013_2015$dod)
ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(between(dod,as.Date('2013-01-01'), as.Date('2015-01-01')))
ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(ood_data_2013_2015$COD %in% c("X40","X41","X42","X43","X44"))
##f_i dataframe to store zip code wise deaths from 2013-2015, n represents deaths)
zip_code_wise_deaths_2013_2015 <- count(ood_data_2013_2015,zip_new) 
colnames(zip_code_wise_deaths_2013_2015)[1] <- "zipcode"
###### missing_zip_codes data 2013-2015 and 2015-2017
missing_zip_codes_2013_2015 <- setdiff(zip_code_wise_deaths_2016_2017$zipcode,zip_code_wise_deaths_2013_2015$zip_new)
missing_zip_codes_2016_2017 <- setdiff(zip_code_wise_deaths_2013_2015$zip_new,zip_code_wise_deaths_2016_2017$zipcode)
ood_for_missing_zip_codes_2013_2015 <- zip_code_wise_deaths_2016_2017 %>% filter(zipcode %in% missing_zip_codes_2013_2015)
ood_for_missing_zip_codes_2016_2017 <- zip_code_wise_deaths_2013_2015 %>% filter(zip_new %in% missing_zip_codes_2016_2017)
colnames(ood_data_2013_2015)[1] <- "zipcode"
### aggregating 2013-2017 ood death ###
### (this is the wrong step)
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
df_0<- read_tsv('zcta_zcta_shard1.tsv')
## The zcta_zcta_shard1.tsv file contains ZIP-level SCIs 
## for ZIP=10001 to 19979, it is contained in 
## us-zip-code-us-zip-code-fb-social-connectedness-index-october-2021.zip
## file and is downloaded from  
## https://data.humdata.org/dataset/social-connectedness-index
df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000))
nvss_zipcodes <- df_ood_nvss_zipcode_level$zipcode
df_1 <- df_0 %>% dplyr::filter(user_loc %in% nvss_zipcodes & fr_loc %in% nvss_zipcodes)
df_1 <- df_1 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000))
left_over_zip_codes <- nvss_zipcodes[!(nvss_zipcodes %in% df_1$user_loc)]
df_ood_nvss_zipcode_level<- df_ood_nvss_zipcode_level[ ! df_ood_nvss_zipcode_level$zipcode %in% left_over_zip_codes, ]
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
#### q_i stores zipcode wise deathspercapita aggregated over temporal window from 2013-2017####
q_i <- df_ood_nvss_zipcode_level[,c(1,26)]
colnames(q_i)[1] <- "fr_loc"
df_s <- merge(df_1,q_i,by ="fr_loc")
df_s <- df_s %>% mutate(wt_sci=probabilites*deaths_per_capita)
dataframe_for_matrix <- df_s %>% dplyr::select(c(user_loc,fr_loc,wt_sci))
nodes <- df_s %>% distinct(user_loc)
g <- graph.data.frame(dataframe_for_matrix, directed=F, vertices=nodes)
sci_proximity <- as_adjacency_matrix(g,attr = "wt_sci",sparse = F)
diag(sci_proximity) <- 0
df_for_matrix_probability <- df_s %>% dplyr::select(c(user_loc,fr_loc,probabilites))
df_for_matrix_probability
k <- graph.data.frame(df_for_matrix_probability, directed=F, vertices=nodes)
cumulative_sci <- as_adjacency_matrix(k,attr="probabilites",sparse=F)
row_wise_sum_sci <- rowSums(cumulative_sci)
row_wise_sum_sci
sci_proximity <- sweep(sci_proximity,2,row_wise_sum_sci,FUN="/")
#sci_proximity <- mapply("/",sci_proximity,row_wise_sum_sci)
sci_proximity_zip_nvss_2013_2017 <- rowSums(sci_proximity)
sci_proximity_zip_nvss_2013_2017
df_ood_nvss_zipcode_level$deaths_sci_proximity_zip <- sci_proximity_zip_nvss_2013_2017
##### deaths spatial proximity##
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
normalised_scale <- colSums(d_i_j)
normalised_scale
zip_distance_proximity<- d_i_j
colnames(zip_distance_proximity) <- df$zipcode
rownames(zip_distance_proximity) <- df$zipcode
diag(zip_distance_proximity) <- 0
zip_distance_proximity
v <- df_ood_nvss_zipcode_level$deaths_per_capita
zip_physical_proximity_dij <- sweep(zip_distance_proximity,2,v,FUN="*")
zip_normalised_physical_porximity <- sweep(zip_physical_proximity_dij,2,normalised_scale,FUN="/")
deaths_physical_proximity_nvss_2013_2017=rowSums(zip_normalised_physical_porximity)
df_ood_nvss_zipcode_level$deaths_physical_proximity = deaths_physical_proximity_nvss_2013_2017
#### covariates#####
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
data_zip_for_ODR_1 <- data_zip_for_ODR %>% select(county,state)
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
ODR_2 <- ODR_1 %>% select("zipcode","Opioid Dispensing Rate Per 100")

df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,ODR_2, by="zipcode")
df_ood_nvss_zipcode_level<- df_ood_nvss_zipcode_level[,-29]
colnames(df_ood_nvss_zipcode_level)[29] <- "ODR"
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% mutate(ODR=scale(as.numeric(ODR)))
######### naloxone administered #####
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


data_naloxone <- data_naloxone %>% 
  filter(!str_detect(county, 'Cameron'))

data_naloxone <- data_naloxone %>% 
  filter(!str_detect(county, 'Sullivan'))

data_naloxone <- data_naloxone %>% 
  filter(!str_detect(county, 'Forest'))


naloxone_administer_per_zip_code_fips_wise <- data_naloxone[,c(1,4)]



colnames(ODR_2)[1] <- "county"
nzr <- stringdist_join(naloxone_administer_per_zip_code_fips_wise , ODR_2, 
                       by='county', #match based on team
                       mode='left', #use left join
                       method = "jw", #use jw distance metric
                       max_dist=99,distance_col='dist')%>%
  group_by(county.x) %>%
  slice_min(order_by=dist, n=1)

nzr <- nzr[,c(2,4)]

df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,nzr,by="zipcode")
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% mutate(naloxone_administered_per_zip_code= scale(naloxone_administered_per_zip_code))
########## ADI#####
PA_ZIP <- read.delim('./GitHub/opioid-sci/PA_2015_ADI_9 Digit Zip Code_v3.1.txt', sep=',')
## The 2015 ADI data set is downloaded from UW-Madison Neighborhood Atlas:
## https://www.neighborhoodatlas.medicine.wisc.edu/download
zips <- PA_ZIP %>% mutate(zip=str_extract(ZIPID,'\\d{5}'))
#zips <- zips[,-c(1,2,10)]
zips_adi_Staternk <- zips[,c(9,10)]
zips_adi_Staternk$zip <- as.integer(zips_adi_Staternk$zip)
zips_adi_Staternk <- zips_adi_Staternk %>% filter(zip %in% zip_code )
zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "PH")
zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "GQ")
zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "PH-GQ")
zips_adi_Staternk <- zips_adi_Staternk %>% filter(ADI_STATERNK != "GQ-PH")
zips_adi_Staternk$ADI_STATERNK <- as.integer(zips_adi_Staternk$ADI_STATERNK)
zips_adi_Staternk <- zips_adi_Staternk %>% drop_na()
zips_adi_Staternk$zips_factor <- as.factor(zips_adi_Staternk$zip)
zip_wise_adi <- aggregate(zips_adi_Staternk$ADI_STATERNK, list(zips_adi_Staternk$zip), FUN=mean)
zip_wise_adi <- zip_wise_adi %>% filter(Group.1 %in% zip_code)
colnames(zip_wise_adi)[1] <- "zip"
colnames(zip_wise_adi)[2] <- "adi_net_rank"
zip_wise_adi <- zip_wise_adi %>% mutate(adi_net_rank=scale(adi_net_rank))
colnames(zip_wise_adi)[1] <- "zipcode"
df_ood_nvss_zipcode_level <- merge(df_ood_nvss_zipcode_level,zip_wise_adi,by="zipcode")
write.csv(df_ood_nvss_zipcode_level,'aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
### regressions###
summary(m3<-lm(deaths_per_capita~deaths_per_capita+log(deaths_sci_proximity_zip)+log(deaths_physical_proximity)+ODR+naloxone_administered_per_zip_code+adi_net_rank, weights=population,data=df_ood_nvss_zipcode_level))
### library(pscl)for zero inflated regression
library(pscl)
summary(m4 <- zeroinfl(deaths ~  log(deaths_sci_proximity_zip) +log(deaths_physical_proximity) +ODR+naloxone_administered_per_zip_code+ adi_net_rank+scale(population), data = df_ood_nvss_zipcode_level))

### git commit message -m "nvss aggregated data 2013 2017 with padded zeroes at ZIP CODE level"
