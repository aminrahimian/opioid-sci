#### nvss data for allegheny zipcodes###
library(tidyverse)
library(zipcodeR)
zip_code_demographics <- search_state('PA')
opioid_deaths_2016_2017_nvss <- read.csv('Combined_Drug_2016_2017.csv')
opioid_deaths_2016_2017_nvss <- opioid_deaths_2016_2017_nvss %>% filter(County=="ALLEGHENY")
opioid_deaths_2016_2017_nvss <- opioid_deaths_2016_2017_nvss %>% filter(opioid_deaths_2016_2017_nvss$COD %in% c("X40","X41","X42","X43","X44"))
table(opioid_deaths_2016_2017_nvss$COD)
opioid_deaths_2016_2017_nvss <- opioid_deaths_2016_2017_nvss %>% filter(State=="PENNSYLVANIA")
data_ood_2016_2017_nvss <- subset(opioid_deaths_2016_2017_nvss,select = c('fileno','Zip','COD'))
unique <- table(unique(data_ood_2016_2017_nvss$zipcode))
colnames(data_ood_2016_2017_nvss)[2] <- "zipcode"
#complete_data_2016_2017 <- merge.data.frame(zip_code_demographics,data_ood_2016,by="zipcode")
data_ood_2016_2017_nvss_allegheny<- data.frame(table(data_ood_2016_2017_nvss$zipcode))
colnames(data_ood_2016_2017_nvss_allegheny)[1] <- "zipcode"
colnames(data_ood_2016_2017_nvss_allegheny)[2] <- "deaths"
data_ood_2016_2017_nvss_allegheny <- merge(data_ood_2016_2017_nvss_allegheny,zip_code_demographics,by="zipcode")
data_ood_2016_2017_nvss_allegheny <- data_ood_2016_2017_nvss_allegheny %>%  mutate(deaths_per_capita = deaths / population)
data_ood_2016_2017_nvss_allegheny<- data_ood_2016_2017_nvss_allegheny %>% drop_na()

#####deaths_social_proximity###
library(readr)
library(igraph)
library(tidyverse)
df_0 <- read_tsv ('zcta_zcta_shard1.tsv') 
## The zcta_zcta_shard1.tsv file contains ZIP-level SCIs 
## for ZIP=10001 to 19979, it is contained in 
## us-zip-code-us-zip-code-fb-social-connectedness-index-october-2021.zip
## file and is downloaded from  
## https://data.humdata.org/dataset/social-connectedness-index
df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000))
zip_code <- data_ood_2016_2017_nvss_allegheny$zipcode
df_1 <- df_0 %>% dplyr::filter(user_loc %in% zip_code & fr_loc %in% zip_code)
df_1 <- df_1 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000))
left_over_zip_codes <- zip_code[!(zip_code %in% df_1$user_loc)]
data_ood_2016_2017_nvss_allegheny <- data_ood_2016_2017_nvss_allegheny[ ! data_ood_2016_2017_nvss_allegheny$zipcode %in% left_over_zip_codes, ]
df_1 <- df_1 %>% distinct(probabilites,.keep_all = TRUE)
deaths_per_capita_allegheny_zipcodes_nvss_data <- data_ood_2016_2017_nvss_allegheny[,c(1,26)]
colnames(deaths_per_capita_allegheny_zipcodes_nvss_data)[1] <- "fr_loc"
df_s <- merge(df_1,deaths_per_capita_allegheny_zipcodes_nvss_data,by ="fr_loc")
df_s <- df_s %>% mutate(wt_sci=probabilites*deaths_per_capita)
df_s <- df_s %>% distinct(probabilites,.keep_all = TRUE)
df_s
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
sci_proximity_zip_allegheny_nvss <- rowSums(sci_proximity)
sci_proximity_zip_allegheny_nvss
data_ood_2016_2017_nvss_allegheny$deaths_social_proximity <- sci_proximity_zip_allegheny_nvss


#####deaths_physical_proximity###
library(geodist)
df <- data_ood_2016_2017_nvss_allegheny %>% dplyr::select(zipcode, lat, lng)
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
deaths_per_capita_nvss_allegheny_zipcodes <- data_ood_2016_2017_nvss_allegheny$deaths_per_capita
zip_physical_proximity_dij <- sweep(zip_distance_proximity,2,deaths_per_capita_nvss_allegheny_zipcodes,FUN="*")
zip_normalised_physical_porximity <- sweep(zip_physical_proximity_dij,2,normalised_scale,FUN="/")
deaths_physical_proximity_alleghney_nvss_zipcodes=rowSums(zip_normalised_physical_porximity)
data_ood_2016_2017_nvss_allegheny$deaths_spatial_proximity = deaths_physical_proximity_alleghney_nvss_zipcodes
###### Covariates ####
library(tidyverse)
library(tigris) # for fips_codes data
library(rvest) #for web scrapping
library(zipcodeR) #for ZIP codes
library(fuzzyjoin) #for string join
zip_code <- unique(data_ood_2016_2017_nvss_allegheny$zipcode)
##################### ODR administered per zip code#####
download_zip_data(force = FALSE)
data_zip <- zip_code_db
data_zip_for_ODR <-  data_ood_2016_2017_nvss_allegheny %>% filter(zipcode %in% zip_code)
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

data_ood_2016_2017_nvss_allegheny <- merge(data_ood_2016_2017_nvss_allegheny,ODR_2, by="zipcode")
data_ood_2016_2017_nvss_allegheny <- data_ood_2016_2017_nvss_allegheny[,-29]
colnames(data_ood_2016_2017_nvss_allegheny)[29] <- "ODR"
data_ood_2016_2017_nvss_allegheny<- data_ood_2016_2017_nvss_allegheny %>% mutate(ODR=scale(as.numeric(ODR)))
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
nzr$zipcode <- as.numeric(as.character(nzr$zipcode))
allegheny_zipcodes_nalaxone_adminsitered_nvss <- nzr %>% distinct(zipcode, .keep_all = TRUE)
data_ood_2016_2017_nvss_allegheny <- merge(data_ood_2016_2017_nvss_allegheny,allegheny_zipcodes_nalaxone_adminsitered_nvss,by="zipcode")
data_ood_2016_2017_nvss_allegheny<- data_ood_2016_2017_nvss_allegheny %>% mutate(naloxone_administered_per_zip_code= scale(naloxone_administered_per_zip_code))

########## ADI#####
#PA_ZIP <- read.delim('./GitHub/opioid-sci/PA_2015_ADI_9 Digit Zip Code_v3.1.txt', sep=',')
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
data_ood_2016_2017_nvss_allegheny<- merge(data_ood_2016_2017_nvss_allegheny,zip_wise_adi,by="zipcode")
write.csv(data_ood_2016_2017_nvss_allegheny, 'allegheny_data_from_nvss_zipcode_2016_2017.csv')
#### regression ###
nvss_regression_allegheny <- lm(deaths_per_capita ~ log(deaths_social_proximity)+log(deaths_spatial_proximity)+ODR+naloxone_administered_per_zip_code+adi_net_rank, weights = population, data=data_ood_2016_2017_nvss_allegheny)
summary(nvss_regression_allegheny)
### comparision ###
mean(data_ood_2016_2017_nvss_allegheny$deaths)
mean(fred_allegheny_ood$ood)

library(stargazer)
stargazer(nvss_regression_allegheny,fred_regression, out= "Comparision of Results between FRED and NVSS data.html" )

