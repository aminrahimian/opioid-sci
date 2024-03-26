##### This R script consists of multiple linear regression and Wtd Multiple Linear Regression##
### R Version 4.1.1###
#### Covariates used ODR, Naloxone, HH income per capita, Illicit drug seizures and are scaled##
#### Results illustrates statistically significant effect size for socail proximity in MlR##
#### Have to explore and add factors for SDOH (Social Determinants of Health)
library(tidyverse)
library(igraph)
library(zipcodeR)
library(tidyverse)
library(tigris) # for fips_codes data
library(rvest) #for web scrapping
library(zipcodeR) #for ZIP codes
library(fuzzyjoin)
library(tidycensus)
library(geodist)
library(haven)
library(scales)
#### getting demographic properties for counties using Tidycensus package###
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

pa_race <- get_acs(
  geography = "county",
  state = "PA",
  variables = race_vars,
  summary_var = "B03002_001",
  year = 2017
) 

pa_race_percent <- pa_race %>% mutate(percent = 100 * (estimate / summary_est)) 
pa_race_percent <- pa_race_percent %>% dplyr::select(NAME, variable, percent)
pa_race_percent <- pa_race_percent %>% mutate(NAME=str_replace_all(pa_race_percent$NAME,"County, Pennsylvania",""))


asian <- pa_race_percent %>% dplyr::filter(variable=="Asian")
asian <- asian %>% mutate(perecent=percent/100)
asian <- asian[,-3]

black <- pa_race_percent %>% dplyr::filter(variable=="Black")
black <- black %>% mutate(perecent=percent/100)
black <- black[,-3]

hipi <- pa_race_percent %>% dplyr::filter(variable=="HIPI")
hipi <- hipi %>% mutate(perecent=percent/100)
hipi <- hipi[,-3]

hispanic <- pa_race_percent %>% dplyr::filter(variable=="Hispanic")
hispanic <- hispanic %>%  mutate(perecent=percent/100)
hispanic <- hispanic[,-3]

native <-pa_race_percent %>% dplyr::filter(variable=="Native")
native <- native %>% mutate(perecent=percent/100)
native <- native[,-3]

white <- pa_race_percent %>% dplyr::filter(variable=="White")
white <- white %>% mutate(perecent=percent/100)
white <- white[,-3]

pa_hh_income <- get_acs(
  geography = "county",
  table = "B19001",
  state = "PA",
  year = 2017
)

pa_hh_income <- pa_hh_income %>% dplyr::filter(pa_hh_income$variable == "B19001_001")
colnames(pa_hh_income)[3] <- "hh_income"
pa_hh_income <- pa_hh_income %>% mutate(NAME=str_replace_all(pa_hh_income$NAME,"County, Pennsylvania",""))
pa_hh_income <- pa_hh_income %>% dplyr::select(c(GEOID,NAME,estimate))

Soc.2017 <- get_acs(geography = "county", year=2017, variables = (c(pop="B01003_001")),state="PA", survey="acs5") %>% mutate(Year = "2017")
Soc.2017 <- Soc.2017 %>% mutate(NAME=str_replace_all(Soc.2017$NAME,"County, Pennsylvania",""))
Soc.2017 <- Soc.2017[,-c(3,5,6)]
colnames(Soc.2017)[3] <- "population"
Soc.2017
census_data_frame <- merge(Soc.2017,pa_hh_income,by="NAME")


#### nvss 2013-2015 data###
ood_data_2013_2015 <-  read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_1999_2015.csv')
library(lubridate)
ood_data_2013_2015$dod<- mdy(ood_data_2013_2015$dod)
ood_data_2013_2015$dod <- as.Date(ood_data_2013_2015$dod)
ood_data_2013_2015 <- ood_data_2013_2015 %>% 
  filter(dod >= as.Date('2013-01-01') & dod <= as.Date('2015-01-01'))

ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(ood_data_2013_2015$COD %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                                                                                  "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))
deaths_in_county_in_2013_2015 <- count(ood_data_2013_2015,county_name)  

pa_counties <- c("ADAMS", "ALLEGHENY", "ARMSTRONG", "BEAVER", "BEDFORD", "BERKS", "BLAIR", 
                 "BRADFORD", "BUCKS", "BUTLER", "CAMBRIA", "CAMERON", "CARBON", "CENTRE", 
                 "CHESTER", "CLARION", "CLEARFIELD", "CLINTON", "COLUMBIA", "CRAWFORD", 
                 "CUMBERLAND", "DAUPHIN", "DELAWARE", "ELK", "ERIE", "FAYETTE", "FOREST", 
                 "FRANKLIN", "FULTON", "GREENE", "HUNTINGDON", "INDIANA", "JEFFERSON", 
                 "JUNIATA", "LACKAWANNA", "LANCASTER", "LAWRENCE", "LEBANON", "LEHIGH", 
                 "LUZERNE", "LYCOMING", "MCKEAN", "MERCER", "MIFFLIN", "MONROE", "MONTGOMERY", 
                 "MONTOUR", "NORTHAMPTON", "NORTHUMBERLAND", "PERRY", "PHILADELPHIA", 
                 "PIKE", "POTTER", "SCHUYLKILL", "SNYDER", "SOMERSET", "SULLIVAN", "SUSQUEHANNA", 
                 "TIOGA", "UNION", "VENANGO", "WARREN", "WASHINGTON", "WAYNE", "WESTMORELAND", 
                 "WYOMING", "YORK")

deaths_in_county_in_2013_2015 <- deaths_in_county_in_2013_2015 %>% filter(deaths_in_county_in_2013_2015$county_name %in% pa_counties)
missing_information_about_counties_2013_2015 <- setdiff(pa_counties,deaths_in_county_in_2013_2015$county_name)
deaths_in_county_in_2013_2015[66,1] <- "CAMERON"
deaths_in_county_in_2013_2015[66,2] <- 0
deaths_in_county_in_2013_2015[67,1] <- "SULLIVAN"
deaths_in_county_in_2013_2015[67,2] <- 0

deaths_in_county_in_2013_2015 <- deaths_in_county_in_2013_2015 %>% arrange(deaths_in_county_in_2013_2015$county_name)
colnames(deaths_in_county_in_2013_2015)[1] <- "County"

data_2016_2017 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_2016_2017.csv')
## This is a private data set of opioid-related deaths in PA that include location and cause 
table(data_2016_2017$COD)
opioid_death_2016_2017 <- data_2016_2017 %>% filter(data_2016_2017$COD %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                                                                              "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))
table(opioid_death_2016_2017$COD)
data_ood_2016 <- subset(opioid_death_2016_2017,select = c('fileno','Zip','COD','County', 'State'))
#data_ood_2016 <- data_ood_2016 %>% filter(State=="PENNSYLVANIA")
n_ood <- count(data_ood_2016,data_ood_2016$County)
colnames(n_ood)[1] <-'County'
n_ood$County <- as.character(n_ood$County)
colnames(n_ood)[1] <- "County"
### using census dataframe to gather Geoid and using incident data to gather County Name##
### conversion of string characters did not work tried it with UTF-8 TO ASCII//TRANSLIT##
opioid_incident_data_pa_gov <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Incident Data/data.csv")
county_name <- unique(opioid_incident_data_pa_gov$Incident.County.Name)
county_name <- toupper(county_name)
deaths_in_county_in_2016_2017 <- n_ood %>% dplyr::filter(County %in% county_name)
deaths_in_county_in_2016_2017 [66,1] <- "CAMERON"
deaths_in_county_in_2016_2017 [66,2] <- 0
deaths_in_county_in_2016_2017 [67,1] <- "FOREST"
deaths_in_county_in_2016_2017 [67,2] <- 0
deaths_in_county_in_2016_2017  <- deaths_in_county_in_2016_2017[order(deaths_in_county_in_2016_2017$County),]
deaths_in_county_in_2016_2017 <- deaths_in_county_in_2016_2017 %>% arrange(deaths_in_county_in_2016_2017$County)
aggregated_ood_deaths_2013_2017 <- left_join(deaths_in_county_in_2013_2015,deaths_in_county_in_2016_2017, by="County")
aggregated_ood_deaths_2013_2017 <- aggregated_ood_deaths_2013_2017 %>% mutate(n=n.x+n.y)
aggregated_ood_deaths_2013_2017 <- aggregated_ood_deaths_2013_2017[,-c(2,3)]
county_wise_ood_with_demogrpahic_information <- cbind(aggregated_ood_deaths_2013_2017,Soc.2017)
county_wise_ood_with_demogrpahic_information <- county_wise_ood_with_demogrpahic_information[,-4]
county_wise_ood_with_demogrpahic_information <- county_wise_ood_with_demogrpahic_information %>% mutate(ood_percapita <- n/population)
colnames(county_wise_ood_with_demogrpahic_information)[1] <- "NAME"
colnames(county_wise_ood_with_demogrpahic_information)[5] <- "deaths_per_capita"
spatial_geoid_name_for_ood_counties <- county_wise_ood_with_demogrpahic_information %>% dplyr::select(NAME,GEOID,deaths_per_capita)
aggregated_ood_per_capita_events_in_each_county <- county_wise_ood_with_demogrpahic_information %>% dplyr::select(GEOID,deaths_per_capita)
aggregated_ood_per_capita_events_in_each_county
colnames(aggregated_ood_per_capita_events_in_each_county)[1] <- "fr_loc"
colnames(aggregated_ood_per_capita_events_in_each_county)[2] <- "deaths_per_capita"

#### sci_proximity####
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
df_1 <- df_0 %>% dplyr::filter(user_loc %in% aggregated_ood_per_capita_events_in_each_county$fr_loc & fr_loc %in% aggregated_ood_per_capita_events_in_each_county$fr_loc)
df_1$user_loc <- as.numeric(as.character(df_1$user_loc))
df_1$fr_loc <- as.numeric(as.character(df_1$fr_loc))
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc)))) ##unique pairs + self loops
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
nodes <- df_1 %>% distinct(fr_loc)
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- census_data_frame$population
### numerator of w_i_j: population_i*cumulative_sci_weighted_i
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
v <- aggregated_ood_per_capita_events_in_each_county$deaths_per_capita
for(i in 1:ncol(cumulative_sci_weighted_test)){
  cumulative_sci_weighted_test[,i] <- cumulative_sci_weighted_test[,i] * v[i]
}
sci_proximity_county_2013_2017 <- rowSums(cumulative_sci_weighted_test)

#### physical proximity ###
### reading nalxone file from data pa gov to get the spatial centroids for each county
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
df <-  data_naloxone %>% distinct(data_naloxone$County.Name,data_naloxone$County.Latitude.Point,data_naloxone$County.Longitude.Point)
colnames(df)[1] <- "county"
df <- df[order(df$county),]
colnames(df)[2] <- "Latitude"
colnames(df)[3] <- "Longitude"
df <- df[,c(1,3,2)]
df$Longitude <- as.numeric(df$Longitude)
df$Latitude <-as.numeric(df$Latitude)
distance_matrix <- geodist(df, measure = 'geodesic') / 1000 # converting it to km
distance_matrix <- 1 + distance_matrix
distance_matrix <- distance_matrix**(-1)
diag(distance_matrix) <- 0
colnames(distance_matrix) <- df$county
rownames(distance_matrix) <- df$county
y <- aggregated_ood_per_capita_events_in_each_county$deaths_per_capita
a_i_j <- data.frame(distance_matrix)
diag(a_i_j) <- 0
a_i_j <- as.matrix(a_i_j)

# Normalize a_i_j
normalised_scale <- rowSums(a_i_j)
a_i_j <- a_i_j / normalised_scale

# Perform matrix multiplication
d_minus_i <- a_i_j %*% y
####### naloxone available###
# naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
# data_naloxone <- naloxone
# data_naloxone <- data_naloxone %>% dplyr::select(County.Name,Cumulative.Kits.Provided)
# data_naloxone <- data_naloxone[order(data_naloxone$County.Name),]
# colnames(data_naloxone)[1] <- "county"
# data_naloxone$Cumulative.Kits.Provided <- as.numeric(gsub(",","",data_naloxone$Cumulative.Kits.Provided))
# data_naloxone <- data_naloxone %>% mutate(county= gsub("(.*),.*", "\\1", data_naloxone$county))
# data_naloxone$naloxone_per_capita <- scale(data_naloxone$Cumulative.Kits.Provided/Soc.2017$population)
naloxone_2013 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2013.sas7bdat")
naloxone_2014 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2014.sas7bdat")
naloxone_2015 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2015.sas7bdat")
naloxone_2016 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2016.sas7bdat")
naloxone_2017 <- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/naloxone/naloxone_county_2017.sas7bdat")
total_naloxone <- rbind(naloxone_2013, naloxone_2014, naloxone_2015, naloxone_2016, naloxone_2017)
total_naloxone <- total_naloxone %>% filter(st_code=="PA") %>%  group_by(county_nm,state_and_county_fip) %>% 
  summarise(avg_total_rx = mean(total_rx, na.rm = TRUE))
total_naloxone$state_and_county_fip <- as.numeric(total_naloxone$state_and_county_fip)
df_ood_nvss_zipcode_level <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
county <- unique(df_ood_nvss_zipcode_level$county)
new_row <- data.frame(county_nm = "Cameron"  , state_and_county_fip = 42023, avg_total_rx = 0)
total_naloxone <- rbind(total_naloxone,new_row)
total_naloxone <- total_naloxone %>% arrange(state_and_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
total_naloxone <- cbind(total_naloxone,county_Wise_zip_codes)
total_naloxone <- total_naloxone[,-c(4,5)]
total_naloxone$avg_total_rx <- rescale(total_naloxone$avg_total_rx,to=c(0,1))
#### odr###
library(haven)
opioids_2013<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2013.sas7bdat")
opioids_2013_PA <- opioids_2013 %>% filter(st_code=="PA")
county_wise_total_rx_2013 <- opioids_2013_PA %>% group_by(county_nm, state_fip_county_fip) %>% summarise( total_rx=sum(total_rx), total_dose=sum(total_dose))
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/nvss_ood_county_wise_2013_2017.csv")
county_wise_total_rx_2013$state_fip_county_fip <- as.numeric(county_wise_total_rx_2013$state_fip_county_fip)
setdiff(nvss_ood_county_wise_2013_2017$GEOID,county_wise_total_rx_2013$state_fip_county_fip)
### opioid back tracing for fulton ###
opioids_2014<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2014.sas7bdat")
opioids_2014_PA <- opioids_2014 %>% filter(st_code=="PA")
opioids_2014_PA_fulton <- opioids_2014_PA %>% filter(county_nm== "FULTON")
opioids_2015<- read_sas("C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county_2015.sas7bdat")
opioids_2015_PA <- opioids_2015 %>% filter(st_code=="PA")
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
county_wise_total_rx_2014$state_fip_county_fip <- as.numeric(county_wise_total_rx_2014$state_fip_county_fip)
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
county_wise_total_rx_2015$state_fip_county_fip <- as.numeric(county_wise_total_rx_2015$state_fip_county_fip)
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
county_wise_total_rx_2016$state_fip_county_fip <- as.numeric(county_wise_total_rx_2016$state_fip_county_fip)
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
county_wise_total_rx_2017$state_fip_county_fip <- as.numeric(county_wise_total_rx_2017$state_fip_county_fip)
county_Wise_zip_codes <- df_ood_nvss_zipcode_level %>% group_by(county) %>%  count(county)
county_wise_total_rx_2017 <- cbind(county_wise_total_rx_2017,county_Wise_zip_codes)
county_wise_total_rx_2017 <- county_wise_total_rx_2017[,-1]
county_wise_total_rx_2017 <- county_wise_total_rx_2017 %>% mutate(total_rx_per_zip_codes <- total_rx/n)
colnames(county_wise_total_rx_2017)[6] <- "total_rx_per_zip_codes"
###### opioid cumulative data frame######
county_total_rx <- rbind(county_wise_total_rx_2013, county_wise_total_rx_2014, county_wise_total_rx_2015, county_wise_total_rx_2016, county_wise_total_rx_2017)
county_total_rx <- county_total_rx %>% group_by(county, state_fip_county_fip) %>% summarise( cumulative_total_dose=sum(total_dose))
county_total_rx$cumulative_total_dose <- rescale(county_total_rx$cumulative_total_dose,to=c(0,1))
### here county total rx explains the cumulative dose from 2013-2017 administered in each county#############
#### health determinant covariates###
health_determinant <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Health Determinant County 2017-2018/SDOH_2017_COUNTY_AHRQ_covariate_Selection.csv')
health_determinant <- health_determinant %>% filter(STATE=="Pennsylvania")
selected_variables <- c("COUNTYFIPS", "ACS_PCT_HU_NO_VEH","POS_MEAN_DIST_ALC","ACS_PCT_OTHER_INS",
"ACS_PCT_LT_HS","AHRF_TOT_COM_HEALTH_GRANT","ACS_MEDIAN_HH_INC","CCBP_BWLSTORES_RATE","AMFAR_MHFAC_RATE")
health_determinant_covariates <- health_determinant %>% dplyr::select(selected_variables)
health_determinant_covariates <- health_determinant_covariates %>% replace(is.na(.), 0)
health_determinant_covariates <- health_determinant_covariates %>%
  mutate(across(selected_variables, ~rescale(.x, to = c(0, 1))))
##### creating the data frame ###
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(11:16)]
nvss_ood_county_wise_2013_2017 <-Soc.2017
nvss_ood_county_wise_2013_2017 <- cbind(Soc.2017,county_wise_ood_with_demogrpahic_information)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(4,6,7)]
colnames(nvss_ood_county_wise_2013_2017)[4] <- "deaths"
### data frame for social spatial proximity ###
proximity_df <- data.frame(d_minus_i,sci_proximity_county_2013_2017)
#### data frame for clinical covariates####
clinical_df <- data.frame(county_total_rx$cumulative_total_dose,total_naloxone$avg_total_rx)
#### column binding the data frames ###
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,proximity_df)
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,clinical_df)
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,health_determinant_covariates)
#### changing column names for spatial and social proximity ###
colnames(nvss_ood_county_wise_2013_2017)[6] <- "deaths_spatial_porximity"
colnames(nvss_ood_county_wise_2013_2017)[7] <- "deaths_social_proximity"
colnames(nvss_ood_county_wise_2013_2017)[8] <- "ODR"
colnames(nvss_ood_county_wise_2013_2017)[9] <- "Naloxone_Available"
####  beupromorphine ###
beupromorphine_2015 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2015.sas7bdat")
beupromorphine_2016 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2016.sas7bdat")
beupromorphine_2017 <- read_sas("C:/Users/kusha/Desktop/Data for Paper/buprenorphine_OUD_treatment/buprenorphine_county_2017.sas7bdat")
beupromorphine_2017 <- beupromorphine_2017[,-2]
total_beupromorphine <- rbind(beupromorphine_2015,beupromorphine_2016,beupromorphine_2017)
total_beupromorphine <- total_beupromorphine %>% filter(state_fip_county_fip %in% 
                                                          nvss_ood_county_wise_2013_2017$GEOID )
total_beupromorphine  <- total_beupromorphine  %>% group_by(state_fip_county_fip) %>% 
  summarise( cumulative_total_beupromorphine=mean(total_rx))
zero_fips_beupromorphine <- setdiff(nvss_ood_county_wise_2013_2017$GEOID,
                                    total_beupromorphine$state_fip_county_fip)
zero_fips_beupromorphine_df <- data.frame(state_fip_county_fip="42053", cumulative_total_beupromorphine=0)
total_beupromorphine <- rbind(total_beupromorphine,zero_fips_beupromorphine_df)
total_beupromorphine <- total_beupromorphine[order(total_beupromorphine$state_fip_county_fip),]
colnames(total_beupromorphine)[1] <- "GEOID"

nvss_ood_county_wise_2013_2017 <- left_join(nvss_ood_county_wise_2013_2017, 
                                            total_beupromorphine, by="GEOID")

#### fentanyl hard coded from the NFLS FILE ###
fentanyl_2013_PA<- 86
fentanyl_2014_PA <- 3+1+1+435
fentanyl_2015_PA <- 193+10+1096+1
fentanyl_2016_PA <-129+53+1+96+3+11+9+12+2895+269+4+12+13
fentanyl_2017_PA <- 122+261+247+60+18+35+110+164+9+101+8206+36+421+76+6+9+1+7+2
St_count_illicit_opioid_reported <- fentanyl_2013_PA+fentanyl_2014_PA+
  fentanyl_2015_PA+fentanyl_2016_PA+fentanyl_2017_PA
total_population_PA <- sum(nvss_ood_county_wise_2013_2017$population)
St_count_illicit_opioid_reported <- St_count_illicit_opioid_reported/total_population_PA
St_count_illicit_opioid_reported <- data.frame(rep(1:67), St_count_illicit_opioid_reported)
nvss_ood_county_wise_2013_2017$St_count_illicit_opioid_reported <- St_count_illicit_opioid_reported$St_count_illicit_opioid_reported
### adding lat and lng columns ###
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
df <-  data_naloxone %>% distinct(data_naloxone$County.Name,data_naloxone$County.Latitude.Point,data_naloxone$County.Longitude.Point)
colnames(df)[1] <- "NAME"
df <- df[order(df$NAME),]
colnames(df)[2] <- "Latitude"
colnames(df)[3] <- "Longitude"
df <- df[,c(1,3,2)]
df$Longitude <- as.numeric(df$Longitude)
df$Latitude <-as.numeric(df$Latitude)
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,df)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-21]

write.csv(nvss_ood_county_wise_2013_2017,'nvss_ood_county_wise_2013_2017.csv')


# 
# library(MASS)
# summary(nb1_PA <- glm.nb(deaths ~ deaths_social_proximity  + deaths_spatial_porximity+ACS_PCT_HU_NO_VEH+
#                           POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+ACS_PCT_LT_HS+AHRF_TOT_COM_HEALTH_GRANT+ACS_MEDIAN_HH_INC+
#                           +CCBP_BWLSTORES_RATE+AMFAR_MHFAC_RATE+
#                       + offset(log(population))
#                       +ODR+ Naloxone_Available+cumulative_total_beupromorphine, 
#                       data = nvss_ood_county_wise_2013_2017,weights=population,
#                       control = glm.control(maxit = 2000)))
# 
# lm_1_PA <- lm(deaths_per_capita ~ deaths_social_proximity  + deaths_spatial_porximity+
#   ACS_PCT_HU_NO_VEH+POS_MEAN_DIST_ALC+ACS_PCT_OTHER_INS+
#   ACS_PCT_LT_HS+ACS_MEDIAN_HH_INC+CCBP_BWLSTORES_RATE
# + offset(log(population))
# +ODR+ Naloxone_Available+cumulative_total_beupromorphine, 
# data = nvss_ood_county_wise_2013_2017,weights=population)
# 
# summary(lm_1_PA)
# 
# library(stargazer)
# stargazer(nb1_PA,type = "latex", 
#           title = "Negative Binomial Model with and without Clustered SE")
# url <- read_html('https://www.cdc.gov/drugoverdose/rxrate-maps/county2016.html')
# sites <- url %>% html_nodes('td') %>% html_text()
# sites_1 <- sites[8977:9244]
# sites_2 <- data.frame(print(sites_1))
# ODR <- as.data.frame(t(matrix(sites_2[,1],nrow = 4)))
# colnames(ODR)[1] <- 'county'
# colnames(ODR)[2] <- 'State'
# colnames(ODR)[3] <- 'Fips Codes'
# colnames(ODR)[4] <- 'Opioid Dispensing Rate Per 100'
# ODR <- ODR %>% mutate(county= gsub("(.*),.*", "\\1", ODR$county))
# ODR$`Opioid Dispensing Rate Per 100` <- as.integer(as.character(ODR$`Opioid Dispensing Rate Per 100`))
# ODR$ODR <- scale(ODR$`Opioid Dispensing Rate Per 100`)
# ### illicit drug source ###
# library(lubridate)
# illicit_opioid_Seizures <- read.csv("https://data.pa.gov/api/views/wmgc-6qvd/rows.csv?accessType=DOWNLOAD")
# illicit_opioid_Seizures$Qtr.Start.Date <- mdy(illicit_opioid_Seizures$Qtr.Start.Date)
# illicit_opioid_Seizures <- illicit_opioid_Seizures %>% filter(between(Qtr.Start.Date,as.Date('2013-01-01'), as.Date('2017-12-31')))
# #### CONVERTING INTO MME ###
# illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(MME_Coversion_Factor=case_when(Drug == "Fentanyl" ~ 0.125, Drug == "Heroin" ~ 1, Drug == "Opium" ~ 1 ))
# illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(Drug.Quantity_mmg = Drug.Quantity/1000)
# illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(MME = Drug.Quantity*MME_Coversion_Factor)
# illicit_opioid_Seizures_2013_2017 <- illicit_opioid_Seizures %>% group_by(County.Name) %>%  summarise(sum(MME))
# colnames(illicit_opioid_Seizures_2013_2017)[1] <- "county"
# colnames(illicit_opioid_Seizures_2013_2017)[2] <- "MME"

# nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,df)
# nvss_ood_county_wise_2013_2017$county_deaths_social_proximity <- deaths_social_proximity_county
# nvss_ood_county_wise_2013_2017$county_deaths_spatial_proximity <- deaths_spatial_proximity_county
# nvss_ood_county_wise_2013_2017$ODR <- ODR$ODR
# nvss_ood_county_wise_2013_2017$naloxone <- data_naloxone$naloxone_per_capita
# nvss_ood_county_wise_2013_2017$illicit_drug_seizures_mme_per_county <- illicit_opioid_Seizures_2013_2017$MME
# nvss_ood_county_wise_2013_2017$illicit_drug_seizures_mme_per_county <- scale(nvss_ood_county_wise_2013_2017$illicit_drug_seizures_mme_per_county)
# nvss_ood_county_wise_2013_2017$hh_income <- scale(pa_hh_income$estimate/nvss_ood_county_wise_2013_2017$population)
#write.csv(nvss_ood_county_wise_2013_2017, 'nvss_ood_county_wise_2013_2017.csv')


### Multiple Linear regression and Wtd Linear Regression #####
nvss_ood_county_wise_2013_2017 <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/2013_2017_PA_Analysis/nvss_ood_county_wise_2013_2017.csv")
glimpse(nvss_ood_county_wise_2013_2017)

####### social adjacency map for phili and allegheny###############
library(spatialreg)
library(spdep)
library(igraph)
library(tidyverse)
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
nvss_county <- nvss_ood_county_wise_2013_2017$GEOID
df_1 <- df_0 %>% dplyr::filter(user_loc %in% nvss_county & fr_loc %in% nvss_county)
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
df_1$fr_loc <- as.numeric(df_1$fr_loc)
nodes <- df_1 %>% distinct(fr_loc)
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- nvss_ood_county_wise_2013_2017$population
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
social_adjacency <- as.data.frame(cumulative_sci_weighted_test )
social_adjacency_phili <-  social_adjacency$`42101`
social_adjacency_phili_geoid <- row.names(social_adjacency)
phili_social_adjacency_data <- data.frame(social_adjacency_phili_geoid, social_adjacency_phili)
colnames(phili_social_adjacency_data)[1] <- "GEOID"
phili_social_adjacency_data$GEOID <- as.numeric(phili_social_adjacency_data$GEOID)

social_adjacency_alle <-  social_adjacency$`42003`
social_adjacency_alle_geoid <- row.names(social_adjacency)
alle_social_adjacency_data <- data.frame(social_adjacency_alle_geoid, social_adjacency_alle)
colnames(alle_social_adjacency_data)[1] <- "GEOID"

#### plotting the social adjacency map for phili and allegheny######
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
phili_social_adjacency_data$GEOID <- as.numeric(phili_social_adjacency_data$GEOID)
phili_counties_merged <- left_join(penn_counties, phili_social_adjacency_data, by = "GEOID")

ggplot(data = phili_counties_merged) +
  geom_sf(aes(fill = social_adjacency_phili)) +
  scale_fill_viridis_c(option = "viridis", trans="sqrt",direction = 1) +
  theme_minimal() +
  ggtitle("Social Adjacency in Pennsylvania Counties")

alle_social_adjacency_data$GEOID <- as.numeric(alle_social_adjacency_data$GEOID)
alle_counties_merged <- left_join(penn_counties, alle_social_adjacency_data, by = "GEOID")

ggplot(data = alle_counties_merged) +
  geom_sf(aes(fill = alle_counties_merged$social_adjacency_alle)) +
  scale_fill_viridis_c(option = "viridis", trans="sqrt",direction = 1) +
  theme_minimal() +
  ggtitle("Social Adjacency in Allegheny Counties")
####################################################
philadelphia <- df_1 %>% filter(user_loc=="42101")
philadelphia_1 <- df_1 %>% filter(fr_loc=="42101")
philadelphia_county_sci <- rbind(philadelphia,philadelphia_1)
philadelphia_county_sci <- philadelphia_county_sci[-68,]


phila_connections <- philadelphia_county_sci %>%
  mutate(philadelphia_loc = ifelse(user_loc == "42101", user_loc, fr_loc),
         other_loc = ifelse(user_loc == "42101", fr_loc, user_loc)) %>%
  select(philadelphia_loc, other_loc, scaled_sci)


allegheny <- df_1 %>% filter(user_loc=="42003")
allegheny_1 <- df_1 %>% filter(fr_loc=="42003")
allegheny_county_sci <- rbind(allegheny,allegheny_1)
allegheny_county_sci <- allegheny_county_sci[-68,]

########### spatial adjacency map ########
library(geodist)
df <- nvss_ood_county_wise_2013_2017 %>% dplyr::select(GEOID, Latitude, Longitude)
df <- df[order(df$GEOID),]
colnames(df)[2] <- "latitude"
colnames(df)[3] <- "longitude"
df <- df[, c(1, 3, 2)]

distance_matrix <- geodist(df, measure = 'geodesic') / 1000 # converting it to km
distance_matrix <- 1 + distance_matrix
distance_matrix <- distance_matrix**(-1)
diag(distance_matrix) <- 0
colnames(distance_matrix) <- df$GEOID
rownames(distance_matrix) <- df$GEOID
a_i_j <- data.frame(distance_matrix)
diag(a_i_j) <- 0
a_i_j <- as.matrix(a_i_j)

# Normalize a_i_j
normalised_scale <- rowSums(a_i_j)
a_i_j <- a_i_j / normalised_scale

spatial_adjacency_matrix <- as.data.frame(a_i_j)


spatial_adjacency_phili <-  spatial_adjacency_matrix$`X42101`
spatial_adjacency_phili_geoid <- row.names(a_i_j)
phili_spatial_adjacency_data <- data.frame(spatial_adjacency_phili_geoid, spatial_adjacency_phili)
colnames(phili_spatial_adjacency_data)[1] <- "GEOID"

spatial_adjacency_alle <-  spatial_adjacency_matrix$`X42003`
spatial_adjacency_alle_geoid <- row.names(a_i_j)
alle_spatial_adjacency_data <- data.frame(spatial_adjacency_alle_geoid, spatial_adjacency_alle)
colnames(alle_spatial_adjacency_data)[1] <- "GEOID"

### plotting the spatial adjacency map#####
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
phili_spatial_adjacency_data$GEOID <- as.numeric(phili_social_adjacency_data$GEOID)
phili_counties_merged <- left_join(penn_counties, phili_spatial_adjacency_data, by = "GEOID")

ggplot(data = phili_counties_merged) +
  geom_sf(aes(fill = phili_counties_merged$spatial_adjacency_phili)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial Adjacency in Pennsylvania Counties")


alle_spatial_adjacency_data$GEOID <- as.numeric(alle_spatial_adjacency_data$GEOID)
alle_counties_merged <- left_join(penn_counties, alle_spatial_adjacency_data, by = "GEOID")

ggplot(data = alle_counties_merged) +
  geom_sf(aes(fill = spatial_adjacency_alle)) +
  scale_fill_viridis_c(option = "viridis",direction = 1) +
  theme_minimal() +
  ggtitle("Spatial Adjacency in Allegheny Counties")


#### chlorpeth map####
# Create a color palette for the map
# Create a color palette for the map
# Create a color palette for the map
install.packages(c("leaflet", "sf", "RColorBrewer"))
# library(leaflet)
# library(sf)
# library(RColorBrewer)
# 
# penn_counties_sf <- st_as_sf(merged_data)
# # Determine pretty breaks for the color scale
# breaks_pretty <- pretty(penn_counties_sf$scaled_sci, n = 5)
# 
# ggplot() +
#   geom_sf(data = penn_counties_sf, aes(fill = scaled_sci),color="black", size = 0.2) +
#   scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, limits = c(1460, 419770), name = "scaled_sci") +
#   theme_minimal() +
#   ggtitle("Phili SCI") +
#   theme(plot.title = element_text(hjust = 0.5))


############ map for county deaths per capita############
library(dplyr)
nvss_ood_county_wise_2013_2017$GEOID <- as.numeric(nvss_ood_county_wise_2013_2017$GEOID)
deaths_per_capita <- nvss_ood_county_wise_2013_2017 %>% dplyr::select(GEOID,NAME,deaths,deaths_per_capita)
deaths_per_capita$GEOID <- as.numeric(deaths_per_capita$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(deaths_per_capita, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = deaths_per_capita), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, limits = c(0, 0.001825261), name = "Deaths per Capita") +
  theme_minimal() +
  ggtitle("Deaths per Capita by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))
###############################################################
### getting the matrices for social proximity weights###
library(readr)
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
county_geoid <- nvss_ood_county_wise_2013_2017$GEOID
df_1 <- df_0 %>% dplyr::filter(user_loc %in% county_geoid & fr_loc %in% county_geoid)
df_1$user_loc <- as.numeric(as.character(df_1$user_loc))
df_1$fr_loc <- as.numeric(as.character(df_1$fr_loc))
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc)))) ##unique pairs + self loops
df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc,fr_loc,scaled_sci))
df_for_matrix_weights
nodes <- df_1 %>% distinct(fr_loc)
k <- graph.data.frame(df_for_matrix_weights, directed=F, vertices=nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k,attr="scaled_sci",sparse=T)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0
population <- census_data_frame$population
### numerator of w_i_j: population_i*cumulative_sci_weighted_i Calculate n_j * SCI_ij
for(i in 1:ncol(cumulative_sci_weighted)){
  cumulative_sci_weighted[,i] <- cumulative_sci_weighted[,i] * population[i]
}
### # # Calculate Σ_{k ≠ i} n_k * SCI_ik
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
# # Calculate w_{ij} = n_j * SCI_ij / Σ_{k ≠ i} n_k * SCI_ik
cumulative_sci_weighted_test <- cumulative_sci_weighted/row_sums_cumulative_sci_weighted
#write.csv(cumulative_sci_weighted_test,'social_w_ij_county.csv')
#####social proximity map####
library(dplyr)
deaths_social_proximity<- nvss_ood_county_wise_2013_2017 %>% dplyr::select(GEOID,NAME,deaths_social_proximity,deaths_per_capita)
deaths_social_proximity$GEOID <- as.numeric(deaths_per_capita$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(deaths_social_proximity, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = deaths_social_proximity), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, limits = c(0,0.0014350880), name = "Deaths per Capita") +
  theme_minimal() +
  ggtitle("Deaths in Social Proximity by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))
##### spatial proximity map #######
library(dplyr)
deaths_spatial_proximity<- nvss_ood_county_wise_2013_2017 %>% dplyr::select(GEOID,NAME,deaths_spatial_porximity,deaths_per_capita)
deaths_spatial_proximity$GEOID <- as.numeric(deaths_per_capita$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(deaths_spatial_proximity, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = deaths_spatial_proximity$deaths_spatial_porximity), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1, limits = c(0,0.0014350880), name = "Deaths per Capita") +
  theme_minimal() +
  ggtitle("Deaths in Spatial Proximity by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))
#### social-spatial proximity #######
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017 %>% 
  mutate(diff_social_spatial_proximity= 
           abs((nvss_ood_county_wise_2013_2017$deaths_social_proximity- nvss_ood_county_wise_2013_2017$deaths_spatial_porximity)))
library(dplyr)
diff_social_spatial_proximity <- nvss_ood_county_wise_2013_2017 %>% dplyr::select(GEOID,NAME,diff_social_spatial_proximity,deaths_per_capita)
diff_social_spatial_proximity$GEOID <- as.numeric(diff_social_spatial_proximity$GEOID)
library(sf)
library(ggplot2)
library(tigris)
options(tigris_year = "2017")
penn_counties <- counties(state = "PA", cb = TRUE, resolution = "20m")
penn_counties$GEOID <- as.numeric(penn_counties$GEOID)
penn_counties_deaths <- penn_counties %>%
  left_join(diff_social_spatial_proximity, by = "GEOID")
ggplot() +
  geom_sf(data = penn_counties_deaths, aes(fill = diff_social_spatial_proximity), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "viridis", direction = 1, limits = c(-0.0005,0.0005)) +
  theme_minimal() +
  ggtitle("Diff in Social Proximity by County in Pennsylvania") +
  theme(plot.title = element_text(hjust = 0.5))

