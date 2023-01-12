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
strong_proximity_counties <- c('Erie', 'Crawford ','Venango', ' Mercer', 'Lawrence', 'Butler', 'Armstrong ','Lebanon ','Lancaster','Chester','Delaware','Philadelphia','Bucks','Lehigh')
weak_proximity_counties <- c('Waren', 'Mckean','Forest','Somrest','Bedford','Fulton','Fayette','Allegheny')
census_data_frame_1 <- census_data_frame %>% filter(NAME %in% strong_proximity_counties)


#### nvss 2013-2015 data###
ood_data_2013_2015 <- read.csv('Combined_Drug_1999_2015.csv')
library(lubridate)
ood_data_2013_2015$dod<- mdy(ood_data_2013_2015$dod)
ood_data_2013_2015$dod <- as.Date(ood_data_2013_2015$dod)
ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(between(dod,as.Date('2013-01-01'), as.Date('2015-01-01')))
ood_data_2013_2015 <- ood_data_2013_2015 %>% filter(ood_data_2013_2015$COD %in% c("X40","X41","X42","X43","X44"))
deaths_in_county_in_2013_2015 <- count(ood_data_2013_2015,county_name)                          
data_2016_2017 <- read.csv('Combined_Drug_2016_2017.csv')
## This is a private data set of opioid-related deaths in PA that include location and cause 
table(data_2016_2017$COD)
opioid_death_2016_2017 <- data_2016_2017 %>% filter(data_2016_2017$COD %in% c("X40","X41","X42","X43","X44"))
table(opioid_death_2016_2017$COD)
data_ood_2016 <- subset(opioid_death_2016_2017,select = c('fileno','Zip','COD','County', 'State'))
n_ood <- count(data_ood_2016,data_ood_2016$County)
colnames(n_ood)[1] <-'County'
n_ood$County <- as.character(n_ood$County)
### using census dataframe to gather Geoid and using incident data to gather County Name##
### conversion of string characters did not work tried it with UTF-8 TO ASCII//TRANSLIT##
opioid_incident_data_pa_gov <- read.csv("data.csv")
county_name <- unique(opioid_incident_data_pa_gov$Incident.County.Name)
county_name <- toupper(county_name)
deaths_in_county_in_2016_2017 <- n_ood %>% dplyr::filter(County %in% county_name)
deaths_in_county_in_2016_2017 [66,1] <- "CAMERON"
deaths_in_county_in_2016_2017 [66,2] <- 1
deaths_in_county_in_2016_2017 [67,1] <- "FOREST"
deaths_in_county_in_2016_2017 [67,2] <- 1
deaths_in_county_in_2016_2017  <- n_ood_pa[order(n_ood_pa$County),]
aggregated_ood_deaths_2013_2017 <- cbind(deaths_in_county_in_2013_2015,deaths_in_county_in_2016_2017 )
colnames(aggregated_ood_deaths_2013_2017)[2] <- 'deaths_2013_2015'
aggregated_ood_deaths_2013_2017 <- aggregated_ood_deaths_2013_2017 %>% mutate(n=n+deaths_2013_2015)
aggregated_ood_deaths_2013_2017 <- aggregated_ood_deaths_2013_2017[,-c(2,3)]
county_wise_ood_with_demogrpahic_information <- cbind(n_ood_pa,Soc.2017)
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
df_0 <- read_tsv ('county_county.tsv')
df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000)) 
df_1 <- df_0 %>% dplyr::filter(user_loc %in% 42001:42133 & fr_loc %in% 42001:42133)
df_1 <- unique(df_1)
df_1 <- df_1 %>% distinct(probabilites,.keep_all = TRUE) ##single repeated pairwise comparisons for sci
user_loc <- df_1 %>% distinct(user_loc)
df_s <- merge(df_1,aggregated_ood_per_capita_events_in_each_county,by="fr_loc")
df_s <- df_s %>% mutate(wt_sci=probabilites*deaths_per_capita)
dataframe_for_matrix <- df_s %>% dplyr::select(c(user_loc,fr_loc,wt_sci))
nodes <- df_s %>% distinct(user_loc)

g <- graph.data.frame(dataframe_for_matrix, directed=F, vertices=nodes)
sci_proximity <- as_adjacency_matrix(g,attr = "wt_sci",sparse = F)
diag(sci_proximity) <- 0
write.csv(sci_proximity,'sci_proximity.csv')
df_for_matrix_probability <- df_s %>% dplyr::select(c(user_loc,fr_loc,probabilites))
df_for_matrix_probability
k <- graph.data.frame(df_for_matrix_probability, directed=F, vertices=nodes)
cumulative_sci <- as_adjacency_matrix(k,attr="probabilites",sparse=F)

#write.csv(cumulative_sci ,'cumulative_sci.csv')
row_wise_sum_sci <- rowSums(cumulative_sci)
row_wise_sum_sci
sci_proximity_county <- sweep(sci_proximity,2,row_wise_sum_sci,FUN="/")
deaths_social_proximity_county<- rowSums(sci_proximity)

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
distance_matrix <- geodist(df, measure = 'geodesic' )/1000 #converting it to km
distance_matrix <- (1+distance_matrix)
distance_matrix<- distance_matrix**(-1)
colnames(distance_matrix) <- df$county
rownames(distance_matrix) <- df$county
d_i_j<- data.frame(distance_matrix)
normalised_scale <- colSums(d_i_j)
normalised_scale
county_distance_proximity<- d_i_j
colnames(county_distance_proximity) <- df$county
rownames(county_distance_proximity) <- df$county
diag(county_distance_proximity) <- 0
county_distance_proximity
county_wise_ood_deaths_per_capita <- aggregated_ood_per_capita_events_in_each_county$deaths_per_capita
county_physical_proximity_dij <- sweep(county_distance_proximity,2,county_wise_ood_deaths_per_capita ,FUN="*")
county_normalised_physical_porximity <- sweep(county_physical_proximity_dij,2,normalised_scale,FUN="/")
deaths_physical_proximity=rowSums(county_normalised_physical_porximity)
deaths_physical_proximity
####### naloxone adminitered###
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
data_naloxone <- data_naloxone %>% dplyr::select(County.Name,Cumulative.Kits.Provided)
data_naloxone <- data_naloxone[order(data_naloxone$County.Name),]
colnames(data_naloxone)[1] <- "county"
data_naloxone$Cumulative.Kits.Provided <- as.numeric(gsub(",","",data_naloxone$Cumulative.Kits.Provided))
data_naloxone <- data_naloxone %>% mutate(county= gsub("(.*),.*", "\\1", data_naloxone$county))
data_naloxone$naloxone_per_capita <- scale(data_naloxone$Cumulative.Kits.Provided/Soc.2017$population)
#### odr###
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

#### SVI #### (still have to find an alternative)
#SVI <- read.csv('Pennsylvania_COUNTY.csv')
#SVI <- SVI$RPL_THEMES


### illicit drug source ###
library(lubridate)
illicit_opioid_Seizures <- read.csv("https://data.pa.gov/api/views/wmgc-6qvd/rows.csv?accessType=DOWNLOAD")
illicit_opioid_Seizures$Qtr.Start.Date <- mdy(illicit_opioid_Seizures$Qtr.Start.Date)
illicit_opioid_Seizures <- illicit_opioid_Seizures %>% filter(between(Qtr.Start.Date,as.Date('2013-01-01'), as.Date('2017-12-31')))
#### CONVERTING INTO MME ###
illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(MME_Coversion_Factor=case_when(Drug == "Fentanyl" ~ 0.125, Drug == "Heroin" ~ 1, Drug == "Opium" ~ 1 ))
illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(Drug.Quantity_mmg = Drug.Quantity/1000)
illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(MME = Drug.Quantity*MME_Coversion_Factor)
#illicit_opioid_Seizures <- illicit_opioid_Seizures %>% mutate(MME=scale(MME))
illicit_opioid_Seizures_2013_2017 <- illicit_opioid_Seizures %>% group_by(County.Name) %>%  summarise(sum(MME))
colnames(illicit_opioid_Seizures_2013_2017)[1] <- "county"
colnames(illicit_opioid_Seizures_2013_2017)[2] <- "MME"
#county <- c("Cameron", "Elk","Forest", "McKean", "Potter", "Sullivan", "Warren" )
#drug_quantity_kilogram <- c(0,0,0,0,0,0,0)
#mdf <- data.frame(county,drug_quantity_kilogram)
#illicit_drug_busted_per_quantity <- rbind(illicit_data_2016_2017,mdf)
#illicit_drug_busted_per_quantity <- illicit_drug_busted_per_quantity[order(illicit_drug_busted_per_quantity$county),]
nvss_ood_county_wise_2013_2017 <-Soc.2017
nvss_ood_county_wise_2013_2017 <- cbind(Soc.2017,county_wise_ood_with_demogrpahic_information)
nvss_ood_county_wise_2013_2017 <- nvss_ood_county_wise_2013_2017[,-c(4,6,7)]
nvss_ood_county_wise_2013_2017 <- cbind(nvss_ood_county_wise_2013_2017,df)
nvss_ood_county_wise_2013_2017$county_deaths_social_proximity <- deaths_social_proximity_county
nvss_ood_county_wise_2013_2017$county_deaths_spatial_proximity <- deaths_physical_proximity
nvss_ood_county_wise_2013_2017$ODR <- ODR$`Opioid Dispensing Rate Per 100`
nvss_ood_county_wise_2013_2017$naloxone <- data_naloxone$naloxone_per_capita
nvss_ood_county_wise_2013_2017$illicit_drug_seizures_mme_per_county <- illicit_opioid_Seizures_2013_2017$MME
nvss_ood_county_wise_2013_2017$ODR <- scale(as.numeric(nvss_ood_county_wise_2013_2017$ODR))
nvss_ood_county_wise_2013_2017$illicit_drug_seizures_mme_per_county <- scale(nvss_ood_county_wise_2013_2017$illicit_drug_seizures_mme_per_county)
nvss_ood_county_wise_2013_2017$hh_income <- scale(pa_hh_income$estimate)
write.csv(nvss_ood_county_wise_2013_2017, 'nvss_ood_county_wise_2013_2017.csv')

#county_final_df$SVI <- SVI
#### output code###
#county_final_df <- county_final_df %>% mutate(sp_county=log(social_proximity))
#county_final_df <- county_final_df %>% mutate(dp=log(deaths_physical_proximity_county))
#write.csv(county_final_df,'spatial_nvss_data.csv')

county_ols_nvss <- lm(deaths_per_capita~ log(deaths_physical_proximity_county)+ log(social_proximity)+illicit_drug_busted_per_quantity_kg+ODR+ naloxone,data=nvss_ood_county_wise_2013_2017)
county_ols_nvss_wts <- lm(deaths_per_capita~ log(deaths_physical_proximity_county)+ log(social_proximity)+ODR+ naloxone, weights=population, data=county_final_df_nvss)
county_ols_data_pa <- lm(deaths_per_capita~ log(deaths_physical_proximity_county)+ log(deaths_social_proximity_county)+illicit_drug_busted_per_quantity_kg+ODR+ naloxone,data=county_final_df_incident)
county_ols_data_pa_wts <- lm(deaths_per_capita~ log(deaths_physical_proximity_county)+ log(deaths_social_proximity_county)+illicit_drug_busted_per_quantity_kg+ODR+ naloxone,wts=population, data=county_final_df_incident)

summary(county_ols_nvss)
summary(county_ols_data_pa)
summary(county_ols_nvss_wts)

qqplot(county_ols$residuals)

