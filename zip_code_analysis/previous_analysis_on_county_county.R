library(tidyverse)
library(tidycensus)
library(choroplethr)
library(choroplethrMaps)
library(geodist)
library(raster)
library(tidyverse)
library(readr)
library(igraph)
library(matlib)
library(rvest) #for web scrapping
library(zipcodeR) #for ZIP codes
library(fuzzyjoin)

#### Census Datframe###
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
  year = 2020
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
  year = 2020
)

pa_hh_income <- pa_hh_income %>% dplyr::filter(pa_hh_income$variable == "B19001_001")
colnames(pa_hh_income)[3] <- "hh_income"
pa_hh_income <- pa_hh_income %>% mutate(NAME=str_replace_all(pa_hh_income$NAME,"County, Pennsylvania",""))
pa_hh_income <- pa_hh_income %>% dplyr::select(c(GEOID,NAME,estimate))

Soc.2020 <- get_acs(geography = "county", year=2020, variables = (c(pop="B01003_001")),state="PA", survey="acs5") %>% mutate(Year = "2020")
Soc.2020 <- Soc.2020 %>% mutate(NAME=str_replace_all(Soc.2020$NAME,"County, Pennsylvania",""))
Soc.2020 <- Soc.2020[,-c(3,5,6)]
colnames(Soc.2020)[3] <- "population"
Soc.2020

census_data_frame <- merge(Soc.2020,pa_hh_income,by="NAME")

##### getting incidents per county ###
###https://data.pa.gov/Opioid-Related/Overdose-Information-Network-Data-CY-January-2018-/hbkk-dwy3####
dat <- read.csv("data.csv")
n_ood <- count(dat,dat$Incident.County.Name)
q_i <- cbind(n_ood,Soc.2020)
q_i <- q_i[,-4]
q_i <- q_i %>% mutate(ood_percapita <- n/population)
colnames(q_i)[1] <- "NAME"
colnames(q_i)[5] <- "deaths_per_capita"
DF <- q_i %>% dplyr::select(NAME,GEOID,deaths_per_capita)
q_i <- q_i %>% dplyr::select(GEOID,deaths_per_capita)
q_i
colnames(q_i)[1] <- "fr_loc"
colnames(q_i)[2] <- "deaths_per_capita"
##sci proximity ##
df_0 <- read_tsv ('county_county.tsv')
df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000)) 
df_1 <- df_0 %>% dplyr::filter(user_loc %in% 42001:42133 & fr_loc %in% 42001:42133)
df_1 <- unique(df_1)
df_1 <- df_1 %>% distinct(probabilites,.keep_all = TRUE) ##single repeated pairwise comparisons for sci
user_loc <- df_1 %>% distinct(user_loc)
df_s <- merge(df_1,q_i,by="fr_loc")
df_s <- df_s %>% mutate(wt_sci=probabilites*deaths_per_capita)
dataframe_for_matrix <- df_1 %>% dplyr::select(c(user_loc,fr_loc,probabilites))
nodes <- df_1 %>% distinct(user_loc)

g <- graph.data.frame(dataframe_for_matrix, directed=F, vertices=nodes)
sci_proximity <- as_adjacency_matrix(g,attr = "probabilites",sparse = F)
diag(sci_proximity) <- 0

df_for_matrix_probability <- df_s %>% dplyr::select(c(user_loc,fr_loc,probabilites))
df_for_matrix_probability
k <- graph.data.frame(df_for_matrix_probability, directed=F, vertices=nodes)
cumulative_sci <- as_adjacency_matrix(k,attr="probabilites",sparse=F)
row_wise_sum_sci <- rowSums(cumulative_sci)
row_wise_sum_sci
sci_proximity <- sweep(sci_proximity,2,row_wise_sum_sci,FUN="/")
sci_proximity_county <- rowSums(sci_proximity)
sci_proximity_county
#### physical proximity county ###
df <- dat %>% dplyr::select(Incident.County.Name, Incident.County.Latitude, Incident.County.Longitude)
df <- df %>% distinct(Incident.County.Name,Incident.County.Latitude,Incident.County.Longitude)
colnames(df)[1] <- "NAME"
df <- df[order(df$NAME),]
colnames(df)[2] <- "latitude"
colnames(df)[3] <- "longitude"
df <- df[,c(1,3,2)]
distance_matrix <- geodist(df, measure = 'geodesic' )/1000
colnames(distance_matrix) <- df$NAME
rownames(distance_matrix) <- df$NAME
d_i_j<- data.frame(distance_matrix)
d_i_j
county_distance_proximity<- distance_matrix**(-1)
colnames(county_distance_proximity) <- df$NAME
rownames(county_distance_proximity) <- df$NAME
diag(county_distance_proximity) <- 0
county_distance_proximity 
v <- DF$deaths_per_capita
v
county_physical_proximity_dij <- sweep(county_distance_proximity,2,v,FUN="*")
county_physical_proximity_dij
#county_physical_proximity_dij <- mapply("*",county_distance_proximity,v)
county_physical_proximity <- rowSums(county_physical_proximity_dij)
county_physical_proximity
##### census data frame ####
census_data_frame <- merge(Soc.2020,pa_hh_income,by="NAME")
census_data_frame_1 <- merge(census_data_frame,asian,by="NAME")
census_data_frame_2 <- merge(census_data_frame_1,black,by="NAME")
census_data_frame_3 <- merge(census_data_frame_2,hipi,by="NAME")
census_data_frame_4 <- merge(census_data_frame_3,hispanic,by="NAME")
census_data_frame_5 <- merge(census_data_frame_4,native,by="NAME")
census_data_frame_6 <- merge(census_data_frame_5,white,by="NAME")
colnames(census_data_frame_6)[7] <- "asian_percent"
colnames(census_data_frame_6)[9] <- "black_percent"
colnames(census_data_frame_6)[11] <- "hipi_percent"
colnames(census_data_frame_6)[13] <- "hispanic_percent"
colnames(census_data_frame_6)[15] <- "native_percent"
colnames(census_data_frame_6)[17] <- "white_percent"
census_data_frame_6 <- census_data_frame_6[,-c(4,6,8,10,12,14,16)]
colnames(q_i)[1] <- "GEOID"
colnames(census_data_frame_6)[2]<- "GEOID"
census_data_frame_7 <- merge(census_data_frame_6,q_i, by="GEOID")
census_data_frame_7
alpha <- data.frame(df$NAME,county_physical_proximity)
colnames(alpha)[1] <- "NAME"
alpha
alpha
as.data.frame(alpha)
as.data.frame(census_data_frame_7)
census_data_frame_7$physical_proximity <- alpha$county_physical_proximity
sci_proximity_county <- as.data.frame(sci_proximity_county)
sci_proximity_county
census_data_frame_7$social_proximity <- sci_proximity_county$sci_proximity_county
census_data_frame_7 <- census_data_frame_7 %>% mutate(income_per_capita <- estimate/population)
colnames(census_data_frame_7)[14]<- "income_per_capita"
census_data_frame_7
write.csv(census_data_frame_7,'social_proximity_data.csv')
data <- read.csv('social_proximity_data.csv')
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
data_naloxone <- data_naloxone %>% dplyr::select(County.Name,Cumulative.Kits.Provided)
data_naloxone <- data_naloxone[order(data_naloxone$County.Name),]
data$naloxone <- data_naloxone$Cumulative.Kits.Provided
data <- data %>% mutate(nalaxone_per_capita= as.numeric(naloxone)/population)
data[is.na(data)] <- 0
m1 <- lm(deaths_per_capita~physical_proximity+social_proximity+income_per_capita+nalaxone_per_capita, data=data)
summary(m1)


#### nalaxone administered per county ###
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
data_naloxone <- data_naloxone %>% dplyr::select(County.Name,Cumulative.Kits.Provided)
data_naloxone <- data_naloxone[order(data_naloxone$County.Name),]
colnames(data_naloxone)[1] <- "county"
data_naloxone$Cumulative.Kits.Provided <- as.numeric(gsub(",","",data_naloxone$Cumulative.Kits.Provided))
data_naloxone <- data_naloxone %>% mutate(county= gsub("(.*),.*", "\\1", data_naloxone$county))
data_naloxone$naloxone_per_capita <- scale(data_naloxone$Cumulative.Kits.Provided/Soc.2020$population)
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


### illicit drug source ###
data <- `Opioid_Seizures_and_Arrests_CY_2013_._Current_Quarterly_County_State_Police.(1)`
illicit_data_2016 <- data %>% filter(Year==c(2018,2019,2020,2021,2022))
illicit_data_2016_2017 <- illicit_data_2016 %>% group_by(County.Name) %>%  summarise(sum(Drug.Quantity))
colnames(illicit_data_2016_2017)[1] <- "county"
colnames(illicit_data_2016_2017)[2] <- "drug_quantity_kilogram"
county <- c("Cameron", "McKean", "Potter", "Sullivan" )
drug_quantity_kilogram <- c(0,0,0,0)
mdf <- data.frame(county,drug_quantity_kilogram)
illicit_drug_busted_per_quantity <- rbind(illicit_data_2016_2017,mdf)
illicit_drug_busted_per_quantity <- illicit_drug_busted_per_quantity[order(illicit_drug_busted_per_quantity$county),]
county_final_df <- Soc.2020
#county_final_df$deaths <- deaths_per_county_data$Total
county_final_df$deaths_per_capita <- q_i$deaths_per_capita
county_final_df$lat <- df$Latitude
county_final_df$lng <- df$Longitude
county_final_df$deaths_physical_proximity_county <- deaths_physical_proximity
county_final_df$deaths_social_proximity_county <- sci_proximity_county
county_final_df$illicit_drug_busted_per_quantity_kg  <- illicit_drug_busted_per_quantity $drug_quantity_kilogram
county_final_df$ODR <- ODR$`Opioid Dispensing Rate Per 100`
county_final_df$ODR <- as.numeric(county_final_df$ODR)
county_final_df$naloxone <- data_naloxone$naloxone_per_capita
county_regression <- lm(deaths_per_capita~ deaths_social_proximity_county+deaths_physical_proximity_county+ODR+naloxone+illicit_drug_busted_per_quantity_kg, data=county_final_df)
summary(county_regression)
stargazer(county_regression, type="html", out="county_regression.html")

final_df <- read.csv('final_df.csv')

write.csv(county_final_df, 'county_final_df_pa_gov.csv')
county_final_df <- read.csv('county_final_df_pa_gov.csv')
