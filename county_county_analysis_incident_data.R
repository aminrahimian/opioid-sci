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
dat <- read.csv("C:/Users/kusha/Desktop/Data for Paper/Incident Data/data.csv")
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
df_0 <- read_tsv ('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000)) 
df_1 <- df_0 %>% dplyr::filter(user_loc %in% q_i$fr_loc & fr_loc %in% q_i$fr_loc)
df_1$user_loc <- as.numeric(as.character(df_1$user_loc))
df_1$fr_loc <- as.numeric(as.character(df_1$fr_loc))
df_1 <- df_1 %>% filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc)))) ##unique pairs + self loops
df_s <- merge(df_1,q_i,by="fr_loc")
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
sci_proximity_county <- sweep(sci_proximity,2,row_wise_sum_sci,FUN="/")
deaths_social_proximity_county<- rowSums(sci_proximity_county)

#### physical proximity county ###
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
v <- q_i$deaths_per_capita
county_physical_proximity_dij <- sweep(county_distance_proximity,2,v,FUN="*")
county_normalised_physical_porximity <- sweep(county_physical_proximity_dij,2,normalised_scale,FUN="/")
deaths_spatial_proximity_county=rowSums(county_normalised_physical_porximity)
deaths_spatial_proximity_county

#### nalaxone administered per county ###
naloxone <- read.csv("https://data.pa.gov/api/views/xqrx-inrr/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
data_naloxone <- naloxone
data_naloxone <- data_naloxone %>% dplyr::select(County.Name,Cumulative.Kits.Provided)
data_naloxone <- data_naloxone[order(data_naloxone$County.Name),]
colnames(data_naloxone)[1] <- "county"
data_naloxone$Cumulative.Kits.Provided <- as.numeric(gsub(",","",data_naloxone$Cumulative.Kits.Provided))
data_naloxone <- data_naloxone %>% mutate(county= gsub("(.*),.*", "\\1", data_naloxone$county))
data_naloxone$naloxone_per_capita <- scale(data_naloxone$Cumulative.Kits.Provided)

###########SDOH##
health_determinant <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Health Determinant County 2017-2018/county_2017.csv')
health_determinant <- health_determinant %>% dplyr::filter(health_determinant$STATE== "Pennsylvania")
health_determinant <- health_determinant %>% dplyr::select(c('COUNTY', 'FIPSCODE' , 'ACS_PCT_SMARTPHONE', 'ACS_PCT_UNEMPLOY', 'ACS_PCT_PERSON_INC99', 'ACS_MEDIAN_HH_INCOME', 'CCBP_RATE_BWLSTORES_PER_1000', 'CHR_HOMICIDES', 'ACS_PCT_NO_VEH', 'AHRF_COMM_HLTH_CNTR', 'AHRF_MENTL_HLTH_CNT', 'CHR_SMOKING', 'CHR_MENTAL_DISTRESS', 'AMFAR_HIVDIAGNOSED'))
county_name_and_fips_value <- data.frame(health_determinant[1:2])
scaled_health_determinant <- data.frame(health_determinant[3:14])
scaled_health_determinant <- scaled_health_determinant %>% replace(is.na(.), 0)
county_final_df <- data.frame(scale(scaled_health_determinant))
county_final_df$deaths_social_proximity_county <- deaths_social_proximity_county
county_final_df$deaths_spatial_proximity_county <- deaths_spatial_proximity_county
county_final_df$naloxone_administered_per_county <- data_naloxone$naloxone_per_capita
county_final_df <- cbind(county_name_and_fips_value,county_final_df)
county_final_df$deaths_per_capita <- q_i$deaths_per_capita
write.csv(county_final_df, 'county_final_df_pa_gov.csv')
########### lasso based variable selection ####
x <- county_final_df[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
x <- as.matrix(x)
y <- as.matrix(county_final_df$deaths_per_capita)
set.seed(123)
index <- createDataPartition(county_final_df$deaths_per_capita, p = 0.5, list = FALSE)
x_train <- x[index, ]
y_train <- y[index]
x_test <- x[-index, ]
y_test <- y[-index]
cv.fit <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
#plot(cv.fit)
lambda_best <- cv.fit$lambda.min
fit <- glmnet(x, y, alpha = 1)
coef(fit, s = lambda_best)
##### new data frame based on the selected covariates###
incident_data_pa_gov <- county_final_df[,c(1,2,4,5,6,7,8,10,11,14,15,16,18)]
##### network autocorrelation model ###









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
county_final_df$deaths_per_capita <- q_i$deaths_per_capita
county_final_df$lat <- df$Latitude
county_final_df$lng <- df$Longitude
county_final_df$deaths_physical_proximity_county <- deaths_physical_proximity
county_final_df$deaths_social_proximity_county <- sci_proximity_county
county_final_df$illicit_drug_busted_per_quantity_kg  <- illicit_drug_busted_per_quantity $drug_quantity_kilogram
county_final_df$ODR <- ODR$`Opioid Dispensing Rate Per 100`
county_final_df$ODR <- as.numeric(county_final_df$ODR)
county_final_df$naloxone <- data_naloxone$naloxone_per_capita





################ regression ####

m1 <- lm(deaths_per_capita~ log(deaths_social_proximity_county)+log(deaths_physical_proximity_county)+ODR+naloxone+illicit_drug_busted_per_quantity_kg, weights = population, data=county_final_df)
summary(m1)

county_final_df_incident <- read.csv('county_final_df_pa_gov_correction_made.csv')
county_final_df_incident <- county_final_df_incident %>% mutate(sp=log(deaths_social_proximity_county))
county_final_df_incident <- county_final_df_incident %>% mutate(dp=log(deaths_physical_proximity_county))
write.csv(county_final_df_incident, 'spatial_df_pa_gov_contains_log_tranfromation.csv')
