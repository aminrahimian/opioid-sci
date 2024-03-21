### POPULATION RANK DENSITY MATRIX ####
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
library(tidycensus)
library(tigris)### for lat and lng
library(sf)### for lat and lng 
library(readxl)
library(spdep)
library(lmtest)
library(sandwich)
library(igraph)
library(haven)
census_api_key("e6460566746931aed6c241abe8a6e2425aa9c699", install = TRUE, overwrite = TRUE)
### demographic ###
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

Soc.2017 <- get_acs(geography = "county", year=2009, variables = (c(pop="B01003_001")),state="PA", survey="acs5") %>% mutate(Year = "2017")
Soc.2017 <- Soc.2017 %>% mutate(NAME=str_replace_all(Soc.2017$NAME,"County, Pennsylvania",""))
Soc.2017 <- Soc.2017[,-c(3,5,6)]
colnames(Soc.2017)[3] <- "population"
Soc.2017
census_data_frame <- merge(Soc.2017,pa_hh_income,by="NAME")


#### nvss 2013-2015 data###
ood_data_2009_2015 <-  read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_1999_2015.csv')
library(lubridate)
ood_data_2009_2015$dod<- mdy(ood_data_2009_2015$dod)
ood_data_2009_2015$dod <- as.Date(ood_data_2009_2015$dod)
ood_data_2009_2015<- ood_data_2009_2015 %>% 
  filter(dod >= as.Date('2009-01-01') & dod <= as.Date('2015-01-01'))

ood_data_2009_2015 <- ood_data_2009_2015 %>% filter(ood_data_2009_2015$COD %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                                                                                  "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))
ood_data_2009_2015$year <- year(ood_data_2009_2015$dod)

# Count OOD deaths per county_name for each year
ood_deaths_by_county_year <- ood_data_2009_2015 %>%
  group_by(county_name, year) %>%
  summarise(deaths = n(), .groups = 'drop') # n() counts the number of rows in each group, summarise() collapses these counts into a summary

# Specify the range of years present in your dataset or of interest
years_range <- 2009:2015

# Create a data frame of all county and year combinations
all_counties_years <- expand.grid(county_name = unique(ood_data_2009_2015$county_name),
                                  year = years_range)

# Left join the summarized death counts with the full county/year grid
complete_ood_deaths_by_county_year <- all_counties_years %>%
  left_join(ood_deaths_by_county_year, by = c("county_name", "year")) %>%
  replace_na(list(deaths = 0)) # Replace NA in deaths with 0

# Vector of all Pennsylvania counties (Example)
# Ensure that these are exactly as they appear in your dataset.
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

# Filter the dataset for Pennsylvania counties
pa_ood_deaths_by_county_year <- complete_ood_deaths_by_county_year %>%
  filter(county_name %in% pa_counties)

colnames(pa_ood_deaths_by_county_year)[1] <- "County"
# The resulting dataset, pa_ood_deaths_by_county_year, now includes only the counties in Pennsylvania.
# Extract the unique list of counties from your complete dataset
all_counties_in_dataset <- unique(complete_ood_deaths_by_county_year$county_name)

# Find counties in your dataset that are not in the list of Pennsylvania counties
extra_counties <- setdiff(all_counties_in_dataset, pa_counties)

### UNKNOWN AND DOVER were the extra counties####



#ood_data_2009_2015 <- count(ood_data_2009_2015,county_name)                          

data_2016_2017 <- read.csv('C:/Users/kusha/Desktop/Data for Paper/NVSS Data/Combined_Drug_2016_2017.csv')
## This is a private data set of opioid-related deaths in PA that include location and cause 
table(data_2016_2017$COD)
opioid_death_2016_2017 <- data_2016_2017 %>% filter(data_2016_2017$COD %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                                                                              "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))
opioid_death_2016_2017$dod<- mdy(opioid_death_2016_2017$dod)
opioid_death_2016_2017$dod <- as.Date(opioid_death_2016_2017$dod)
table(opioid_death_2016_2017$COD)
data_ood_2016 <- subset(opioid_death_2016_2017,select = c('fileno','Zip','COD','County', 'State', 'dod'))
data_ood_2016$year <- year(data_ood_2016$dod)

#data_ood_2016 <- data_ood_2016 %>% filter(State=="PENNSYLVANIA")
data_ood_2016 <- data_ood_2016 %>%
  group_by(County, year) %>%
  summarise(deaths = n(), .groups = 'drop') 
pa_ood_deaths_by_county_year_2016_2017 <- data_ood_2016 %>%
  filter(County %in% pa_counties)

### missing_county_informations###
counties_not_in_filtering <- setdiff(pa_counties,pa_ood_deaths_by_county_year_2016_2017$County)

### filling the missing year##
years_range <- 2016:2017

# Create a data frame of all county and year combinations
counties_year_2016_2017 <- expand.grid(County = unique(pa_ood_deaths_by_county_year_2016_2017$County),
                                  year = years_range)

# Left join the summarized death counts with the full county/year grid
complete_ood_deaths_by_county_year_2016_2017 <- counties_year_2016_2017 %>%
  left_join(pa_ood_deaths_by_county_year_2016_2017, by = c("County", "year")) %>%
  replace_na(list(deaths = 0)) # Replace NA in deaths with 0

specific_entries <- data.frame(
  County = c("CAMERON", "CAMERON", "FOREST", "FOREST"),
  year = c(2016, 2017, 2016, 2017),
  deaths = c(0, 1, 0, 1)
)

complete_ood_deaths_by_county_year_2016_2017 <- rbind(complete_ood_deaths_by_county_year_2016_2017, specific_entries)

complete_ood_deaths_by_county_year_2016_2017_sorted <- complete_ood_deaths_by_county_year_2016_2017 %>%
  arrange(County)

### merging overdose deaths in year 2009-2015 and 2016-2017
pa_health_statisitcs_ood_2009_2017 <- rbind(pa_ood_deaths_by_county_year,complete_ood_deaths_by_county_year_2016_2017_sorted)
pa_health_statisitcs_ood_2009_2017 <- pa_health_statisitcs_ood_2009_2017 %>%
  arrange(County)

### nchs data 2018 and 2019 ###
cdc_2018_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2018_drugoverdose.sas7bdat')
cdc_2019_mort_data <- read_sas('C:/Users/kusha/Desktop/Data for Paper/CDC Mortality Data/mort2019_drugoverdose.sas7bdat')
cdc_combined_mort_data_2018_2019 <- rbind(cdc_2018_mort_data,cdc_2019_mort_data)

###filtering counties in PA###
cdc_2018_2019_mort_data_PA <- cdc_combined_mort_data_2018_2019 %>% filter(stposto %in% c("PA"))
fips_code <- tidycensus::fips_codes
fips_code <- fips_code %>% filter(state %in% c("PA"))
fips_code$GEOID <- paste0(fips_code$state_code, fips_code$county_code)
Soc.2019 <- get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
                    state="PA", survey="acs5", geometry = FALSE)

Soc.2019<- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")
Soc.2019 <- merge(fips_code,Soc.2019, by="GEOID")
### extracting state code and fips code ###
st_code_st_abb <- Soc.2019 %>% dplyr::select(state,state_code)
st_code_st_abb <- unique(st_code_st_abb)
colnames(st_code_st_abb)[1] <- "stposto"
### merging the state_fips_code with cdc_2018_2019_to_get_5_digits_fips_code###
cdc_2018_2019_mort_data_PA <- left_join(cdc_2018_2019_mort_data_PA,st_code_st_abb,
                                                          by = "stposto")
cdc_2018_2019_mort_data_PA <- cdc_2018_2019_mort_data_PA %>% 
  mutate(FullFIPS = paste0(state_code, cntfipso))
### reordering the columns###
cdc_2018_2019_mort_data_PA <- cdc_2018_2019_mort_data_PA %>%
  dplyr::select(1, 93, 94, 2:92)
colnames(cdc_2018_2019_mort_data_PA)[3] <- "GEOID"

cdc_2018_2019_mort_data_PA <- cdc_2018_2019_mort_data_PA %>% 
  filter(icd10_3 %in% c("X40", "X41", "X42", "X43", "X44", "X60", "X61", "X62",
                        "X63", "X64", "X85", "Y10", "Y11", "Y12", "Y13", "Y14"))

cdc_mort_data_fips_wise_death_certificates <- cdc_2018_2019_mort_data_PA %>% 
  group_by(year,stnchsxo,GEOID) %>% summarise(total_count = n())

### adding the missing counties information in the cdc_mort_data_fips_wise_death_certificates####
years_range <- 2018:2019

# Create a data frame of all county and year combinations
all_counties_years_2018_2019 <- expand.grid(GEOID = unique(cdc_mort_data_fips_wise_death_certificates$GEOID),
                                  year = years_range)

### changing cdc_mort_data_fips_Wise_death_certificates to integer for left_join ###

cdc_mort_data_fips_wise_death_certificates$year <- as.integer(cdc_mort_data_fips_wise_death_certificates$year)
# Left join the summarized death counts with the full county/year grid
complete_ood_deaths_by_county_year_2018_2019 <- all_counties_years_2018_2019  %>%
  left_join(cdc_mort_data_fips_wise_death_certificates, by = c("GEOID", "year")) %>%
  replace_na(list(total_count = 0)) # Replace NA in deaths with 0
# Step 1: Create a dataframe for Cameron County for 2018 and 2019 with zero total_counts
missing_cameron <- data.frame(
  GEOID = c("42023", "42023"),
  year = c(2018, 2019),
  stnchsxo = c("PA","PA"),
  total_count = c(0, 0)
)

# Step 2: Append this to your existing dataframe
complete_ood_deaths_by_county_year_2018_2019 <- bind_rows(complete_ood_deaths_by_county_year_2018_2019, missing_cameron)

## arranging the data with geoid ###
complete_ood_deaths_by_county_year_2018_2019 <- complete_ood_deaths_by_county_year_2018_2019 %>% arrange(GEOID)
colnames(complete_ood_deaths_by_county_year_2018_2019)[4] <- "deaths"

################ merging pa helath statistics data with nchs data #####
county_geoid_mapping <- data.frame(County=c(pa_counties), GEOID= unique(c(complete_ood_deaths_by_county_year_2018_2019$GEOID)))
# Merge GEOID information based on county names
pa_health_statisitcs_ood_2009_2017  <- merge(pa_health_statisitcs_ood_2009_2017, county_geoid_mapping, by = "County", all.x = TRUE)
pa_health_statisitcs_ood_2009_2017 <- pa_health_statisitcs_ood_2009_2017[,-1]
pa_health_statisitcs_ood_2009_2017 <- pa_health_statisitcs_ood_2009_2017[,c(3,1,2)]
pa_health_statisitcs_ood_2009_2017 <- pa_health_statisitcs_ood_2009_2017 %>%
  mutate(stnchsxo = "PA") %>%
  # Relocate 'stnchsxo' to be the third column
  relocate(stnchsxo, .after = 2)
### mortality_data_for_eachcounty 
mort_data_overdose_deaths_2009_2019 <- rbind(pa_health_statisitcs_ood_2009_2017,complete_ood_deaths_by_county_year_2018_2019, by="GEOID")
mort_data_overdose_deaths_2009_2019 <- mort_data_overdose_deaths_2009_2019 %>% arrange(GEOID)
##### sdoh data ####





### POPULATION ###
pa_population <- get_acs(geography = "county",
                         variables = "B01003_001", # Total population variable
                         year = 2016, # Example year, adjust as needed
                         state = "PA",
                         survey = "acs5") # Adjust to "acs1" or "decennial" based on your requirement
pa_population <- pa_population[,c(1,4)]
colnames(pa_population)[2] <- "Population"

#### ADDING POPULATION INFO TO THE DATA FRAME ####
mort_data_overdose_deaths_2009_2019 <- merge(mort_data_overdose_deaths_2009_2019, 
                                                      pa_population, 
                                                      by = c("GEOID"), 
                                                      all.x = TRUE)
mort_data_overdose_deaths_2009_2019$Population < as.integer(mort_data_overdose_deaths_2009_2019$Population)
mort_data_overdose_deaths_2009_2019$deaths <- as.numeric(mort_data_overdose_deaths_2009_2019$deaths)
#### overdose death rates ####
mort_data_overdose_deaths_2009_2019 <- mort_data_overdose_deaths_2009_2019 %>% mutate(death_per_capita=deaths/Population)
mort_data_overdose_deaths_2009_2019 <- mort_data_overdose_deaths_2009_2019[-738,]
#### OODS_FOR EACH YEAR ###
data_list <- split(mort_data_overdose_deaths_2009_2019, mort_data_overdose_deaths_2009_2019$year)
# Rename the list elements
names(data_list) <- paste0("ood_", names(data_list))
# Create data frames in the global environment
list2env(data_list, envir = .GlobalEnv)
#### calculating social proximity #####
calculateSCI <- function(ood_df, file_path) {
  # Subset and rename columns
  social_df <- ood_df[, c(1, 5, 6)]
  colnames(social_df)[1] <- "fr_loc"
  colnames(social_df)[3] <- "deaths_per_capita"
  social_df <- social_df[order(social_df$fr_loc),]
  
  # Reading and processing the TSV file
  df_0 <- read_tsv(file_path)
  df_1 <- df_0 %>%
    dplyr::filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc) %>%
    filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
  
  df_for_matrix_weights <- df_1 %>% dplyr::select(c(user_loc, fr_loc, scaled_sci))
  nodes <- df_1 %>% distinct(fr_loc)
  k <- graph.data.frame(df_for_matrix_weights, directed = FALSE, vertices = nodes)
  cumulative_sci_weighted <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE)
  cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
  diag(cumulative_sci_weighted) <- 0
  
  # Adjusting weights by population
  population <- social_df$Population
  for (i in 1:ncol(cumulative_sci_weighted)) {
    cumulative_sci_weighted[, i] <- cumulative_sci_weighted[, i] * population[i]
  }
  
  row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
  cumulative_sci_weighted_test <- cumulative_sci_weighted / row_sums_cumulative_sci_weighted
  
  # Storing the matrix weight spatial format
  w_i_j <- as.matrix(cumulative_sci_weighted_test)
  diag(w_i_j) <- 0
  lw_1 <- mat2listw(w_i_j, style = 'W')
  
  # Further calculations
  v <- social_df$deaths_per_capita
  for (i in 1:ncol(cumulative_sci_weighted_test)) {
    cumulative_sci_weighted_test[, i] <- cumulative_sci_weighted_test[, i] * v[i]
  }
  
  s_minus_i <- rowSums(cumulative_sci_weighted_test)
  
  # Returning the results
  return(list(s_minus_i = s_minus_i))
}

## social_proximity_results ###########
# result_2009 <- calculateSCI(ood_2009, 'C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')
# Assuming calculateSCI is defined and takes a dataframe and a filepath as inputs

# Initialize an empty list to store the results
results_list <- list()

# Path to the county-county TSV file
sci_file_path <- 'C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv'

# Loop through the years 2009 to 2019
for(year in 2009:2019) {
  # Dynamically create the name of the dataframe (e.g., ood_2009)
  df_name <- paste0("ood_", year)
  
  # Evaluate the name to get the actual dataframe
  ood_data <- eval(parse(text=df_name))
  
  # Call calculateSCI function for the current year's data and store the result
  results_list[[as.character(year)]] <- calculateSCI(ood_data, sci_file_path)
}


#### adding lat and lng to each data frame ##
#### GETITNG LAT AND LNG FROM THE EXCISTING DATA SET ###
counties <- counties(cb = TRUE, class = "sf")
pa_fips_vector <-ood_2009[order(ood_2009$GEOID),]
pa_fips_vector <- pa_fips_vector$GEOID
### filtering the american_fips_Vector from counties###
selected_counties <- counties[counties$GEOID %in% pa_fips_vector, ]
#### getting the centroids ###
centroids <- st_centroid(selected_counties)
centroids <- centroids[order(centroids$GEOID ),]
coords <- st_coordinates(centroids)
### getting the lat lng ###
geoid_lat_lng<- data.frame(
  GEOID =pa_fips_vector,
  Longitude = coords[,1],
  Latitude = coords[,2]
)
ood_data_list <- list(ood_2009, ood_2010, ood_2011, ood_2012, ood_2013, ood_2014, ood_2015, ood_2016, ood_2017, ood_2018, ood_2019)
# Loop through each data frame in the list and merge with geoid_lat_lng
merged_data_list <- lapply(ood_data_list, function(df) {
  merge(df, geoid_lat_lng, by = "GEOID")
})
list2env(setNames(merged_data_list, paste0("ood_", 2009:2019)), envir = .GlobalEnv)
#### spatial proximity ###
calculateSpatialProximity <- function(ood_df) {

  # Selecting and preparing spatial data
  spatial_df <- ood_df %>% 
    select(GEOID, Longitude, Latitude) %>%
    arrange(GEOID)
  
  # Calculating distance matrix
  distance_matrix <- geodist(spatial_df, measure = "geodesic") / 1000  # converting to km
  distance_matrix <- 1 + distance_matrix  # Adjusting distances
  distance_matrix <- distance_matrix ** (-1)  # Inverse distance
  diag(distance_matrix) <- 0  # Setting diagonal to zero
  
  # Setting matrix row and column names
  colnames(distance_matrix) <- spatial_df$GEOID
  rownames(distance_matrix) <- spatial_df$GEOID
  
  # Ordering oods data and calculating deaths per capita
  ood_df <- ood_df %>% arrange(GEOID)
  y <- ood_df$death_per_capita
  
  # Ensure y is numeric
  y <- as.numeric(y)
  
  # Preparing a_i_j matrix
  a_i_j <- as.data.frame(distance_matrix)
  diag(a_i_j) <- 0
  a_i_j <- as.matrix(a_i_j)
  
  # Ensure a_i_j is numeric
  a_i_j <- matrix(as.numeric(a_i_j), nrow = nrow(a_i_j), ncol = ncol(a_i_j))
  
  # Normalizing a_i_j
  normalised_scale <- rowSums(a_i_j)
  a_i_j <- a_i_j / normalised_scale
  
  # Calculating deaths in spatial proximity
  d_minus_i <- a_i_j %*% y
  
  return(d_minus_i)
}


# Initialize the list to store results
spatial_list <- list()

for(year in 2009:2019) {
  # Dynamically create the name of the dataframe (e.g., ood_2009)
  df_name <- paste0("ood_", year)
  
  # Evaluate the name to get the actual dataframe
  ood_data <- eval(parse(text=df_name))
  
  # Call calculateSpatialProximity function for the current year's data and store the result
  spatial_list [[as.character(year)]] <- calculateSpatialProximity(ood_data)
}



#### clinical covariates ####3
# Function to read multiple SAS files and store them into separate data frames
read_opioids_sas_files <- function(base_path, start_year, end_year) {
  # Initialize a list to store data frames
  df_list <- list()
  
  # Iterate through the specified years
  for(year in start_year:end_year) {
    # Construct the file path dynamically
    file_path <- sprintf("%s_%d.sas7bdat", base_path, year)
    
    # Read the SAS file
    df <- read_sas(file_path)
    
    # Store the data frame in the list with a name
    df_list[[paste0("opioids_county_", year)]] <- df
  }
  
  # Return the list of data frames
  return(df_list)
}

base_path <- "C:/Users/kusha/Downloads/OneDrive_2023-05-10/IQVIA prescriptions/opioid/opioids_county"
opioids_dfs <- read_opioids_sas_files(base_path, 2009, 2019)
opioids_dfs <- lapply(opioids_dfs, function(df) {
  filter(df, st_code == "PA")
})


opioids_dfs <- lapply(opioids_dfs, function(df) {
  df %>%
    group_by(state_fip_county_fip) %>%
    summarise(total_dose_sum = sum(total_dose, na.rm = TRUE))
})

opioids_dfs <- lapply(opioids_dfs, function(df) {
  rename(df, GEOID = state_fip_county_fip)
})

# Iterate through the list and assign each data frame to a dynamically named variable
for(year in names(opioids_dfs)) {
  # Construct the variable name dynamically based on the year
  var_name <- paste0(year)
  
  # Assign the data frame to the dynamically named variable
  assign(var_name, opioids_dfs[[year]])
}

### ADDING SOCIAL PROXIMITY TO THE DATA FRAMES ###

# Loop through the years 2009 to 2019
# Loop through the years 2009 to 2019
for(year in 2009:2019) {
  # Construct the variable name for the data frame
  df_name <- paste0("ood_", year)
  
  # Retrieve the data frame by its name
  df <- get(df_name)
  
  # Access the nested list for the given year in results_list
  # Assuming the only element in the list for each year contains the s_minus_i values
  s_minus_i_values <- unlist(results_list[[as.character(year)]])
  
  # Add the s_minus_i values as a new column to the data frame
  df$deaths_social_proximity <- s_minus_i_values
  
  # Assign the modified data frame back to its original variable name
  assign(df_name, df)
}

# After executing this loop, each data frame (ood_2009 to ood_2019) will have a new column
# named "deaths_social_proximity" containing the s_minus_i values from the results_list.


# After this loop, each data frame (ood_2009 to ood_2019) will have a new column "deaths_social_proximity"
# added from the corresponding result_matrix (result_2009 to result_2019).

# Assuming spatial_list is a list with each element containing the values for each year, ordered from 2009 to 2019

# Loop through the years 2009 to 2019
for(year in 2009:2019) {
  # Construct the variable name for the data frame
  df_name <- paste0("ood_", year)
  
  # Retrieve the data frame by its name
  df <- get(df_name)
  
  # Retrieve the corresponding values from spatial_list
  # Note: We calculate the index as year - 2008 to match the list's indexing starting from 1
  spatial_values <- spatial_list[[year-2008]]
  
  # Add the values as a new column to the data frame
  df$deaths_spatial_proximity <- spatial_values
  
  # Assign the modified data frame back to its original variable name
  assign(df_name, df)
}

# After this loop, each data frame (ood_2009 to ood_2019) will have a new column named "deaths_spatial_proximity"
# containing the values from the corresponding element in spatial_list.

# Loop through the years 2009 to 2019
for (year in 2009:2019) {
  # Construct the data frame names for the opioid data and the ood data
  opioid_df_name <- paste0("opioids_county_", year)
  ood_df_name <- paste0("ood_", year)
  
  # Retrieve the data frames by their names
  opioid_df <- get(opioid_df_name)
  ood_df <- get(ood_df_name)
  
  # Merge the data frames by the GEOID column
  merged_df <- merge(opioid_df, ood_df, by = "GEOID", all = TRUE)
  
  # Assign the merged data frame back to a new variable in the global environment
  merged_df_name <- paste0("ood_", year)
  assign(merged_df_name, merged_df)
}

# After running this loop, there will be new data frames in the global environment
# named merged_2009, merged_2010, ..., merged_2019
# Each data frame is the result of merging the corresponding opioid and ood data frames by GEOID.


########### SDOH #####
# Loop through the years 2009 to 2019
# Loop through the years 2009 to 2019
# Define the desired column names
desired_columns <- c('ACS_PCT_UNEMPLOY', 'ACS_PCT_PERSON_INC_BELOW99', 'ACS_PCT_HU_NO_VEH', 
                     'ACS_PCT_OTHER_INS', 'ACS_PCT_LT_HS', 'CCBP_BWLSTORES_RATE', 
                     'AMFAR_MHFAC_RATE', 'ACS_MEDIAN_AGE', 'ACS_PCT_MALE', 'ACS_PCT_FEMALE',
                     'ACS_PCT_BLACK','ACS_PCT_ASIAN','ACS_PCT_AIAN','ACS_PCT_NHPI')

# Loop through the years 2009 to 2019
for (year in 2009:2019) {
  # Construct the file path dynamically
  file_path <- paste0("C:/Users/kusha/Desktop/Data for Paper/ood_prediction_data/sdoh_2009_2019/sdoh_", year, ".csv")
  
  # Read the CSV file
  sdoh_data <- read.csv(file_path)
  
  # Filter for the state of "Pennsylvania"
  sdoh_data <- sdoh_data[sdoh_data$STATE == "Pennsylvania", ]
  
  # Ensure all desired columns exist, adding them if they do not
  for (col in desired_columns) {
    if (!col %in% names(sdoh_data)) {
      sdoh_data[[col]] <- NA  # Add the column filled with NAs
    }
  }
  
  # Now select only the desired columns, keeping the order
  sdoh_data <- sdoh_data[, c('STATE','COUNTYFIPS',desired_columns)]
  
  # Construct the variable name for the data frame
  sdoh_df_name <- paste0("sdoh_", year)
  
  # Assign the filtered and augmented data frame to the dynamically named variable
  assign(sdoh_df_name, sdoh_data)
}

# After this loop, data frames named sdoh_2009 to sdoh_2019 with filtered data will be created in the global environment.
# Loop through the years 2009 to 2019
for (year in 2009:2019) {
  # Construct the variable names for the sdoh and ood data frames
  sdoh_df_name <- paste0("sdoh_", year)
  ood_df_name <- paste0("ood_", year)
  
  # Retrieve the data frames by their names
  sdoh_df <- get(sdoh_df_name)
  ood_df <- get(ood_df_name)
  
  # Merge the data frames by specifying the appropriate column names
  # Use 'GEOID' for the ood_df and 'COUNTYFIPS' for the sdoh_df
  merged_df <- merge(ood_df, sdoh_df, by.x = "GEOID", by.y = "COUNTYFIPS", all.x = TRUE)
  
  # Assign the merged data frame back to the ood data frame variable
  assign(ood_df_name, merged_df)
}

# After running this loop, each ood_YEAR data frame will now have the additional
# columns from the corresponding sdoh_YEAR data frame, matched according to their respective GEOID and COUNTYFIPS columns.

spatio_temporal_prediction_df <- rbind(ood_2009,ood_2010,ood_2011,ood_2012,ood_2013,ood_2014,ood_2015,ood_2016,ood_2017,ood_2018,ood_2019)
spatio_temporal_prediction_df <- spatio_temporal_prediction_df %>% mutate(ODR=total_dose_sum/Population)

write.csv(spatio_temporal_prediction_df,'prediction_df_2009_2019_PA.csv')




# ###### PA boundaries ####
# pa_counties <- counties(state = "PA", cb = TRUE, class = "sf", year=2010)
# pa_counties <- st_transform(pa_counties, crs = 5070) # Transform to Albers Equal Area
# pa_counties$land_area_sqkm <- st_area(pa_counties) / 10^6 # Calculate area in square kilometers
# pa_counties <- pa_counties %>%
#   mutate(COUNTY = paste0("42", COUNTY))
# pa_counties <- pa_counties %>% 
#   select(NAME, COUNTY, land_area_sqkm) %>%
#   arrange(NAME)
# colnames(pa_counties)[2] <- "GEOID"
# pa_counties <- pa_counties %>%
#   select(GEOID, land_area_sqkm) %>%
#   st_drop_geometry()
# ##### PA population #######
# pa_county_population <- get_acs(
#   geography = "county",
#   variables = "B01003_001", # Total population
#   year = 2016,
#   state = "PA",
#   survey = "acs5"
# )
# pa_county_population <- pa_county_population %>%
#   select(GEOID, NAME, variable, estimate) %>% # 
#   rename(total_population = estimate) %>%
#   arrange(NAME)
# 
# pa_county_population <- pa_county_population %>% select("NAME", "GEOID", "total_population")

# ### pa_population desnity###
# pa_county_population_density <- left_join(pa_counties, pa_county_population, by="GEOID")
# pa_county_population_density <- pa_county_population_density %>% mutate(population_density= total_population/land_area_sqkm)
# pa_county_population_density <- pa_county_population_density %>% select("GEOID", "population_density")

### pa_rank_density_matrix #####
# Initialize the matrix with zeros
# Initialize an empty matrix

# Assuming 'pa_county_population_density' is a dataframe containing the provided data

# Extract the population density values
# Assuming 'pa_county_population_density' is a dataframe containing the provided data

# Extract GEOIDs and population densities
# geoids <- as.character(pa_county_population_density$GEOID)
# pop_densities <- pa_county_population_density$population_density
# 
# # Number of counties
# n <- length(geoids)
# 
# # Initialize an empty matrix for the adjacency matrix
# adj_matrix <- matrix(0, n, n, dimnames = list(geoids, geoids))
# 
# # Calculate relative ranks based on population density differences
# for (i in 1:n) {
#   for (j in 1:n) {
#     if (i != j) {
#       # Calculate the absolute difference in population density as a proxy for "distance"
#       adj_matrix[i, j] <- abs(pop_densities[i] - pop_densities[j])
#     }
#   }
# }
# 
# # Convert the differences into ranks within each row
# # This operation adjusts the "distance" into a rank for each county comparison
# for (i in 1:n) {
#   adj_matrix[i,] <- rank(adj_matrix[i,], ties.method = "average")
# }
# 
# # Set the diagonal to 0, as the rank with itself is zero
# diag(adj_matrix) <- 0
# 
# # Assuming 'adj_matrix' contains the rank-based values
# 
# # Find the maximum rank value in the matrix
# max_rank <- max(adj_matrix)
# 
# # Invert ranks to represent proximity (skip diagonal elements)
# n <- nrow(adj_matrix) # Assuming a square matrix
# for (i in 1:n) {
#   for (j in 1:n) {
#     if (i != j) { # Avoid diagonal elements
#       adj_matrix[i, j] <- max_rank + 1 - adj_matrix[i, j]
#     }
#   }
# }
# 
# # Diagonal elements can be set to the maximum value for consistency,
# # indicating maximum proximity to oneself, or left as 0, depending on the analysis needs
# diag(adj_matrix) <- 0 
# ### proximity matrix ###
# diff_matrix <- matrix(0, n, n)
# for (i in 1:n) {
#   for (j in 1:n) {
#     if (i != j) {
#       diff_matrix[i, j] <- abs(pop_densities[i] - pop_densities[j])
#     }
#   }
# }
# 
# # Multiply the inverted ranks (representing proximity) by the absolute differences
# # This creates a new adjacency matrix where values represent the weighted metric of proximity and magnitude of difference
# for (i in 1:n) {
#   for (j in 1:n) {
#     if (i != j) { # Avoid diagonal elements
#       adj_matrix[i, j] <- adj_matrix[i, j] * diff_matrix[i, j]
#     }
#   }
# }
# 
# # Optionally, you can adjust the diagonal elements again if needed
# # For example, setting them to 0 or any other value that indicates self-comparison
# diag(adj_matrix) <- 0
# ### row normalising ####
# # Normalize each row so that the sum of each row equals 1
# adj_matrix_norm <- t(apply(adj_matrix, 1, function(x) x / sum(x)))
# 
# # Ensure diagonal elements are set as desired (e.g., to 0) after normalization
# diag(adj_matrix_norm) <- 0
# 
# ## calculating spatial proximity ###
# y <- aggregated_ood_per_capita_events_in_each_county$deaths_per_capita
# d_minus_i <- adj_matrix_norm %*% y


