install.packages("haven")
library(haven)
sas_data <- read_sas("opioids_county_2021.sas7bdat")
sas_data_1 <- read_sas("naloxone_county_2021.sas7bdat")

mortality_data <- read_sas("mort2019_drugoverdose.sas7bdat")
table(mortality_data$icd10_4)