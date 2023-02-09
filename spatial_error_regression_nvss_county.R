##### This R script  is for spatial error regression for nvss data##
### geographical unit = counties##
### R Version 4.1.1###
#### Covariates used ODR, Naloxone, HH income per capita, Illicit drug seizures and are scaled##
#### Results illustrates statistically significant effect size for socail proximity in SER##
#### Have to explore and add factors for SDOH (Social Determinants of Health)

packages = c('sf','tmap','spdep','rgdal', 'tidyverse')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


library(tidyverse)
library(rgeoda)

PA <- readOGR(dsn = "C:/Users/kusha/Desktop/Data for Paper/Spatial County Boundaries Data", layer = "Pennsylvania_County_Boundaries")
PA@data <- PA@data %>% arrange(COUNTY_NAM)
data <- read.csv('nvss_ood_county_wise_2013_2017.csv')
data <- data[,-1]
data$MSLINK <- PA$MSLINK
PA@data <- left_join(PA@data, data, by='MSLINK')
#qtm(PA, "SVI")
coords <- coordinates(PA)
knn6 <- knn2nb(knearneigh(coords, k =66))
#plot(PA, border = 'lightgrey')
#plot(knn6, coords, pch = 19, cex = 0.6, add = TRUE, col = 'red')
dist <- nbdists(knn6, coords, longlat = TRUE)
ids <- lapply(dist, function(x) 1/(x))
ids
invd.weights.knn <- nb2listw(knn6,glist = ids,style = "S",zero.policy = T)
invd.weights.knn$weights[1]



##### spatial regression ####
library(spatialreg)
serrRslt <- spatialreg::errorsarlm(deaths_per_capita ~ log(county_deaths_social_proximity) +
                                     log(county_deaths_spatial_proximity) + illicit_drug_seizures_mme_per_county + 
                                     naloxone+hh_income ,
                                   data = PA,
                                   listw = invd.weights.knn,
                                   zero.policy = TRUE, 
                                   na.action = na.omit)
summary(serrRslt)



