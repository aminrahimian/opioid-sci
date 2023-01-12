packages = c('sf','tmap','spdep','rgdal', 'tidyverse')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}


library(tidyverse)
library(rgeoda)

PA <- readOGR(dsn = "C:/Users/kusha/Desktop/Spatial Analysis Project", layer = "Pennsylvania_County_Boundaries")
PA@data <- PA@data %>% arrange(COUNTY_NAM)
data <- read.csv('spatial_nvss_data.csv')
data <- data[,-1]
PA@data <- left_join(PA@data, data, by='MSLINK')
qtm(PA, "SVI")
coords <- coordinates(PA)
knn6 <- knn2nb(knearneigh(coords, k =66))
plot(PA, border = 'lightgrey')
plot(knn6, coords, pch = 19, cex = 0.6, add = TRUE, col = 'red')
dist <- nbdists(knn6, coords, longlat = TRUE)
ids <- lapply(dist, function(x) 1/(x))
ids
invd.weights.knn <- nb2listw(knn6,glist = ids,style = "B",zero.policy = T)
invd.weights.knn$weights[1]
##### spatial regression ####
library(spatialreg)
serrRslt <- spatialreg::errorsarlm(deaths_per_capita ~ log(deaths_social_proximity_county) +
                                     log(deaths_physical_proximity_county) + illicit_drug_seizures_kg + 
                                     ODR + naloxone,
                                   data = PA,
                                   listw = invd.weights.knn,
                                   zero.policy = TRUE, 
                                   na.action = na.omit)
summary(serrRslt)
