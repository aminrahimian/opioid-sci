install.packages("sf")
install.packages("ggplot2")
install.packages("usmap")
library(sf)
library(ggplot2)
library(usmap)
library(tigris)
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% 
  dplyr::rename(ZCTA5CE10 = zipcode)
us_zip_map <- tigris::zctas(cb = TRUE, year = 2017, class = "sf")
df_ood_nvss_zipcode_level_sf <- df_ood_nvss_zipcode_level %>% 
  dplyr::select(ZCTA5CE10, lng, lat,deaths_sci_proximity_zip  , deaths_spatial_proximity_zip) %>%
  st_as_sf(coords = c("lng", "lat" ), crs = 4269) %>% 
  st_transform(st_crs(us_zip_map))
us_zip_map_merged <- st_join(us_zip_map, df_ood_nvss_zipcode_level_sf, join = st_intersects, left = FALSE)
# Social proximity plot
deaths_social_proximity_plot <- ggplot(data = us_zip_map_merged) +
  geom_sf(aes(fill = deaths_sci_proximity_zip)) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1) +
  theme_minimal() +
  ggtitle(" Deaths Social Proximity by ZIP Code (2013-2017)")

deaths_social_proximity_plot
# Spatial proximity plot
deaths_spatial_proximity_plot <- ggplot(data = us_zip_map_merged) +
  geom_sf(aes(fill = deaths_spatial_proximity_zip)) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", direction = 1) +
  theme_minimal() +
  ggtitle(" Deaths Spatial Proximity by ZIP Code (2013-2017)")
deaths_spatial_proximity_plot

