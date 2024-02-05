install.packages("sf")
install.packages("ggplot2")
install.packages("usmap")
library(sf)
library(ggplot2)
library(usmap)
library(tigris)
df_ood_nvss_zipcode_level <- read.csv('C:/Users/kusha/Desktop/Data for Paper/Data From Analysis/aggregated_zip_data_2013_2107_nvss_zero_padded.csv')
df_ood_nvss_zipcode_level <- df_ood_nvss_zipcode_level %>% 
  dplyr::rename(ZCTA5CE10 = zipcode)
us_zip_map <- tigris::zctas(cb = TRUE, year = 2017, class = "sf")
df_ood_nvss_zipcode_level_sf <- df_ood_nvss_zipcode_level %>% 
  dplyr::select(ZCTA5CE10, lng, lat,deaths_per_capita,deaths_sci_proximity_zip  , deaths_spatial_proximity_zip) %>%
  st_as_sf(coords = c("lng", "lat" ), crs = 4269) %>% 
  st_transform(st_crs(us_zip_map))
us_zip_map_merged <- st_join(us_zip_map, df_ood_nvss_zipcode_level_sf, join = st_intersects, left = FALSE)
colnames(us_zip_map_merged)[7] <- "deaths per capita"
colnames(us_zip_map_merged)[8] <- "social proximity"
colnames(us_zip_map_merged)[9] <- "spatial proximity"
us_zip_map_merged <- us_zip_map_merged %>% dplyr::mutate(diff_sci_spatial = us_zip_map_merged$`social proximity`- us_zip_map_merged$`spatial proximity`)

###### maps ####
min_val <- min(us_zip_map_merged$deaths_per_capita, na.rm = TRUE)
max_val <- max(us_zip_map_merged$deaths_per_capita, na.rm = TRUE)
min_val_sci <- min(us_zip_map_merged$`social proximity`, na.rm = TRUE)
max_val_sci <-  max(us_zip_map_merged$`social proximity`, na.rm = TRUE)
min_val_spatial <- min(us_zip_map_merged$deaths_spatial_proximity_zip, na.rm = TRUE)
max_val_spatial <- max(us_zip_map_merged$`spatial proximity`, na.rm = TRUE)


overall_min_val <- min(min_val, min_val_sci, min_val_spatial)
overall_max_val <- max(max_val, max_val_sci, max_val_spatial)

# Set the limits using overall minimum and maximum values
common_scale <- scale_fill_viridis_c(option = "viridis", trans="sqrt",direction = 1, limits = c(0, 0.0019))


# Social proximity plot
deaths_social_proximity_plot <- ggplot(data = us_zip_map_merged) +
  geom_sf(aes(fill = `social proximity`)) +
  common_scale +
  theme_minimal() +
  ggtitle("Deaths in Social Proximity by ZIP Code (2013-2017)")

deaths_social_proximity_plot

# Spatial proximity plot
deaths_spatial_proximity_plot <- ggplot(data = us_zip_map_merged) +
  geom_sf(aes(fill = `spatial proximity`)) +
  common_scale +
  theme_minimal() +
  ggtitle("Deaths in Spatial Proximity by ZIP Code (2013-2017)")

deaths_spatial_proximity_plot
common_scale <- scale_fill_viridis_c(option = "viridis", trans="sqrt",direction = 1, limits = c(0, 0.026))
# Deaths Per Capita
deaths_per_capita <- ggplot(data = us_zip_map_merged) +
  geom_sf(aes(fill = `deaths per capita`)) +
  common_scale+
  theme_minimal() +
  ggtitle("Deaths per capita by ZIP Code (2013-2017)")
deaths_per_capita

### diff in social spatial proximity
common_scale <- scale_fill_viridis_c(option = "viridis", direction = 1, limits = c(-0.0005034545, 0.0006170684))
diff_sci_spatial <- ggplot(data = us_zip_map_merged) +
  geom_sf(aes(fill = diff_sci_spatial)) +
  common_scale+
  theme_minimal() +
  ggtitle("Difference in social and spatial proximity by ZIP Code (2013-2017)")
diff_sci_spatial

