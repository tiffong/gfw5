#load packages
library(oceanmap)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

#load coastline polygon and unite
world = ne_countries(continent = 'north america', returnclass = 'sf', scale = 'medium')

#define bbox
lon = c(10, -170)
lat = c(15.56, 31.79)


#get bathmyetry to use as basemap
basemap = get.bathy(lon=lon, lat=lat, visualize = F, res = 5)

#Transform raster into a tidy data.frame
basemap_df = data.frame(coordinates(basemap)) %>%
  mutate(bathy = -values(basemap))

#make the map
ggplot() +
  geom_raster(data = basemap_df, mapping = aes(x=x, y=y, fill =bathy)) +
  geom_sf(data = world) +
  scale_x_continuous(limits = lon, expand = c(0,0)) +
  scale_y_continuous(limits = lat, expand = c(0,0)) +
  scale_fill_viridis_c(name = "bathymetry m") +
  labs(x='long', y = 'lat')