### Map of HPAI A/H5N8 and A/H5N6 detection in Cambodia

## load required packages

library(cowplot)
library(googleway)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(libwgeom)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# path where the boundary data will be stored 
geodata_download_path = 'C:/Users/Username/Documents/R/GADM'

# generate shapefiles for Cambodia and Vietnam
# required arguments: country - iso code of country, level - admin area: 0 (country), 1 (state), 2 (district)
cambodia_state <- raster::getData(name = 'GADM', country = 'KHM', level = 1, path = geodata_download_path) %>% sf::st_as_sf()
vietnam <- raster::getData(name = 'GADM', country = 'VNM', level = 0, path = geodata_download_path) %>% sf::st_as_sf()

# import data frame of H5N8|N6 detection in live poultry markets to annotate on the map
H5Nx <- read.csv("H5N8_H5N6_LPM_coords.csv")

# limit to one row per market
mkts <- H5Nx %>% distinct(market, .keep_all= TRUE)

# get centroids for the state labels
cambo_points <- st_centroid(cambodia_state)
cambo_points <- cbind(cambodia_state, st_coordinates(st_centroid(cambodia_state$geometry)))


# plot a pretty map
ggplot(data = cambodia_state) +
  geom_sf(fill = "antiquewhite1", size=0.2) +
  geom_sf(data = vietnam, size=0.2) +
  geom_text(data= cambo_points, aes(x=X,y=Y,label=NAME_1),
            size=5, color="darkblue", fontface="bold", alpha=0.8,check_overlap = TRUE)+
  xlab("Longitude") + ylab("Latitude") +
  theme(text = element_text(size = 20))+
  #ggtitle("HPAI A/H5N8 in Cambodia Poultry Markets")+
  coord_sf(xlim = c(103.75, 106.25), ylim = c(10.25, 12.25), expand = FALSE)+
  annotation_scale(location = "br", width_hint = 0.5, pad_x = unit(0.25, "in")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = mkts, aes(x = long, y = lat), size = 2, 
             shape = 23, fill = "darkred")+
  geom_text_repel(data = mkts, aes(x=long, y=lat, label = market), size = 7) +
  theme(panel.background = element_rect(fill = "aliceblue"))


# save as pdf
ggsave("Figure1_H5N8_H5N6_map.pdf")