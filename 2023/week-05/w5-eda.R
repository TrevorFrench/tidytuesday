library(tidyverse)
library(mapview)
library(osmdata)
library(sf)
library(gganimate)
library(osrm)
library(ggmap)

cats_uk <- read.csv('./2023/week-05/data/cats_uk.csv')
cats_uk_reference <- read.csv('./2023/week-05/data/cats_uk_reference.csv')

mapview(cats_uk, xcol = "location_long", ycol = "location_lat", crs = 4269, grid = FALSE)
# Max-Tag

#map_data("world", )
data <- getbb("St. Newlyn East")

big_streets <- data %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway"
                            , "primary"
                            , "motorway_link"
                            , "primary_link")
  ) %>%
  osmdata_sf()

med_streets <- data %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("secondary"
                            , "tertiary"
                            , "secondary_link"
                            , "tertiary_link")
  ) %>%
  osmdata_sf()

small_streets <- data %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential"
                            , "living_street"
                            , "unclassified"
                            , "service"
                            , "footway")
  ) %>%
  osmdata_sf()

map <- ggplot() +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .9,
          alpha = .6) +
geom_sf(data = med_streets$osm_lines,
        inherit.aes = FALSE,
        color = "black",
        size = .5,
        alpha = .5) +
geom_sf(data = small_streets$osm_lines,
        inherit.aes = FALSE,
        color = "#666666",
        size = .4,
        alpha = .3)

max_cat <- filter(cats_uk, tag_id == "Max-Tag")
points_sf <- max_cat %>% 
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326)
map_with_data <- map + geom_sf(
  data = points_sf,
  aes(group = timestamp),
  #aes(#size = Size,
    #col = Person),
  alpha = 0.8,
  size = 2
)
# map_with_data <- map + coord_sf(xlim = c(-5.049690),
#                                 ylim = c(50.36747),
#                                 expand = FALSE)
# map_with_data <- map + geom_point(max_cat, mapping=aes(x = "location_long", y = "location_lat"))
map_with_data
times <- as.POSIXct(max_cat$timestamp, "%Y-%m-%dT%H:%M:%Sz")
map_with_animation <- map_with_data +
  transition_time(times) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
# num_years <- max(points_sf$timestamp) - min(points_sf$timestamp) + 1
num_years <- nrow(max_cat) + 1
animate(map_with_animation, nframes = num_years)

######

max_cat <- filter(cats_uk, tag_id == "Max-Tag")
points_sf <- max_cat %>% 
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326)

osroute <- osrm::osrmRoute(loc = points_sf,
                           returnclass = "sf")
osroute_sampled <- st_transform(osroute) %>%
  st_sample(type = 'regular', size = 50) %>%
  st_cast('POINT') %>%
  st_as_sf()

osroute_xy <- osroute_sampled %>% 
  mutate(seq = 1:nrow(.),
         x = st_coordinates(.)[,"X"],
         y = st_coordinates(.)[,"Y"])
# BMM <- get_stamenmap(bbox = data,
#                      zoom = 6,
#                      maptype = "terrain")

BMM <- get_stamenmap(bbox = c(-6, 49.5, -3, 51),
                     zoom = 10,
                     maptype = "watercolor")


animation <- ggmap(BMM) + 
  geom_point(data = osroute_xy,
             aes(x = x, y = y),
             color = "red",
             size = 4) +
  theme_void() +
  transition_reveal(seq) +
  shadow_wake(wake_length = 1/6)
gganimate::animate(animation, 
                   nframes = 2*(nrow(osroute_xy)+1), 
                   height = 800, 
                   width = 760,
                   fps = 10, 
                   renderer = gifski_renderer(loop = T))
gganimate::anim_save('animated_bmm.gif', animation = last_animation())