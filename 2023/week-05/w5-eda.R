library(tidyverse)
library(mapview)
library(osmdata)
library(sf)

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

map_with_animation <- map_with_data +
  transition_time(year) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(test_data$year) - min(test_data$year) + 1
animate(map_with_animation, nframes = num_years)
