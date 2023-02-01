#Clears environment
rm(list = ls(all.names = TRUE))
#-------------------------------------------------------------------------------
#-----------------------------------LIBRARIES-----------------------------------
#-------------------------------------------------------------------------------
library(dplyr)
library(osrm)
library(ggmap)
library(sf)
library(gganimate)
library(gifski)

#-------------------------------------------------------------------------------
#--------------------------------------DATA-------------------------------------
#-------------------------------------------------------------------------------
cats_uk <- read.csv('./2023/week-05/data/cats_uk.csv')
cats_uk_reference <- read.csv('./2023/week-05/data/cats_uk_reference.csv')

#-------------------------------------------------------------------------------
#------------------------------------BASE MAP-----------------------------------
#-------------------------------------------------------------------------------
base_map <- get_stamenmap(bbox = c(-5.79117, 49.94680,-4.57169, 50.42777),
                     zoom = 10,
                     maptype = "watercolor")
sample_size = 75

#-------------------------------------------------------------------------------
#----------------------------------CAT FUNCTION---------------------------------
#-------------------------------------------------------------------------------
cat_data <- function(cat_df, sample = sample_size) {
  points_sf <- cat_df %>%
    st_as_sf(coords = c("location_long", "location_lat"),
             crs = 4326)
  
  osroute <- osrmRoute(loc = points_sf, returnclass = "sf")
  
  osroute_sampled <- st_transform(osroute) %>%
    st_sample(type = 'regular', size = sample) %>%
    st_cast('POINT') %>%
    st_as_sf()
  
  osroute_xy <- osroute_sampled %>%
    mutate(seq = 1:nrow(.),
           x = st_coordinates(.)[, "X"],
           y = st_coordinates(.)[, "Y"])
  
  return(osroute_xy)
}

#-------------------------------------------------------------------------------
#---------------------------------------MAX-------------------------------------
#-------------------------------------------------------------------------------
max <- filter(cats_uk, tag_id == "Max-Tag")
max_route <- cat_data(max)

#-------------------------------------------------------------------------------
#---------------------------------SMOKEY LONGNOSE-------------------------------
#-------------------------------------------------------------------------------
smokeylongnose <- filter(cats_uk, tag_id == "SmokeyLongnose-Tag")
smokeylongnose_route <- cat_data(smokeylongnose)

#-------------------------------------------------------------------------------
#--------------------------------------PANTS------------------------------------
#-------------------------------------------------------------------------------
pants <- filter(cats_uk, tag_id == "Pants-Tag")
pants_route <- cat_data(pants)

#-------------------------------------------------------------------------------
#--------------------------------------MISSY------------------------------------
#-------------------------------------------------------------------------------
missy <- filter(cats_uk, tag_id == "Missy-Tag")
missy_route <- cat_data(missy)

#-------------------------------------------------------------------------------
#---------------------------------------CJ--------------------------------------
#-------------------------------------------------------------------------------
cj <- filter(cats_uk, tag_id == "CJ-Tag")
cj_route <- cat_data(cj)

animation <- ggmap(base_map) +
  geom_point(data = max_route,
             aes(x = x, y = y),
             color = "#0094C6",
             size = 4) +
  geom_point(data = smokeylongnose_route,
             aes(x = x, y = y),
             color = "#E05263",
             size = 4) +
  geom_point(data = pants_route,
             aes(x = x, y = y),
             color = "red",
             size = 4) +
  geom_point(data = missy_route,
             aes(x = x, y = y),
             color = "#2F1847",
             size = 4) +
  geom_point(data = cj_route,
             aes(x = x, y = y),
             color = "#175676",
             size = 4) +
  theme_void() +
  transition_reveal(seq) +
  shadow_wake(wake_length = 1 / 6)

gganimate::animate(
  animation,
  # nframes = 2 * (nrow(osroute_xy) + 1),
  nframes = 2 * (sample_size + 1),
  height = 800,
  width = 760,
  fps = 10,
  renderer = gifski_renderer(loop = T)
)
gganimate::anim_save('./2023/week-05/cat_herding.gif', animation = last_animation())