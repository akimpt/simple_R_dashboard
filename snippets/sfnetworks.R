install.packages(c("dplyr", "ggplot2", "sfnetworks", "tmap", "osmdata"))

library(osmdata)

# build the query
we_foot <- opq("west end, meanjin") %>% 
  add_osm_features(features = c('"highway"="footway"',
                                '"highway"="steps"',
                                '"foot"="yes"',
                                '"highway"="living_street"')) %>% 
  osmdata_sf()

names(we_foot)

we_foot_lines <- we_foot$osm_lines

library(sf)
we_foot_lines %>% 
  # st_crop(west_end_sf) %>% 
  st_geometry() %>% 
  plot()