#https://geospatial-community.netlify.app/post/2022-03-31-spatial-networks/
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

library(dplyr)
we_foot_lines <- we_foot_lines %>% 
  filter(is.na(route))

we_foot$osm_polygons %>% 
  st_geometry() %>% 
  plot()

# cast polygons to lines
poly_to_lines <- st_cast(we_foot$osm_polygons, "LINESTRING")
# bind all lines together
library(dplyr)
we_foot_lines <- bind_rows(we_foot_lines, poly_to_lines)

# plot it
we_foot_lines %>% 
  st_geometry() %>% # this function pulls out just the geometry so that it does not default to a panel for each var
  plot()

library(sfnetworks)
foot_net <- as_sfnetwork(we_foot_lines, directed = FALSE)
plot(foot_net)

st_crs(foot_net)

foot_net <- st_transform(foot_net, 7856)

library(tidygraph)
foot_simple <- convert(foot_net, to_spatial_smooth)
plot(foot_simple)

foot_net <- convert(foot_net, to_spatial_subdivision) # this takes care of ring geometries and lines running over nodes etc.
# convert is a function from tidygraph that is specifically for this set of objects that includes sf files

foot_net <- foot_net %>% 
  activate("edges") %>% 
  mutate(weight = edge_length())

library(ggplot2)
ggplot() +
  geom_sf(data = st_as_sf(foot_net, "edges"),
          mapping = aes(colour = as.numeric(weight))) +
  labs(colour = "Edge length (m)")

library(tmap)
tmap_mode("view") # set to interactive mode
tm_tiles("CartoDB.Positron") +
  tm_shape(st_as_sf(foot_net, "edges")) +
  tm_lines(col = "footway", palette = "Accent", colorNA = "red") +
  tm_shape(st_as_sf(foot_net, "nodes")) +
  tm_dots()

foot_net <- foot_net %>% 
  activate(nodes) %>% 
  mutate(neighbourhood = local_size(order = 6)) %>% 
  filter(neighbourhood > 5)

kurilpa_lib <- opq_osm_id(id = 523925261, type = "way") %>% 
  osmdata_sf() %>% 
  .$osm_polygons %>% 
  st_centroid() %>% 
  st_set_crs(4326) %>% # (if the following step generate GDAL error)
  st_transform(crs = 7856)

foot_net <- activate(foot_net, "nodes")
iso <- foot_net %>%
  dplyr::filter(node_distance_from(st_nearest_feature(kurilpa_lib, foot_net), weights = as.numeric(weight)) <= 1000)

iso_poly <- iso %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()
plot(foot_net, col = "grey")
plot(iso_poly, col = NA, border = "black", lwd = 3, add = TRUE)
plot(iso, col = "lightgreen", add = TRUE)
plot(kurilpa_lib, col = "red", pch = 8, cex = 2, lwd = 2, add = TRUE)

# get the location of the Sailing Club's entrance
sailing_club <- opq_osm_id(id = 7622867925, type = "node") %>% 
  osmdata_sf() %>% 
  .$osm_points %>%
  st_set_crs(4326) %>% # (if the following step generate GDAL error)
  st_transform(crs = 7856)
# calculate the shortest path
shortest <- foot_net %>% 
  activate(edges) %>% 
  st_network_paths(from = sailing_club, to = kurilpa_lib)
# extract the node IDs
node_path <- shortest %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()
# only keep the network for these nodeIDs
path_sf <- foot_net %>% 
  activate(nodes) %>% 
  slice(node_path) %>% 
  st_as_sf("edges")
# visualise
tm_tiles("CartoDB.Positron") +
  tm_shape(path_sf) +
  tm_lines(col = "red") +
  tm_shape(sailing_club) +
  tm_dots(col = "blue", size = 0.1) +
  tm_shape(kurilpa_lib) +
  tm_dots(col = "green", size = 0.1)