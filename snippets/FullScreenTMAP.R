# Create an interactive map of UQ St Lucia

# for this process to work, use the latest master
# of the osmdata package:
# remotes::install_github("ropensci/osmdata")
# or
# devtools::install_github("ropensci/osmdata")

### Buildings ----

library(osmdata)
# download buildings in (and around) UQ St Lucia

# get bounding box
uqbb <- getbb("University of Queensland",
            featuretype = "amenity:university",
            format_out = "sf_polygon",
            silent = FALSE)
# featuretype is more restricted in osmdata <= 0.1.1
# install latest master to have unrestricted options
# (as described in comments above)

library(sf) # needed for trim function
# get buildings
uq <- uqbb %>% 
  # query bounding box is rectangular even though a "polygon" format is used...
  opq() %>% 
  add_osm_feature(key = "building") %>% 
  osmdata_sf() %>% 
  # ... but here, only keep buildings inside the UQ polygon:
  trim_osmdata(uqbb)

# get building polygons out
library(dplyr)
poly <- uq$osm_polygons %>% 
  filter(!is.na(building))
# get multipolygons out
mp <- uq$osm_multipolygons

# remove multipolygon geometry names so it can display on interactive maps
# see this bug: https://github.com/rstudio/leaflet/issues/631
for(i in seq(nrow(mp))) {
  names(mp$geometry[[i]][[1]]) = NULL 
}

# load package to create thematic maps
library(tmap)

# show both polygons and multipolygons
tm_shape(poly) +
  tm_polygons("building") +
tm_shape(mp) +
  tm_polygons("building")

# merge all buildings to apply same style to all
buildings <- st_as_sf(data.table::rbindlist(list(poly, st_cast(mp, "POLYGON")), fill = TRUE))
# ref for this method:
# https://github.com/r-spatial/sf/issues/798#issuecomment-405157853

# save a copy if needed
# st_write(buildings, "buildings.geojson")
# read it
# buildings <- st_read("buildings.geojson")

# interactive map
tmap_mode("view")
tm_shape(buildings) +
  tm_polygons("building",
              id = "name", # what is in hovering tooltip
              popup.vars = c("alt_name", "ref")) + # shown when clicked
  # replace default basemap:
  tm_basemap(server = "CartoDB.PositronNoLabels")

# see list of basemap providers, to change background in
# view mode:
leaflet::providers
# can also find them online:
# https://leaflet-extras.github.io/leaflet-providers/preview/

### Bicycle parking ----

# get bicycle parking
bp <- getbb("University of Queensland", format_out = "polygon",
            featuretype = "amenity:university") %>% 
  opq() %>% 
  add_osm_feature(key = "amenity", value = "bicycle_parking") %>% 
  osmdata_sf() %>% 
  .$osm_points %>% 
  st_intersection(uqbb) # alternative to trim_osmdata()

# capacity is stored as string; needs converting
bp$capacity <- as.numeric(bp$capacity)

# write it if needed
# st_write(bp, "bp.geojson")
# read it
# bp <- st_read("bp.geojson")

# layer bicycle parking on top of buildings
uq_map <- tm_basemap(server = "CartoDB.PositronNoLabels") +
  tm_shape(buildings) +
  tm_polygons("building",
              id = "name",
              popup.vars = c("alt_name", "ref")) +
tm_shape(bp) +
  tm_dots(col = "dodgerblue",
          size = "capacity", # size of circle prop. to capacity
          perceptual = TRUE, # more readable size calculation
          alpha = 0.5, # some transparency
          id = "bicycle_parking",
          popup.vars = c("capacity", "covered")) +
tm_shape(uqbb) + # add shape of St Lucia campus grounds
  tm_polygons(alpha = 0) + # no fill
  # and add a title:
  tm_layout(title = "UQ St Lucia's buildings and bicycle parking")

# view it
uq_map

# export interactive map as HTML
tmap_save(uq_map, "uq_map.html")
# great to send to others! Very portable!

# Example of OSM editing helper: buildings with no names ----

# here, we create a static map that can help
# figure out what data is missing from the OSM database

# first, fetch a background:
# 1. get bounding box
bb <- getbb("University of Queensland", featuretype = "amenity:university")
# 2. download raster
library(tmaptools)
bg <- read_osm(bb) # need the package OpenStreetMap for this to work

# then, add buildings on top, colouring them in red if the
# building name is missing
tmap_mode("plot")
tm_shape(bg) +
  tm_rgb() +
buildings %>% 
  mutate(no.name = is.na(name)) %>% 
  tm_shape() +
    tm_polygons("no.name", palette = c("grey", "red"))
# print that, and go into the field to add data to OSM!