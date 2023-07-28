#data loader
library(stringr)
library(rgdal)
library(janitor)

#functions
csv.2.sf <- function(csv.file) {
  csv.file %>%
    clean_names() %>%
    as_tibble() %>%
    filter(longitude != "NA", latitude != "NA") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(inner_cities), remove = FALSE)
}
point.shp.2.sf <- function(shp.file) {
  shp.file %>%
    st_as_sf() %>%
    st_transform(st_crs(inner_cities)) %>%
    clean_names()
}

poly.shp.2.sf <- function(shp.file) {
  shp.file %>%
    st_as_sf() %>%
    st_transform(st_crs(inner_cities)) %>%
    st_buffer(0.000000001) %>% #fixes bowties
    clean_names()
}