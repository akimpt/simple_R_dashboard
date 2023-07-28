#data loader
library(sf)
library(tidyverse)
library(stringr)
library(rgdal)
library(janitor)

# UNZIP
for (file in list.files(path = "./../data", pattern = "*.zip")) {
  print(paste("./../data/", file, sep = ""))
  unzip(
    zipfile = paste("./../data/", file, sep = ""),
    exdir = "./../data",
    overwrite = TRUE
  )
}

# CSV
for (file in list.files(path = "./../data", pattern = "*.csv")) {
  path <- paste("./../data/", file, sep = "")
  name <-
    paste0("csv_", gsub("-", "_", gsub(" ", "_", str_to_lower(
      gsub(".csv", "", file)
    ))))
  assign(name, read.csv(path))
  assign(name, clean_names(get(name)))
}

# SHP
for (file in list.files(path = "./../data", pattern = "*.shp")) {
  path <- paste("./../data/", file, sep = "")
  name <-
    paste0("sf_", gsub("-", "_", gsub(" ", "_", str_to_lower(
      gsub(".shp", "", file)
    ))))
  assign(name, readOGR(dsn = path))
  assign(name, st_as_sf(get(name)))
  assign(name, clean_names(get(name)))
}
# re-project
the_crs <- st_crs(sf_mb_2016_qld)

for (file in ls(pattern = "sf_")) {
  assign(file, st_transform(get(file), crs = the_crs))
}
