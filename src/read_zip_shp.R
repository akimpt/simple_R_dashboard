#For reading shapefiles from zip files
read_zip_shp <- function(x, y, the_crs = 4326) {
  print(basename(x))
  
  the_dir <- paste0(tempdir(), "/", gsub(".zip", "", basename(x)))
  
  print(the_dir)
  
  unzip(zipfile = x,
        exdir = the_dir,
        overwrite = TRUE)
  
  sf1 <- st_read(dsn = paste0(the_dir, "/", y)) %>%
    st_as_sf() %>%
    clean_names() %>%
    filter(!st_is_empty(.)) %>%
    st_transform(crs = the_crs)
  
  return(sf1)
}