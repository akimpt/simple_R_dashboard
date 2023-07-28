# data cleaner
library(tmaptools)
library(stringr)
library(tidyverse)
library(janitor)
library(sf)
library(data.table)
library(lubridate)

clip.range <- 20 #we don't want data beyond 20km from central station

# check objects in environment
if (exists("raw_2011census_b29_aust_sa2_short")) {
  print("All the necessary objects available in environment.")
  } else {
    print("Need to run the read script...")
    source("./1-load.R")
    print("...all the necessary objects now available in environment.")
    }

ls()

palette <- RColorBrewer::brewer.pal(12, 'Set3')
#display.brewer.all()

# combining pipes and ggplot with curley braces
mymap <- . %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  {tm_shape(.) +
      tm_markers()}



# common coding
sf1 <- shp_file %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(inner_cities)) %>% # reproject to match another file
  filter(LGA_NAME16 == "Sydney (C)" | LGA_NAME16 == "Melbourne (C)") %>% #or selectors
  st_cast("POLYGON") %>% #separating multipolygons
  slice(3:n()) %>%
  replace_na(list(type = "other",
                  residential_bays = 0)) %>%
  plyr::rbind.fill(parking_meters) # rbind when different column numbers
# st_centroid(.)
# st_as_sf(coords = c("X", "Y"), crs = st_crs(inner_cities))
cbind(., apply(st_distance(., central_stations, by_element = FALSE), 1, FUN=min)) %>%
  rename("km_from_central" = names(.)[length(names(.))-1]) %>%
  mutate(km_from_central = km_from_central/1000) %>%
  filter(km_from_central < clip.range) %>%
  group_by(id, state, source) %>%
  summarise(pnr_bays = sum(as.numeric(pnr_bays)),
            X = mean(X),
            Y = mean(Y)) %>%
  ungroup() %>%
  rename("new_name" = "old_name") %>%
  clean_names() %>%
  na_if("") %>%
  tidyr::fill(sa2, bedd) %>% #fill downwards
  mutate(cars = stringr::str_extract_all(vehd, "[0-9]+"), # extract numbers
                  cars = ifelse(cars == "character(0)", "0", cars),
         vehicle_2011 = ifelse(grepl("car|motorbike|taxi", mtwp), 1, 0), #contains any of string
         mtwp = stringr::str_to_lower(mtwp), # to lowercase
         vehicle_2016 = ifelse(inner_city_workers_2016 < 10, NA,
                               round(vehicle_2016/inner_city_workers_2016*100,1)), #conditional NAs
         licence_plate = toupper(licence_plate), #to upper case
         licence_plate = gsub("\\?", " ", licence_plate), #replace special character
         licence_plate = gsub("VIC", "", licence_plate, fixed = TRUE), # replace character
         licence_plate = trimws(licence_plate), #trime whitespace
         timestamp = dmy_hm(timestamp), #encode timestamp
         ans_q4_parking_preference = ifelse(ans_q1_car == "No" | ans_q1_car_passenger == "Yes", NA,
                                            q4_if_you_needed_to_park_what_factors_influenced_your_parking_location),
         ans_q14_date = dmy(date),
         ans_q15_time = hms(q16_what_is_the_current_time),
         ans_q14_date = wday(ans_q14_date, label = TRUE, abbr = FALSE))  %>%
  st_join(., inner_cities) # defaults to intersects
mutate_if(is.character, factor) %>%
  st_buffer(300) %>%
  st_intersection(sf1a) %>%
  mutate(greenspace_ha = as.numeric(st_area(.))/1000)

meters.from.central <- st_distance(rapid.transit.node, central.station, by_element = TRUE) # fast distacne from single feature

statistical.area.one <- statistical.area.one[st_intersects(statistical.area.one, local.government.area, sparse = FALSE),] # select the intersecting

rename_at(.vars = vars(ends_with("_services")),
          .funs = funs(sub("_services", "", .))) %>%
  rename_all(function(x) paste0("services_", x)) %>%
  arrange(street_marker, year, whour, count) %>%

  car.free.dwelling <- statistical.area.one %>% st_sample(size = statistical.area.one$car.free.dwellings) # gen random points inside

group_by(lot_plan) %>%
  summarise(count = n())
st_join(sf1, join = st_nearest_feature)

  reside <- geocode_OSM(reside$ans_q9_reside, as.sf = TRUE) # geocoding

pivot_longer(!timestamp, names_to = "parking_bay", values_to = "licence_plate") %>%

saveRDS(SA2_data, file = "../outputs/SA2_data.RData") # export RData

st_write(sf1, "../outputs/sf1.shp", driver="ESRI Shapefile", delete_layer =TRUE) # it'll crop longer names

write.csv(unique_licence_plates, file = "../unique_licence_plates.csv") #writing csv

ggplot(data = polygons) +
  geom_sf(fill = "grey90", color = "grey70") +
  geom_sf(data = centroids, aes(size = older.percent), fill = col1, colour = col1, alpha = 0.5) + theme_void()

my.static <- function(state, var, colour){
  temp <- SA2_JTW %>%
    select(!!rlang::sym(var), ORIGIN_X, ORIGIN_Y, DESTINATION_X, DESTINATION_Y, ORIGIN_STATE_NAME, DESTINATION_STATE_NAME) %>%
    filter(!!rlang::sym(var) > 4, ORIGIN_STATE_NAME == state, DESTINATION_STATE_NAME == state)

knitr::include_graphics("./tab1.png")

# manual file
central_stations <- data.frame(
  id = "Central Station",
  longitude = c(153.0261831, 151.2069512, 144.9617319),
  latitude = c(-27.466224, -33.8831733, -37.8109836)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(inner_cities))
