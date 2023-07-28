library(GADMTools)

sf3 <- gadm_sf_loadCountries("AUS", 1)

df2 <- df1 %>%
  mutate(longitude = id_poa_long,
         latitude = id_poa_lat) %>%
  select(longitude, latitude, support_dst) %>%
  as_data_frame()

isopleth(sf3, data = df2, palette = "PuOr")

df2 <- SURVEY %>%
  select(support_dst, id_poa_long, id_poa_lat) %>%
  mutate(support_dst = as.numeric(support_dst)-1) %>%
  mutate(longitude = id_poa_long,
         latitude = id_poa_lat) %>%
  select(longitude, latitude, support_dst) %>%
  as_data_frame()

isopleth(sf3, data = df2, palette = "PuOr")