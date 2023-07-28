install.packages("owmr")
library(owmr) # couldn't work out how to call historical data but the city list is handy for getting coordinates for the next
Sys.setenv(OWM_API_KEY = "495acdc48cdcf3e4412f8ebc682fd54a")
current.weather <- get_current("Brisbane, AU", units = "metric") %>% owmr_as_tibble()
city.list <- owmr::owm_cities

install.packages("darksky") # easy enough to make up to 1000 calls a day however you need to use coordinates  
library(darksky)
Sys.setenv(darksky_api_key = "9a1524293a780190a26729173210697d")
historical <- get_forecast_for(-27.46794, 153.02809, "2013-05-06T12:00:00-0400", exclude = "currently,minutely,hourly,alerts,flags") %>% as.data.frame()