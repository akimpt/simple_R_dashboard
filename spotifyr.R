#https://www.rcharlie.com/spotifyr/
library(spotifyr)
library(tidyverse)
library(knitr)

Sys.setenv(SPOTIFY_CLIENT_ID = '28f5a7ce72094cb1be14909aa8f11d82')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd208f13cc6dc4476b2a32ba948276726')

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles')

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5)

#https://musicovery.com/api/V5/doc/documentation.php

