library(tidyverse)

starwars <- starwars %>% 
  select(name, hair_color, skin_color, gender, species)

set.seed(1234)

foo <- starwars %>%
  slice_sample(n=50, replace = FALSE) %>% 
  mutate(eye_witness = "Foo")

dput(eye_witness1)

set.seed(4321)

bar <- starwars %>%
  slice_sample(n=50, replace = FALSE) %>% 
  mutate(eye_witness = "Bar")

evidence <- rbind(foo, bar) 
  



