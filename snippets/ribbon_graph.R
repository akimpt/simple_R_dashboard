library(tidyverse)
library(ggh4x)

df <- data.frame(
  x = 1:100,
  y = cumsum(rnorm(100)),
  z = cumsum(rnorm(100)),
  s = "NSW")

df <- data.frame(
  x = 1:100,
  y = cumsum(rnorm(100)),
  z = cumsum(rnorm(100)),
  s = "Vic") %>% 
  rbind(df)

df <- data.frame(
  x = 1:100,
  y = cumsum(rnorm(100)),
  z = cumsum(rnorm(100)),
  s = "Qld") %>% 
  rbind(df)

df <- data.frame(
  x = 1:100,
  y = cumsum(rnorm(100)),
  z = cumsum(rnorm(100)),
  s = "etc.") %>% 
  rbind(df) %>% 
  mutate(s = ordered(s, c("NSW", "Vic", "Qld", "etc.")))

ggplot(df, aes(x = x)) +
  facet_wrap(vars(s)) +
  stat_difference(aes(ymin = y, ymax = z), alpha = 0.3) +
  geom_line(aes(y = y, colour = "census")) +
  geom_line(aes(y = z, colour = "address+traveller"))
