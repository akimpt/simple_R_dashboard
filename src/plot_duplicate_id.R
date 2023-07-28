library(tidyverse)
library(ggplot2)

plot_dup_id <- function(the_file, the_var) {
  df1 <- thefile %>% 
    select({{thevar}}) %>% 
    group_by({{thevar}}) %>% 
    mutate(n = n())
  
  qplot(df1$n)
}