rm(list = ls())

# DATA PREP

library(tidyverse)

# create variables

data <-
  as_tibble(read.csv('D_CleanExperimentalData/clean_data/clean_data.csv')) %>% 
  mutate(cell = cells - 1,
         x = cell%%8,
         y = trunc(cell/8,0)) 

write.csv(data, "Data/data_coord.csv", row.names = FALSE)
