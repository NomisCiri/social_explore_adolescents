rm(list = ls())

# DATA PREP

library(tidyverse)

# create x & y variables
data <-
  as_tibble(read.csv('D_CleanExperimentalData/clean_data/clean_data.csv')) %>% 
  mutate(cell = cells - 1, # account for JavaScript indexing
         x = cell%%8,
         y = trunc(cell/8,0)) 


# find round when gem was first found
#when_gem_found <-

when_gem_found <-   data %>%
  group_by(player, round) %>%
  dplyr::slice(match(1, gem)) %>%
  mutate(round_gem_found = trial) %>%
  ungroup() %>%
  mutate(performance_group = ntile(tot_points, 3)) %>%
  
  
  mutate(performance_group_f = ifelse(
    performance_group == 1,
    "low",
    ifelse(performance_group == 2, "medium",
           "high")
  )) %>%
  select(unique_rounds,
         round_gem_found,
         performance_group_f,
         performance_group)



# join the datasets
data <- left_join(data, when_gem_found) %>% 
  group_by(unique_rounds) %>% 
  fill(round_gem_found, .direction = "updown") %>% 
  fill(performance_group, .direction = "updown") %>% 
  fill(performance_group_f, .direction = "updown")


write.csv(data, "Data/data_coord.csv", row.names = FALSE)
 
