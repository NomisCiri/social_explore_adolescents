rm(list = ls())

# DATA PREP

library(tidyverse)

# create x & y variables
data <-
  as_tibble(read.csv('D_CleanExperimentalData/clean_data/social/clean_data.csv')) %>% 
  mutate(cell = cells - 1, # account for JavaScript indexing
         x = cell%%8,
         y = trunc(cell/8,0)) 


  
# find round when gem was first found
#when_gem_found <-

when_gem_found <-  data %>%
  group_by(player, round) %>%
  dplyr::slice(match(1, gem)) %>%
  mutate(round_gem_found = trial) %>%
  ungroup()  %>%
  select(unique_rounds,
         round_gem_found)

# join the datasets
data <- left_join(data, when_gem_found) %>% 
  group_by(unique_rounds) %>% 
  fill(round_gem_found, .direction = "updown") %>% 
  mutate(gem_found = ifelse(round_gem_found > 0, 1, 0),
         social_info_use = ifelse(cell == social_info, "copy", "ignore"))

data$gem_found[is.na(data$gem_found)] <- 0



data <- data %>%
  group_by(gempresent, gem_found) %>%
  mutate(performance_group = ntile(tot_points, 3)) %>%
  mutate(performance_group_f = ifelse(
    performance_group == 1,
    "low",
    ifelse(performance_group == 2, "medium",
           "high")
  )) %>%
  ungroup() %>%
  mutate(
    social_info_type = case_when(
      soc_info_round %in% c(800, 921, 254, 1599, 1899, 408, 838) ~ 1,
      soc_info_round %in% c(1650, 504, 376, 868, 332, 1434, 1177, 1244, 468, 1639) ~ 2,
      soc_info_round %in% c(905, 1625, 1912, 335, 343, 795) ~ 3,
      TRUE ~ NaN
    ),
    social_info_factor = ifelse(
      social_info_type == 1,
      "no gem",
      ifelse(
        social_info_type == 2,
        "gem not found",
        ifelse(social_info_type == 3, "gem found", NA)
      )
    )
  )



write.csv(data, "data/social/data_social_coord.csv", row.names = FALSE)



