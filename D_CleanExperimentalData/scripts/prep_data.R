rm(list = ls())

# DATA PREP

library(tidyverse)

# create x & y variables
data <-
  as_tibble(read.csv("D_CleanExperimentalData/clean_data/clean_data.csv")) %>%
  mutate(
    cell = cells - 1, # account for JavaScript indexing
    x = cell %% 8,
    y = trunc(cell / 8, 0)
  )


# find round when gem was first found
# when_gem_found <-

when_gem_found <- data %>%
  group_by(player, round) %>%
  dplyr::slice(match(1, gem)) %>%
  mutate(round_gem_found = trial) %>%
  ungroup() %>%
  select(
    unique_rounds,
    round_gem_found
  )

# join the datasets
data <- left_join(data, when_gem_found) %>%
  group_by(unique_rounds) %>%
  fill(round_gem_found, .direction = "updown") %>%
  mutate(gem_found = ifelse(round_gem_found > 0, 1, 0))

data$gem_found[is.na(data$gem_found)] <- 0

data <- data %>%
  group_by(gempresent, gem_found) %>%
  mutate(performance_group = ntile(tot_points, 3)) %>%
  mutate(performance_group_f = ifelse(
    performance_group == 1,
    "low",
    ifelse(performance_group == 2, "medium",
      "high"
    )
  ))

write.csv(data, "Data/data_coord.csv", row.names = FALSE)
write.csv(data, "select_participants_app/data_coord.csv", row.names = FALSE)
