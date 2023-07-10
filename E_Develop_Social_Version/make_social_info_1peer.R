### Script to make social info file for specific environments.

### This works for the simplified version of the task, with one peer

# load packages
pacman::p_load(tidyverse, rjson, here, jsonlite)

# load dataset
explore_data <- read_csv(file = paste0(here(), "/data/data_coord.csv"))

# add unique rounds id
explore_data <- explore_data %>%
  filter(player != 188) %>%
  group_by(player, round) %>%
  mutate(
    trial = 1:25,
    unique_rounds = cur_group_id()
  )

# Here write all the rounds that should be selected

preselected_rounds <- c(
  800, 921,
  254, 1599,
  1899, 408,
  1650, 504,
  376, 868,
  332, 1434,
  905, 1625,
  1912, 335, # env 7
  343, 795,
  838, 2195,
  1177, 1244, # env 8
  468, 1639
) # env 9

### make this more elegant in the future

explore_data$env_number[explore_data$unique_rounds == 1912 | explore_data$unique_rounds == 335] <- 7
explore_data$env_number[explore_data$unique_rounds == 1177 | explore_data$unique_rounds == 1244] <- 8
explore_data$env_number[explore_data$unique_rounds == 468 | explore_data$unique_rounds == 1639] <- 9


# have a look of env that have been selected
<<<<<<< HEAD
info_social_rounds <- explore_data %>%
  ungroup() %>%
  filter(unique_rounds %in% preselected_rounds) %>%
  select(env_number, gempresent, gem_found, round_gem_found, performance_group_f, unique_rounds) %>%
=======
info_social_rounds <- explore_data %>% 
  ungroup() %>% 
  filter(unique_rounds %in% preselected_rounds) %>% 
  select(env_number, gempresent, gem_found, round_gem_found, performance_group_f, unique_rounds, player, cells, cell) %>% 
>>>>>>> bd0218473428c9a92ae4d9f8c33ec4004b59eb7c
  distinct()

# when are the gem found in the social trials?
ggplot(data = info_social_rounds) +
  geom_histogram(aes(x = round_gem_found))


# which environments are being used?
ggplot(data = info_social_rounds) +
  geom_histogram(aes(x = env_number)) +
  facet_wrap(~gempresent)

list_info <- list()
list_info_all <- list()


# make matrix with indices
index_matrix <- matrix(1:64, nrow = 8, ncol = 8) # %>%as.array()
# rotate function
rotate <- function(x) {
  t(apply(x, 2, rev))
}

# save rotated index in array
order <- index_matrix %>%
  rotate() %>%
  as.numeric()

# subset the data to have info about round number, env number, and decisions in those rounds
for (n in 1:length(preselected_rounds)) {
  current_round <- preselected_rounds[n]
  df <- explore_data %>% 
  filter(unique_rounds == current_round) %>% 
    ungroup() %>% 
    select(gempresent, gem,unique_rounds, cells,cell, env_number) 
  
  df
  
  ######## remove if it goes wrong
  if(df$env_number[1] >6){
    
    df <- df %>% 
      mutate(cells = order[cells+1])
    df$cells[is.na(df$cells)] <- 1
    
  }
  ########

  # make list of lists [] with all the info for each round
  list_info[1] <- df$env_number[1]
  list_info[[2]] <- df$cells
  list_info[3] <- df$unique_rounds[1]
  list_info[4] <- df$gempresent[1]


  # give names that will be reflected in the JSON
  names(list_info) <- c("envNr", "cells", "roundNr", "gempresent")

  list_info_all[[n]] <- list_info
  list_info <- list()
}

## stucture of the json file: env nr, social_info,
# Create folder called social_info_json

if (!dir.exists("E_Develop_Social_Version/rounds_social_info/1_peer") == TRUE) {
  dir.create("E_Develop_Social_Version/rounds_social_info/1_peer")
}

## Create folder called social_info_json

if (!dir.exists("E_Develop_Social_Version/rounds_social_info/1_peer/social_info_json") == TRUE) {
  dir.create("E_Develop_Social_Version/rounds_social_info/1_peer/social_info_json")
}


# write and save jsons files
for (n in 1:length(list_info_all)) {
  social_infoJSON <- toJSON(list_info_all[[n]][1:4],
    pretty = TRUE, auto_unbox = TRUE,
  )
  json_name <-
    paste("round",
      (list_info_all[[n]][3]),
      sep = "_"
    )
  write(
    social_infoJSON,
    paste0(
      "E_Develop_Social_Version/rounds_social_info/1_peer/social_info_json/",
      json_name,
      ".json"
    )
  )
}
