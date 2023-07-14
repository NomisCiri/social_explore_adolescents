###########################################################################
###########################################################################
###                                                                     ###
###                        DATA WRANGLING SCRIPT                        ###
###           CALCULATE PERFORMANCE, WHEN GEMS ARE FOUND, ETC           ###
###                                                                     ###
###########################################################################
###########################################################################

pacman::p_load(tidyverse)

## create x & y variables
data_adolescents <-
  as_tibble(read.csv('D_CleanExperimentalData/adolescents_data/clean_data/social/clean_data.csv')) %>% 
  mutate(cell = cells - 1, # account for JavaScript indexing
         x = cell %% 8,
         y = trunc(cell/8,0),
         group = 'adolescents') 

## create x & y variables
data_adults <-
  as_tibble(read.csv('D_CleanExperimentalData/adults_data/clean_data/social/clean_data.csv')) %>% 
  mutate(cell = cells - 1, # account for JavaScript indexing
         x = cell %% 8,
         y = trunc(cell/8,0),
         group = 'adults')

## merge datasets
data <- bind_rows(data_adolescents, data_adults) %>% 
  ungroup() %>% 
  group_by(player, group) %>% 
  mutate(uniqueID = cur_group_id()) %>% 
  ungroup()

# same uniqueIDs as unique participants
length(unique(data$uniqueID)) == nrow(data)/25/12

## find round when gem was first found
when_gem_found <-  data %>%
  group_by(player, round) %>%
  dplyr::slice(match(1, gem)) %>%
  mutate(round_gem_found = trial) %>%
  ungroup()  %>%
  select(unique_rounds,
         round_gem_found)

## join the datasets
data <- left_join(data, when_gem_found) %>% 
  group_by(unique_rounds) %>% 
  fill(round_gem_found, .direction = "updown") %>% 
  mutate(gem_found = ifelse(round_gem_found > 0, 1, 0),
         social_info_use = ifelse(cell == social_info, "copy", "ignore"))

data$gem_found[is.na(data$gem_found)] <- 0

## calculate performance, add labels of social info rounds
data <- data %>%
  group_by(gempresent, gem_found) %>%
  mutate(performance_group = ntile(tot_points, 3)) %>%
  mutate(performance_group_f = ifelse(
    performance_group == 1,
    "low",
    ifelse(performance_group == 2, "medium",
           "high")
  )) %>%
  ungroup()

#### add info about demonstrators

SI_path <- "./E_Develop_Social_Version/rounds_social_info/1_peer/social_info_json/"

## load jsons with the selected social information and concat them in dataframe.
## double check if the sequence is correct (how?)
## figure out which are supposed to be rotated. makes no difference for the simulation
demonstrators <- list.files(SI_path) %>%
  purrr::map_dfr(., ~ {
    tibble(
      choices = read_json(paste0(SI_path, .))$cells %>% unlist(),
      env = read_json(paste0(SI_path, .))$envNr %>% unlist(),
      player = read_json(paste0(SI_path, .))$roundNr %>% unlist(),
      gem = read_json(paste0(SI_path, .))$gempresent %>% unlist()
    )
  })

demonstrators %>% 
  select(player, env, gem) %>% 
  distinct() %>% 
  #filter(gem == 0) %>% 
  ggplot() +
  geom_bar(aes(x = factor(env)))


## give labels to unique identifying rounds
no_gem <- c(800, 921, 254, 1599, 1899, 408)
gem_not_found <- c(1650, 504, 376, 868, 332, 1434)
gem_found <- c(905, 1625, 1912, 335, 343, 795)
never_exploit <- c(838, 2195, 1177, 1244, 468, 1639)

## give labels to rounds in the demonstrator data frame
demonstrators <- demonstrators %>%
  mutate(
    type = case_when(
      player %in% no_gem ~ "no_gem",
      player %in% gem_not_found ~ "gem_not_found",
      player %in% gem_found ~ "gem_found",
      player %in% never_exploit ~ "never_exploit"
    ),
    env = ifelse(gem == 0 & env == 5, env - 1,
                 ifelse(
                   gem == 1 & env < 6, env + 8,env)),
    env = ifelse(env > 5, env - 1, env))

## check there are 12 number of envs; 1:4 no gems, 5-12 gems

demonstrators %>% 
  select(player, env, gem, type) %>% 
  distinct() %>% 
  #filter(gem == 0) %>% 
  ggplot() +
  geom_bar(aes(x = factor(env), fill = type)) +
  facet_wrap(~gem)


#### Add demonstrator data in social data

## new data in social data
data$demonstrator <- NA
data$demo_type <- NA

## loop through participants
for (p in unique(data$uniqueID)) {
  ## outerloop
  for (r in unique(data[data$uniqueID == p, ]$round)) {
    ## get demonstrator for participant
    demo <- data[data$round == r &
                          data$uniqueID == p, ]$social_info
    for (d in unique(demonstrators$player)) {
      ## loop through demonstrators
      c <- demonstrators[demonstrators$player == d, ]$choices
      type <- demonstrators[demonstrators$player == d, ]$type %>% unique()
      
      if (setequal(demo, c)) {
        ## if demonstrators and social info seen by participant match
        data[data$round == r &
                      data$uniqueID == p, ]$demonstrator <- d
        data[data$round == r &
                      data$uniqueID == p, ]$demo_type <- type
      } else {
        ## do nothing
      }
    }
  }
}

## save dataset
write.csv(data, "data/social/data_social_all_participants.csv", row.names = FALSE)



