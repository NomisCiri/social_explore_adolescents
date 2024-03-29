###########################################################################
###########################################################################
###                                                                     ###
###                        DATA WRANGLING SCRIPT                        ###
###           CALCULATE PERFORMANCE, WHEN GEMS ARE FOUND, ETC           ###
###                                                                     ###
###########################################################################
###########################################################################

pacman::p_load(tidyverse, jsonlite)

## function to select something that IS NOT in a array/dataframe etc.
'%!in%' <- function(x,y)!('%in%'(x,y))


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
  ungroup() %>% 
  group_by(uniqueID, round) %>% 
  mutate(unique_rounds = cur_group_id())
  

# same uniqueIDs as unique participants
length(unique(data$uniqueID)) == nrow(data)/25/12

## find round when gem was first found
when_gem_found <-  data %>%
  group_by(uniqueID, round) %>%
  dplyr::slice(match(1, gem)) %>%
  mutate(round_gem_found = trial) %>%
  ungroup()  %>%
  select(unique_rounds,
         round_gem_found)

##

## join the datasets
data <- left_join(data, when_gem_found) %>% 
  group_by(unique_rounds) %>% 
  fill(round_gem_found, .direction = "updown") %>% 
  mutate(gem_found = ifelse(round_gem_found > 0, 1, 0),
         social_info_use = ifelse(cell == social_info, "copy", "ignore"),
         )

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
#never_exploit <- c(838, 2195, 1177, 1244, 468, 1639)


## is this the problem???
never_exploit_gem <- c(1177, 1244, 468, 1639)
never_exploit_no_gem <- c(838, 2195)

## give labels to rounds in the demonstrator data frame
demonstrators <- demonstrators %>%
  mutate(
    type = case_when(
      player %in% no_gem ~ "no_gem",
      player %in% gem_not_found ~ "gem_not_found",
      player %in% gem_found ~ "gem_found",
     # player %in% never_exploit ~ "never_exploit"
      
     player %in% never_exploit_gem ~ "never_exploit_gem",
     player %in% never_exploit_no_gem ~ "never_exploit_no_gem"
    ))


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

data <- data %>% 
  mutate(env_number = ifelse(gempresent == 0 & env_number == 5, env_number - 1,
             ifelse(
               gempresent == 1 & env_number < 6, env_number + 8,env_number)),
         env_number = ifelse(env_number > 5, env_number - 1, env_number))

## check there are 12 number of envs; 1:4 no gems, 5-12 gems

data %>% 
  select(env_number, gempresent, demo_type) %>% 
  distinct() %>% 
  #filter(gem == 0) %>% 
  ggplot() +
  geom_bar(aes(x = factor(env_number), fill = demo_type)) +
  facet_wrap(~gempresent)


## load bootstrapped dataset of participants playing at random
rewards_bt <-
  read_rds("A_GeneratedFiles/bootstrapped_random_rewards.rds")

# only take participants who are credibly better than random
data <- data %>% 
  ungroup() %>% 
  group_by(player) %>%
  mutate(p_value_rand = t.test(points, rewards_bt, alternative = "greater") %>%
           .$p.value) %>%
  ungroup() %>%
  dplyr::filter(p_value_rand < 0.05)

data <-
  data %>%    group_by(unique_rounds) %>%   mutate(
    mean_points = mean(points),
    sd_points = sd(points) ,
    z = (points - mean_points) / sd_points,
    gem_cell = cell[match(round_gem_found, trial)],
    #data_source = 'experiment', 
    gemlabel = ifelse(gempresent == 0, "gem absent", "gem present")   )   


# final exclusion: participants who attempted to refresh task to find where gems are, and 1 round (?) with NAs
data <- data %>%
 dplyr::filter(attempt_refresh <= 0 ) %>%
  dplyr::filter(!is.na(points)) %>%
  dplyr::filter(!is.na(gempresent)) %>%
  dplyr::filter(!is.na(demo_type))


### move this to cleaning script

data <-
  data %>%   ungroup() %>%
  group_by(unique_rounds) %>% 
  mutate(remain_gem = case_when(
    trial > round_gem_found &
      cell == gem_cell ~ 1,
    trial > round_gem_found &
      cell != gem_cell ~ 0,
    TRUE ~ NA
  )
  ) %>%   ungroup()  


## exclude rounds in which participants explored more than 3 rounds after finding the gem

problem_data <- data %>%
  #filter(remain_gem != 1) %>%  
  filter(gem_found == 1 & trial > round_gem_found) %>%
  ungroup() %>% 
  group_by(uniqueID, round) %>% 
  mutate(n_explore_after_gem = length(unique(cell)),                                            remaining_trials = 25 - round_gem_found,
         n_clicks =  n_explore_after_gem )

problem_people <- problem_data %>% 
  select(uniqueID, group, round, n_explore_after_gem) %>% 
  filter(n_explore_after_gem > 3) %>% 
  distinct() %>% 
  group_by(uniqueID, group) %>% 
  summarise(n = n())

problem_adults <- problem_people %>% 
  filter(group == 'adults') 

problem_data %>%
  select(unique_rounds, group, n_explore_after_gem) %>% 
  filter(n_explore_after_gem > 3) %>% 
  distinct() %>% 
  select(unique_rounds) %>% 
  pull() -> problem_rounds


data <- data %>% 
  ungroup() %>% 
  group_by(soc_info_round) %>% 
  mutate(gem_found_social_info = ifelse(soc_info_round == 335, 11,
                                        ifelse(soc_info_round == 795, 16,
                                               ifelse(soc_info_round == 1625, 6,
                                                      ifelse(soc_info_round == 1912, 14,
                                                             ifelse(soc_info_round == 905, 8,
                                                                    ifelse(soc_info_round == 343, 17, NA)))))),
         gem_found_how = ifelse(is.na(round_gem_found), 'not_found',
                                ifelse( round_gem_found < gem_found_social_info,
           'alone',
           ifelse(
             round_gem_found >= gem_found_social_info,
             'copier', ""
           )
         ))) %>%
  filter(unique_rounds %!in% problem_rounds) %>% 
  mutate(demo_quality = if_else(demo_type == "gem_found", "best",
                                ifelse((demo_type == "gem_not_found" | demo_type == "no_gem"), "medium", "worst"))) 


  ## save dataset
write.csv(data, "data/social/data_social_all_participants.csv", row.names = FALSE)



