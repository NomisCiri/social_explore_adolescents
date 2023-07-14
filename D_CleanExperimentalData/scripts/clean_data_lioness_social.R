rm(list = ls())

#### CLEAN AND MERGE DATASETS ####

pacman::p_load(tidyverse)


library(tidyverse)
library(dplyr)
library(tibble)
library(readxl)

source('D_CleanExperimentalData/scripts/readLIONESS.R')

folder <- 'D_CleanExperimentalData/raw_data/social/lioness/'
writinglocation <- 'D_CleanExperimentalData/raw_data/social/'
periods <- 12 # how many max periods in this 

# function input: 
data <- readLIONESS(folder, writinglocation, periods) %>% 
  select(-c(gender,age))

# remove first 16 participants that come from prolific dataset
data_adults <-  data[(1:180),]

  # remove first 16 participants that come from prolific dataset
  
data <- data[-c(1:180),]

# importa datasets with demographics
demographics <- read.csv('data/social/clean_data_demographics.csv')

# merge datasets

# get match of playerNr and participant Nr

linking_data <- read.csv('D_CleanExperimentalData/raw_data/social/2-session-raw.csv') %>% 
  select(playerNr, externalID)


data_with_id <- left_join(data, linking_data, by="playerNr") %>% 
  mutate(participantNr = externalID)

# how many participants finished the experiments
length(unique(data_with_id$playerNr))

# how many participants finished the experiments and have an id
length(unique(data_with_id$participantNr))


#### Who is missing? 

finished <- unique(data_with_id$participantNr)
all <- unique(demographics$participantNr)

not_included <- all[!all %in% finished]

# 
# 
# # find players for whom the ID was not saved automatically and was entered manually during experiment 
# # (therefore it is saved)
# 
# missing_players <- data$playerNr[which(!is.na(data$participantNr))]
# missing_players_IDs <- data$participantNr[which(!is.na(data$participantNr))]
# 
# # for (n in 1:length(missing_players)){
# #   
# #   # replace missing players IDs with appropriate ID
# #   data_with_id$participantNr[data_with_id$playerNr == missing_players[n]] <- missing_players_IDs[n]
# #   
# # }
# 
# # check if substitution worked
# which(is.na(data_with_id$participantNr))

# change ID to participant who made mistake
data_with_id$participantNr[data_with_id$participantNr == 30523] <- 30623 

data_full <- left_join(data_with_id, demographics, by="participantNr")

# check if any missing values
data_full %>% 
  select(participantNr, gender, age) %>% 
  filter_all(any_vars(is.na(.))) 


#remove rounds where clicked where not saved
data_full <- data_full %>% 
  filter(!is.na(clickedCells) & !is.na(socialInfo))


## REPLACE WITH APPLY WHWN WILL BECOME BIG

data_long <- data.frame(player = numeric(0),
                        points = numeric(0),
                        cells = numeric(0),
                        social_info = numeric(0),
                        unique_rounds = numeric(0),
                        round = numeric(0),
                        env_number = numeric(0),
                        gem_present = numeric(0),
                        tot_points = numeric(0),
                        age = numeric(0),
                        gender = numeric(0),
                        soc_info_round = numeric(0)
                        )

for (n in 1:length(unique(data_full$playerNr.x))){
  
  # create empty df
  playerMat <- data.frame()
  
  # select current player
  one_player_data <- data_full %>% 
      filter(playerNr.x == unique(data_full$playerNr.x)[n])
  

  for (t in 1:nrow(one_player_data)){

    # extract relevant variables from data_frame
    points <-  as.numeric(unlist(str_split(one_player_data$points[t], ",")))
    cells <-  as.numeric(unlist(str_split(one_player_data$clickedCells[t], ",")))
    social_info <-  as.numeric(unlist(str_split(one_player_data$socialInfo[t], ",")))
    unique_rounds <-  one_player_data$thisRound[t]
    env_number <- one_player_data$thisEnv[t]
    gempresent <- one_player_data$gemPresent[t]
    age <- one_player_data$age[t]
    gender <- one_player_data$gender[t]
    soc_info_round <- one_player_data$thisRound[t]
    participantNr <- one_player_data$participantNr[t]

    # save them into temporary data frame
    temp <- data.frame(
      player = rep(n, length(points)),
      points = points,
      cells = cells,
      social_info = social_info,
      unique_rounds = rep(unique_rounds, length(points)),
      env_number = rep(env_number, length(points)),
      age = rep(age, length(points)),
      gender = rep(gender, length(points)),
      soc_info_round = rep(soc_info_round, length(points)),
      participantNr = rep(participantNr, length(points)),
      gempresent = rep(gempresent, length(points)),
      round = rep(t, length(points)),
      tot_points = rep(one_player_data$totalPoints[t], length(points))
    )
    
    # if (nrow( temp %>% 
    #           filter_all(any_vars(is.na(.))) ) > 0){
    #   print(paste('problem with playerNr', temp$player))
    # }
    # 
    # 
    
    playerMat <- rbind(playerMat, temp)
  }
  
    data_long <- rbind(data_long, playerMat)
}

# inspect data that comes out with some NAs
problem_data <- data_long %>% filter_all(any_vars(is.na(.))) 

# sanity checks
unique(data_long$tot_points)
length(unique(data_long$player)) ## should be == to people that participated
sum(data_long$gempresent,na.rm = TRUE) ## should be 3/4 of tot rows 

# save raw decisions dataset
write.csv(data_full, 'D_CleanExperimentalData/raw_data/social/raw_data_decisions.csv', row.names = FALSE)

# create unique id rounds
data_long <- data_long %>% 
  mutate(gem = ifelse(points > 200, 1, 0)) %>% 
  group_by(player, round) %>%
  mutate(trial = 1:25,
         unique_rounds = cur_group_id()) 

# save clean dataset
write.csv(data_long, 'D_CleanExperimentalData/clean_data/social/clean_data.csv', row.names = FALSE)
