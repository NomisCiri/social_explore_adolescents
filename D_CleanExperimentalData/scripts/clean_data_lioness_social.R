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
data <- readLIONESS(folder, writinglocation, periods)

# calculate S and substitute playerNr with ID

## REPLACE WITH APPLY WHWN WILL BECOME BIG

data_long <- data.frame(player = numeric(0),
                        points = numeric(0),
                        cells = numeric(0),
                        social_info = numeric(0),
                        unique_rounds = numeric(0),
                        round = numeric(0),
                        env_number = numeric(0),
                        gem_present = numeric(0),
                        tot_points = numeric(0))

for (n in 1:length(unique(data$playerNr))){
  
  # create empty df
  playerMat <- data.frame()
  
  # select current player
  one_player_data <- data %>% 
    
    filter(playerNr == unique(data$playerNr)[n])
  

  for (t in 1:12){

    # extract relevant variables from data_frame
    points <-  as.numeric(unlist(str_split(one_player_data$points[t], ",")))
    cells <-  as.numeric(unlist(str_split(one_player_data$clickedCells[t], ",")))
    social_info <-  as.numeric(unlist(str_split(one_player_data$socialInfo[t], ",")))
    unique_rounds <-  one_player_data$thisRound[t]
    env_number <- one_player_data$thisEnv[t]
    gempresent <- one_player_data$gemPresent[t]
    
    # save them into temporary data frame
    temp <- data.frame(
      player = rep(n, length(points)),
      points = points,
      cells = cells,
      social_info = social_info,
      unique_rounds = rep(unique_rounds, length(points)),
      env_number = rep(env_number, length(points)),
      gempresent = rep(gempresent, length(points)),
      round = rep(t, length(points)),
      tot_points = rep(one_player_data$totalPoints[t], length(points))
    )
    
    playerMat <- rbind(playerMat, temp)
  }
  
    data_long <- rbind(data_long, playerMat)
}

# sanity checks
unique(data_long$tot_points)
lenght(unique(data_long$player)) ## should be == to people that participated
sum(data_long$gempresent) ## should be 3/4 of tot rows

# save raw decisions dataset
write.csv(data, 'D_CleanExperimentalData/raw_data/social/raw_data_decisions.csv', row.names = FALSE)

# create unique id rounds
data_long <- data_long %>% 
  mutate(gem = ifelse(points > 200, 1, 0)) %>% 
  group_by(player, round) %>%
  mutate(trial = 1:25,
         unique_rounds = cur_group_id()) 

# save clean dataset
write.csv(data_long, 'D_CleanExperimentalData/clean_data/social/clean_data.csv', row.names = FALSE)
