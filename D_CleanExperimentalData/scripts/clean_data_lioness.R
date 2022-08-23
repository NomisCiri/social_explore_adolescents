rm(list = ls())

#### CLEAN AND MERGE DATASETS ####

library(tidyverse)
library(dplyr)
library(tibble)
library(readxl)

source('D_CleanExperimentalData/scripts/readLIONESS.R')


folder <- 'D_CleanExperimentalData/raw_data/lioness/'
writinglocation <- 'D_CleanExperimentalData/raw_data'

data <- readLIONESS(folder, writinglocation)

# calculate S and substitute playerNr with ID

## REPLACE WITH APPLY WHWN WILL BECOME BIG

data_long <- data.frame(player = numeric(0),
                        points = numeric(0),
                        cells = numeric(0),
                        round = numeric(0),
                        env_number = numeric(0),
                        gem_present = numeric(0),
                        tot_points = numeric(0))


for (n in 1:length(unique(data$playerNr))){
  
  playerMat <- data.frame()
  
  one_player_data <- data %>% 
    filter(playerNr == data$playerNr[n])
  

  for (t in 1:12){

    points <-  as.numeric(unlist(str_split(one_player_data$points[t], ",")))
    cells <-  as.numeric(unlist(str_split(one_player_data$clickedCells[t], ",")))
    env_number <- as.numeric(unlist(str_split(one_player_data$orderEnvs[1], ",")))[t]
    gempresent <- as.numeric(unlist(str_split(one_player_data$orderTreatment[1], ",")))[t]
    
    
    temp <- data.frame(player = rep(n, length(points)),
     points = points,
    cells = cells,
    env_number = rep(env_number, length(points)),
    gempresent = rep(gempresent, length(points)),
    
    round = rep(t, length(points)),
    tot_points = rep(one_player_data$totalPoints[t], length(points)))
    
    playerMat <- rbind(playerMat, temp)
  }
  
    data_long <- rbind(data_long, playerMat)
}

# save raw decisions dataset
write.csv(data, 'D_CleanExperimentalData/raw_data/raw_data_decisions.csv', row.names = FALSE)


data_long <- data_long %>% 
  mutate(gem = ifelse(points > 200, 1, 0))


# save clean dataset

write.csv(data_long, 'D_CleanExperimentalData/clean_data/clean_data.csv', row.names = FALSE)
