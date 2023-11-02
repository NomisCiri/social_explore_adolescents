###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                         TEST DATA INTEGRITY                         ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

library(tidyverse)
data <- read.csv('data/social/data_social_all_participants.csv')

colSums(is.na(data))

data_with_NAs <- data %>% 
  filter(is.na(points)) 

## packages and source code
pacman::p_load(tidyverse, readxl)
source('D_CleanExperimentalData/scripts/readLIONESS.R')
source('D_CleanExperimentalData/scripts/readPROLIFIC.R')

## Import adolescents data
folder_ado <- 'D_CleanExperimentalData/adolescents_data/raw_data/social/lioness/'
writinglocation_ado <- 'D_CleanExperimentalData/adolescents_data/raw_data/social/'
periods <- 12 # how many max periods in this 
data_adolescents <- readLIONESS(folder_ado, writinglocation_ado, periods)

## get match of playerNr and participantNr (saved in externalID)
linking_data <- read.csv('D_CleanExperimentalData/adolescents_data/raw_data/social/1-session-raw.csv') %>% 
  select(playerNr, externalID)

## merge with main dataset
data_with_id <- left_join(data_adolescents, linking_data, by = "playerNr") %>%
  mutate(participantNr = externalID)

problem_data <- data_with_id %>% 
  filter(participantNr %in% data_with_NAs$participantNr) 
  filter(refreshTaskCount >= 1)
