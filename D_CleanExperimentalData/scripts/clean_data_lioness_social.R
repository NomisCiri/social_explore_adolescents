###########################################################################
###########################################################################
###                                                                     ###
###                  SCRIPT TO CLEAN DATA FROM LIONESS                  ###
###                                                                     ###
###########################################################################
###########################################################################

## packages and source code
pacman::p_load(tidyverse, readxl)
source('D_CleanExperimentalData/scripts/readLIONESS.R')
source('D_CleanExperimentalData/scripts/readPROLIFIC.R')

## Import adolescents data
folder_ado <- 'D_CleanExperimentalData/adolescents_data/raw_data/social/lioness/'
writinglocation_ado <- 'D_CleanExperimentalData/adolescents_data/raw_data/social/'
periods <- 12 # how many max periods in this 
data_adolescents <- readLIONESS(folder_ado, writinglocation_ado, periods)

## Import adults data
folder_adu <- 'D_CleanExperimentalData/adults_data//raw_data/social/lioness/'
writinglocation_adu <- 'D_CleanExperimentalData/adults_data/raw_data/social/'
periods <- 12 # how many max periods in this 
data_adults <- readLIONESS(folder_adu, writinglocation_adu, periods, prolificID = TRUE) %>% 
  select(-c(age,gender))

## import dataset with demographics for adolescents
demographics_ado <- read.csv('data/social/clean_data_demographics_adolescents.csv') %>% 
  select(-c(playerNr))

## import dataset with demographics for adults
demographics_adu <- read.csv('data/social/clean_data_demographics_adults.csv')

##---------------------------------------------------------------
##               create adolescents long dataset               --
##---------------------------------------------------------------

## get match of playerNr and participantNr (saved in externalID)
linking_data <- read.csv('D_CleanExperimentalData/adolescents_data/raw_data/social/1-session-raw.csv') %>% 
  select(playerNr, externalID)

## merge with main dataset
data_with_id <- left_join(data_adolescents, linking_data, by = "playerNr") %>%
  mutate(participantNr = externalID)

## how many participants finished the experiments
length(unique(data_with_id$playerNr))

## how many participants finished the experiments and have an id
length(unique(data_with_id$participantNr))

## Who is missing? 
finished <- unique(data_with_id$participantNr)
all <- unique(demographics_ado$participantNr)
not_included <- all[!all %in% finished]

## find players for whom the ID was not saved automatically and was entered manually during experiment 
## (therefore it is saved)
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

## change ID to participant who made mistake
data_with_id$participantNr[data_with_id$participantNr == 30523] <- 30623 

data_full_adolescents <-  left_join(data_with_id, demographics_ado, by = "participantNr") %>% 
   replace_na(replace = list(refreshTaskCount = 0))
  

## check if any missing values
data_full_adolescents %>% 
  select(participantNr, gender, age) %>% 
  filter_all(any_vars(is.na(.))) 

## andrea: do something with these participants?

data_long_adolescents <- data.frame(player = numeric(0),
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
                                    soc_info_round = numeric(0),
                                    attempt_refresh = numeric(0)
)

for (n in 1:length(unique(data_full_adolescents$playerNr))) {

  # create empty df
  playerMat <- data.frame()

  # select current player
  one_player_data <- data_full_adolescents %>%
    filter(playerNr == unique(data_full_adolescents$playerNr)[n])


  for (t in 1:nrow(one_player_data)) {

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
    attempt_refresh <- one_player_data$refreshTaskCount[t]

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
      tot_points = rep(one_player_data$totalPoints[t], length(points)),
      attempt_refresh = rep(attempt_refresh, length(points))
      
    )

    # if (nrow( temp %>%
    #           filter_all(any_vars(is.na(.))) ) > 0){
    #   print(paste('problem with playerNr', temp$player))
    # }
    #
    #

    playerMat <- rbind(playerMat, temp)
  }

   data_long_adolescents <- rbind(data_long_adolescents, playerMat)


  }

## make long dataset, clean variable names and rename columns
# data_long_adolescents <- separate_longer_delim(data_full_adolescents, c(points, socialInfo, clickedCells), delim = ",") %>%
#  dplyr::select(playerNr, points, clickedCells, socialInfo, thisRound, thisEnv, totalPoints, gemPresent, age, gender, thisRound, participantNr, refreshTaskCount, orderRounds) %>%
#   mutate(gem = ifelse(points > 200, 1, 0),
#          points = as.numeric(points),
#          cells = as.numeric(clickedCells),
#          social_info = as.numeric(socialInfo),
#          tot_points = as.numeric(totalPoints)) %>%
#   dplyr::rename(env_number = thisEnv,
#                 soc_info_round = thisRound,
#                 order_rounds = orderRounds,
#                 gempresent = gemPresent) %>%
#   group_by(playerNr) %>%
#   tidyr::fill(order_rounds) %>%
#   mutate(player = cur_group_id(),
#          round = rep(1:12, each = 25)) %>%
#   group_by(player, round) %>%
#   mutate(unique_rounds = cur_group_id(),
#          trial = 1:25) %>%
#   dplyr::select(-c(clickedCells, socialInfo, playerNr, totalPoints)) %>%
#   replace_na(replace = list(refreshTaskCount = 0))

  # create unique id rounds
  data_long_adolescents <- data_long_adolescents %>%
    mutate(gem = ifelse(points > 200, 1, 0)) %>%
    group_by(player, round) %>%
    mutate(trial = 1:25,
           unique_rounds = cur_group_id())
  # 
  
## inspect data that comes out with some NAs
data_with_nas <- data_long_adolescents %>% filter_all(any_vars(is.na(.))) 

## sanity checks @andrea @simon: think of more?
ggsave('D_CleanExperimentalData/sanity_plots/points_distribution.png',
       data_long_adolescents %>% 
         select(player, tot_points) %>% 
         distinct() %>% 
         ggplot() +
         geom_histogram(aes(x = tot_points)) )


length(unique(data_long_adolescents$player)) ## should be == to people that participated
if (sum(data_long_adolescents$gempresent,na.rm = TRUE) == nrow(data_long_adolescents) / 12 * 8) { ## should be 3/4 of tot rows 
  print('correct number of gem present rounds')
} else (print('double check number of treatment rounds'))

# for 1 participant 1 round was not recorded, hence error.

# save raw decisions dataset
write.csv(data_full_adolescents, 'D_CleanExperimentalData/adolescents_data/raw_data/social/raw_data_decisions.csv', row.names = FALSE)

# save clean dataset
write.csv(data_long_adolescents, 'D_CleanExperimentalData/adolescents_data/clean_data/social/clean_data.csv', row.names = FALSE)

##----------------------------------------------------------------
##                  create adults long dataset                  --
##----------------------------------------------------------------

## add demographics info

demographics_adu <- demographics_adu %>% 
  dplyr::select(Participant.id, Age, Sex) %>% 
  dplyr::rename(age = Age) %>% 
  dplyr::mutate(age = as.integer(age),
         gender = ifelse(Sex == "Female", 1, ifelse(Sex == "Male", 2, 3)))


data_adults <- left_join(data_adults, demographics_adu, by = "Participant.id") %>% 
  replace_na(replace = list(refreshTaskCount = 0))


# ## remove rounds where clicked where not saved
# data_adults <- data_adults %>% 
#   filter(!is.na(clickedCells) & !is.na(socialInfo))

## @andrea: REPLACE WITH APPLY when WILL BECOME BIG
data_long_adults <- data.frame(player = numeric(0),
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
                                    soc_info_round = numeric(0),
                                    attempt_refresh = numeric(0)
                               
)

# create unique IDs
data_adults <- data_adults %>% 
  group_by(playerNr, file_nr) %>% 
  mutate(playerNr = cur_group_id())


for (n in 1:length(unique(data_adults$playerNr))) {
  
  # create empty df
  playerMat <- data.frame()
  
  # select current player
  one_player_data <- data_adults %>% 
    filter(playerNr == unique(data_adults$playerNr)[n])
  
  
  for (t in 1:nrow(one_player_data)) {
    
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
    attempt_refresh <- one_player_data$refreshTaskCount[t]
    #participantNr <- one_player_data$participantNr[t]
    
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
      gempresent = rep(gempresent, length(points)),
      round = rep(t, length(points)),
      tot_points = rep(one_player_data$totalPoints[t], length(points)),
      attempt_refresh = rep(attempt_refresh, length(points))
      
    )
    
    # if (nrow( temp %>% 
    #           filter_all(any_vars(is.na(.))) ) > 0){
    #   print(paste('problem with playerNr', temp$player))
    # }
    # 
    # 
    
    playerMat <- rbind(playerMat, temp)
  }
  
  data_long_adults <- rbind(data_long_adults, playerMat)
}

## inspect data that comes out with some NAs
problem_data <- data_long_adults %>% 
 # select(-c(age,gender)) %>% 
  filter_all(any_vars(is.na(.))) 

## sanity checks @andrea @simon: think of more?
ggsave('D_CleanExperimentalData/sanity_plots/points_distribution.png',
       data_long_adults %>% 
         select(player, tot_points) %>% 
         distinct() %>% 
         ggplot() +
         geom_histogram(aes(x = tot_points)) )

length(unique(data_long_adults$player)) ## should be == to people that participated
if (sum(data_long_adults$gempresent,na.rm = TRUE) == nrow(data_long_adults) / 12 * 8) { ## should be 3/4 of tot rows 
  print('correct number of gem present rounds')
} else (print('double check number of treatment rounds'))

# for 1 participant 1 round was not recorded, hence error.

# save raw decisions dataset
write.csv(data_adults, 'D_CleanExperimentalData/adults_data/raw_data/social/raw_data_decisions.csv', row.names = FALSE)

# create unique id rounds
data_long_adults <- data_long_adults %>% 
  mutate(gem = ifelse(points > 200, 1, 0)) %>% 
  group_by(player, round) %>%
  mutate(trial = 1:25,
         unique_rounds = cur_group_id()) 

# save clean dataset
write.csv(data_long_adults, 'D_CleanExperimentalData/adults_data/clean_data/social/clean_data.csv', row.names = FALSE)



