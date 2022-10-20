# check if simulations match the environment players played in


# read data in

explore_data <- read_csv(file = "Data/data_coord.csv")
nogems <- subset(explore_data, gempresent == 0)
                   
                   
 # plot next to behavioral data
  group_by(player, env_number) %>% 
  ggplot(aes(x = trial, y = points)) +
  stat_summary() +
  theme_minimal(base_size = 20)

simd_data <- NULL

for (n in 1:length(unique(explore_data$player))) {
 one_player <-  readRDS(file = paste0("A_GeneratedFiles/simulations/simulated_choices", n))
simd_data <- bind_rows(simd_data, one_player)
}
 
hist(simd_data$points_new_choice)
 unique(simd_data$round)

 

# PLOT results------------------------------------------------------------------

# points by round: should go up
 
 simd_data %>% 
   group_by(playerNr, env_counter) %>% 
   ggplot(aes(x = trial, y = points_new_choice)) +
   stat_summary() +
   theme_minimal(base_size = 20)


# read in simulated data
 
 simd_data %>% 
   group_by(playerNr, learning_rate) %>% 
   summarise(tot_point = sum(points_new_choice)) %>% 
   ggplot(aes(x = learning_rate, y = tot_point)) +
   geom_point() +
   theme_minimal(base_size = 20)
 
 

