# check if simulations match the environment players played in


# read data in

explore_data <- read_csv(file = "Data/data_coord.csv")
#nogems <- subset(explore_data, gempresent == 0)
                   
                   
simd_data <- NULL

for (n in 1:length(unique(explore_data$player))) {
  
  # all gems
  
  one_player <-  readRDS(file = paste0("A_GeneratedFiles/simulations/all_envs/simulated_choices", n)) %>% 
    mutate(gem_label = ifelse(env_type == 1, "with gem", "no gem"))
  
  # only no gems
  # one_player <-  readRDS(file = paste0("A_GeneratedFiles/simulations/simulated_choices", n))
simd_data <- bind_rows(simd_data, one_player)
}
 
hist(simd_data$points_new_choice)
hist(simd_data$z_new_choice)




# PLOT results------------------------------------------------------------------


# total point by treatment

simd_data %>%
  ggplot(aes(x = points_new_choice)) +
  geom_histogram() +
  theme_minimal(base_size = 15) +
  facet_wrap(~ gem_label) +
  labs(title = 'total points all players')



# points by round: should go up
 
 simd_data %>% 
   group_by(playerNr, env_counter) %>% 
   ggplot(aes(x = trial, y = points_new_choice)) +
   stat_summary() +
   theme_minimal(base_size = 15) +
   facet_wrap(~ gem_label) +
   labs(title = 'avg points per round time')

# read in simulated data
 
 simd_data %>% 
   group_by(playerNr, learning_rate) %>% 
   summarise(tot_point = sum(points_new_choice)) %>% 
   ggplot(aes(x = learning_rate, y = tot_point)) +
   geom_point() +
   theme_minimal(base_size = 20) 
 
 
 # plot next to behavioral data
 group_by(player, env_number) %>% 
   ggplot(aes(x = trial, y = points)) +
   stat_summary() +
   theme_minimal(base_size = 20)
