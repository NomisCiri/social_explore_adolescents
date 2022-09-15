# check if simulations match the environment players played in


# read data in

explore_data <- read_csv(file = "Data/data_coord.csv")
nogems <- subset(explore_data, gempresent == 0)



# select player number

for (n in 1:length(unique(explore_data$player))) {
  
  d_subset <- subset(explore_data, player == playerNr & gempresent == 0)
  unique(d_subset$round)
  
    simd_data <-  readRDS(file = paste0("A_GeneratedFiles/simulations/simulated_choices", playerNr))
  unique(simd_data$round)
  
  simd_data %>% 
    group_by(round()) %>% 
    ggplot +
    geom_point( x = )
  
}

playerNr = 5




# read in simulated data

