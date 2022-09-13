# check if simulations match the environment players played in


# read data in

explore_data <- read_csv(file = "Data/data_coord.csv")

# select player number

playerNr = 4

d_subset <- subset(explore_data, player == playerNr & gempresent == 0)

unique(d_subset$round)


# read in simulated data

readRDS(file = paste0("A_GeneratedFiles/simulations/simulated_choices", playerNr))
