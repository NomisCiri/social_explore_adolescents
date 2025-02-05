### create social info for social version of experiment

pacman::p_load(tidyverse, rjson, here, jsonlite)

# load data from the experiment
explore_data <- read_csv(file = paste0(here(), "/data/data_coord.csv"))

explore_data <- explore_data %>%
  filter(player != 188) %>%
  group_by(player, round) %>%
  mutate(
    trial = 1:25,
    unique_rounds = cur_group_id()
  )

# load text files with info on which rounds we want to use

# this is now just for gem present, high performance @phil, extend this to the other conditions
gem_yes_high <- read.table("E_Develop_Social_Version/rounds_social_info/gem_present_found_high.txt", header = TRUE)

# extracting the 6th round as an example
this_round <- as.numeric(unlist(strsplit(gem_yes_high$rounds, ",")))[6]


### @andrea: and env number

# extract the choices in that round
social_info <- explore_data %>%
  filter(unique_rounds == this_round) %>%
  ungroup() %>%
  select(cells) %>%
  pull()

# print to check we have a vector with 25 decisions
social_info

# format according to Lioness functions:
# we need 64 [0,0,0,0] arrays, where each element is a corner of a tile where social info is displayed
empty_cells <- rep(list(c(0, 0, 0, 0)), 64)

# then we repeat 25 times (for 25 clicks)
social_info_mat <- rep(list(empty_cells), 25)

# now we fill the empty matrix with social info from the vector
for (n in 1:length(social_info)) {
  social_info_mat[[n]][[social_info[n]]][1] <- 1
}


### from here on, some ugly code to format everything into a json file so it can be read by the experiment
# eventually (andrea/simon/phil) find a way to make this prettier
json_trial <- list()

for (n in 1:length(social_info)) {
  json_trial[n] <- gem_json1 <- social_info_mat[[n]] %>% toJSON(dataframe = "columns")
}

# gems
gem_json <- cat('{"1":', unlist(json_trial[1]),
  ',"2":', unlist(json_trial[2]),
  ',"3":', unlist(json_trial[3]),
  ',"4":', unlist(json_trial[4]),
  ',"5":', unlist(json_trial[5]),
  ',"6":', unlist(json_trial[6]),
  ',"7":', unlist(json_trial[7]),
  ',"8":', unlist(json_trial[8]),
  ',"9":', unlist(json_trial[9]),
  ',"10":', unlist(json_trial[10]),
  ',"11":', unlist(json_trial[11]),
  ',"12":', unlist(json_trial[12]),
  ',"13":', unlist(json_trial[13]),
  ',"14":', unlist(json_trial[14]),
  ',"15":', unlist(json_trial[15]),
  ',"16":', unlist(json_trial[16]),
  ',"17":', unlist(json_trial[17]),
  ',"18":', unlist(json_trial[18]),
  ',"19":', unlist(json_trial[19]),
  ',"20":', unlist(json_trial[20]),
  ',"21":', unlist(json_trial[21]),
  ',"22":', unlist(json_trial[22]),
  ',"23":', unlist(json_trial[23]),
  ',"24":', unlist(json_trial[24]),
  ',"25":', unlist(json_trial[25]),
  "}",
  sep = " ",
  file = "prova.json"
)


# social_infoJSON=toJSON(social_info_mat,pretty=TRUE,auto_unbox=TRUE)
# write.table()
# write(social_infoJSON, 'E_Develop_Social_Version/rounds_social_info/social_info_gem_yes_found_high.json')
# expor(social_infoJSON, 'E_Develop_Social_Version/rounds_social_info/social_info_gem_yes_found_high.json')


write.table(social_info_mat, "E_Develop_Social_Version/rounds_social_info/social_info_gem_yes_found_high.txt")

# save output according to each treatment
