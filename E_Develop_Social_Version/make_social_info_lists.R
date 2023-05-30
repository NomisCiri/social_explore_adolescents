### create social info for social version of experiment

pacman::p_load(tidyverse, rjson, here, jsonlite)


# load data from the experiment

explore_data <- read_csv(file = paste0(here(), "/data/data_coord.csv"))

explore_data <- explore_data %>% 
  filter(player != 188) %>% 
  group_by(player, round) %>% 
  mutate(trial = 1:25,
         unique_rounds = cur_group_id())


# load all conditions of trials & transform to list of vectors

gem_rounds_files <- list.files(path='E_Develop_Social_Version/rounds_social_info', pattern="^gem.*\\.txt$", full.names = TRUE)
gem_rounds_names <- gsub("\\.txt$", "", basename(gem_rounds_files))


# Create a list of example participants per condition

gem_rounds_list <- sapply(gem_rounds_files, function(file) {
  rounds_df <- read.table(file, header = TRUE)
  rounds_vec <- as.numeric(unlist(strsplit(rounds_df$rounds, ",")))
  setNames(list(rounds_vec), basename(file))
})
names(gem_rounds_list) <- gem_rounds_names


# Extract social information

social_info_list <- gem_rounds_list
for (n in 1:length(gem_rounds_list)) {
  social_info_list_dummy <- list()
  for (m in 1:length(gem_rounds_list[[n]])) {
    social_info <- explore_data %>% 
      filter(unique_rounds == gem_rounds_list[[n]][m]) %>% 
      ungroup() %>% 
      select(cells) %>% 
      pull
    social_info_list_dummy[[m]] <- social_info 
  }
  names(social_info_list_dummy) <- gem_rounds_list[[n]]
  social_info_list[[n]] <- social_info_list_dummy
}


# fancy version to extract social information
# @phil: same as above?

 social_info_list <- lapply(gem_rounds_list, function(rounds) {
  social_info_list_dummy <- list()
  for (m in 1:length(rounds)) {
    social_info <- explore_data %>% 
      filter(unique_rounds == rounds[[m]]) %>% 
      ungroup() %>% 
      select(cells) %>% 
      pull
    social_info_list_dummy[[m]] <- social_info 
  }
  names(social_info_list_dummy) <- rounds
  social_info_list_dummy
})

# Apply social information to matrix format

## we need 64 [0,0,0,0] arrays, where each element is a corner of a tile where social info is displayed

empty_cells <- rep(list(c(0,0,0,0)), 64)

## then we repeat 25 times (for 25 clicks)

social_info_mat <- rep(list(empty_cells), 25)

## now we fill empty matrix with social info from the vector and save in a list

social_info_mat_list <- social_info_list
for (n in 1:length(social_info_list)) {
  social_info_mat_list_dummy <- list()
  
  for (m in 1:length(social_info_list[[n]])) {
    social_info_mat_dummy <- social_info_mat
    
    for (l in 1:length(social_info_list[[n]][[m]])) {
      social_info_mat_dummy[[l]][[social_info_list[[n]][[m]][l]]][1] <- 1
    }
    
    social_info_mat_list_dummy[[m]] <- social_info_mat_dummy
  }
  
  names(social_info_mat_list_dummy) <- gem_rounds_list[[n]]
  social_info_mat_list[[n]] <- social_info_mat_list_dummy
}


# save matrices with social info as .txt files in seperate folder

## Create folder called social_info_mat

if (!dir.exists('E_Develop_Social_Version/rounds_social_info/social_info_mat') == TRUE) {
  dir.create('E_Develop_Social_Version/rounds_social_info/social_info_mat')
}

## Create .txt files

### takes a bit of time

for (n in 1:length(social_info_mat_list)) {
  for (m in 1:length(social_info_mat_list[[n]])) {
    mat_name <- paste('mat',names(social_info_mat_list[n]),names(social_info_mat_list[[n]][m]), sep = "_")
    write.table(social_info_mat_list[[n]][[m]], paste0('E_Develop_Social_Version/rounds_social_info/social_info_mat/',mat_name,'.txt'))
  }
}


# Matrices to JSON lists

json_trial_list <- social_info_mat_list
for (n in 1:length(social_info_list)) {
  for (m in 1:length(social_info_list[[n]])) {
    json_trial_dummy <- list()
    for(l in 1:length(social_info_list[[n]][[m]])) {
      json_trial_dummy[l] <- gem_json1 <-social_info_mat_list[[n]][[m]][[l]] %>% toJSON(dataframe = 'columns')
    }
    json_trial_list[[n]][[m]] <- json_trial_dummy
  }
}

# save JSON format in folder

## Create folder called social_info_json

if (!dir.exists('E_Develop_Social_Version/rounds_social_info/social_info_json') == TRUE) {
  dir.create('E_Develop_Social_Version/rounds_social_info/social_info_json')
}

## Create .json files

### takes a bit of time
for (n in 1:length(social_info_mat_list)) {
  for (m in 1:length(social_info_mat_list[[n]])) {
    social_infoJSON = toJSON(social_info_mat_list[[n]][[m]],
                             pretty = TRUE,
                             auto_unbox = TRUE)
    json_name <-
      paste('json',
            names(social_info_mat_list[n]),
            names(social_info_mat_list[[n]][m]),
            sep = "_")
    write(
      social_infoJSON,
      paste0(
        'E_Develop_Social_Version/rounds_social_info/social_info_json/',
        json_name,
        '.json'
      )
    )
  }
}

# @phil do we need what it's after?

# not sure if it does what it should, since the object is NULL
gem_json_list <- json_trial_list
for (n in 1:length(json_trial_list)) {
  for (m in 1:length(json_trial_list[[n]])) {
    gem_json_dummy <- cat(paste(sprintf('"%d":', 1:length(json_trial_list[[n]][[m]])), 
                                unlist(json_trial_list[[n]][[m]]), collapse = ','),
                          file = 'prova.json')
    gem_json_list[[n]][[m]] <- gem_json_dummy
  }
}
