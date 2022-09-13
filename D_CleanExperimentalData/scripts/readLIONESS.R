##### Function to import data from (multiple) LIONESS scripts ####

# Important: this works only all the raw files are stored in the same folder,
# and specific folder only contains LIONESS files
library(tidyverse)
library(tibble)
library(readxl)

# function takes the folder path and the number of periods expected from complete participants as input

readLIONESS <- function(folder, writinglocation, periods) {
  merged_data <- data.frame()
  
  # how many raw files are in the folder?
  num_files <- length(list.files(folder))
  
  # loop through them and merge them into 1 file
  for (n in 1:num_files) {
    
    filename <- paste(folder ,list.files(folder)[n], sep = '')
    
    # read the core sheet
    core <-  read_excel(path = filename,
                        sheet = "core") %>%
      write_csv(paste(writinglocation, n, "-core-raw.csv"))
    
    decisions <-  read_excel(path = filename,
                             sheet = "decisions") %>%
      write_csv(paste(writinglocation, n, "-decisions-raw.csv"))
    
    
    # which players completed the experiment
    player_finished <- core %>%
      filter(period == periods & onPage == 'end') %>%
      select(playerNr)
    
    # exclude incomplete participants
    decisions <- decisions %>%
      filter(playerNr %in% player_finished$playerNr)
    
 
    # append to other batches (if existing)
    merged_data <-
      bind_rows(merged_data, decisions)
  }
  return(merged_data)
}
