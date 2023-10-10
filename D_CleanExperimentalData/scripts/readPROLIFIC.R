###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###              EXTRACT DEMOGRAPHIC INFO OF PROLIFIC USERS              ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## Important: this works only all the raw files are stored in the same folder,
## and specific folder only contains LIONESS files
pacman::p_load(tidyverse)

## function takes the folder path and the number of periods expected from complete participants as input
readProlific <- function(folder, writinglocation) {
  merged_data <- data.frame()
  
  ## how many raw files are in the folder?
  num_files <- length(list.files(folder))
  
  ## loop through them and merge them into 1 file
  for (n in 1:num_files) {
    filename <- paste(folder, list.files(folder)[n], sep = "")
    
    ## read the core sheet
    demographics <- readr::read_csv(file = filename,
    ) %>% 
      dplyr::filter(Status == "APPROVED") 
    
    ## append to other batches (if existing)
    merged_data <-
      bind_rows(merged_data, demographics) 
  }
  
  write_csv(merged_data, paste0(writinglocation,  "clean_data_demographics_adults.csv"))

}
