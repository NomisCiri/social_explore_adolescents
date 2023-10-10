#########################################################################################
#########################################################################################
###                                                                                   ###
###  FUNCTION TO READ LIONESS FILES (XLS) AND OUTPUT CSV FILES FROM DIFFERENT SHEETS  ###
###                                                                                   ###
#########################################################################################
#########################################################################################

## Important: this works only all the raw files are stored in the same folder,
## and specific folder only contains LIONESS files
pacman::p_load(tidyverse, readxl)

## function takes the folder path and the number of periods expected from complete participants as input
readLIONESS <- function(folder, writinglocation, periods, prolificID = FALSE) {
  merged_data <- data.frame()

  ## how many raw files are in the folder?
  num_files <- length(list.files(folder))

  ## loop through them and merge them into 1 file
  for (n in 1:num_files) {
    filename <- paste(folder, list.files(folder)[n], sep = "")

    ## read the core sheet
    core <- openxlsx::read.xlsx(
      xlsxFile = filename,
      sheet = "core"
    ) %>%
      write_csv(paste0(writinglocation, n, "-core-raw.csv"))

    ## read the decision sheet
    decisions <- openxlsx::read.xlsx(
      xlsxFile = filename,
      sheet = "decisions"
    ) %>%
      write_csv(paste0(writinglocation, n, "-decisions-raw.csv"))
    
    ## save session sheet
    raw <- openxlsx::read.xlsx(xlsxFile = filename,
               sheet = "session") %>%
      write_csv(paste0(writinglocation, n, "-session-raw.csv"))
    
    ## which players completed the experiment
    player_finished <- core %>%
      dplyr::filter(period == periods & onPage == "end") %>%
      select(playerNr, )

    ## exclude incomplete participants
    decisions <- decisions %>%
      dplyr::filter(playerNr %in% player_finished$playerNr) %>% 
      mutate(file_nr = n)
    
    ## add prolific ID if specified
    if (prolificID == TRUE) {
      prolificIDs <- raw %>% 
        select(playerNr, externalID) %>%
        rename(Participant.id = externalID) %>% # rename to match prolific file output
        dplyr::filter(playerNr %in% player_finished$playerNr)
      
      ## merge with rest of dataset
      decisions <- left_join(decisions, prolificIDs, by = "playerNr")
    }

    ## append to other batches (if existing)
    merged_data <-
      bind_rows(merged_data, decisions) 
  }
  
  return(merged_data)
}
