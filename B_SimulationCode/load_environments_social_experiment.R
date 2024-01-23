##################################################################
##           load environments from social experiment           ##
##################################################################

load_envs_social <- function(path = "A_GeneratedFiles/") {
  
  ## load the experimental data
  ## load the generated environments
  envs_no_gems_list <-
    rjson::fromJSON(file = paste0(path, "/env_no_gems.json")) # max gem value: 250; variance: 25
  
  envs_gems_list <-
    rjson::fromJSON(file = paste0(path, "/env_gems.json")) # max gem value: 250; variance: 25
  
  ## merge environments together and add identifier
  envs_no_gems <- map(envs_no_gems_list, as.data.table)
  envs_no_gems <- rbindlist(envs_no_gems) %>%
    mutate(gems = 0) %>% 
    filter(env_idx != 4 & env_idx != 6) # only 1, 2, 3, 5 were used in the exp
  
  envs_gems <- map(envs_gems_list, as.data.table)
  
  ## update to match data in social info file
  envs_gems[[7]]$env_idx <- 7
  envs_gems[[8]]$env_idx <- 8
  envs_gems[[9]]$env_idx <- 9
  
  ## remove info about original coordinates
  envs_gems[[7]][,6:8] <- NULL
  envs_gems[[8]][,6:8] <- NULL
  envs_gems[[9]][,6:8] <- NULL
  
  ## rename envs
  envs_gems <- rbindlist(envs_gems) %>%
    mutate(gems = 1) %>% 
    filter(env_idx > 1) # only 2:8 were used in the exp
  
  ## dataframe with environments
  envs <- rbind(envs_gems, envs_no_gems) %>% 
    mutate(env = ifelse(gems == 0 & env_idx == 5, env_idx - 1,
                 ifelse(
                   gems == 1 & env_idx < 6, env_idx + 8,env_idx))) %>% 
    mutate(env = ifelse(env > 5, env - 1, env)) # apply same transformation to env_id that has been applied to data

  
  envs %>% 
    select(env, gems) %>% 
    distinct() %>% 
    #filter(gem == 0) %>% 
    ggplot() +
    geom_bar(aes(x = factor(env), fill = gems)) +
    facet_wrap(~gems)
  
  
  return(envs)
}

