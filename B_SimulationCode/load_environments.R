load_envs<-function(path="A_GeneratedFiles/"){
  # load the experimental data
  # load the generated environments
  envs_no_gems_list <-
    rjson::fromJSON(file = paste0(path,"environments_no_gem_var25max.json")) # max gem value: 250; variance: 25
  
  envs_gems_list <-
    rjson::fromJSON(file = paste0(path,"environments_gem_250_var25max.json")) # max gem value: 250; variance: 25
  
  # merge environments together and add identifier
  
  envs_no_gems <- map(envs_no_gems_list, as.data.table)
  envs_no_gems <- rbindlist(envs_no_gems) %>% 
    mutate(gems = 0)
  
  envs_gems <- map(envs_gems_list, as.data.table)
  envs_gems <- rbindlist(envs_gems) %>%
    mutate(env_idx=env_idx+6)%>%
    mutate(gems = 1)
  
  #dataframe with environments
  envs <- rbind(envs_gems, envs_no_gems)
  return(envs)
}
