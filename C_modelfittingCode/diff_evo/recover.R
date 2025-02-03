# rscript to perform parameter recovery
pacman::p_load(here,brms,tidyverse, rjson, DEoptim, doParallel, foreach,data.table)
here::i_am("./C_modelfittingCode/diff_evo/recover.R")
bashInput <- as.numeric(commandArgs(trailingOnly=TRUE))#c(1, 8)
# make not in operator
'%!in%' <- function(x,y)!('%in%'(x,y))

## loads the script where the learning models are specified (Q-learning, social Q-learning, UCB etc)
source(here::here("./C_modelfittingCode/","diff_evo/","learning_models.R"))

## load functions that perform Maximul Likelihood Estimation
source(here::here("./C_modelfittingCode/","diff_evo/","models_mle_2.R"))
source(here::here("./B_SimulationCode","sim_models.R")) # modelcode for simulation

## load functions that initialize the model fit
source(here::here("./C_modelfittingCode/","diff_evo/","model_fitting_functions_social_bandits_2.R"))

# load environments into workspace
source(here::here("./B_SimulationCode","load_environments_social_experiment.R")) # environments
source(here::here("./B_SimulationCode","sim_models.R")) # modelcode for simulation

#environment files are in generated files
environments <- load_envs_social(path = here::here("A_GeneratedFiles","environments/"))


#load df
social_fits<-readRDS(file = here::here("A_GeneratedFiles","modelfits","tau_slearn_v_shape2.rds"))%>%ungroup()


# make cluster
cl <- makeCluster(detectCores(), type = 'PSOCK')
registerDoParallel(cl)
#opts <- list(chunkSize=2)

random_G2s <-  -1 * (log((1 / 64)) * 24 * 4)


#environment files are in generated files
environments <- load_envs_social(path = here::here("A_GeneratedFiles","environments/"))
environment_lookup<-environments%>%group_split(env)
social_sims_plot_d <- list()

cl <- makeCluster(detectCores(), type = 'PSOCK')
registerDoParallel(cl)

#i=i+1
social_data_recov<-foreach(
  player_nr = unique(social_fits$uniqueID),
  .packages = c("DEoptim", "tidyverse","here","data.table"),
  .combine = "rbind"
) %dopar%{
  
  # print(rptx)
  Xnew <-as.matrix(expand.grid(0:7, 0:7)) # do this outside the loop for better speed
  
  # social data TODO: concatenate nonsocial data.
  dat <- social_fits %>% filter(
    uniqueID == player_nr,
  ) 
  
  #### unpack parameters
  estimates <- c(
    unique(dat$p1),
    unique(dat$p2),
    unique(dat$p3),
    unique(dat$p4)
  )
  
  ####
  rounds<-unique(dat$round)
  ####
  sim_dat<-NULL
  for (r in rounds) { # loop through rounds in roundList
    d1<-dat%>%filter(round==r)
    env_nr<-unique(d1$env_number)
    # simulations for parameter recovery
    sim_dat_r <- s_policy_shaping_sim(
      par = estimates,
      si = d1$social_info,
      envs = environment_lookup[[env_nr]]
    ) # only try one sub
    sim_dat_r$round=r
    sim_dat_r$player = player_nr
    sim_dat_r$group = unique(d1$group)
    sim_dat<-rbind(sim_dat,sim_dat_r)
  }
  # cv$run = rptx
  return(sim_dat)
}

#container for the parameter recovery analysis
social_recovery <- foreach(
  player_nr = unique(social_data_recov$player),
  .packages = c("DEoptim", "tidyverse","here","data.table"),
  .combine = "rbind"
) %dopar%{
  # environments <- load_envs_social(path = here("A_GeneratedFiles","environments/"))
  #for(player_nr in unique(social_data$player)){
  Xnew <- as.matrix(expand.grid(0:7, 0:7)) # do this outside the loop for better speed
  # social data TODO: concatenate nonsocial data.
  #print(s)
  print(player_nr)
  d1 <- social_data_recov %>% mutate(
    choice=index
  )%>%filter(
    player == player_nr
    #soc_info_round == s
  )
  #sometimes it breaks; just make this thing go through, kick out NAs later.
  d_recov <- tryCatch(        # might throw an error, just continue if it does and dont be annoying
    {
      fit_policy(d1=d1)
    },
    warning = function(w) {
      # Handle warnings
      message("Warning occurred: ", conditionMessage(w))
      fit_policy(d1=d1)
    },
    error = function(e) {
      # Handle errors
      message("Error occurred: ", conditionMessage(e))
      return(NA)  # Return a default value
    }
  )  
  # collect fit indices
  d_recov$fit <- unlist(cv[1])
  #d1$lr_p <- unlist(cv[2])
  #d1$intercept <- unlist(cv[2])
  d_recov$p1<-unlist(cv[2])
  d_recov$p2<-unlist(cv[3])
  d_recov$p3<-unlist(cv[4])
  d_recov$p4<-unlist(cv[5])
  # d1$lr<-unlist(cv[4])
  #d1$ut<-unlist(cv[4])
  # d1$streak_w<-unlist(cv[4])
  # d1$prior<-unlist(cv[6])
  # d1$sw <- unlist(cv[4])
  return(d_recov)
}
saveRDS(social_recovery,file = here::here("A_GeneratedFiles",paste_0("tardis_recovery_",bashInput[1],".rds")))
