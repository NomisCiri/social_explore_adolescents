# rscript to perform parameter recovery
pacman::p_load(here,brms,tidyverse, rjson, DEoptim, doParallel, foreach,data.table)
here::i_am("./C_modelfittingCode/diff_evo/recover.R")
bashInput <- as.numeric(commandArgs(trailingOnly=TRUE))#c(1, 8)


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
# make not in operator
'%!in%' <- function(x,y)!('%in%'(x,y))

#load df
social_fits<-readRDS(file = here::here("A_GeneratedFiles","modelfits","tau_slearn.rds"))%>%ungroup()


# make cluster
cl <- makeCluster(detectCores(), type = 'PSOCK')
registerDoParallel(cl)
#opts <- list(chunkSize=2)

random_G2s <-  -1 * (log((1 / 64)) * 24 * 4)


#environment files are in generated files
environments <- load_envs_social(path = here::here("A_GeneratedFiles","environments/"))
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
  print(player_nr)
  
  # social data TODO: concatenate nonsocial data.
  d1 <- social_fits %>% filter(
    uniqueID == player_nr,
  ) %>%
    #filter(gempresent == 0) %>%  # for now only have rounds without gems 
    group_by(round) %>%
    mutate(z = points, #(points - mean(points)) / sd(points), ventually figure out what is the best outcome for this
           social_info = social_info,
           choices = cells,
           soc_info_round=demo_quality,
    ) %>%ungroup()
  
  #### unpack parameters
  lr<-unique(d1$lr)
  tau<-unique(d1$tau_1)
  sw<-unique(d1$sw_1)
  prior<-unique(d1$prior)
  
  estimates <- c(
    lr,
    tau,
    sw,
    -1
  )
  ####
  #for (r in rounds) { # loop through rounds in roundList
  cv <- s_learn_prior_sim(
    par = estimates,
    learning_model_fun = RW_Q,
    data = d1,
    envs = environments
  ) # only try one sub
  
  cv$player = player_nr
  cv$group = unique(d1$group)
  return(cv)
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
  d1 <- social_data_recov %>% filter(
    player == player_nr
    #soc_info_round == s
  ) %>%
    #filter(gempresent == 0) %>%  # for now only have rounds without gems 
    group_by(round) %>%
    mutate(z = z, #(points - mean(points)) / sd(points), ventually figure out what is the best outcome for this
           social_info = social_info,
           choices = index,
           env_number=env_idx
    ) %>%ungroup()
  #rounds <- unique(d1$round)
  #only fit if you have 25 klicks.
  d_recov <- fit_slr(d1 = d1,environments=environments) # only try one sub
  
  # collect fit indices
  d_recov$fit <- unlist(cv[1])
  #d1$lr_p <- unlist(cv[2])
  #d1$intercept <- unlist(cv[2])
  d_recov$lr<-unlist(cv[2])
  d_recov$tau_1<-unlist(cv[3])
  d_recov$sw_1<-unlist(cv[4])
  d_recov$prior<--1#unlist(cv[5])
  # d1$lr<-unlist(cv[4])
  #d1$ut<-unlist(cv[4])
  # d1$streak_w<-unlist(cv[4])
  # d1$prior<-unlist(cv[6])
  # d1$sw <- unlist(cv[4])
  return(d_recov)
}
saveRDS(social_recovery,file = here::here("A_GeneratedFiles",paste_0("tardis_recovery_",bashInput[1],"sw.rds")))
