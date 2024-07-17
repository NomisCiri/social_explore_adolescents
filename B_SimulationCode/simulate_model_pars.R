



source("./B_SimulationCode/load_environments_social_experiment.R") # environments
source("./B_SimulationCode/sim_models.R") # modelcode for simulation

#environment files are in generated files
environments <- load_envs_social(path = "./A_GeneratedFiles/experiment/")
social_sims_plot_d <- list()
social_sims_plot_one <- list()

# ENVIRONMENTINDEXING FOR SIMS is 1:8
i <- 1
rn <- i

#social_fits_d<-social_fits_d

#filter out participants that could not be fit and wierd trials that have na values in environments
#social_fits_d<-social_fits_d%>%filter(!is.na(fit),!is.na(env_number))

for (player_nr in unique(social_fits_d$uniqueID)) {
  Xnew <- as.matrix(expand.grid(0:7, 0:7)) # do this outside the loop for better speed
  output <- c()
  print(player_nr)
  
  # social data TODO: concatenate nonsocial data.
  #   d1 <- social_fits_d %>% filter(uniqueID == player_nr)%>%unique()
  # }
  
  d1 <- social_fits_d %>% filter(uniqueID == player_nr) %>%
    group_by(round) %>%
    mutate(z = points,
           #(points - mean(points)) /sd(points),
           social_info = social_info,
           choices = cells) %>% rowwise() %>%
    mutate(social_info = ifelse(social_info == 64, 1, social_info)) %>%
    ungroup()
  
  #### unpack parameters
  estimates <- c(
    unique(d1$lr_1),
    unique(d1$tau_1),
    unique(d1$sw_1),
    unique(d1$lr_2),
    unique(d1$tau_2),
    unique(d1$sw_2)
  )
  ####
  #for (r in rounds) { # loop through rounds in roundList
  cv <- exploreEnv2_lr_3sw(
    par = estimates,
    learning_model_fun = RW_Q,
    acquisition_fun = NULL,
    data = d1,
    envs = environments
  ) # only try one sub
  #collect fitted parameters and fit index and return
  #social_data$modelfit=cv
  #output <- rbind(output, cv)
  #}
  cv$player = player_nr
  cv$group = unique(d1$group.y)
  cv$run = rn
  social_sims_plot_one[[player_nr]] <- cv
}

social_sims_plot_d[[i]] <- do.call("rbind", social_sims_plot_one)

#}

social_sims_plot_dhuge <- do.call("rbind", social_sims_plot_d)

gem<-environments%>%group_by(env)%>%
  summarise(gem=mean(gems))%>%arrange(env)%>%
  pull(gem)



social_sims_plot_dhuge %>%mutate(gem=ifelse(env_idx>4,1,0))%>%
  dplyr::group_by(trial, player, group,gem,round,demo_quality) %>%
  filter(trial > 0) %>%
  #filter(group!="adults")%>%
  dplyr::summarise(m_rew = mean(z))%>%mutate(pats_or_sim="sims")->sims

social_fits %>%filter(!is.na(env_number))%>%mutate(gem=ifelse(env_number>4,1,0))%>%
  dplyr::group_by(trial, player, group,gem,round,demo_quality) %>%
  filter(trial > 0) %>% #filter(group!="adults")%>%
  dplyr::summarise(m_rew = mean(points)) %>%mutate(pats_or_sim="pats")->pats

simsub_dat<-rbind(sims,pats)

labeller_patsim=c(
  "pats"="participants",
  "sims"="simulations"
)

labeller_gem=c(
  "0"="no gem",
  "1"="gem"
)

simsub_dat%>%
  ggplot(aes(x = trial, y = m_rew, color = group,linetype=pats_or_sim)) +
  #ggtitle("Simulations") +
  # stat_summary() +
  stat_smooth(method = "lm") +
  facet_grid(.~ pats_or_sim,
             labeller = labeller(
               pats_or_sim = labeller_patsim,
               gem=labeller_gem
             ))+
  theme_bw() -> comp_plot


comp_plot


# Simulate counterfact

simulate!
  
source("./B_SimulationCode/load_environments_social_experiment.R") # environments
source("./C_modelfittingCode/learning_models.R") # environments
source("./B_SimulationCode/sim_models.R") # modelcode for simulation

#environment files are in generated files
environments <- load_envs_social(path = "./A_GeneratedFiles/experiment/")
social_sims_plot_d <- list()
social_sims_plot_one <- list()
#filter out participants that could not be fit and wierd trials that have na values in environments
#social_fits_d<-social_fits_d%>%filter(!is.na(fit),!is.na(env_number))
sw_simvec<-seq(0,25,length.out=120)
lrs_simvec<-seq(0,0.7,length.out=20)

sim_dat<-expand.grid(
  lr=lrs_simvec,
  sw=sw_simvec
)

future::plan("multisession", workers = 40)
huge<-furrr::future_map_dfr(1:length(sim_dat$lr),~{
  for (i in 1:100) {
    # make sure you have the parameter lookup dataframe in the futures
    sw_simvec<-seq(0,25,length.out=120)
    lrs_simvec<-seq(0,0.7,length.out=20)
    sim_dat<-expand.grid(
      lr=lrs_simvec,
      sw=sw_simvec
    )
    # make sure you have the environment lookup dataframe in the futures
    Xnew <- as.matrix(expand.grid(0:7, 0:7)) 
    
    #make data (arbitrary participants)
    d1 <- social_data %>% filter(uniqueID == 3) %>%
      group_by(round) %>%
      mutate(
        choices = cells
      ) %>% rowwise() %>%
      mutate(social_info = ifelse(social_info == 64, 1, social_info)) %>%
      ungroup()
    
    #### unpack parameters (learning rate, temperature, social weight)
    estimates <- c(sim_dat$lr[.x],1,sim_dat$sw[.x])
    #print(estimates)
    
    ####
    ####
    #SIMULATE
    ####
    ####
    cv <- exploreEnv1lrsw(
      par = estimates,
      learning_model_fun = RW_Q,
      acquisition_fun = NULL,
      data = d1,
      envs = environments
    )
    
    # Collect
    cv$player = i
    cv$sw=sim_dat$sw[.x]
    cv$lr<-sim_dat$lr[.x]
    social_sims_plot_one[[i]] <- cv
  }
  social_sims_plot_d <- do.call("rbind", social_sims_plot_one)
  return(social_sims_plot_d)
})

saveRDS(huge,file = "A_GeneratedFiles/bootstrapped_rewards.rds")

gem<-environments%>%group_by(env)%>%
  summarise(gem=mean(gems))%>%arrange(env)%>%
  pull(gem)



huge %>%mutate(gem=ifelse(env_idx>4,1,0))%>%
  dplyr::group_by(gem,demo_quality,sw,lr) %>%
  filter(trial > 0) %>%
  #filter(group!="adults")%>%
  dplyr::summarise(m_rew = mean(z))%>%mutate(pats_or_sim="sims")->sims


sims%>%#dplyr::group_by(sw,lr)%>%
  #dplyr::summarise(m_rew = mean(m_rew))%>%#(lr>0.2 & lr <0.4))%>%
  ggplot(aes(x = sw, y = lr,color=m_rew,fill=m_rew)) +
  geom_tile()+
  #stat_smooth()+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  facet_grid(. ~ demo_quality)+
  theme_bw() -> comp_gems


sims%>%filter(gem==0)%>%
  ggplot(aes(x = sw, y = lr,color=m_rew,fill=m_rew)) +
  geom_tile()+
  #stat_smooth()+
  scale_color_viridis_c()+
  scale_fill_viridis_c()+
  facet_grid()+
  theme_bw()-> comp_no_gems

cowplot::plot_grid(comp_gems,comp_no_gems,cols = 1)



