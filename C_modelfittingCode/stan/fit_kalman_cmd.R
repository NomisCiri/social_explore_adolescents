###################################
####
####   This script fits the kalman filter; social utility model on 
####   the social grid experiment data
####   © Simon Ciranka 2023
###################################
###################################
library(tidyverse)
library(rstan)

social_data <-  read_csv(file = paste0("./data/social/data_social_all_participants.csv"))
social_data<-social_data%>%mutate(player=ifelse(group=="adults",player,player+1000))#%>%filter(player %in% 1:50)

## get unreasonable nans
nans<-social_data%>%
  filter(is.na(demo_type) | is.na(cells))%>%pull(player)%>%unique()

social_data<-social_data%>%filter(!(player %in% nans))
R_subj=social_data%>%group_by(player)%>%summarise(n_r=length(unique(round)))%>%pull(n_r)


social_data<-social_data%>%#drop_na()
  filter(!is.na(demo_type) & !is.na(cells))%>%#filter(player<30)%>%# get a sub-sample of both adults and adolescents 
  mutate(soctype=case_when(
    demo_type=="gem_found"~1,# i need that to index the different social weight parameters
    demo_type=="gem_not_found"~2,
    demo_type=="never_exploit"~3,
    demo_type=="no_gem"~4)
  )
#give kids a unqiquely numbered indx
social_data<-social_data%>%filter(gempresent==0)

R_subj=social_data%>%group_by(player)%>%summarise(n_r=length(unique(round)))%>%pull(n_r)

#keep<-unique(social_data$player)[R_subj==12]# keep only participants who completed all trials
# first: cheat and take out "incomplete" cases
social_data_forfit<-social_data%>%filter(group=="adolescents")

keep_forfit<-unique(social_data_forfit$player)#[R_subj==1]# keep only participants who completed all trials
social_data_forfit<-social_data_forfit%>%filter(player %in% keep_forfit)

#make it a list of round numbers
R_subj<-social_data_forfit%>%group_by(player)%>%summarise(n_r=list(unique(round)))%>%pull(n_r)

# experiment specs
T_max<-25
R_max<-social_data_forfit%>%group_by(player)%>%summarize(env=length(unique(env_number)))%>%pull()%>%max()
# number of participants
N<-length(unique(social_data_forfit$player))

#put relevant data into lists
choice=array(-1,c(T_max,R_max,N))
reward=array(-1,c(T_max,R_max,N))
social_info=array(-1,c(T_max,R_max,N))
env_rnd=array(-1,c(R_max,N))

use_rounds<-rep(-1,N)

for (ppt in 1:N){
  use_rounds[ppt]<-length(R_subj[[ppt]])
  for (r in 1:use_rounds[ppt]){  #loops throug unique round indices per participant
    
    env_rnd[r,ppt]<- social_data_forfit%>%filter(
      round==R_subj[[ppt]][r] ,
      player==keep_forfit[ppt]
    )%>%pull(env_number)%>%unique()
    
    for (t in 1:T_max){
      # get choices
      choice[t,r,ppt]<- social_data_forfit%>%filter(
        trial==t ,
        round==R_subj[[ppt]][r] ,
        player==keep_forfit[ppt]
      )%>%pull(cells)
      
      # get points
      reward[t,r,ppt]<-  social_data_forfit%>%filter(
        trial==t,
        round==R_subj[[ppt]][r],
        player==keep_forfit[ppt]
      )%>%pull(points)
      
      # get demonstrator
      social_info[t,r,ppt]<-social_data_forfit%>%filter(
        trial==t,
        round==R_subj[[ppt]][r],
        player==keep_forfit[ppt]
      )%>%pull(social_info)
      
    }
  }
}

reward_z=(reward-mean(reward))/sd(reward)

data_list<-list(
  N=N,
  T_max=T_max,
  R_max=R_max,
  R_subj=use_rounds,
  choices=choice,
  rewards=reward_z#/80#just rescaled
  #social_info=social_info,
  #env_rnd=env_rnd
)

kalman_mod<-stan(
  file="./C_modelfittingCode/stan/model_code/kalman_nokeeper.stan",
  data=data_list,
  pars =c("lr","tau"),
  #init=1,
  iter = 2000,
  chains=4,
  cores=4,
  control = list(adapt_delta=0.99)
)

saveRDS(kalman_mod,"./A_GeneratedFiles/modelfits/stan/kalman_psi_mix_kids.rds")

#mod<-readRDS("./A_GeneratedFiles/modelfits/stan/kalman_psi_mix_kids.rds")
# 
# library(tidybayes)
# mod%>%tidybayes::spread_draws(lr[ppt],tau[ppt])%>%
#   ggplot(aes(x = lr))+
#   stat_density()
# 
# launch_shinystan(mod)
# 
# mod%>%tidybayes::spread_draws(lr[ppt],tau[ppt])%>%group_by(ppt)%>%
#   dplyr::summarise(sd_tau=sd(tau),
#             m_tau=mean(tau),
#             sd_lr=sd(lr),
#             m_lr=mean(lr)
#             )%>%
#   ggplot(aes(x =log(m_tau)))+
#   geom_histogram()
#   
#   
  


# todo: filter out participants with n_eff= supersmall
