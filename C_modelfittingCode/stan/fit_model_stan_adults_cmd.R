
pacman::p_load(tidyverse, rjson, DEoptim, doParallel, here,data.table,jsonlite,lme4,nnet,rstan,brms)

social_data <-  read_csv(file = paste0("./data/social/data_social_all_participants.csv"))
social_data<-social_data%>%mutate(player=ifelse(group=="adults",player,player+1000))

## get unreasonable nans
nans<-social_data%>%
  filter(is.na(demo_type) | is.na(cells))%>%pull(player)%>%unique()

social_data<-social_data%>%filter(!(player %in% nans))
R_subj=social_data%>%group_by(player)%>%summarise(n_r=length(unique(round)))%>%pull(n_r)


 social_data<-social_data%>%#drop_na()
  filter(!is.na(demo_type) & !is.na(cells))%>%#filter(player<30)%>%# get a sub-sample of both adults and adolescents 
  mutate(soctype=case_when(
    demo_type=="gem_found"~1,# i need that to index the different social weight parameters
    demo_type=="gem_not_found" | demo_type=="no_gem"~2,
    str_detect(demo_type,"never_exploit") ~ 3
    )
  )
#give kids a unqiquely numbered indx
social_data<-social_data%>%mutate(player=ifelse(group=="adults",player,player+1000))
R_subj=social_data%>%group_by(player)%>%summarise(n_r=length(unique(round)))%>%pull(n_r)
#keep<-unique(social_data$player)[R_subj==12]# keep only participants who completed all trials

#only keep comsocial_dataplete rounds
#social_data<-social_data%>%filter(player %in% keep)
# Fit model to social data

# first: cheat and take out "incomplete" cases
social_data_forfit<-social_data%>%filter(player<1000)
R_subj<-social_data_forfit%>%group_by(player)%>%summarise(n_r=list(unique(round)))%>%pull(n_r)#%>%length()

keep_forfit<-unique(social_data_forfit$player)#[R_subj==12]# keep only participants who completed all trials
social_data_forfit<-social_data_forfit%>%filter(player %in% keep_forfit)

#make it a list of round numbers

# experiment specs
T_max<-25
R_max<-social_data_forfit%>%group_by(player)%>%summarize(env=length(unique(env_number)))%>%pull()%>%max()
# number of participants
N<-length(unique(social_data_forfit$player))

#put relevant data into lists
choice=array(-1,c(T_max,R_max,N))
reward=array(-1,c(T_max,R_max,N))
social_info=array(-1,c(T_max,R_max,N))
demo_type_dat=array(-1,c(T_max,R_max,N))
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
      
       # get demonstrator
      demo_type_dat[t,r,ppt]<-social_data_forfit%>%filter(
        trial==t,
        round==R_subj[[ppt]][r],
        player==keep_forfit[ppt]
      )%>%pull(soctype)
      
    }
  }
}

data_list_adolescents<-list(
  N=N,
  T_max=T_max,
  R_max=R_max,
  R_subj=use_rounds,
  choices=choice,
  rewards=reward,#/80,#just rescaled
  social_info=social_info,
  demo_type=demo_type_dat,
  env_rnd=env_rnd
)

Q_mod_adul<-stan(
  file="./C_modelfittingCode/stan/model_code/Q_nokeeper_sw.stan",
  data=data_list_adolescents,
  pars =c("lr","tau","sw"),
 # init=1,
  iter = 1000,
  chains=3,
  cores=3
  )

saveRDS(object = Q_mod_adul,file = "./A_GeneratedFiles/modelfits/Q_sw_stan_adults.rds")

