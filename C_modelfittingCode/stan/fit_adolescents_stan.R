
# this code fits the range normalization stan model on the social bandits data
pacman::p_load(tidyverse, here,rstan)
here::i_am("./C_modelfittingCode/stan/fit_adolescents_stan.R")
refit=T

social_data <-  read_csv(file = paste0(here("data","social","data_social_all_participants.csv")))
social_data<-social_data%>%mutate(player=ifelse(group=="adults",player,player+1000))

## get unreasonable nans
nans<-social_data%>%
  filter(is.na(demo_type) | is.na(cells))%>%pull(player)%>%unique()

social_data<-social_data%>%filter(!(player %in% nans))
R_subj=social_data%>%group_by(player)%>%summarise(n_r=length(unique(round)))%>%pull(n_r)

# recode
social_data<-social_data%>%#drop_na()
  filter(!is.na(demo_type) & !is.na(cells))%>%#filter(player<30)%>%# get a sub-sample of both adults and adolescents 
  mutate(soctype=case_when(
    demo_type=="gem_found"~3,# i need that to index the different social weight parameters
    demo_type=="gem_not_found" | demo_type=="no_gem"~2,
    str_detect(demo_type,"never_exploit") ~ 1
  )
  )
#give kids a unqiquely numbered indx
R_subj=social_data%>%group_by(player)%>%summarise(n_r=length(unique(round)))%>%pull(n_r)
keep_forfit_adolescents<-social_data%>%filter(group=="adolescents")%>%pull(player)%>%unique()#[R_subj==12]# keep only participants who completed all trials


data_list_adolescents<-readRDS(file = here("A_GeneratedFiles","data_list_adolescents.rds"))

#range normalization
rewards_new_adol<-data_list_adolescents$rewards
# rescale rewards for adolescents
for (x in 1:length(data_list_adolescents$rewards[1,,1])){
  for (y in 1:length(data_list_adolescents$rewards[1,1,])){
    for (z in 1:length(data_list_adolescents$rewards[,1,1])){
      
      rewards_new_adol[z,x,y]=ifelse(data_list_adolescents$gem_found[z,x,y]==2,
                                     (data_list_adolescents$rewards[z,x,y]+75)/150,
                                     (data_list_adolescents$rewards[z,x,y]+75)/363
                                     
      )
    }
  }
}

data_list_adolescents$rewards<-rewards_new_adol


model_compiled<-stan_model(
  model_name = "Q_sw_rangenorm",
  file=here("C_modelfittingCode","stan","model_code","Q_nokeeper_sw_gem_nogem_range_n.stan"),
  allow_optimizations=T,
  warn_pedantic=T
)

Q_mod_adol<-sampling(
  model_compiled,
  data=data_list_adolescents,
  pars =c("lr","tau","sw","log_lik"),
  init=0,
  iter = 4000,
  chains=6,
  cores=6
)

saveRDS(object = Q_mod_adol,file = here("A_GeneratedFiles","modelfits","Q_nokeeper_sw_gem_nogem_range_n_adolescents_2.rds"))
#Q_mod_adol<-readRDS(file = "./A_GeneratedFiles/modelfits/Q_sw_gem_nogem_stan_adolescents.rds")

