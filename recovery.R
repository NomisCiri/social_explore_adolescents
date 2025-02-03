# parameter recovery

fitagain=T
#Iteration: 200 bestvalit: 435.902250 bestmemit:    8.708619    0.021584    0.005103    0.336568
if (fitagain){
  social_recovery <- foreach(
    player_nr = unique(social_sims$player),
    .packages = c("DEoptim", "tidyverse","here","data.table"),
    .combine = "rbind"
  ) %dopar%{
    environments <- load_envs_social(path = here("A_GeneratedFiles","environments/"))
    
    Xnew <- as.matrix(expand.grid(0:7, 0:7)) # do this outside the loop for better speed
    d1 <- social_sims %>% 
      filter(player == player_nr, run==1) %>%
      mutate(z =z,
             choices = index
      )%>%ungroup()
    
    cv <- tryCatch(        # might throw an error, just continue if it does and dont be annoying
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
    
    print(cv)
    # collect fit indices
    
    d1$fit <- unlist(cv[1])# is always the fit index
    d1$p1<-unlist(cv[2])
    d1$p2<-unlist(cv[3])#low
    d1$p3<-unlist(cv[4])#medium
    d1$p4<-unlist(cv[5])#high
    
    return(d1)
    
  }
  
  saveRDS(social_recovery,file = here("A_GeneratedFiles","modelfits","policy_recovery.rds"))
}else{
  social_recovery<-readRDS(file = here("A_GeneratedFiles","modelfits","policy_recovery.rds"))%>%ungroup()
  # social_fits<-readRDS(file = here("A_GeneratedFiles","modelfits","tau_slearn_rangs_no_prior.rds"))%>%ungroup()
  # social_fits_prior<-readRDS(file = here("A_GeneratedFiles","modelfits","tau_slearn_rangs_prior.rds"))%>%ungroup()
  # social_fits_old<-readRDS(file = here("A_GeneratedFiles","modelfits","2tau_slearn_rangs.rds"))%>%ungroup()
}

recov_df<-social_recovery%>%mutate(
  which="recovery",
  ppt=ifelse(group=="adults",player+1000,player)# unique player idx
)%>%select(p1,p2,p3,p4,ppt,group,which)%>%unique()

fits_df<-social_fits%>%mutate(
  which="original",
  ppt=ifelse(group=="adults",player+1000,player)
)%>%select(p1,p2,p3,p4,ppt,group,which)%>%unique()

recovery_df_plots<-rbind(recov_df,fits_df)%>%
  pivot_wider(id_cols = ppt,names_from = c(which),values_from = c(p1,p2,p3,p4))

recovery_df_plots%>%ggplot(aes(x=p1_original,p1_recovery))+
  geom_point()+
  geom_smooth()

recovery_df_plots%>%ggplot(aes(x=p2_original,p2_recovery))+
  geom_point() +
  geom_smooth()

recovery_df_plots%>%ggplot(aes(x=p3_original,p3_recovery))+
  geom_point() +
  geom_smooth()

recovery_df_plots%>%ggplot(aes(x=p4_original,p4_recovery))+
  geom_point() +
  geom_smooth()

fits_df
recov_df

