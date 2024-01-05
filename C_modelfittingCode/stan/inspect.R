kids_dat<-mod_k%>%spread_draws(lr[ppt],tau[ppt])%>%mutate(group="kids")
adults_dat<-mod%>%spread_draws(lr[ppt],tau[ppt])%>%mutate(group="adults")

all_dat<-rbind(kids_dat,adults_dat)

all_dat%>%
  ggplot(aes(x=lr,y=group))+
  stat_summary()


all_dat%>%
  ggplot(aes(x=tau,y=group))+
  stat_summary()
