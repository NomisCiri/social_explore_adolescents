
library(tidyverse)
library(ggplot2)
library(brms)

data_long <- read.csv('clean_data/clean_data.csv')

plot1 <- data_long %>% 
  group_by(player,  gempresent) %>%  
  mutate(mean_score = mean(tot_points)) 

ggplot(plot1) +
  geom_col(data = plot1,
           aes(x = player, y = ))+
  facet_wrap(~gempresent)

pilot_data<-read_csv(file = "clean_data/clean_data.csv")

env_gem="clean_data/environments_gem_250_var25max.json"
env_no_gem="clean_data/environments_no_gem_var25max.json"


pilot_data%>%group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  pivot_longer(cols=c(exploit,explore),names_to = "explore_exploit")%>%
  ggplot(aes(x=points,y=value,color=explore_exploit))+
  stat_summary()+
  theme_minimal(20)

pilot_data%>%
  group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  mutate(gem=ifelse(points>150,1,0))%>%
  brm(data=.,explore~gem+(1|player),cores=4,family = bernoulli)



# Exploration changes depending on outcomes
pilot_data%>%
  group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  mutate(out_cat=case_when(points>150~1,
                           points<0~3,
                           TRUE~2
  )
  )%>%pivot_longer(cols=c(exploit,explore),names_to = "explore_exploit")%>%
  ggplot(aes(x=out_cat,y=value,color=explore_exploit,shape=explore_exploit))+
  stat_summary(size=1)+
  stat_summary(geom="line")+
  scale_y_continuous(name="roportion")+
  scale_x_continuous(name="category",breaks=c(1,2,3),labels=c("gem","no gem","loss"))+
  theme_minimal(60)


pilot_data%>%
  group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  mutate(out_cat=case_when(points>150~1,
                           points<0~3,
                           TRUE~2
  )
  )%>%
  brm(data=.,explore~as.factor(out_cat)+(1|player),cores=4,family = bernoulli)



#max(pilot_data$player)
pilot_data%>%
  group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  group_by(player,round)%>%
  mutate(trial=1:n())%>%
  filter(gem==1) 



mutate(out_cat=case_when(points>150~1,
                         points<0~3,
                         TRUE~2
)
)
# exploration environments

pilot_data%>%
  group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  ggplot(aes(x=as.factor(gempresent),y=explore))+
  stat_summary()+
  scale_x_discrete(name="gem present",breaks=c(0,1),labels=c("No","Yes"))+
  scale_y_continuous(name="proportion exploration")+
  theme_minimal(20)


pilot_data%>%
  group_by(player)%>%
  mutate(exploit=ifelse(lead(cells)==cells,1,0),
         explore=ifelse(lead(cells)!=cells,1,0))%>%
  mutate(out_cat=case_when(points>0~"reward",
                           points<=0~"loss",
                           #TRUE>2
  )
  )%>%
  ggplot(aes(x=out_cat,fill=as.factor(gempresent)))+
  geom_bar(stat="count",position="dodge")+
  scale_fill_discrete(name="Gem Present")+
  theme_minimal(20)
#facet_wrap(~gempresent)
                          
                          