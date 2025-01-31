# This file makes model figures
here::i_am("./make_model_figs.R")
pacman::p_load(here,tidyverse,colorspace)
environments <- load_envs_social(path = here("A_GeneratedFiles","environments/"))


set.seed(123)
rnd=sample(1:64,5)# make 5 decisions
environments$choices=NA

one_sample_plot_df<-environments%>%
  filter(env_idx==1)%>%
  mutate(idx=1:n())%>%rowwise()%>%
  dplyr::mutate(choices = ifelse(idx %in% rnd ,rnorm(1,Mean,Variance),NA))#  sample 5 times from environment

# make example env
one_sample_plot<-one_sample_plot_df%>%ggplot(aes(x=x,y=y,fill=choices))+
  geom_tile(width=0.9, height=0.9,color="grey",size=1)+
  scale_fill_distiller(palette = "RdBu",na.value = "white",limits = c(-75, 75))+
  geom_text(aes(label=round(choices)))+
  guides(fill=F)+
  theme_void()+
  theme(aspect.ratio = 1)



# make choice probs (have to fill NAs with 0 now)
one_sample_plot_cp_df<-one_sample_plot_df%>%
  dplyr::mutate(choices = ifelse(is.na(choices) ,0,choices))%>%ungroup()%>%# sample(1,Mean,Variance))
  dplyr::mutate(cp_smfx=exp(choices*0.02))%>%# just softmaximize the choices
  dplyr::mutate(cp=cp_smfx/sum(cp_smfx))#turn into choice probabilities


one_sample_plot_cp<-one_sample_plot_cp_df%>%
  ggplot(aes(x=x,y=y))+
  geom_tile(aes(fill=cp),width=0.9, height=0.9,color="grey",size=1)+
  #scale_fill_distiller(palette = "PiYG",na.value = "white",direction = 1,)+
  scale_fill_continuous_divergingx(name="Choice\nprobability",
                                   palette = 'PiYG', mid = 0.01499,rev=FALSE)+
  guides(fill=F)+
  theme_void()+
  theme(aspect.ratio = 1)

# option 43 sampled by other
one_sample_plot_cp_df$si=0
one_sample_plot_cp_df$si[43]=1

one_sample_plot_cp_SI<-one_sample_plot_cp_df%>%
  mutate(cp=ifelse(si==0,cp*(1-0.1),(0.1) + (cp*(1-0.1))))%>%
  ggplot(aes(x=x,y=y))+
  geom_tile(aes(fill=cp),width=0.9, height=0.9,color="grey",size=1)+
  #scale_fill_distiller(palette = "PiYG",na.value = "white",direction = 1,)+
  scale_fill_continuous_divergingx(name="Choice\nprobability",
                                   palette = 'PiYG', mid = 0.01499,rev=FALSE)+
  # geom_text(aes(label=round(cp)))+
  guides(fill=F)+
  theme_void()+
  theme(aspect.ratio = 1)


# empty env?
empty<-environments%>%
  filter(env_idx==1)%>%
  mutate(Mean="1")%>%
  ggplot(aes(x=x,y=y,fill=Mean))+
  geom_tile(width=0.9, height=0.9,color="grey",size=1)+
  scale_fill_manual(values="white")+
  guides(fill=F)+
  theme_void()+
  theme(aspect.ratio = 1)


ggsave(plot = empty,filename = here::here("plots","1_empty_env.png"),width = 4.5,height = 4.5)
ggsave(plot = one_sample_plot_cp_SI,filename = here::here("plots","4_SI_cp_env.png"),width = 4.5,height = 4.5)
ggsave(plot = one_sample_plot_cp,filename = here::here("plots","3_cp_env.png"),width = 4.5,height = 4.5)
ggsave(plot = one_sample_plot,filename = here::here("plots","2_rewards.png"),width = 4.5,height = 4.5)


#make legends
#
#
#



one_sample_plot_leg<-one_sample_plot_df%>%ggplot(aes(x=x,y=y,fill=choices))+
  geom_tile(width=0.9, height=0.9,color="grey",size=1)+
  scale_fill_distiller(name="points",palette = "RdBu",na.value = "white",limits = c(-75, 75))+
  geom_text(aes(label=round(choices)))+
  # guides(fill=F)+
  theme_void()+
  theme(aspect.ratio = 1)


one_sample_plot_cp_leg<-one_sample_plot_cp_df%>%
  ggplot(aes(x=x,y=y))+
  geom_tile(aes(fill=cp),width=0.9, height=0.9,color="grey",size=1)+
  #scale_fill_distiller(palette = "PiYG",na.value = "white",direction = 1,)+
  scale_fill_continuous_divergingx(name="Choice\nprobability",
                                   palette = 'PiYG', mid = 0.01499,rev=FALSE,
                                    breaks=c(0.05,0.045,0.035,0.025,0.01499,0.01),labels=c("0.05","0.045","0.035","0.025","0.015 (random)","0.01"))+
 # guides(fill=F)+
  theme_void()+
  theme(aspect.ratio = 1)

ggsave(filename = here("plots","points_leg.png"),get_legend(one_sample_plot_leg))

ggsave(filename = here("plots","cp_leg.png"),get_legend(one_sample_plot_cp_leg))
