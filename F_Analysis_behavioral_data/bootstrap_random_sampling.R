# Bootstrap 

envs<-load_envs_social()

#bootstrap for random null distribution

gem<-envs%>%filter(env==9)
no_gem<-envs%>%filter(env==4)

bootsraps=1000
rewards_bt<-NULL

for (bt in 1:bootsraps){
  rews_1<-NULL
  for (i in 1:12){
    for (t in 1:25){
      idx<-sample(1:64,1)
      if (i<9){
        rew=rnorm(mean = gem[idx,]%>%pull(Mean),
                  sd = gem[idx,]%>%pull(Variance),
                  n = 1)
      }else {
        rew=rnorm(mean = no_gem[idx,]%>%pull(Mean),
                  sd = no_gem[idx,]%>%pull(Variance),
                  n = 1)
      }
      rews_1<-cbind(rews_1,as.numeric(rew))
    }
  }
  rewards_bt<-rbind(rewards_bt,rews_1)
}

write_rds(rewards_bt,file = "A_GeneratedFiles/bootstrapped_random_rewards.rds")