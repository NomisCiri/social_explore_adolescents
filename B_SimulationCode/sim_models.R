##############################################################################################
##############################################################################################
###                                                                                        ###
###  FUNCTIONS TO SIMULATE EXPLORATION OF ENVIRONMENT GIVEN PARAMETERS FROM MODEL FITTING  ###
###                                                                                        ###
##############################################################################################
##############################################################################################


#-----------------------------------------------------------
##              Model: Q-learning lr prior               --
##----------------------------------------------------------------

s_learn_prior_sim <- function(par, learning_model_fun, acquisition_fun, data, envs) {
  # parameters are unpacked
  theta <-par[1]# "learningrate"
  
  tau<-par[2]
  omega<-par[3]
  #b<-par[3]
  
  #hardcoded moving parts
  mu0 <-  0# prior
  tau_copy<-1#par[4]
  prior_copy<-par[4]
  
  # a bunch of containers
  mu=list()
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  all_choices<-NULL
  
  for (r in unique(data$round)){
    
    # collect choices for current round
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#
    trials <- nrow(round_df)
    # social information
    social_choices <- round_df$social_info
   # streak<-round_df$streak
    copyUtil<- rep(prior_copy, length(round_df$choices))#roughly chance
    
    
    # Utilties of each choice
    out<-rep(mu0,64)
    #here, too
    ind <- round_df$choices[1]
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = 0))
    
    # store first choice of round
    # store first choice of round
    #first choice from previous
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=social_choices[1],
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soc_info_round),
      social_reward=0,
      copy_util=copyUtil[1],
      copy_prob=1/64
    )
    
    #did you see the gem for the first time?
    # loop over trials
    for (t in 1:(trials - 1)) {
      # learn
      # if you found a gem, rescale tau
      out <- RW_Q(ind, y[t], theta = theta, prevPost = out, mu0Par = mu0)
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      if (ind==social_choices[t]){
        copyUtil[t:trials]=copyUtil[t]+omega*(y[t]-copyUtil[t])
      }
      copy_prob=1/(1+exp(-(copyUtil[t])/tau_copy))
      utilityVec <- out
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(utilityVec / tau)
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      p_sl=p_sfmx*(1-copy_prob)
      p_sl[social_choices[t]] <- (copy_prob) + (p_sfmx[social_choices[t]]*(1-copy_prob))
      social_reward<- env[round_df$social_info[t], ]$Mean
      
      if (is.na(sum(p_sl))){
        browser()
      }
      ind <- sample(1:64, 1, prob = p_sl) # choice index
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd =sqrt(env[ind, ]$Var)))) 
      
      
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t+1, 1]),
        y = as.numeric(X[t+1 , 2]),
        z = as.numeric(y[t+1]),
        index = ind,
        social_info = social_choices[t],
        round = r,
        util_list = I(list(out)),
        p_list = I(list(out)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soc_info_round),
        social_reward=env[social_choices[t], ]$Mean,
        copy_util=copyUtil[t+1],
        copy_prob=copy_prob
      )
      
      
      # concat round choices
      round_choices <- rbind(round_choices, one_trial_choices)
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  return(all_choices)
}





  #-----------------------------------------------------------
  ##              Model: Q-learning no social weight              --
  ##----------------------------------------------------------------

s_learn_sim <- function(par, learning_model_fun, acquisition_fun, data, envs) {
  # parameters are unpacked
  tau<-par[1]
  omega<-par[2]
  #b<-par[3]
  
  #hardcoded moving parts
  theta <-1# par[1]# "learningrate"
  mu0 <-  0# prior
  tau_copy<-0.1#par[4]
  prior_copy<--2.5#par[5]
  
  # a bunch of containers
  mu=list()
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  all_choices<-NULL
  
  for (r in unique(data$round)){
    
    # collect choices for current round
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#
    trials <- nrow(round_df)
    # social information
    social_choices <- round_df$social_info
    #streak<-round_df$streak
    copyUtil<- rep(prior_copy, length(round_df$choices))#roughly chance
    
    
    # Utilties of each choice
    out<-rep(mu0,64)
    #here, too
    ind <- round_df$choices[1]
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = 0))
    
    # store first choice of round
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=social_choices[1],
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soc_info_round),
      social_reward=0,
      copy_util=copyUtil[1],
      copy_prob=1/64
    )
    
    #did you see the gem for the first time?
    # loop over trials
    for (t in 2:(trials-1)) {
      # learn
      # if you found a gem, rescale tau
      out <- RW_Q(ind, y[t], theta = theta, prevPost = out, mu0Par = mu0)
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      if (ind==social_choices[t]){
        copyUtil[t:trials]=copyUtil[t]+omega*(y[t]-copyUtil[t])
      }
      copy_prob=1/(1+exp(-(copyUtil[t])/0.01))
      utilityVec <- out
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(utilityVec / tau)
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      p_sl=p_sfmx*(1-copy_prob)
      p_sl[social_choices[t]] <- (copy_prob) + (p_sfmx[social_choices[t]]*(1-copy_prob))
      social_reward<- env[round_df$social_info[t], ]$Mean
      
      if (is.na(sum(p_sl))){
        browser()
      }
      ind <- sample(1:64, 1, prob = p_sl) # choice index
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd =sqrt(env[ind, ]$Var)))) 
      
      
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t + 1, 1]),
        y = as.numeric(X[t + 1, 2]),
        z = as.numeric(y[t + 1]),
        index = ind,
        social_info = social_choices[t + 1],
        round = r,
        util_list = I(list(out)),
        p_list = I(list(out)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soc_info_round),
        social_reward=env[social_choices[t], ]$Mean,
        copy_util=copyUtil[t+1],
        copy_prob=copy_prob
      )
      
      
      # concat round choices
      round_choices <- rbind(round_choices, one_trial_choices)
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  return(all_choices)
}







##-----------------------------

#
#add social reward
#

social_rewards<- function(data, envs) {
  all_choices <- NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    #streak<-round_df$streak
    
    # Utilties of each choice
    #here, too
    ind <- round_df$choices[1]
    points<-round_df$points[1]
    # store first choice of round
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      points=points,
      ind=ind,
      social_reward=0,
      round=r,
      demo_type=round_df$demo_type[1]
    )
    
    
    # loop over trials
    for (t in 2:(trials)) {
      
      ind <- round_df$choices[t]
      points<-round_df$points[t]
      social_choices<-round_df$social_info[t]
      social_reward<- env[round_df$social_info[t], ]$Mean
     
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t, 
        points=points,
        ind=ind,
        social_reward=social_reward,
        round=r,
        demo_type=round_df$demo_type[t]
      )
      
      
      # concat round choices
      round_choices <- rbind(round_choices, one_trial_choices)
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  return(all_choices)
}


##----------------------------------------------------------------
##              Model: Q-learning no social weight              --
##----------------------------------------------------------------

sim_range_1lr_sws <- function(par, learning_model_fun, acquisition_fun, data, envs) {
  # parameters are unpacked lated
  lr=1
  mu0 <-  0# prior
  tau<-par[1]
  # a bunch of containers
  mu=list()
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    streak<-round_df$streak
    
    # Utilties of each choice
    out<-rep(mu0,64)
    #here, too
    ind <- round_df$choices[1]
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = 0))
    y_scaled<-rep(0,length(y))
    # store first choice of round
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      y_scaled=0,
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soc_info_round),
      social_reward=0
    )
    
    #did you see the gem for the first time?
    first=TRUE
   # browser()
    
   
    
    # loop over trials
    for (t in 1:(trials-1)) {
      
      #range adaptation and conditions
      if(max(y[1:t])<120){
        tau<-par[1]
        # if (streak[t]>1){
        #   omega<-par[3]#+par[2]*streak[t]#+par[3]
        # }else{
        #   omega<-0
        # }
        #
        if (streak[t]>1){
          omega<-par[3]#+par[2]*streak[t]#+par[3]
        }else{
          omega<-0
        }
        
        y_scaled[t] = (y[t]+75)/150#range normalization rule: (x-rmin)/(rmax-rmin)
      }else {# when there is a gem. 
        #crucial: rescale q-values the first time we saw the gem
        omega=0
        tau<-par[2]
        
        if(first){
          out<-out*0.002754821
          first=FALSE
        }
        # reward value of the new range
        y_scaled[t] = (y[t]+75)/(363)#range normalization rule for gems: (x-rmin)/(rmax-rmin)
      }
      
      
      out <- RW_Q(ind, y_scaled[t], theta = lr, prevPost = out, mu0Par = mu0)
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      utils<-out
      
      utils[social_choices[t]] <- utils[social_choices[t]] + omega
      p_sfmx <- exp(utils / tau)
      p_sfmx[p_sfmx <= -Inf]<-0
      p_sfmx[p_sfmx >= Inf]<-100
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      
      # probabilities
      # numerical overflow
      p_sfmx <- (pmax(p_sfmx, 0.00001))
      p_sfmx <- (pmin(p_sfmx, 0.99999))
      
      if (is.na(sum(p_sfmx))){
        browser()
      }
      ind <- sample(1:64, 1, prob = p_sfmx) # choice index
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd =0))) 
      
      
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t + 1, 1]),
        y = as.numeric(X[t + 1, 2]),
        z = as.numeric(y[t + 1]),
        y_scaled=as.numeric(y_scaled[t+1]),
        index = ind,
        social_info = social_choices[t + 1],
        round = r,
        util_list = I(list(out)),
        p_list = I(list(out)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soc_info_round),
        social_reward=env[social_choices[t], ]$Mean
      )
      
      
      # concat round choices
      round_choices <- rbind(round_choices, one_trial_choices)
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  return(all_choices)
}

##----------------------------------------------------------------
##              Model: Q-learning no social weight              --
##----------------------------------------------------------------

sim_sw3 <- function(par, learning_model_fun, acquisition_fun, data, envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- 1# "learningrate"
  tau <- par[1] #  "random" exploration
  omegas<- par[2:4]
  mu0 <-  0
  
  mu=list()
  all_choices <- NULL
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      env_idx=unique(round_df$env_number),
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      omega<-omegas[round_df$qual[t]]
      
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      # browser()
      if (t > 1) {
        out <- RW_Q(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
      } else {
        out <- RW_Q(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #utilities
      utilityVec <- out
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]+omega# social update of utility
      
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / sum(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t + 1, 1]),
        y = as.numeric(X[t + 1, 2]),
        z = as.numeric(y[t + 1]),
        index = ind,
        env_idx=unique(round_df$env_number),
        social_info = social_choices[t + 1],
        round = r,
        util_list = I(list(utilityVec)),
        p_list = I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}


##############################################################################################
##############################################################################################
###                                                                                        ###
###  Depreciated  ###
###                                                                                        ###
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
###                                                                                        ###
###  Depreciated  ###
###                                                                                        ###
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
###                                                                                        ###
###  Depreciated  ###
###                                                                                        ###
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
###                                                                                        ###
###  Depreciated  ###
###                                                                                        ###
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
###                                                                                        ###
###  Depreciated  ###
###                                                                                        ###
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
###                                                                                        ###
###  Depreciated  ###
###                                                                                        ###
##############################################################################################
##############################################################################################s


##---------------------------------------------------------------
##softmax & epsilon greedy exploration ucb for Kalman filter   -
##---------------------------------------------------------------
KF_mix_softmax_epsilonGreedy <- function(out, epsilon=0.1,tau=0.1,beta=0,zeta=0,social_choices,y,t){
  #out is data frame
  n <- length(out$mu)
  #browser()
  if(max(y>150)){
    #if gem was found: greedy
    p <- rep(1/n*epsilon, n)#how many options
    utility_vec <- out$mu +(beta *sqrt(out$sig))
    p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  }else{
    #if gem was not found: social-softmax
    utility_vec <- out$mu +(beta *sqrt(out$sig))
    #utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]
    p <- exp(utility_vec / tau)
    # probabilities
    p <- p / sum(p)
  }
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}


##---------------------------------------------------------------
##softmax & epsilon greedy exploration ucb for Kalman filter   -
##---------------------------------------------------------------
KF_mix_2softmax_sw<- function(out,tau=c(0.1,0.1),beta=0,y,t){
  #out is data frame
  n <- length(out$mu)
  #browser()
  if(max(y>150)){
    #if gem was found: use tau1
    utility_vec <- out$mu +(beta *sqrt(out$sig))
    utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]+zeta
    p <- exp(utility_vec / tau[1])
    # probabilities
    p <- p / sum(p)
  }else{
    #if gem was not found: use tau2
    utility_vec <- out$mu +(beta *sqrt(out$sig))
    utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]+zeta
    p <- exp(utility_vec / tau[2])
    # probabilities
    p <- p / sum(p)
  }
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}


##----------------------------------------------------------------
##              Model: Q-learning no social weight              --
##----------------------------------------------------------------

sim_range_1lr <- function(par, learning_model_fun, acquisition_fun, data, envs) {
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[1]# "learningrate"
  mu0 <-  0
  
  mu=list()
  all_choices <- NULL
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    out<-rep(mu0,64)
    #here, too
    ind <- round_df$choices[1]
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    y_scaled<-rep(0,length(y))
    # store first choice of round
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soc_info_round)
    )
    
    #did you see the gem for the first time?
    first=TRUE
    
    # loop over trials
    for (t in 1:(trials-1)) {
      #range adaptation
      if(max(y[1:t])<120){
        tau <- par[2] #  "random" exploration
        
        y_scaled[t] = (y[t]+75)/150#range normalization rule: (x-rmin)/(rmax-rmin)
      }else {# when there is a gem. 
        #crucial: rescale q-values to gem range first time we saw the gem
        if(first){
          tau <- par[1] #  "random" exploration
          
          out<-out/4.84# (this is: out/((rmax_gem-rmin)/(rmax_non_gem-rmin))
          first=FALSE
        }
        # reward value of the new range
        y_scaled[t] = (y[t]+75)/(363)#range normalization rule for gems: (x-rmin)/(rmax-rmin)
      }
      
      out <- RW_Q(x=ind, y=y_scaled[t], theta = lr, prevPost = out, mu0Par = mu0)
      utilityVec <- out
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]# social update of utility
      
      # softmaximization
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / sum(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      ind <- sample(1:64, 1, prob = p) # choice index
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t + 1, 1]),
        y = as.numeric(X[t + 1, 2]),
        z = as.numeric(y[t + 1]),
        index = ind,
        social_info = social_choices[t + 1],
        round = r,
        util_list = I(list(utilityVec)),
        p_list = I(list(p)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soc_info_round)
      )
      
      
      # concat round choices
      round_choices <- rbind(round_choices, one_trial_choices)
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  return(all_choices)
}



##----------------------------------------------------------------
##              Model: Q-learning no social weight              --
##----------------------------------------------------------------

exploreEnv1lr <- function(par, learning_model_fun, acquisition_fun, data, envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[1]# "learningrate"
  tau <- par[2] #  "random" exploration
  mu0 <-  0
  
  mu=list()
  all_choices <- NULL
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      # browser()
      if (t > 1) {
        out <- RW_Q(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
      } else {
        out <- RW_Q(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      browser()
      #utilities
      utilityVec <- ucb(out, 0)
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]# social update of utility
      
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / rowSums(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t + 1, 1]),
        y = as.numeric(X[t + 1, 2]),
        z = as.numeric(y[t + 1]),
        index = ind,
        social_info = social_choices[t + 1],
        round = r,
        util_list = I(list(utilityVec)),
        p_list = I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}


##---------------------------------------------------------------
##     Model: Q-learning 2 learning rates no social weight     --
##---------------------------------------------------------------

exploreEnv2lr <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- c(par[1],par[2])# "learningrates, 1: pos, 2: neg"
  tau <- par[3] #  "random" exploration
  mu0=0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      # browser()
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #browser()
      #utilities
      utilityVec <- out
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] # social update of utility
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / colSums(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t + 1,
        x = as.numeric(X[t + 1, 1]),
        y = as.numeric(X[t + 1, 2]),
        z = as.numeric(y[t + 1]),
        index = ind,
        social_info = social_choices[t + 1],
        round = r,
        util_list = I(list(utilityVec)),
        p_list = I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}

##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with social weight    --
##---------------------------------------------------------------

exploreEnv2lrsw <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- c(par[1],par[2])# "learningrates, 1: pos, 2: neg"
  tau <- par[3] #  "random" exploration
  zeta <- par[4] # scales social info use
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      # browser()
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #browser()
      #utilities
      utilityVec <- ucb(out, 0)
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + zeta# social update of utility
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / colSums(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}



##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with social weight    --
##---------------------------------------------------------------

exploreEnv1lrsw <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- 1#par[1]# "learningrates, 1: pos, 2: neg"
  tau <- par[1] #  "random" exploration
  zeta <- par[2] # scales social info use
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs

    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    #browser()
    
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soc_info_round)
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      # browser()
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #utilities
      utilityVec <- out
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + zeta# social update of utility
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / sum(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      #y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #only expectation value
      y <- rbind(y, env[ind, ]$Mean) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        util_list=I(list(utilityVec)),
        p_list=I(list(p)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soc_info_round)
      )
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}




exploreEnv_2lr_2sw_2tau <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[c(1,2)]# "learningrates, 1: pos, 2: neg"
  tau <- par[c(3,4)] #  "random" exploration
  zeta <- par[c(5,6)] # scales social info use
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    
    ind <- round_df$choices[1]# first choice
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soc_info_round)
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # don't forget mean centering and standardization.... mean is already 0 :)
      # browser()
      if(max(y)>150){
        zeta_curr<-zeta[1]
        lr_curr<-lr[1]
        tau_curr<-tau[1]
      }else{
        zeta_curr<-zeta[2]
        lr_curr<-lr[2]
        tau_curr<-tau[2]
      }
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr_curr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr_curr, prevPost = NULL, mu0Par = mu0)
      }
      #utilities
      utilityVec <- out
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + zeta_curr# social update of utility
      # also different taus (empirical medians)
      p <- exp(utilityVec / tau_curr)
      # probabilities
      p <- p / sum(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        util_list=I(list(utilityVec)),
        p_list=I(list(p)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soc_info_round)
      )
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}






##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with social weight    --
##---------------------------------------------------------------

exploreEnv2_lrsw <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[1]# "learningrates, 1: pos, 2: neg"
  tau <- par[2] #  "random" exploration
  zeta <- par[3] # scales social info use
  
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    #browser()
    
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$demo_quality)
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      
      if (max(y) >150){
        lr <- par[4]# "learningrates, 1: pos, 2: neg"
        tau <- par[5] #  "random" exploration
        zeta <- par[6] # scales social info use
      }else{
        lr <- par[1]# "learningrates, 1: pos, 2: neg"
        tau <- par[2] #  "random" exploration
        zeta <- par[3] # scales social info use
      }
      # output by GP with particular parameter settings
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #utilities
      utilityVec <- out
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + zeta# social update of utility
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / sum(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        util_list=I(list(utilityVec)),
        p_list=I(list(p)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$demo_quality)
      )
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}







##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with social weight    --
##---------------------------------------------------------------

exploreEnv2_lr_3sw <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[1]# "learningrates, 1: pos, 2: neg"
  tau <- par[2] #  "random" exploration
  zeta <- c(par[4],par[5],par[6]) # scales social info use
  
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    round_df <- data%>%filter(round == r)
    # get the right environment to sample from
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    # is variance the problem maybe?
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(rep(1/64,64))),
      env_idx=unique(round_df$env_number),
      demo_quality=unique(round_df$soctype)
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      
      if (max(y) >150){
        tau <- par[3] #  "random" exploration
      }else{
        tau <- par[2] #  "random" exploration
      }
      # output by GP with particular parameter settings
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #utilities
      utilityVec <- out
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + zeta[unique(round_df$soctype)]# social update of utility with unique soctype
      # softmaximization
      #utilities=utilityVec-max(utilityVec)# get no NAs
      p <- exp(utilityVec / tau)
      # probabilities
      p <- p / sum(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        util_list=I(list(utilityVec)),
        p_list=I(list(p)),
        env_idx=unique(round_df$env_number),
        demo_quality=unique(round_df$soctype)
      )
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}

##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with social weight and greedy   --
##---------------------------------------------------------------

simumalte_2lr_sw_2greedy <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- c(par[1],par[2])# "learningrates, 1: pos, 2: neg"
  epsilon <- c(par[3],par[4]) #  "random" exploration
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      envi=unique(round_df$env_number),
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      #browser()
      #print(r)
      #print(t)
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #browser()
      #utilities
      p<-two_epsilonGreedy(out,epsilon,y[1:t])
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        envi=unique(round_df$env_number),
        #util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}






##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with social weight and greedy   --
##---------------------------------------------------------------

simumalte_2lr_sw_softmax_egreedy <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] # scales social info use
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      envi=unique(round_df$env_number),
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      #browser()
      #print(r)
      #print(t)
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #browser()
      #utilities
      p<-mix_soc_softmax_epsilonGreedy(out,epsilon,tau,zeta,social_choices,y=y[1:t],t)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        envi=unique(round_df$env_number),
        #util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}



##---------------------------------------------------------------
##    Model: Q-learning 2 learning rates with 3social weight and greedy   --
##---------------------------------------------------------------

simumalte_2lr_3sw_softmax_egreedy <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- c(par[5],par[6],par[7],par[8]) # scales social info use
  # scales social info use
  mu0 <- 0
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)#which type of SI?
    
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      envi=unique(round_df$env_number),
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      #browser()
      #print(r)
      #print(t)
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
      }
      #browser()
      #utilities
      p<-mix_3soc_softmax_epsilonGreedy(out,epsilon,tau,zeta,social_choices,soctype,y=y[1:t],t)#(out,epsilon,tau,zeta,social_choices,y=y[1:t],t)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      #  browser()
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        envi=unique(round_df$env_number),
        #util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}


##---------------------------------------------------------------
##    Model: BMT with UCB  --
##---------------------------------------------------------------

simulate_bmt_ucb_softmax_egreedy <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  error_var <- c(par[1])# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  mu0 <- 0
  var0<-10
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      envi=unique(round_df$env_number),
      exp_ch=round_df$choices[1],
      exp_rew=round_df$points[1],
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      #browser()
      #print(r)
      #print(t)
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = error_var, prevPost = out, mu0Par = mu0,var0Par=var0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = error_var, prevPost = NULL, mu0Par = mu0,var0Par=var0)
      }
      #browser()
      #utilities
      p<-KF_mix_softmax_epsilonGreedy(out,epsilon,tau,ucb,y=y[1:t],t)
      #browser()
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      
      ind <- sample(1:64, 1, prob = p) # choice index
      
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        envi=unique(round_df$env_number),
        exp_ch=round_df$choices[t+1],
        exp_rew=round_df$points[t+1],
        #util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}




##----------------------------------------------------------------
##     Model: KF-learning 1 learning rate with social weight & greedy     --
##----------------------------------------------------------------


simulate_bmt_ucb_softmax_egreedy_socw <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  error_var <- c(par[1])# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta<-par[5]
  mu0 <- 0
  var0<-10
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      envi=unique(round_df$env_number),
      exp_ch=round_df$choices[1],
      exp_rew=round_df$points[1],
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      #browser()
      #print(r)
      #print(t)
      if (t > 1) {
        out <- learning_model_fun(ind, y[t], theta = error_var, prevPost = out, mu0Par = mu0,var0Par=var0)
        
      } else {
        out <- learning_model_fun(ind, y[t], theta = error_var, prevPost = NULL, mu0Par = mu0,var0Par=var0)
      }
      #browser()
      #utilities
      p<-KF_sw_mix_softmax_epsilonGreedy(out,epsilon = epsilon,tau = tau,ucb = ucb,zeta=zeta,y=y[1:t],t,social_choices = social_choices)
      #browser()
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #  browser()
      
      ind <- sample(1:64, 1, prob = p) # choice index
      
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        envi=unique(round_df$env_number),
        exp_ch=round_df$choices[t+1],
        exp_rew=round_df$points[t+1],
        #util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}







##----------------------------------------------------------------
##     Model: KF-learning 1 learning rate with social weight 2 taus --
##----------------------------------------------------------------


simulate_bmt_ucb_2softmax_socw <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  error_var <- c(par[1])# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-c(par[3],par[4])
  zeta<-par[5]
  mu0 <- 0
  var0<-10
  
  mu <- list()
  all_choices <- NULL
  dummy <- NULL
  
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  # browser()
  all_choices<-NULL
  
  for (r in unique(data$round)){
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    # browser()
    # get the right environment to sample from
    #browser()
    env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
    #browser()
    trials <- nrow(round_df)
    # social information
    social_choices<-round_df$social_info
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    ind <- round_df$choices[1]
    nTrials <- length(social_choices)
    X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    # store first choice of round
    round_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      envi=unique(round_df$env_number),
      exp_ch=round_df$choices[1],
      exp_rew=round_df$points[1],
      p_list=I(list(rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0, standardization  :)
      
      if (t > 1) {
        out <- bayesianMeanTracker(ind, y[t]/sd(y), theta = error_var, prevPost = out, mu0Par = mu0,var0Par=var0)
        
      } else {
        out <- bayesianMeanTracker(ind, y[t]/sd(y), theta = error_var, prevPost = NULL, mu0Par = mu0,var0Par=var0)
      }
      #utilities
      p<-KF_sw_mix_2softmax(out,tau = tau,ucb = ucb,zeta=zeta,y=y[1:t],t,social_choices = social_choices)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
      # y_real=rbind
      # write it to the next trial index because choice has already been made, learning will happen in next round
      one_trial_choices <- data.frame(
        trial = t+1, 
        x = as.numeric(X[t+1, 1]), 
        y = as.numeric(X[t+1, 2]),
        z = as.numeric(y[t+1]),
        index=ind,
        social_info=social_choices[t+1],
        round=r,
        envi=unique(round_df$env_number),
        exp_ch=round_df$choices[t+1],
        exp_rew=round_df$points[t+1],
        #util_list=I(list(utilityVec)),
        p_list=I(list(p))
      )
      
      round_choices <- rbind(round_choices, one_trial_choices)
      # browser()
    }
    all_choices<-rbind(all_choices,round_choices)
  }# end rounds
  #browser()
  return(all_choices)
}
##----------------------------------------------------------------
##     Model: Q-learning 1 learning rate with social weight     --
##----------------------------------------------------------------

# explore_env_social_fitted_pars_social <- function(par, learning_model_fun, acquisition_fun,data,envs) {
#   # for (rep in 1:ntrialss){
#   # unpack
#   #browser()
#   #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
#   lr <- par[1]# "learningrate"
#   tau <- par[2] #  "random" exploration
#   zeta<-par[3] # scales social info use
#   mu0=0
#   
#   mu=list()
#   all_choices <- NULL
#   dummy <- NULL
#   #look up samples
#   dat=expand.grid(x1=0:7,x2=0:7)
#   plot_dat=list()
#   chosen=NULL
#  # browser()
#   all_choices<-NULL
#   
#   for (r in unique(data$round)){
#     # collect choices for current round
#     #print(r)
#     round_df <- data%>%filter(round == r)
#     #get environment as seen by participant
#    # browser()
#     # get the right environment to sample from
#     #browser()
#     env=envs%>%filter(env==unique(round_df$env_number))#[[unique(round_df$env_number)]]
#     #browser()
#     trials <- nrow(round_df)
#     # social information
#     social_choices<-round_df$social_info
#     # Utilties of each choice
#     utilities <- NULL
#     prevPost <- NULL # set the previous posterior computation to NULL for qlearning
#     pMat <- NULL
#     #here, too
#     ind <- round_df$choices[1]
#     nTrials <- length(social_choices)
#     X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
#     y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
#     # store first choice of round
#     round_choices<-data.frame(
#       trial = 1, 
#       x = as.numeric(X[1, 1]), 
#       y = as.numeric(X[1, 2]),
#       z = as.numeric(y[1]),
#       index=ind,
#       social_info=NA,
#       round=r,
#       util_list=NA,# there are no utilities yet
#       p_list=I(list(rep(1/64,64)))
#     )
#     
#     # loop over trials
#     for (t in 1:(trials-1)) {
#       # output by GP with particular parameter settings
#       # don't forget mean centering and standardization.... mean is already 0 :)
#      # browser()
#       if (t > 1) {
#         out <- RW_Q(ind, y[t], theta = lr, prevPost = out, mu0Par = mu0)
#       } else {
#         out <- RW_Q(ind, y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
#       }
#       #browser()
#       #utilities
#       utilityVec <- ucb(out, 0)
#       utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]+zeta# social update of utility
#       # softmaximization
#       #utilities=utilityVec-max(utilityVec)# get no NAs
#       p <- exp(utilityVec / tau)
#       # probabilities
#       p <- p / colSums(p)
#       # numerical overflow
#       p <- (pmax(p, 0.00001))
#       p <- (pmin(p, 0.99999))
#     #  browser()
#       ind <- sample(1:64, 1, prob = p) # choice index
#       
#       # collect x y coordinate of choice
#       X <- rbind(X, as.matrix(dat[ind, 1:2]))
#       # sample from environment
#       y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) 
#     #  browser()
#      # y_real=rbind
#       # write it to the next trial index because choice has already been made, learning will happen in next round
#       one_trial_choices <- data.frame(
#         trial = t+1, 
#         x = as.numeric(X[t+1, 1]), 
#         y = as.numeric(X[t+1, 2]),
#         z = as.numeric(y[t+1]),
#         index=ind,
#         social_info=social_choices[t+1],
#         round=r,
#         util_list=I(list(utilityVec)),
#         p_list=I(list(p))
#       )
#       
#       round_choices <- rbind(round_choices, one_trial_choices)
#      # browser()
#     }
#     all_choices<-rbind(all_choices,round_choices)
#   }# end rounds
#   #browser()
#   return(all_choices)
# }
























