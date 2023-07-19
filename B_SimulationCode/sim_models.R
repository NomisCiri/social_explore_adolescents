##############################################################################################
##############################################################################################
###                                                                                        ###
###  FUNCTIONS TO SIMULATE EXPLORATION OF ENVIRONMENT GIVEN PARAMETERS FROM MODEL FITTING  ###
###                                                                                        ###
##############################################################################################
##############################################################################################

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
      #browser()
      #utilities
      utilityVec <- ucb(out, 0)
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]# social update of utility
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
      utilityVec <- ucb(out, 0)
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









#########
######### Exploration Environmental change
#########
explore_env_social_fitted_pars_social <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[1]# "learningrate"
  tau <- par[2] #  "random" exploration
  zeta<-par[3] # scales social info use
  mu0=0
  
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
      #browser()
      #utilities
      utilityVec <- ucb(out, 0)
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]+zeta# social update of utility
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




















#########
######### Exploration Environmental change
#########
explore_env_social_fitted_pars_2lr <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- c(par[1],par[2])# "learningrates, 1: pos, 2: neg"
  tau <- par[3] #  "random" exploration
  zeta<-par[4] # scales social info use
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
        out <- RW_Q_2(ind, (y[t] - mean(y))/sd(y), theta = lr, prevPost = NULL, mu0Par = mu0)
        
        
      } else {
        out <- RW_Q_2(ind, rnorm(1), theta = lr, prevPost = out, mu0Par = mu0)
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



























