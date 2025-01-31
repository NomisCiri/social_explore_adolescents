############################################################################
############################################################################
###                                                                      ###
###                SCRIPT WHERE MLE FUNCTIONS ARE WRITTEN                ###
###                                                                      ###
############################################################################
############################################################################

##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##
##                    Learning models  
##
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-



##---------------------------------------------------------------
##       policy shaping               --
##---------------------------------------------------------------
s_policy_shaping <- function(par, dat) {
  # unpack
  lr <- par[1]# "learningrate"
  tau<-par[2]# exploration
  lr_soc<-par[3]# social meta learning rate
  tau_copy<-par[4]# social copy temperature
  # hardcoded but potentially moving parts
  prior_copy<- -2.5 # rougly chance when tau copy is 1
  mu0 <-0.5 # exploration bonus
  ## preallocate negative log likelihood
  nLL <- rep(0, length(dat$round))
  #set copy utility low.
  for (r in unique(dat$round)) {
    ## collect choices for current round
    round_df <- subset(dat, round == r)
    gem <- round_df$gem
    qual= unique(round_df$qual)
    trials <- nrow(round_df)
    out<-rep(mu0,64)# priors
    copy_util<- rep(prior_copy, length(round_df$choice))#roughly chance
    ## Observations of subject choice behavior 
    chosen <- round_df$choice
    ## social information
    social_choices <- round_df$social_info
    ## rewards (apply range normalization for non-gems to be between 0 and 1)
    y <- (round_df$z[0:(trials - 1)]+75)/(150) # trim off the last observation, because it was not used to inform a choice (round already over)
    p<-NULL#reset probability vector every round
    for (t in 1:(trials - 1)) {
      # individual learning
      out <- RW_Q(chosen[t], y[t], theta = lr, prevPost = out, mu0Par = mu0)
      #social meta-learning: was advice good?      #first compute surrogate rewards for social signals
      target = as.numeric(max(out)==out[social_choices[t]])#maximum Q value = social info, this is a surrogate reward?
      target=ifelse(max(out)>1.5,target*3,target)#*3so that all of a sudden a gem is not negative
      #prediction error of copy utility t+1 because new social information is always shown after a choice in one trail
      copy_util[(t+1):trials]=copy_util[t]+lr_soc*(target - out[social_choices[t]])
      copy_prob=1/(1+exp(-(copy_util[t+1])/tau_copy))#softmaxing copy_util
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(out / tau)
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      #greedy copy policy
      p_sl=p_sfmx*(1-copy_prob)# first decrease probability by copy probability
      p_sl[social_choices[t]] <- (copy_prob) + (p_sfmx[social_choices[t]])# add copy probability to demostrated option
      #reset
      p <- rbind(p, t(p_sl)) 
    }
    # avoid over or underflow
    p <- (pmax(p, 0.00000000001))
    p <- (pmin(p, 0.99999999999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
  }
  #avoid nan and unreasonable value in objective function
  if (any(is.nan(sum(nLL))) | par[2]>1 | par[2]<0)
  { 
    nLL=10 ^ 30# really high value (not good)
  }
  #return sum of negative log likelihod
  return(sum(nLL))
}


##---------------------------------------------------------------
##       value shaping               --
##---------------------------------------------------------------
s_value_shaping <- function(par, dat) {
  
  # unpack
  lr <- 0.5 #par[1]# "learningrate"
  tau<-c(par[1])
  lr_soc_m<-c(par[2],par[3],par[4])
  #range<-
  soc_rew<-1# c(par[3])
  mu0 <- 0.5# par[4] # exploration bonus
  ## preallocate negative log likelihood
  nLL <- rep(0, length(dat$round))
  #set copy utility low.
  for (r in unique(dat$round)) {
    #reset
    lr_soc<-0
    ## collect choices for current round
    round_df <- subset(dat, round == r)
    gem <- round_df$gem
    qual= unique(round_df$qual)
    trials <- nrow(round_df)
    out<-rep(mu0,64)# priors
    ## Observations of subject choice behavior 
    chosen <- round_df$choice
    ## social information
    social_choices <- round_df$social_info
    ## rewards (apply range normalization)
    y <- (round_df$z[0:(trials - 1)]+75)/(150) # trim off the last observation, because it was not used to inform a choice (round already over)
    p<-NULL
    
    for (t in 1:(trials - 1)) {
      # individual learning
      out <- RW_Q(chosen[t], y[t], theta = lr, prevPost = out, mu0Par = mu0)
      #surrogate rewards for social signals
      out[social_choices[t]] = out[social_choices[t]] + lr_soc * (soc_rew - out[social_choices[t]])
      #meta-learning: was advice good?
      target = max(out)==out[social_choices[t]]
      
      if (social_choices[t]==chosen[t]){
        target=y[t]
      }
      lr_soc = lr_soc + lr_soc_m[qual] * (target-lr_soc)# update advice learning rate
      #softmax
      p_sfmx <- exp(out / tau)
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      if(ncol(t(p_sfmx))!=64){
        browser()
      }
      #reset
      #out[social_choices[t]] = out[social_choices[t]]# - lr_soc_m
      p <- rbind(p, t(p_sfmx)) 
    }
    p <- (pmax(p, 0.00000000001))
    p <- (pmin(p, 0.99999999999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
  }
  #avoid nan and unreasonablein objective function
  if (any(is.nan(sum(nLL))) |any(par<0) | any(par>5))
  { 
    return(10 ^ 30)
  }else
  {
    return(sum(nLL))
  }
}



##---------------------------------------------------------------
##       social learning q model with copy probs               --
##---------------------------------------------------------------

s_learn <- function(par, learning_model_fun, acquisition_fun, dat,envs) {
  
  # unpack
  lr <- par[1]# "learningrate"
  tau<-par[2]
  lr_soc<-c(par[3],par[4])
  # lr_soc<-par[3]
  
  # b<-par[3]
  tau_copy<-0.01#par[4]
  # prior_copy<- par[4]
  
  
  mu0 <- 0# par[4] # exploration bonus
  ## preallocate negative log likelihood
  nLL <- rep(0, length(dat$round))
  #set copy utility low.
  for (r in unique(dat$round)) {
    ## collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    out<-rep(mu0,64)
    #browser()
    if(!exists("envs")){
      browser()
    }
    env=envs%>%filter(env==unique(round_df$env_number))#
    ## Observations of subject choice behavior 
    chosen <- round_df$choices
    copyUtil<- rep(prior_copy, length(round_df$choices))#roughly chance
    ## rewards
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    ## social information
    social_choices <- round_df$social_info
    # create observation matrix
    p<-NULL
    for (t in 1:(trials - 1)) {
      
      out <- RW_Q(chosen[t], y[t], theta = lr, prevPost = out, mu0Par = mu0)
      
      if (chosen[t]==social_choices[t]){
        social_reward<- env[round_df$social_info[t], ]$Mean
        copyUtil[(t+1):trials]=copyUtil[t]+lr_soc*(y[t]-copyUtil[t])
      }
      # 
      copy_prob=1/(1+exp(-(copyUtil[t+1])/tau_copy))
      
      utilityVec <- out
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(utilityVec / tau)
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      p_sl=p_sfmx*(1-copy_prob)
      p_sl[social_choices[t]] <- (copy_prob) + (p_sfmx[social_choices[t]]*(1-copy_prob))
      
      if(ncol(t(p_sl))!=64){
        browser()
      }
      
      p <- rbind(p, t(p_sl)) 
    }
    # softmaximization
    #browser()
    
    p
    # numerical overflow
    p <- (pmax(p, 0.00000000001))
    p <- (pmin(p, 0.99999999999))
    
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
  }
  #avoid nan in objective function
  if (any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
  }else
  {
    return(sum(nLL))
  }
}



##---------------------------------------------------------------
##                    basic Q-Learning model                   --
##---------------------------------------------------------------

sw <- function(par, learning_model_fun, acquisition_fun, dat) {
  
  # unpack
  theta <- par[1]# "learningrate"
  tau<-par[2]
  omega<-par[3]
  mu0 <- 0# par[4] # exploration bonus
  ## preallocate negative log likelihood
  nLL <- rep(0, length(dat$round))
  
  for (r in unique(dat$round)) {
    ## collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    out<-rep(mu0,64)
    ## Observations of subject choice behavior 
    chosen <- round_df$choices
    
    ## rewards
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    y_scaled<-rep(0,length(y))
    ## social information
    social_choices <- round_df$social_info
    # create observation matrix
    p<-NULL
    first=TRUE
    for (t in 1:(trials - 1)) {
      out <- RW_Q(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      
      utilityVec <- out
      # next choice getrials a social utility
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + omega
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(utilityVec / tau)
      #  p_sfmx[p_sfmx <= -Inf]<-0
      #  p_sfmx[p_sfmx >= Inf]<-100
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      if(ncol(t(p_sfmx))==65){
        browser()
      }
      p <- rbind(p, t(p_sfmx)) 
    }
    # softmaximization
    #browser()
    
    p
    # numerical overflow
    p <- (pmax(p, 0.000001))
    p <- (pmin(p, 0.999999))
    
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
  }
  #avoid nan in objective function
  if (any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
  }else
  {
    return(sum(nLL))
  }
}


##---------------------------------------------------------------
##       learning model with 3 sws                 --
##---------------------------------------------------------------

sw_3 <- function(par, learning_model_fun, acquisition_fun, dat) {
  
  # unpack
  theta <- 1#par[1]# "learningrate"
  tau<-par[1]
  omegas<-par[2:4]
  mu0 <- 0# par[4] # exploration bonus
  ## preallocate negative log likelihood
  nLL <- rep(0, length(dat$round))
  
  for (r in unique(dat$round)) {
    ## collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    out<-rep(mu0,64)
    ## Observations of subject choice behavior 
    chosen <- round_df$choices
    ## rewards
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    y_scaled<-rep(0,length(y))
    ## social information
    social_choices <- round_df$social_info
    # create observation matrix
    p<-NULL
    first=TRUE
    for (t in 1:(trials - 1)) {
      omega<-omegas[round_df$qual[t]]
      #range adaptation and conditions
      #if(max(y[1:t])<120){
      #}else {# when there is a gem. 
      #crucial: rescale q-values the first time we saw the gem
      #  omega=0
      #  tau<-par[2]
      #}
      # reward value of the new range
      
      
      out <- RW_Q(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      
      utilityVec <- out
      # next choice getrials a social utility
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + omega
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(utilityVec / tau)
      #  p_sfmx[p_sfmx <= -Inf]<-0
      #  p_sfmx[p_sfmx >= Inf]<-100
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      if(ncol(t(p_sfmx))==65){
        #browser()
      }
      p <- rbind(p, t(p_sfmx)) 
    }
    # softmaximization
    #browser()
    
    p
    # numerical overflow
    p <- (pmax(p, 0.000001))
    p <- (pmin(p, 0.999999))
    
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
  }
  #avoid nan in objective function
  if (any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
  }else
  {
    return(sum(nLL))
  }
}
