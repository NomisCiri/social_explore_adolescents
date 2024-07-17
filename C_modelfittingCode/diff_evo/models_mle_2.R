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
##       social learning q model with copy probs                   --
##---------------------------------------------------------------

s_learn <- function(par, learning_model_fun, acquisition_fun, dat,envs) {
  
  # unpack
  theta <- par[1]# "learningrate"
  tau<-par[2]
  omega<-par[3]
 # b<-par[3]
  tau_copy<-0.01#par[4]
  prior_copy<- -1 #par[4]
  
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
    first=TRUE
    for (t in 1:(trials - 1)) {
    #  tau<-ifelse(max(y[1:t])>150,par[2],par[1])
      
      out <- RW_Q(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      
      if (chosen[t]==social_choices[t]){
       # social_reward<- env[round_df$social_info[t], ]$Mean
        copyUtil[(t+1):trials]=copyUtil[t]+omega*(y[t]-copyUtil[t])
        # browser()
      }
    #   + b*streak[t]
      copy_prob=1/(1+exp(-(copyUtil[t+1])/tau_copy))
      
      utilityVec <- out
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      p_sfmx <- exp(utilityVec / tau)
      # probabilities
      p_sfmx <- p_sfmx / sum(p_sfmx)
      p_sl=p_sfmx*(1-copy_prob)
      p_sl[social_choices[t]] <- (copy_prob) + (p_sfmx[social_choices[t]]*(1-copy_prob))
      
      if(ncol(t(p_sfmx))==65){
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
