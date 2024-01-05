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
##                    Aquisition models  
##
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-

##---------------------------------------------------------------
##                    epsilon greedy exploration                   --
##---------------------------------------------------------------
epsilonGreedy_sw <- function(out, epsilon=.1,zeta=1,beta=1,social_choices,t){
  n <- length(out)
  t_w=beta*t
  trembl<-(epsilon/t_w)
  p <- rep(1/n*trembl, n)#how many options
  utility_vec <- out #+(beta *sqrt(out$sig))
  utility_vec[social_choices[t]] <- utility_vec[social_choices[t]] + zeta#does not work
  p[which.is.max(utility_vec)] <- (1-trembl) + (1/n*trembl)
  return(p)
}

##---------------------------------------------------------------
##                    epsilon greedy exploration 2 params                   --
##---------------------------------------------------------------
two_epsilonGreedy <- function(out, epsilon=c(0.1,0.1),y){
  n <- length(out)
  trembl<-ifelse(max(y>150),(epsilon[1]),(epsilon[2]))
  p <- rep(1/n*trembl, n)#how many options
  utility_vec <- out #+(beta *sqrt(out$sig))
  p[which.is.max(utility_vec)] <- (1-trembl) + (1/n*trembl)
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}

##---------------------------------------------------------------
## softmax & epsilon greedy exploration 2 lr    -
##---------------------------------------------------------------
mix_softmax_epsilonGreedy <- function(out, epsilon=0.1,tau=0.1,y,t){
  n <- length(out)
  #browser()
  if(max(y>150)){
    #if gem was found: greedy
    p <- rep(1/n*epsilon, n)#how many options
    utility_vec <- out #+(beta *sqrt(out$sig))
    p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  }else{
    #if gem was not found: social-softmax
    utilityVec <- out
    utilityVec=utilityVec-max(utilityVec)
    p <- exp(utilityVec / tau)
    # probabilities
    p <- p / sum(p)
  }
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}


##---------------------------------------------------------------
## softmax & epsilon greedy exploration 2 lr 4sw    -
##---------------------------------------------------------------
mix_sw_softmax_epsilonGreedy <- function(out, epsilon=0.1,tau=0.1,zeta=0,social_choices,y,t){
  n <- length(out)
  #browser()
  if(max(y>150)){
    #if gem was found: greedy
    p <- rep(1/n*epsilon, n)#how many options
    utility_vec <- out #+(beta *sqrt(out$sig))
    utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]+zeta
    
    p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  }else{
    #if gem was not found: social-softmax
    utility_vec <- out
    utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]+zeta
    p <- exp(utility_vec / tau)
    # probabilities
    p <- p / sum(p)
  }
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}


##---------------------------------------------------------------
##softmax & epsilon greedy exploration 2 params 3 social parm     -
##---------------------------------------------------------------
mix_4sw_softmax_epsilonGreedy <- function(out, epsilon=0.1,tau=0.1,zeta=c(0,0,0,0),social_choices,soctype,y,t){
  n <- length(out)
  #browser()
  if(max(y>150)){
    #if gem was found: greedy
    utility_vec <- out
    utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]+zeta[soctype]
    
    p <- rep(1/n*epsilon, n)#how many options
    p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  }else{
    #if gem was not found: social-softmax
    utility_vec <- out
    #utilityVec=utilityVec-max(utilityVec)
    utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]+zeta[soctype]
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
KF_mix_softmax_epsilonGreedy <- function(out, epsilon=0.1,tau=0.1,ucb=0,y,t){
  #out is data frame
  n <- length(out$mu)
  #browser()
  if(max(y>150)){
    #if gem was found: greedy
    p <- rep(1/n*epsilon, n)#how many options
    utility_vec <- out$mu +(ucb *sqrt(out$sig))
    p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  }else{
    #if gem was not found: social-softmax
    utility_vec <- out$mu +(ucb *sqrt(out$sig))
    #utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]
    p <- exp(utility_vec / tau)
    # probabilities
    p <- p / sum(p)
  }
  #browser()
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}



##---------------------------------------------------------------
##softmax & epsilon greedy exploration ucb for Kalman filter   -
##---------------------------------------------------------------
KF_sw_mix_softmax_epsilonGreedy <- function(out, epsilon=0.1,tau=0.1,ucb=0,zeta=0,y,t,social_choices){
  #out is data frame
  n <- length(out$mu)
  #browser()
  if(max(y>200)){
    #if gem was found: greedy
    p <- rep(1/n*epsilon, n)#how many options
    utility_vec <- out$mu +(ucb *sqrt(out$sig))
    utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]+zeta
    
    p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  }else{
    #if gem was not found: social-softmax
    utility_vec <- out$mu +(ucb *sqrt(out$sig))
    utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]+zeta
    p <- exp(utility_vec / tau)
    # probabilities
    p <- p / sum(p)
  }
  #browser()
  #p[social_choices[t]] <- (1-zeta) + (1/n*zeta)
  return(p)
}

KF_epsilon_greedy <- function(out, epsilon=0.1,tau=0.1,ucb=0,zeta=0,y,t,social_choices){
  #out is data frame
  n <- length(out$mu)
  #browser()
  #if gem was found: greedy
  p <- rep(1/n*epsilon, n)#how many options (1/64)
  utility_vec <- out$mu +(ucb *sqrt(out$sig))
  utility_vec[social_choices[t]]<-utility_vec[social_choices[t]]#+zeta
  p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  return(p)
}

KF_social_greedy <- function(out, epsilon=1,maximize=1,zeta=1,y,t,social_choices){
  #out is data frame
  #browser()
  n <- length(out$mu)
  #softmaxing probs
  vals<-c(exp(epsilon),exp(maximize),exp(zeta))/sum(c(exp(epsilon),exp(maximize),exp(zeta)))
  # compute utilities
  utility_vec <- out$mu #+(ucb *sqrt(out$sig))
  # assign probabilites
  p <- rep(1/n*vals[1], n)#all opts get explore probability
  p[which.is.max(utility_vec)] <- vals[2]
  p[social_choices[t]] <- vals[3]
  #simply add explore and copy probability
  if(which.is.max(utility_vec)==social_choices[t]){
    p[which.is.max(utility_vec)] <- vals[2]+vals[3]# add copy and maximizing probability
  }
  return(p)
}

#next: argmax with some prob after gem, otherwise do value based exploration.

##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##
##                    Learning models  
##
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-
##-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-=x=-


##---------------------------------------------------------------
##                    basic Q-Learning model                   --
##---------------------------------------------------------------

utility_1lr <- function(par, learning_model_fun, acquisition_fun, dat) {
  
  # for (rep in 1:ntrialss){
  # unpack
  
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate"
  tau <- par[2] #  "random" exploration
  mu0 <- 0# par[4] # exploration bonus
  
  ## preallocate negative log likelihood
  nLL <- rep(0, length(dat$round))
  
  for (r in unique(dat$round)) {
    
    ## collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    
    ## Observations of subject choice behavior 
    chosen <- round_df$choices
    
    ## rewards
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    
    ## social information
    social_choices <- round_df$social_info
    
    # create observation matrix
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    
    #here, too
    for (t in 1:(trials - 1)) {
      
      #learn
      
      #browser()
      if (t > 1) {
        out <- RW_Q(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      
      ## ?? what is this?
      utilityVec <- ucb(out, 0)
      
      # next choice getrials a social utility
      #utilityVec=utilityVec-max(utilityVec)
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]]
      
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      utilities <- rbind(utilities, t(utilityVec)) 
      #browser()
    }
    
    # softmaximization
    # browser()
    p <- exp(utilities / tau)
    
    # probabilities
    p <- p / rowSums(p)
    
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    
    #browser()
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
    #browser()
  }
  #browser()
  
  #avoid nan in objective function
  if (any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}

##----------------------------------------------------------------
##       Q-Learning model with 2 learning rates (pos&neg)       --
##----------------------------------------------------------------

utility_2lr <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau <- par[3] #  "random" exploration
  
  mu0 <-0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      utilityVec <- ucb(out, 0)
      #next choice getrials a social utility
      #utilityVec=utilityVec-max(utilityVec)
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      utilities <- rbind(utilities, t(utilityVec)) 
      #browser()
    }
    # softmaximization
    # browser()
    p <- exp(utilities / tau)
    # probabilities
    p <- p / rowSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #browser()
  #avoid nan in objective function
  if(any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}

##------------------------------------------------------------------------
##  Q-Learning model with 2 learning rates (pos&neg) and social weight  --
##------------------------------------------------------------------------

utility_2lr_sw <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau <- par[3] #  "random" exploration
  zeta <- par[4] # scales social info use
  mu0 <- 0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      utilityVec <- ucb(out, 0)
      #next choice getrials a social utility
      #utilityVec=utilityVec-max(utilityVec)
      
      utilityVec[social_choices[t]] <- utilityVec[social_choices[t]] + zeta
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      utilities <- rbind(utilities, t(utilityVec)) 
      #browser()
    }
    # softmaximization
    # browser()
    p <- exp(utilities / tau)
    # probabilities
    p <- p / rowSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #browser()
  #avoid nan in objective function
  if(any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}

##------------------------------------------------------------------------
##  Q-Learning model with 2 learning rates (pos&neg) and social weight  --
##------------------------------------------------------------------------

utility_2lr_sw_e_greedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  epsilon <- par[3] #  "random" exploration
  beta<-par[4]
  zeta <- par[5] # scales social info use
  mu0 <- 0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
        
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      #trial wise probabilites
      ps_t<-epsilonGreedy_social(out,epsilon,zeta,beta,social_choices,t)
      p<-rbind(p, t(ps_t))
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}




##------------------------------------------------------------------------
##  Q-Learning model with 2 learning rates and epsilon greed --
##------------------------------------------------------------------------

utility_2lr_2e_greedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  epsilon <- c(par[3],par[4]) #  "random" exploration
  mu0 <- 0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      #trial wise probabilites
      ps_t<-two_epsilonGreedy(out,epsilon,y=y[1:t])
      p<-rbind(p, t(ps_t))
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}


##------------------------------------------------------------------------
##Q-Learning model with 2 learning rates and epsilon greedy --
##------------------------------------------------------------------------


utility_2lr_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  mu0 <- 0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      #trial wise probabilites
      ps_t<-mix_softmax_epsilonGreedy(out,epsilon,tau,y,t)
      p<-rbind(p, t(ps_t))
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}


##------------------------------------------------------------------------
##Q-Learning model with 2 learning rates and social weight and epsilon greedy --
##------------------------------------------------------------------------


utility_2lr_sw_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] # scales social info use
  mu0 <- 0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      #trial wise probabilites
      ps_t<-mix_sw_softmax_epsilonGreedy(out,epsilon,tau,zeta,social_choices,y=y[1:t],t)
      p<-rbind(p, t(ps_t))
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}


##------------------------------------------------------------------------
##Q-Learning model with 2 learning rates and 4social weight and epsilon greedy --
##------------------------------------------------------------------------

utility_2lr_4sw_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- c(par[5],par[6],par[7],par[8]) # scales social info use
  mu0 <- 0# par[4] # exploration bonus
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q_2(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      #trial wise probabilites
      #browser()
      ps_t<-mix_4sw_softmax_epsilonGreedy(out,epsilon,tau,zeta,social_choices,soctype,y=y[1:t],t)
      p<-rbind(p, t(ps_t))
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}


##------------------------------------------------------------------------
##BAYESIAN MEAN TRACKER model with ucb and epsilon greedy--
##------------------------------------------------------------------------

kalman_ucb_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1])# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  mu0 <- 0# par[4] # exploration bonus
  var0<-40
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0,var0Par = var0)
      } else {
        # first t of each round, start new
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0,var0Par = var0)
      }
      
      ps_t<-KF_mix_softmax_epsilonGreedy(out,epsilon,tau,ucb,y=y[1:t],t)
      p<-rbind(p, t(ps_t))
      #browser()
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}

##------------------------------------------------------------------------
##BAYESIAN MEAN TRACKER model with ucb and epsilon greedy and social weight--
##------------------------------------------------------------------------

kalman_ucb_sw_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  # par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] #  "random" exploration
  
  mu0 <- 0# par[4] # exploration bonus
  var0<-10
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0,var0Par = var0)
      } else {
        # first t of each round, start new
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0,var0Par = var0)
      }
      
      ps_t<-KF_sw_mix_softmax_epsilonGreedy(out,epsilon,tau,ucb,zeta,y=y[1:t],t,social_choices)
      p<-rbind(p, t(ps_t))
      # browser()
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}

kalman_lr_ucb_sw_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  # par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] #  social weight
  
  mu0 <- 0# par[4] # exploration bonus
  var0<-10
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0,var0Par = var0)
      } else {
        # first t of each round, start new
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0,var0Par = var0)
      }
      
      ps_t<-KF_sw_mix_softmax_epsilonGreedy(out,epsilon,tau,ucb,zeta,y=y[1:t],t,social_choices)
      p<-rbind(p, t(ps_t))
      # browser()
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -2*sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}









##------------------------------------------------------------------------
##BAYESIAN MEAN TRACKER model with ucb and epsilon greedy and social lr weight--
##------------------------------------------------------------------------

kalman_ucb_slr_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  # par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] #  "random" exploration
  
  mu0 <- 0# par[4] # exploration bonus
  var0<-5
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- bayesianMeanTracker_soc(chosen[t], y[t], theta = theta,soc_w=zeta, prevPost = out, mu0Par = mu0,var0Par = var0,social_choices[t])
      } else {
        # first t of each round, start new
        out <- bayesianMeanTracker_soc(chosen[t], y[t], theta = theta,soc_w=zeta, prevPost = NULL, mu0Par = mu0,var0Par = var0,social_choices[t])
      }
      
      ps_t<-KF_mix_softmax_epsilonGreedy(out,epsilon,tau,ucb,y=y[1:t],t)
      p<-rbind(p, t(ps_t))
      # browser()
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}







##------------------------------------------------------------------------
##BAYESIAN MEAN TRACKER model with ucb and epsilon greedy and social weight--
##------------------------------------------------------------------------

kalman_ucb_asw_softmax_egreedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  # par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] #  "random" exploration
  
  mu0 <- 0# par[4] # exploration bonus
  var0<-10
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0,var0Par = var0)
        if (social_choices[t-1]==social_choices[t]){
          soc_w=soc_w+zeta
        }else{
          soc_w=0
        }
      } else {
        # first t of each round, start new
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0,var0Par = var0)
        soc_w=0
      }
      # social weight aquisition function
      ps_t<-KF_sw_mix_softmax_epsilonGreedy(out,epsilon,tau,ucb,soc_w,y=y[1:t],t,social_choices)
      p<-rbind(p, t(ps_t))
      # browser()
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}



##------------------------------------------------------------------------
##non-greedy BAYESIAN MEAN TRACKER model with ucb and epsilon and social weight--
##------------------------------------------------------------------------

kalman_ucb_sw_softmax_e <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  # par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate" 1 is positive, 2 is negative
  ucb<-par[2]
  tau<-par[3]
  epsilon <- par[4] #  "random" exploration
  zeta <- par[5] #  "random" exploration
  
  mu0 <- 0# par[4] # exploration bonus
  var0<-10
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    # Utilties of each choice
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      # browser()
      if (t > 1) {
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = out, mu0Par = mu0,var0Par = var0)
        if (social_choices[t-1]==social_choices[t]){
          soc_w=soc_w+zeta
        }else{
          soc_w=0
        }
      } else {
        # first t of each round, start new
        out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0,var0Par = var0)
        soc_w=0
      }
      
      ps_t<-KF_sw_mix_softmax_epsilon(out,epsilon,tau,ucb,soc_w,y=y[1:t],t,social_choices)
      p<-rbind(p, t(ps_t))
      # browser()
    }
    # softmaximization
    # compute choice probabilites
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}













##------------------------------------------------------------------------
## Copy probablity mixture model--
##------------------------------------------------------------------------

kalman_social_greedy <- function(par, dat) {
  # unpack
  theta <- par[1]# "learningrate"
  epsilon<-par[2]# exploration prob
  maximize<-par[3]# maximize prob
  zeta<-par[4]# copy prob
  mu0 <- 0# par[4] # exploration bonus
  var0<-10
  # create a parameter vector
  # preallocate negative log likelihood
  nLL <- rep(0, 12)
  for (r in unique(dat$round)){
    # collect choices for current round
    round_df <- subset(dat, round == r)
    trials <- nrow(round_df)
    # Observations of subject choice behavior
    chosen <- round_df$choices
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    # social information
    social_choices<-round_df$social_info
    soctype<-unique(round_df$soctype)
    # create observation matrix
    p <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      if (t == 1) {
        #reset on the first trial
        out=NULL
      }
      out <- bayesianMeanTracker(chosen[t], y[t], theta = theta, prevPost = NULL, mu0Par = mu0,var0Par = var0)
      ps_t<-KF_social_greedy(out,epsilon,maximize,zeta,y=y[1:t],t,social_choices)
      p<-rbind(p, t(ps_t))
      # browser()
    }
    #handle overflow
    p <- (pmax(p, 0.00000000001))
    p <- (pmin(p, 0.99999999999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -2*sum(log(p[cbind(c(1:(trials-1)), chosen[2:length(chosen)] )]))
    #browser()
  }
  #avoid nan or na in objective function
  if(any(is.nan(sum(nLL))) | any(is.na(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}



