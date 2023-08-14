############################################################################
############################################################################
###                                                                      ###
###                SCRIPT WHERE MLE FUNCTIONS ARE WRITTEN                ###
###                                                                      ###
############################################################################
############################################################################

##---------------------------------------------------------------
##                    epsilon greedy exploration                   --
##---------------------------------------------------------------
epsilon_greedy <- function(out, epsilon=.1,zeta=1,social_choices,t){
  n <- length(out$mu)
  p <- rep(1/n*epsilon, n)#how many options
  utility_vec <- out$mu #+(beta *sqrt(out$sig))
  utility_vec[social_choices[t]] <- utility_vec[social_choices[t]] + zeta
  p[which.is.max(utility_vec)] <- (1-epsilon) + (1/n*epsilon)
  return(p)
}

##---------------------------------------------------------------
##                    basic Q-Learning model                   --
##---------------------------------------------------------------

utilityModel <- function(par, learning_model_fun, acquisition_fun, dat) {
  
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
    
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <-
      -sum(log(p[cbind(c(1:(trials - 1)), chosen[2:length(chosen)])]))
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

utilityModel2lr <- function(par, dat) {
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

utilityModel2lrsw <- function(par, dat) {
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

utilityModel2lrsw_e_greedy <- function(par, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- c(par[1], par[2])# "learningrate" 1 is positive, 2 is negative
  epsilon <- par[3] #  "random" exploration
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
      #trial wise probabilites
      ps_t<-epsilonGreedy_social(out,epislon,zeta,social_choices,t)
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






