

#########
######### Exploration Environmental change
#########
explore_env_social_fitted_pars <- function(par, learning_model_fun, acquisition_fun,data,envs) {
  # for (rep in 1:ntrialss){
  # unpack
  #browser()
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  lr <- par[1]# "learningrate"
  tau <- par[2] #  "random" exploration
  zeta<-par[3] # scales social info use
  
  mu0 <- par[4] # exploration bonus
  mu=list()
  all_choices <- NULL
  dummy <- NULL
  #look up samples
  dat=expand.grid(x1=0:7,x2=0:7)
  plot_dat=list()
  chosen=NULL
  
  for (r in 1:12){
    
    # collect choices for current round
    #print(r)
    round_df <- data%>%filter(round == r)
    #get environment as seen by participant
    browser()
    # get the right environment to sample from
    env=envs%>%filter(env_idx==unique(round_df$env_number))#[[unique(round_df$env_number)]]
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
    y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance)/100)
    
    # store first choice of round
    all_choices<-data.frame(
      trial = 1, 
      x = as.numeric(X[1, 1]), 
      y = as.numeric(X[1, 2]),
      z = as.numeric(y[1]),
      index=ind,
      social_info=NA,
      round=r,
      util_list=NA,# there are no utilities yet
      p_list=I(list(p=rep(1/64,64)))
    )
    
    # loop over trials
    for (t in 1:(trials-1)) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      browser()
      if (t > 1) {
        out <- RW_Q(X[t, 1:2], y[t], theta = lr, prevPost = out, mu0Par = mu0)
      } else {
        out <- RW_Q(X[t, 1:2], y[t], theta = lr, prevPost = NULL, mu0Par = mu0)
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
      # browser()
      ind <- sample(1:64, 1, prob = p) # choice index
      
      # collect x y coordinate of choice
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # sample from environment
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance)/100)) 
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
      
      all_choices <- rbind(all_choices, one_trial_choices)
      browser()
    }
    # # collect the data so you can plot it later
    # plot_dat[[r]] <- expand.grid(x = 1:8, y = 1:8, trials = 0:max(all_choices$trial))
    # plot_dat[[r]]$sample <- 0
    # plot_dat[[r]]$out <- 0
    # plot_dat[[r]]$mu <- 0
    # 
    # for (i in 1:length(all_choices$x)) {
    #   all_choices$y[i]#
    #   all_choices$x[i]
    #   #was the item chosen?
    #   plot_dat[[r]][plot_dat[[r]]$x == all_choices$x[i] & plot_dat[[r]]$y == all_choices$y[i] & plot_dat[[r]]$trials == all_choices$trial[i], ]$sample <- 1
    #   #outcomes
    #   plot_dat[[r]][plot_dat[[r]]$trials == all_choices$trial[i], ]$out <- all_choices$z[i]
    #   plot_dat[[r]][plot_dat[[r]]$trials == all_choices$trial[i], ]$mu <- mu[[i]]
    # }
    # # browser()
    # all_choices<-NULL
  }# end rounds
  return(all_choices)
}






























#########
######### deprecated
#########
explore_env_social_agents <- function(explore_func, choiceRule, env, cntrl, iter) {
  # for (rep in 1:ntrialss){
  # unpack
  lambda <- cntrl$lambda# generalization for rbf kernel
  beta <- cntrl$beta # this scales risk attitude.
  zeta<-cntrl$social_weight # scales social info use
  
  tau <- cntrl$tau
  mu0 <- cntrl$mu0 # exploration bonus
  var0 <- cntrl$var0
  # create a parameter vector
  parVec <- cntrl$parVec
  #
  ExploreBonus <- cntrl$ExploreBonus
  # kernel is RBF
  # k<-rbf
  # loop through trials
  out <- cntrl$out
  AllChoices <- cntrl$AllChoices
  dummy <- cntrl$dummy
  trial <- cntrl$trial
  social_choices<-cntrl$social_choices
  dat <- cntrl$dat
  mu <- list()
  sig <- list()
  
  ind <- sample(1:64, 1)
  nTrials <- length(social_choices)
  X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
  y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
  
  Xstar <- as.matrix(dat[, 1:2])
  
  for (trial in 1:nTrials) {
    # output by GP with particular parameter settings
    # don't forget mean centering and standardization.... mean is already 0 :)
    if (trial > 1) {
      out <- bayesianMeanTracker(X[trial, 1:2], y[trial], theta = lambda, prevPost = out, mu0Par = mu0, var0Par = var0)
    } else {
      out <- bayesianMeanTracker(X[trial, 1:2], y[trial], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
    }
    # browser()
    # utility vector. transpose if you use greedyMean
    # where is everybody?
    # here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
    #
    # browser()
    utilityVec <- ucb(out, beta)
    utilityVec[social_choices[trial]]<-utilityVec[social_choices[trial]]+zeta# social update of utility
    # softmaximization
    p <- exp(utilityVec / tau)
    # probabilities
    p <- p / colSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt
    
    X <- rbind(X, as.matrix(dat[ind, 1:2]))
    # bind y-observations
    y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) # change this into a sample.
    
    one_trial_choices <- data.frame(
      trial = trial, 
      x = as.numeric(X[trial, 1]), 
      y = as.numeric(X[trial, 2]),
      z = as.numeric(y[trial])
    )
    
    AllChoices <- rbind(AllChoices, one_trial_choices)
    mu[[trial]] <- out$mu
    sig[[trial]] <- out$sig
  }
  # dummy data frame
  # }
  # This Here is for Plotting
  
  Plot_dat <- expand.grid(x = 1:8, y = 1:8, trials = 0:max(one_trial_choices$trial))
  Plot_dat$sample <- 0
  Plot_dat$out <- 0
  Plot_dat$mu <- 0
  Plot_dat$sig <- 40
  
  for (i in 1:length(AllChoices$x)) {
    AllChoices$y[i]
    AllChoices$x[i]
    Plot_dat[Plot_dat$x == AllChoices$x[i] & Plot_dat$y == AllChoices$y[i] & Plot_dat$trials == AllChoices$trial[i], ]$sample <- 1
    Plot_dat[Plot_dat$trials == AllChoices$trial[i], ]$out <- AllChoices$z[i]
    Plot_dat[Plot_dat$trials == AllChoices$trial[i], ]$mu <- mu[[i]]
    Plot_dat[Plot_dat$trials == AllChoices$trial[i], ]$sig <- sig[[i]]
  }
  # browser()
  
  Plot_dat$iter <- iter
  return(Plot_dat)
}







