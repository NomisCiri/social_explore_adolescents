########
# BMT Model: maybe change into Q learning
#######
bayesianMeanTracker <- function(x, y, theta, prevPost = NULL, mu0Par, var0Par) {
  # Updates the previous posterior based on a single observation
  # parameters
  mu0 <- mu0Par # prior mean
  var0 <- var0Par # prior variance
  vare <- theta[1] # error varriance
  if (is.null(prevPost)) { # if no posterior prior, assume it is the first observation
    predictions <- data.frame(mu = rep(mu0, 64), sig = rep(var0, 64))
  } else { # if previous posterior is provided, update
    predictions <- prevPost
  }
  # Which of the 121 options were chosen at time?
  allopts <- expand.grid(1:8, 1:8)
  chosen <- which(allopts$Var1 == x[1] & allopts$Var2 == x[2])
  # Kalman gain
  kGain <- predictions$sig[chosen] / (predictions$sig[chosen] + 3600) # feed the uncertainty in here.
  # update mean
  predictions$mu[chosen] <- predictions$mu[chosen] + (kGain * (y - predictions$mu[chosen]))
  # update variance for observed arm
  predictions$sig[chosen] <- predictions$sig[chosen] * (1 - kGain)
  # return output
  #  browser()

  return(predictions)
}
class(bayesianMeanTracker) <- c(class(bayesianMeanTracker), "KalmanFilter")


########
## UCB sampling....
########
ucb <- function(out, pars, refactor = F) {
  if (refactor == TRUE) {
    gamma <- pars[1]
    beta_star <- pars[2]
    # calulate all the upper confidence bounds
    outtotal <- (gamma * out$mu) #+(beta_star*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    # avoid borderline cases
    # outtotal[outtotal<=0]<-0.0001
    # outtotal[outtotal>100]<-100
    outtotal <- matrix(outtotal, ncol = 1, byrow = TRUE)
  } else {
    beta <- pars[1]
    # calulate all the upper confidence bounds
    outtotal <- out$mu + (beta * sqrt(out$sig)) # refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    # avoid borderline cases
    # outtotal[outtotal<=0]<-0.0001
    # outtotal[outtotal>99]<-99
    outtotal <- matrix(outtotal, ncol = 1, byrow = TRUE)
  }
  # return them
  return(outtotal)
}





#########
######### Exploration Environmental change
#########
exploreEnv <- function(explore_func, choiceRule, env2, env1, cntrl, iter) {
  # for (rep in 1:ntrialss){
  # unpack
  lambda <- cntrl$lambda
  # get beta
  beta <- cntrl$beta # this scales risk attitude.
  # get tau
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
  overallCnt <- cntrl$overallCnt
  dat <- cntrl$dat
  mu <- list()
  sig <- list()

  for (nround in 1:3) {
    # get parameters for participant on that round
    if (nround == 1) {
      # define vectors that are used by the kalman filter
      lowestx <- 4
      highestx <- 9
      sampleVec <- as.numeric(rownames(dat[dat$x1 >= lowestx & dat$x1 <= highestx & dat$x2 < 7, ])) # here you define where a child should sample from
      ind <- sample(sampleVec, 1)
      nTrials <- 400
    } else {
      ind <- sample(1:64, 1)
      nTrials <- 400
    }
    # random initialization as observation t=0
    # y matrix
    if (nround == 1 & overallCnt == 1) {
      X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
      y <- as.matrix(rnorm(1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))
    } else if (overallCnt == 1) {
      print("Youre an adolescent now")
      X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
      y <- as.matrix(rnorm(1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))
    }
    # X-start, i.e. all possible observations
    Xstar <- as.matrix(dat[, 1:2])

    for (trial in 1:nTrials) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      if (overallCnt > 1) {
        out <- bayesianMeanTracker(X[overallCnt, 1:2], y[overallCnt], theta = lambda, prevPost = out, mu0Par = mu0, var0Par = var0)
      } else {
        out <- bayesianMeanTracker(X[overallCnt, 1:2], y[overallCnt], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
      }
      # browser()
      # utility vector. transpose if you use greedyMean
      # where is everybody?
      # here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
      #
      # browser()
      utilityVec <- ucb(out, beta)
      utilities <- utilityVec - max(utilityVec)
      # softmaximization
      p <- exp(utilities / tau)
      # probabilities
      p <- p / colSums(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      # index is sampled proprotionally to softmaxed utitily vector
      if (nround == 1) { # subset the probability vector so that it corresponds to the right tiles.
        ind <- sample(sampleVec, 1, prob = p[dat$x1 >= lowestx & dat$x1 <= highestx & dat$x2 < 7, ]) # sample from a childhood environemnt
        # this monster just scales exploration boni
      } else {
        ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt
        # print(ind)
      }
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # bind y-observations
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))) # change this into a sample.
      # if(y[overallCnt]<0){
      #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe.
      # }

      dummy <- data.frame(
        trial = overallCnt, x = as.numeric(X[overallCnt, 1]), y = as.numeric(X[overallCnt, 2]),
        z = as.numeric(y[overallCnt]), round = nround
      )

      AllChoices <- rbind(AllChoices, dummy)
      mu[[overallCnt]] <- out$mu
      sig[[overallCnt]] <- out$sig
      overallCnt <- overallCnt + 1
    }
    # dummy data frame
  }
  # }
  # This Here is for Plotting

  Plot_dat <- expand.grid(x = 1:8, y = 1:8, trials = 0:max(dummy$trial))
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









#########
######### Exploration Environmental change
#########
explore_env_social <- function(explore_func, choiceRule, env, cntrl, iter) {
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
  overallCnt <- cntrl$overallCnt
  social_choices<-cntrl$social_choices
  dat <- cntrl$dat
  mu <- list()
  sig <- list()

  for (nround in 1:3) {
    # get parameters for participant on that round
    if (nround == 1) {
      # define vectors that are used by the kalman filter
      lowestx <- 4
      highestx <- 9
      sampleVec <- as.numeric(rownames(dat[dat$x1 >= lowestx & dat$x1 <= highestx & dat$x2 < 7, ])) # here you define where a child should sample from
      # make first decision randomly (not true anymore, we could also compute utilities and add SI already in the first choice)
      ind <- sample(sampleVec, 1)
      # as many trails as social info
      nTrials <- length(social_choices)
    } else {
      ind <- sample(1:64, 1)
      nTrials <- length(social_choices)
    }
    # random initialization as observation t=0
    # y matrix
    if (nround == 1 & overallCnt == 1) {
      X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
      y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    } else if (overallCnt == 1) {
      print("Youre an adolescent now")
      X <- as.matrix(dat[ind, 1:2]) # generate a new vector of Xs
      y <- as.matrix(rnorm(1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))
    }
    # X-start, i.e. all possible observations
    Xstar <- as.matrix(dat[, 1:2])

    for (trial in 1:nTrials) {
      # output by GP with particular parameter settings
      # don't forget mean centering and standardization.... mean is already 0 :)
      if (overallCnt > 1) {
        out <- bayesianMeanTracker(X[overallCnt, 1:2], y[overallCnt], theta = lambda, prevPost = out, mu0Par = mu0, var0Par = var0)
      } else {
        out <- bayesianMeanTracker(X[overallCnt, 1:2], y[overallCnt], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
      }
      # browser()
      # utility vector. transpose if you use greedyMean
      # where is everybody?
      # here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
      #
      # browser()
      utilityVec <- ucb(out, beta)
      utilityVec[social_choices[trial]]<-utilityVec[social_choices[trial]]+zeta# social update of utility
      utilities <- utilityVec - max(utilityVec)
      # softmaximization
      p <- exp(utilities / tau)
      # probabilities
      p <- p / colSums(p)
      # numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      # index is sampled proprotionally to softmaxed utitily vector
      if (nround == 1) { # subset the probability vector so that it corresponds to the right tiles.
        ind <- sample(sampleVec, 1, prob = p[dat$x1 >= lowestx & dat$x1 <= highestx & dat$x2 < 7, ]) # sample from a childhood environemnt
        # this monster just scales exploration boni
      } else {
        ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt
        # print(ind)
      }
      X <- rbind(X, as.matrix(dat[ind, 1:2]))
      # bind y-observations
      y <- rbind(y, as.matrix(rnorm(n = 1, mean = env[ind, ]$Mean, sd = env[ind, ]$Variance))) # change this into a sample.
      # if(y[overallCnt]<0){
      #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe.
      # }

      dummy <- data.frame(
        trial = overallCnt, x = as.numeric(X[overallCnt, 1]), y = as.numeric(X[overallCnt, 2]),
        z = as.numeric(y[overallCnt]), round = nround
      )

      AllChoices <- rbind(AllChoices, dummy)
      mu[[overallCnt]] <- out$mu
      sig[[overallCnt]] <- out$sig
      overallCnt <- overallCnt + 1
    }
    # dummy data frame
  }
  # }
  # This Here is for Plotting

  Plot_dat <- expand.grid(x = 1:8, y = 1:8, trials = 0:max(dummy$trial))
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








####
#### Social exploration with recursion for ABM.
####
WhereIsEverybody <- function(HowManyOthers, others, otherLoc, dat_social, X_oth, y_oth, diminishingSocial, trial, cntrl, sampleVec) {
  lowestx <- 4
  highestx <- 9
  lambda <- cntrl$lambda
  # get beta
  beta <- cntrl$beta # this scales risk attitude.
  # get tau
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

  mu0 <- 100
  if (HowManyOthers == 1) {
    return(list(Others = dat_social$others, LastSamples = X_oth, LastReturn = y_oth, OthersUtil = others)) # basecase, terminate
  } else {
    ####
    #### Update_socialing process for each individual
    ####
    # browser()
    if (trial == 1) {
      ind <- sample(1:64, 1)
      print("Youre an adolescent now")
      X_oth[[HowManyOthers]] <- as.matrix(dat_social[ind, 1:2]) # generate a new vector of Xs
      y_oth[HowManyOthers] <- as.matrix(rnorm(1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))
    }
    if (trial > 1) {
      others[[HowManyOthers]] <- bayesianMeanTracker(X_oth[[HowManyOthers]], y_oth[HowManyOthers], theta = lambda, prevPost = others[[HowManyOthers]], mu0Par = mu0, var0Par = var0)
    } else {
      others[[HowManyOthers]] <- bayesianMeanTracker(X_oth[[HowManyOthers]], y_oth[HowManyOthers], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
    }
    utilityVec <- ucb(others[[HowManyOthers]], beta)
    # browser()
    # if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    # } else{
    # social impact follows a power law starting in adolescnece.
    # browser()
    utilityVec <- utilityVec + otherLoc^diminishingSocial
    # }
    utilities <- utilityVec - max(utilityVec)
    # utilities=utilities
    # softmaximization
    p <- exp(utilities / tau)
    # probabilities
    p <- p / colSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # index is sampled proprotionally to softmaxed utitily vector
    ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt
    # print(ind)
    X_oth[[HowManyOthers]] <- as.matrix(dat_social[ind, 1:2])
    # bind y-observations
    y_oth[HowManyOthers] <- as.matrix(rnorm(n = 1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance)) # change this into a sample.
    dat_social[ind, ]$others <- dat_social[ind, ]$others + 1

    ####
    #### Now, let the others play
    ####
    WhereIsEverybody(HowManyOthers - 1, others, otherLoc, dat_social, X_oth, y_oth, diminishingSocial, trial, cntrl, sampleVec) # recursion function returns itslef until no others are left
  }
}











####
#### wrapper function for recursive ABM.
####


exploreEnv_Social <- function(explore_func, choiceRule, socialfunc, env2, env1, cntrl, iter) {
  # for (rep in 1:ntrialss){
  # unpack
  EnvirionemntAdol <- env2
  lambda <- cntrl$lambda
  # get beta
  beta <- cntrl$beta # this scales risk attitude.
  # get tau
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
  AllChoices_social <- cntrl$AllChoices_social
  dummy <- cntrl$dummy
  # overallCnt=cntrl$overallCnt
  dat_social <- cntrl$dat_social
  dat_social$others <- 0 # to get the right indices
  otherLoc <- dat_social$others # location of others in last turn i need it to pass it to the social updating function to avoid social info to have a cumulative effect relative to trials.

  HowManyOthers <- cntrl$HowManyOthers
  diminishingSocial <- cntrl$diminishingSocial
  # info about the agents
  others <- vector(mode = "list", length = HowManyOthers) # environment for everybody_needed for learning
  X_oth <- vector(mode = "list", length = HowManyOthers) # new sample
  y_oth <- vector(length = HowManyOthers) # new outcome
  AllOthers <- NULL

  # for (nround in 1:3){

  # get parameters for participant on that round
  ind <- sample(1:64, 1)
  nTrials <- 25
  X <- as.matrix(dat_social[ind, 1:2]) # generate a new vector of Xs
  y <- as.matrix(rnorm(1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))
  # random initialization as observation t=0
  # y matrix
  Xstar <- as.matrix(dat_social[, 1:2])

  for (trial in 1:nTrials) {
    # dat_social$others=0
    # output by GP with particular parameter settings
    # don't forget mean centering and standardization.... mean is already 0 :)
    if (trial > 1) {
      out <- bayesianMeanTracker(X[trial, 1:2], y[trial], theta = lambda, prevPost = out, mu0Par = mu0, var0Par = var0)
    } else {
      out <- bayesianMeanTracker(X[trial, 1:2], y[trial], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
    }
    # utility vector. transpose if you use greedyMean
    # where is everybody?
    # here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
    ####
    ####
    Out_Others <- WhereIsEverybody(HowManyOthers, others, otherLoc, dat_social, X_oth, y_oth, diminishingSocial, trial, cntrl, sampleVec) # recursion.
    otherLoc <- Out_Others$Others # how many others are in one spot
    X_oth <- Out_Others$LastSamples
    y_oth <- Out_Others$LastReturn
    others <- Out_Others$OthersUtil
    # print(otherLoc)
    utilityVec <- ucb(out, beta)
    # if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    # } else{
    # social impact follows a power law starting in adolescnece.
    browser()
    utilityVec <- utilityVec + otherLoc^diminishingSocial
    # }
    # utilityVec=utilityVec+otherLoc^diminishingSocial#add social info
    utilities <- utilityVec - max(utilityVec)
    # softmaximization
    p <- exp(utilities / tau)
    # probabilities
    p <- p / colSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # browser()
    # index is sampled proprotionally to softmaxed utitily vectorelse {
    ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt

    X <- rbind(X, as.matrix(dat_social[ind, 1:2]))
    # bind y-observations
    y <- rbind(y, as.matrix(rnorm(n = 1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))) # change this into a sample.
    # if(y[overallCnt]<0){
    #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe.
    # }
    AllOthers <- rbind(AllOthers, data.frame(Loc = otherLoc, trial = trial))
    dummy <- data.frame(
      trial = trial, x = as.numeric(X[trial, 1]), y = as.numeric(X[trial, 2]),
      z = as.numeric(y[trial])
    )

    AllChoices_social <- rbind(AllChoices_social, dummy)
    # trial=trial+1
  }
  # dummy data frame
  # }
  # This Here is for Plotting
  Plot_dat_social <- expand.grid(x = 1:8, y = 1:8, trials = 0:max(dummy$trial))
  Plot_dat_social$sample <- 0
  Plot_dat_social$out <- 0


  for (i in 1:length(AllChoices_social$x)) {
    AllChoices_social$y[i]
    AllChoices_social$x[i]
    Plot_dat_social[Plot_dat_social$x == AllChoices_social$x[i] & Plot_dat_social$y == AllChoices_social$y[i] & Plot_dat_social$trials == AllChoices_social$trial[i], ]$sample <- 1
    Plot_dat_social[Plot_dat_social$trials == AllChoices_social$trial[i], ]$out <- AllChoices_social$z[i]
  }

  ## here add the tally of "others".
  Plot_dat_social$Others <- 0
  for (k in unique(Plot_dat_social$trials)) {
    if (k > 0) {
      Plot_dat_social[Plot_dat_social$trials == k, ]$Others <- AllOthers[AllOthers$trial == k, ]$Loc
    }
  }
  Plot_dat_social$iter <- iter
  return(Plot_dat_social)
}
















####
#### Social exploration
####
WhereIsEverybodyProb <- function(HowManyOthers, others, otherLoc, dat_social, X_oth, y_oth, diminishingSocial, trial, cntrl, sampleVec) {
  lowestx <- 4
  highestx <- 9
  lambda <- cntrl$lambda
  # get beta
  beta <- cntrl$beta # this scales risk attitude.
  # get tau
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

  mu0 <- 100
  if (HowManyOthers == 1) {
    return(list(Others = dat_social$others, LastSamples = X_oth, LastReturn = y_oth, OthersUtil = others)) # basecase, terminate
  } else {
    ####
    #### Update_socialing process for each individual
    ####
    # browser()
    if (trial == 1) {
      ind <- sample(1:64, 1)
      print("Youre an adolescent now")
      X_oth[[HowManyOthers]] <- as.matrix(dat_social[ind, 1:2]) # generate a new vector of Xs
      y_oth[HowManyOthers] <- as.matrix(rnorm(1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))
    }
    if (trial > 1) {
      others[[HowManyOthers]] <- bayesianMeanTracker(X_oth[[HowManyOthers]], y_oth[HowManyOthers], theta = lambda, prevPost = others[[HowManyOthers]], mu0Par = mu0, var0Par = var0)
    } else {
      others[[HowManyOthers]] <- bayesianMeanTracker(X_oth[[HowManyOthers]], y_oth[HowManyOthers], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
    }
    utilityVec <- ucb(others[[HowManyOthers]], beta)
    # browser()
    # if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    # } else{
    # social impact follows a power law starting in adolescnece.
    browser()
    # }
    utilities <- utilityVec - max(utilityVec)
    utilities <- utilities + otherLoc * diminishingSocial
    # utilities=utilities
    # softmaximization
    p <- exp(utilities / tau)
    # probabilities
    p <- p / colSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # index is sampled proprotionally to softmaxed utitily vector
    ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt
    # print(ind)
    X_oth[[HowManyOthers]] <- as.matrix(dat_social[ind, 1:2])
    # bind y-observations
    y_oth[HowManyOthers] <- as.matrix(rnorm(n = 1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance)) # change this into a sample.
    dat_social[ind, ]$others <- dat_social[ind, ]$others + 1

    ####
    #### Now, let the others play
    ####
    WhereIsEverybodyProb(HowManyOthers - 1, others, otherLoc, dat_social, X_oth, y_oth, diminishingSocial, trial, cntrl, sampleVec) # recursion function returns itslef until no others are left
  }
}




exploreEnv_SocialProb <- function(explore_func, choiceRule, socialfunc, env2, env1, cntrl, iter) {
  # for (rep in 1:ntrialss){
  # unpack
  EnvirionemntAdol <- env2
  lambda <- cntrl$lambda
  # get beta
  beta <- cntrl$beta # this scales risk attitude.
  # get tau
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
  AllChoices_social <- cntrl$AllChoices_social
  dummy <- cntrl$dummy
  # overallCnt=cntrl$overallCnt
  dat_social <- cntrl$dat_social
  dat_social$others <- 0 # to get the right indices
  otherLoc <- dat_social$others # location of others in last turn i need it to pass it to the social updating function to avoid social info to have a cumulative effect relative to trials.

  HowManyOthers <- cntrl$HowManyOthers
  diminishingSocial <- cntrl$diminishingSocial
  # info about the agents
  others <- vector(mode = "list", length = HowManyOthers) # environment for everybody_needed for learning
  X_oth <- vector(mode = "list", length = HowManyOthers) # new sample
  y_oth <- vector(length = HowManyOthers) # new outcome
  AllOthers <- NULL

  # for (nround in 1:3){

  # get parameters for participant on that round
  ind <- sample(1:64, 1)
  nTrials <- 25
  X <- as.matrix(dat_social[ind, 1:2]) # generate a new vector of Xs
  y <- as.matrix(rnorm(1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))
  # random initialization as observation t=0
  # y matrix
  Xstar <- as.matrix(dat_social[, 1:2])

  for (trial in 1:nTrials) {
    # dat_social$others=0
    # output by GP with particular parameter settings
    # don't forget mean centering and standardization.... mean is already 0 :)
    if (trial > 1) {
      out <- bayesianMeanTracker(X[trial, 1:2], y[trial], theta = lambda, prevPost = out, mu0Par = mu0, var0Par = var0)
    } else {
      out <- bayesianMeanTracker(X[trial, 1:2], y[trial], theta = lambda, prevPost = NULL, mu0Par = mu0, var0Par = var0)
    }
    # utility vector. transpose if you use greedyMean
    # where is everybody?
    # here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
    ####
    ####
    Out_Others <- WhereIsEverybodyProb(HowManyOthers, others, otherLoc, dat_social, X_oth, y_oth, diminishingSocial, trial, cntrl, sampleVec) # recursion.
    otherLoc <- Out_Others$Others # how many others are in one spot
    X_oth <- Out_Others$LastSamples
    y_oth <- Out_Others$LastReturn
    others <- Out_Others$OthersUtil
    # print(otherLoc)
    utilityVec <- ucb(out, beta)
    # if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    # } else{
    # social impact follows a power law starting in adolescnece.
    # }
    # utilityVec=utilityVec+otherLoc^diminishingSocial#add social info
    utilities <- utilityVec - max(utilityVec)
    utilities <- utilities + otherLoc * diminishingSocial

    # softmaximization
    p <- exp(utilities / tau)
    # probabilities
    p <- p / colSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # browser()
    # index is sampled proprotionally to softmaxed utitily vectorelse {
    ind <- sample(1:64, 1, prob = p) # sample from an adolescent environemnt

    X <- rbind(X, as.matrix(dat_social[ind, 1:2]))
    # bind y-observations
    y <- rbind(y, as.matrix(rnorm(n = 1, mean = EnvirionemntAdol[ind, ]$Mean, sd = EnvirionemntAdol[ind, ]$Variance))) # change this into a sample.
    # if(y[overallCnt]<0){
    #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe.
    # }
    AllOthers <- rbind(AllOthers, data.frame(Loc = otherLoc, trial = trial))
    dummy <- data.frame(
      trial = trial, x = as.numeric(X[trial, 1]), y = as.numeric(X[trial, 2]),
      z = as.numeric(y[trial])
    )

    AllChoices_social <- rbind(AllChoices_social, dummy)
    # trial=trial+1
  }
  # dummy data frame
  # }
  # This Here is for Plotting
  Plot_dat_social <- expand.grid(x = 1:8, y = 1:8, trials = 0:max(dummy$trial))
  Plot_dat_social$sample <- 0
  Plot_dat_social$out <- 0


  for (i in 1:length(AllChoices_social$x)) {
    AllChoices_social$y[i]
    AllChoices_social$x[i]
    Plot_dat_social[Plot_dat_social$x == AllChoices_social$x[i] & Plot_dat_social$y == AllChoices_social$y[i] & Plot_dat_social$trials == AllChoices_social$trial[i], ]$sample <- 1
    Plot_dat_social[Plot_dat_social$trials == AllChoices_social$trial[i], ]$out <- AllChoices_social$z[i]
  }

  ## here add the tally of "others".
  Plot_dat_social$Others <- 0
  for (k in unique(Plot_dat_social$trials)) {
    if (k > 0) {
      Plot_dat_social[Plot_dat_social$trials == k, ]$Others <- AllOthers[AllOthers$trial == k, ]$Loc
    }
  }
  Plot_dat_social$iter <- iter
  return(Plot_dat_social)
}








# Function to simulate data from parameters estimated from model fitting procedure.
# Takes as input the parameters, the data, the

modelSim <-
  function(par,
           subjD,
           acquisition,
           k,
           horizonLength,
           rounds,
           env_numbers) {
    #  Extract and process parameters
    if (inherits(acquisition, "epsilonGreedy")) {
      epsilon <-
        1 / (1 + exp(-(par[length(par)]))) # transform back from unbounded space; epsilon is the last parameter for epsilon greedy
    }

    # Exponentiate parameters to make a non-negative and convex optimization surface
    par <- exp(par)

    # Last parameter for all other models is always inverse temperature for softmax
    tau <- par[length(par)]

    # Which posterior function to use; therefore, which parameters to use
    if (inherits(k, "KalmanFilter")) {
      # null kernel indicates kalman filter model
      kNoise <- par[1]
      parVec <-
        c(kNoise) # Vector of parameters to send to the KF posterior function
    } else if (inherits(k, "GP")) {
      # lambda
      lambda <- par[1]
      parVec <-
        c(lambda, lambda, 1, .0001) # Vector of parameters to send to the GP posterior vector, where sF and sN are fixed
    } else if (inherits(k, "bmt_free_priors")) {
      prior_mu <- log(par[1]) # put back into native space between -100 and 100
      prior_var <- par[2]
      kNoise <- par[3]
      parVec <- c(prior_mu, prior_var, kNoise)
    }


    # Additional acquisition function dependent parameters
    if (inherits(acquisition, "UCB") |
      inherits(acquisition, "exploreCounts") |
      inherits(acquisition, "epsilonGreedy")) {
      # check if UCB is used
      beta <- par[length(par) - 1] # If UCB, beta is always 2nd last
      # refactor beta and tau into gamma and beta_star, where gamma = 1/tau and beta_star = beta/tau
    }

    # which rounds to consider? @simon: is this working? now it takes everything
    trainingSet <- subset(subjD, round %in% rounds)

    # Vector to store negative log likelihoods
    nLL <- rep(0, length(rounds))

    # simple counter to know which iteration this is
    counter <- 0
    choices_round <- data.frame(
      choice_index = numeric(0),
      points_new_choice = numeric(0),
      x_new_choice = numeric(0),
      y_new_choice = numeric(0),
      z_new_choice = numeric(0),
      trial = numeric(0),
      env_number = numeric(0),
      env_counter = numeric(0),
      env_type = numeric(0)
    )

    for (r in unique(trainingSet$round)) {
      # Begin looping through each round
      # subset of data for round r

      # @andreaL don't need the counter anymore, now it's a sequence
      counter <- counter + 1

      roundD <- subset(subjD, round == r)
      horizon <- nrow(roundD)

      # Which tiles where clicked?
      chosen <- roundD$cells

      # trim first observation, since it wasn't a choice but a randomly revealed tile
      chosen <- chosen[2:length(chosen)]

      ### Get first observation, to start the cycle of simulating choices

      points <- roundD$points[1] # absolute n of points
      y <- roundD$z[1] # rewards (standardized at environment type level, maybe refine?)
      x1 <- roundD$x[1] # x position
      x2 <- roundD$y[1] # y position
      is_gem <- roundD$gempresent[1]

      # bind into an observation matrix
      X <- as.matrix(cbind(x1, x2))

      # @simon: do we need this line?
      # Xnew <- as.matrix(Xnew)

      # Utilties of each choice

      utilities <- NULL
      prevPost <- NULL # set the previous posterior computation to NULL for the kalman filter
      pMat <- NULL
      choices <- NULL

      # loop through observations I (until i - 1)
      # skip the last observation, because no choice was made based on that information


      for (i in 1:(horizon - 1)) {
        # browser()
        # in first round, get the real choice of the participant and associated reward
        if (i == 1) {
          X1 <- matrix(X[1:i, ], ncol = 2)
          y1 <- y[1:i]

          # make df to update over loop
          choices <- data.frame(
            choice_index = x1 + 1 + (x2 * 8), # interesting!
            points_new_choice = points,
            x_new_choice = x1,
            y_new_choice = x2,
            z_new_choice = y,
            trial = 1,
            env_number = r,
            env_counter = counter,
            env_type = is_gem
          )
        } else {
          X1 <- matrix(choices[1:i, 3:4], ncol = 2)
          y <- choices[1:i, 5]
        }

        # Which posterior function to use

        if (inherits(k, "KalmanFilter")) {
          # kalman filter model
          out <-
            bayesianMeanTracker(
              x = X1[i, ],
              y = y[i],
              prevPost = prevPost,
              theta = parVec
            )

          # update prevPost for the next round
          prevPost <- out
        } else if (inherits(k, "bmt_free_priors")) {
          # kalman filter model with free priors
          out <-
            bmt_free_priors(
              x = X1[i, ],
              y = y[i],
              prevPost = prevPost,
              theta = parVec
            )
          # update prevPost for the next round
          prevPost <- out
        }
        #   # GP with length-scale parameterized kernel
        #   out <-
        #     gpr(
        #       X.test = Xnew,
        #       theta = parVec,
        #       X = X1,
        #       Y = y1,
        #       k = k
        #     ) #Mu and Sigma predictions for each of the arms; either GP or Kalman filter
        # } else if (inherits(k, 'Null')) {
        #   #null model
        #   out <-
        #     nullModel() #Mu and Sigma predictions for each of the arms; either GP or Kalman filter
        # }

        # Slightly different function calls for each acquisition function
        if (inherits(acquisition, "UCB")) {
          # UCB takes a beta parameter
          utilityVec <- acquisition(out, c(beta))
        } else if (inherits(acquisition, "exploreCounts")) {
          # count-based exploration
          utilityVec <-
            exploreCounts(out, roundD$chosen[1:i], c(beta))
        } else if (inherits(acquisition, "epsilonGreedy")) {
          p <- epsilonGreedy(out, beta, epsilon)
          pMat <- rbind(pMat, t(p))
        } else {
          # any other
          utilityVec <- acquisition(out)
        }

        if (inherits(acquisition, "softmax")) {
          utilityVec <- utilityVec - max(utilityVec) # avoid overflow

          utilities <-
            rbind(utilities, t(utilityVec)) # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
        }

        # use softmax to transform utilites inti probabilities

        p <- exp(utilities / tau)
        p <- p / rowSums(p)

        # avoid underflow by setting a floor and a ceiling
        p <- (pmax(p, 0.00001))
        p <- (pmin(p, 0.99999))

        pMat <- p

        # make a choice based on the probabilites
        # are there gems or not?

        # WRITE LITTLE FUNCTION HERE

        this_env <- env_numbers[counter, ]

        if (this_env$gempresent == 1) {
          this_env_data <- envs_gems_list[this_env$env_number]
          env_type <- 1
        } else {
          this_env_data <- envs_no_gems_list[this_env$env_number]
          env_type <- 0
        }

        # browser()
        new_choice <- make_a_choice(pMat, this_env_data, i, subjD, counter, is_gem)
        choices <- as.matrix(rbind(choices, new_choice))
        # prevPost <- out
      }
      # browser()
      choices_round <- rbind(choices_round, choices)
    }
    #  end of loop through rounds

    # check if free priors or kalman filter and add respective parameters.
    if (inherits(k, "KalmanFilter")) {
      choices_round <- choices_round %>%
        mutate(
          playerNr = subjD$player[1],
          prior_mu = NA,
          prior_var = NA,
          learning_rate = parVec,
          temperature = tau
        )
    } else if (inherits(k, "bmt_free_priors")) {
      # kalman filter model with free priors
      choices_round <- choices_round %>%
        mutate(
          playerNr = subjD$player[1],
          prior_mu = parVec[1],
          prior_var = parVec[2],
          learning_rate = parVec[3],
          temperature = tau
        )
    }

    return(choices_round)
  }
