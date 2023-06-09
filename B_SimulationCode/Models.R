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
######### Exploration Environmental change
#########
explore_env_social <- function(explore_func, choiceRule, env, cntrl, iter) {
  # for (rep in 1:ntrialss){
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







