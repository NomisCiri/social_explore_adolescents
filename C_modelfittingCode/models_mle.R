########
# Models for the social information project SC_06_2023
#######


########
# BMT Model: maybe change into Q learning
#######
bayesianMeanTracker <- function(x, y, theta, prevPost = NULL, mu0Par, var0Par) {
  # Updates the previous posterior based on a single observation
  # parameters
  mu0 <- mu0Par # prior mean
  var0 <- var0Par # prior variance
  vare <- 3600#theta[1] # error varriance
  if (is.null(prevPost)) { # if no posterior prior, assume it is the first observation
    predictions <- data.frame(mu = rep(mu0, 64), sig = rep(var0, 64))
  } else { # if previous posterior is provided, update
    predictions <- prevPost
  }
  # Which of the 121 options were chosen at time?
  alloptrials <- expand.grid(0:7, 0:7)
  chosen <- which(alloptrials$Var1 == x[1] & alloptrials$Var2 == x[2])
  # Kalman gain
  kGain <- predictions$sig[chosen] / (predictions$sig[chosen] + vare) # feed the uncertainty in here.
  # update mean
  predictions$mu[chosen] <- predictions$mu[chosen] + (kGain * (y - predictions$mu[chosen]))
  # update variance for observed arm
  predictions$sig[chosen] <- predictions$sig[chosen] * (1 - kGain)
  # return output
  #  browser()
  
  return(predictions)
}

########
# RW-Q learning
#######
RW_Q <- function(x, y, theta, prevPost = NULL, mu0Par) {
  # Updates the previous posterior based on a single observation
  # parameters
  mu0 <- mu0Par # prior mean
  lr=theta[1]
  if (is.null(prevPost)) { # if no posterior prior, assume it is the first observation
    predictions <- rep(mu0, 64)
  } else { # if previous posterior is provided, update
    predictions <- prevPost
  }
  # Which of the 64 options were chosen at time?
  alloptrials <- expand.grid(x1=0:7, x2=0:7)
  chosen <- which(alloptrials$x1 == x[1] & alloptrials$x2 == x[2])
  # value update
  predictions[chosen] <- predictions[chosen] + (lr * (y - predictions[chosen]))
  # browser()
  
  return(predictions)
}

########
## UCB sampling.... 
########
ucb <- function(out, pars, refactor = F) {
  beta <- pars[1]
  # calulate all the upper confidence bounds
  outtotal <- out #+ (beta * sqrt(out$sig))
  # turn into matrix
  outtotal <- matrix(outtotal, ncol = 1, byrow = TRUE)
  # return them
  return(outtotal)
}


#########
######### social exploration model
#########
soc_utility_model <- function(par, learning_model_fun, acquisition_fun, dat) {
  # for (rep in 1:ntrialss){
  # unpack
  #par<-exp(par)#parameters are defined in logspace, we exponentiate them here
  theta <- par[1]# "learningrate"
  tau <- par[2] #  "random" exploration
  zeta<-par[3] # scales social info use
  
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
    # trim first observation, since it wasn't a choice but a randomly revealed tile (not informative for log likelihood).
    chosen <- chosen[2:length(chosen)] 
    y <- round_df$z[0:(trials - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    x1 <- round_df$x[0:(trials - 1)]
    x2 <- round_df$y[0:(trials - 1)]
    
    # social information
    social_choices<-round_df$social_info
    # create observation matrix
    X <- as.matrix(cbind(x1, x2))
    #Xnew <- as.matrix(Xnew)# unsure whatrials the use of this
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for qlearning
    pMat <- NULL
    #here, too
    for (t in 1:(trials-1)) {
      #learn
      if (t > 1) {
        out <- RW_Q(X[t, 1:2], y[t], theta = theta, prevPost = out, mu0Par = mu0)
      } else {
        # first t of each round, start new
        out <- RW_Q(X[t, 1:2], y[t], theta = theta, prevPost = NULL, mu0Par = mu0)
      }
      utilityVec <- ucb(out, 0)
      #next choice getrials a social utility
      #utilityVec=utilityVec-max(utilityVec)
      utilityVec[social_choices[t]]<-utilityVec[social_choices[t]]+zeta
      #browser()
      # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      utilities <- rbind(utilities, t(utilityVec)) 
      #browser()
    }
    # softmaximization
    p <- exp(utilities / tau)
    # probabilities
    p <- p / rowSums(p)
    # numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # add loglik nasty way of checking the predicted choice probability a the item that was chosen
    nLL[which(unique(dat$round) == r)] <- -sum(log(p[cbind(c(1:(trials-1)), chosen)]))
    #browser()
  }
  browser()
  #avoid nan in objective function
  if(any(is.nan(sum(nLL))))
  { 
    return(10 ^ 30)
    
  }else
  {
    return(sum(nLL))
  }
}


##############################################################################################################
# FITTING FUNCTION
##############################################################################################################
# function to plug in to the optimaztion routine
# selector: scalar, indicates a specific participant
# learning_model_fun, only bayesan meantracker works
# acquisition_fun, function, can be "ucb"

fit_fun <- function(d1) {
  # subselect participant, horizon and rounds not left out
  #which rounds to use
  rounds <- 1:12
  nParams<-3
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,-4)
  ubound <- c(1,10,4)
  
  
  #####
  # Begin cross validation routine
  # TRAINING SET
  fit <- DEoptim(
    soc_utility_model, 
    lower = lbound, 
    upper = ubound, 
    dat = d1,
    DEoptim.control(itermax = 100)
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  # TEST SET
  predict <- soc_utility_model(
    par = paramEstimates, 
    dat = d1
  )
  output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}













##############################################################################################################
# CROSS VALIDATION FUNCTION (NOT USED)
##############################################################################################################
# function to plug in to the optimaztion routine
# selector: scalar, indicates a specific participant
# kernel, only bayesan meantracker works
# acquisition, function, can be "ucb", "probofimp", "expofimp", or "PMU"
# horizonLength is number of search decisions in each t
# leaveoutindex is [1,2,...,n_rounds]
# inertiaWeight: whether nor not acquisition functions are weighted by inertia

cvfun <- function(selector, learning_model_fun, acquisition_fun, leaveoutindex) {
  # subselect participant, horizon and rounds not left out
  d1 <- subset(data, id == selector)
  # training set
  rounds <- 1:12
  trainingSet <- rounds[!rounds == leaveoutindex] # remove round specified by leaveoutindex
  # test set
  testrialset <- leaveoutindex
  nParams <- 1
  
  # this allows for changing the parameter number and boundaries more flexibly
  if (inheritrials(acquisition_fun, "UCB") | inheritrials(acquisition_fun, "exploreCountrials") | inheritrials(acquisition_fun, "epsilonGreedy")) {
    nParams <- nParams + 1 # add beta parameter
  }
  if (inheritrials(learning_model_fun, "GP") | inheritrials(learning_model_fun, "KalmanFilter")) {
    nParams <- nParams + 1 # add lambda or error variance
  }
  # Set upper and lower bounds based on nParams
  lbound <- rep(-5, nParams)
  ubound <- rep(4, nParams)
  if (inheritrials(acquisition, "epsilonGreedy")) { # separate range for epsilon greedy, which after inverse logic transform is bounded between [0,1]
    lbound[length(lbound)] <- -10
    ubound[length(ubound)] <- 10
  }
  
  
  #####
  # Begin cross validation routine
  # TRAINING SET
  fit <- DEoptim(
    modelFit, 
    lower = lbound, 
    upper = ubound, 
    subjD = d1, 
    k = kernelFun, 
    rounds = trainingSet, 
    acquisition = acquisition,
    DEoptim.control(itermax = 100)
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  # TEST SET
  predict <- modelFit(par = paramEstimates, subjD = d1, acquisition = acquisition, k = kernelFun, rounds = testrialset)
  output <- c(leaveoutindex, predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}

