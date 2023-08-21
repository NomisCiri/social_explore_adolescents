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
  vare <- theta[1] # error varriance
  if (is.null(prevPost)) { # if no posterior prior, assume it is the first observation
    predictions <- data.frame(mu = rep(mu0, 64), sig = rep(var0, 64))
  } else { # if previous posterior is provided, update
    predictions <- prevPost
  }
  # Which of the 121 options were chosen at time?
  alloptrials <- expand.grid(0:7, 0:7)
  chosen <- x# which(alloptrials$Var1 == x[1] & alloptrials$Var2 == x[2])
  # Kalman gain
  kGain <- predictions$sig[chosen] / (predictions$sig[chosen] + vare) # feed the uncertainty in here.
  # update mean
  predictions$mu[chosen] <- predictions$mu[chosen] + (kGain * (y - predictions$mu[chosen]))
  # update variance for observed arm
  predictions$sig[chosen] <- predictions$sig[chosen] * (1 - kGain)
  # return output
    #browser()
  
  return(predictions)
}

########
# RW-Q learning no social info
#######
RW_Q <- function(x, y, theta, prevPost = NULL, mu0Par) {
  
  # Updates the previous posterior based on a single observation
  # parameters
  mu0 <- mu0Par # prior mean
  lr <- theta[1]
  
  if (is.null(prevPost)) { # if no posterior prior, assume it is the first observation
    predictions <- rep(mu0, 64)
  } else { # if previous posterior is provided, update
    predictions <- prevPost
  }
  # Which of the 64 options were chosen at time?
  #alloptrials <- expand.grid(x1=0:7, x2=0:7)
  chosen <- x#which(alloptrials$x1 == x[1] & alloptrials$x2 == x[2])
  # value update
  predictions[chosen] <- predictions[chosen] + (lr * (y - predictions[chosen]))
  # browser()
  
  return(predictions)
}


########
# RW-Q 2 learning rates (positive and negative); no social-info

#######
RW_Q_2 <- function(x, y, theta, prevPost = NULL, mu0Par) {
  # Updates the previous posterior based on a single observation
  # parameters
  mu0 <- mu0Par # prior mean
  lr_p = theta[1]
  lr_n = theta[2]
  
  if (is.null(prevPost)) { # if no posterior prior, assume it is the first observation
    predictions <- rep(mu0, 64)
  } else { # if previous posterior is provided, update
    predictions <- prevPost
  }
  # Which of the 64 options were chosen at time?
  #alloptrials <- expand.grid(x1=0:7, x2=0:7)
  chosen <- x#which(alloptrials$x1 == x[1] & alloptrials$x2 == x[2])
  # value update
  
  # calculate preditction error
  pe <- y - predictions[chosen]
  
  # evaluate if pe > 0 or < 0: negative pe will have higher lr
  
  if (pe  > 0) {
    predictions[chosen] <- predictions[chosen] + (lr_p * pe)
  }
  
  else if (pe <= 0) {
    predictions[chosen] <- predictions[chosen] + (lr_n * pe)
  }

  return(predictions)
}


