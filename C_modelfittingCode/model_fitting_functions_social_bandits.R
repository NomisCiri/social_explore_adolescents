###########################################################################
###########################################################################
###                                                                     ###
###                          FITTING FUNCTIONS                          ###
###                                                                     ###
###########################################################################
###########################################################################

# functions to plug in to the optimaztion routine
# selector: scalar, indicates a specific participant
# learning_model_fun, only bayesan meantracker works
# acquisition_fun, function, can be "ucb"

##---------------------------------------------------------------
##                    basic Q-Learning model                   --
##---------------------------------------------------------------

fit_1lr <- function(d1, rounds) {
  # subselect participant, horizon and rounds not left out
  
  #which rounds to use
  nParams <- 2
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001) 
  ubound <- c(1,10)                           
  
  #####
  # Begin cross validation routine
  # TRAINING SET
  fit <- DEoptim(
    utility_1lr, 
    lower = lbound, 
    upper = ubound, 
    dat = d1,
    DEoptim.control(itermax = 100)
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  
  # TEST SET
  predict <- utility_1lr(
    par = paramEstimates, 
    dat = d1
  )
  output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}


##----------------------------------------------------------------
##       Q-Learning model with 2 learning rates (pos&neg)       --
##----------------------------------------------------------------

fit_2lr <- function(d1) {
  # subselect participant, horizon and rounds not left out
  
  #which rounds to use
  nParams <- 3
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,0.00000001) # first 2 are lr (pos, neg), then temperature, and social weight
  ubound <- c(1,1,10)                            # first 2 are lr (pos, neg), then temperature, and social weight
  
  
  #####
  # Begin cross validation routine
  # TRAINING SET
  fit <- DEoptim(
    utility_2lr, 
    lower = lbound, 
    upper = ubound, 
    dat = d1,
    DEoptim.control(itermax = 100)
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  # TEST SET
  predict <- utility_2lr(
    par = paramEstimates, 
    dat = d1
  )
  output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}

##------------------------------------------------------------------------
##  Q-Learning model with 2 learning rates (pos&neg) and social weight  --
##------------------------------------------------------------------------

fit_2lr_sw <- function(d1) {
  # subselect participant, horizon and rounds not left out
  #which rounds to use
  rounds <- 1:12
  nParams<- 4
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,0.00000001,0) # first 2 are lr (pos, neg), then temperature, and social weight
  ubound <- c(1,1,10,40)                            # first 2 are lr (pos, neg), then temperature, and social weight
  
  #####
  # Begin cross validation routine
  # TRAINING SET
  fit <- DEoptim(
    utility_2lr_sw, 
    lower = lbound, 
    upper = ubound, 
    dat = d1,
    DEoptim.control(itermax = 100)
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  # TEST SET
  predict <- utility_2lr_sw(
    par = paramEstimates, 
    dat = d1
  )
  output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, prameters....
  return(output) # return optimized value
}


##------------------------------------------------------------------------
##  e-greedy Q-Learning model with 2 learning rates (pos&neg) and social weight --
##------------------------------------------------------------------------

fit_2lr_sw_gre <- function(d1) {
  # subselect participant, horizon and rounds not left out
  #which rounds to use
  rounds <- 1:12
  nParams<- 4
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,0.00000001,-1,-50) # first 2 are lr (pos, neg), then greedy, and social weight
  ubound <- c(2,2,0.99,2,50)                            # first 2 are lr (pos, neg), then greedy, and social weight
  
  #####
  # Begin cross validation routine
  output_catched<-tryCatch({#if sth crashes
    # TRAINING SET
    fit <- DEoptim(
      utility_2lr_sw_e_greedy, 
      lower = lbound, 
      upper = ubound, 
      dat = d1,
      DEoptim.control(itermax = 100)
    )
    paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
    # TEST SET
    predict <- utility_2lr_sw_e_greedy(
      par = paramEstimates, 
      dat = d1
    )
    output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, prameters....
    return(output)
  },
  error = function(e) {
    output<-rep(NA,length(lbound+1))
    # TEST SET
    return(output)
  }
  )
  return(output_catched)
}


##------------------------------------------------------------------------
##  2 e-greedy Q-Learning model with 2 learning rates (pos&neg) and social weight --
##------------------------------------------------------------------------
fit_2lr_sw_2gre <- function(d1,leaveoutindex) {
  # subselect participant, horizon and rounds not left out
  #which rounds to use
  rounds <- unique(d1$round)
  trainingSet <- rounds[!rounds == leaveoutindex] # remove round specified by leaveoutindex
  # test set
  testrialset <- leaveoutindex
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,0.00000001,0.00000001) # first 2 are lr (pos, neg), then greedy, and social weight
  ubound <- c(2,2,0.99999999,0.99999999)                            # first 2 are lr (pos, neg), then greedy, and social weight
  
  #####
  # Begin cross validation routine
  output_catched<-tryCatch({#if sth crashes
    # TRAINING SET
    fit <- DEoptim(
      utility_2lr_2e_greedy, 
      lower = lbound, 
      upper = ubound, 
      dat = d1%>%filter(round %in% trainingSet),
      DEoptim.control(itermax = 50,NP=5)
    )
    paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
    # TEST SET
    predict <- utility_2lr_2e_greedy(
      par = paramEstimates, 
      dat = d1%>%filter(round==leaveoutindex)
    )
    output <- c(leaveoutindex,predict, fit$optim$bestmem) # leaveoutindex, nLL, prameters....
    return(output)
  },
  error = function(e) {
    output<-rep(NA,length(lbound+1))
    # TEST SET
    return(output)
  }
  )
  return(output_catched)
}


##------------------------------------------------------------------------
##  2 e-greedy Q-Learning model with 2 learning rates (pos&neg) and social weight --
##------------------------------------------------------------------------
fit_2lr_sw_softmax_egreedy <- function(d1,leaveoutindex) {
  # subselect participant, horizon and rounds not left out
  #which rounds to use
  rounds <- unique(d1$round)
  trainingSet <- rounds[!rounds == leaveoutindex] # remove round specified by leaveoutindex
  # test set
  testrialset <- leaveoutindex
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,0.00000001,0.00000001,0) # first 2 are lr (pos, neg),tau, greedy, and social weight
  ubound <- c(5,5,20,0.99999999,50)                            # first 2 are lr (pos, neg),tau, greedy, and social weight
  
  #####
  # Begin cross validation routine
  output_catched<-tryCatch({#if sth crashes
    # TRAINING SET
    fit <- DEoptim(
      utility_2lr_sw_softmax_egreedy, 
      lower = lbound, 
      upper = ubound, 
      dat = d1%>%filter(round %in% trainingSet),
      DEoptim.control(itermax = 100,NP=20)
    )
    paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
    # TEST SET
    predict <- utility_2lr_sw_softmax_egreedy(
      par = paramEstimates, 
      dat = d1%>%filter(round==leaveoutindex)
    )
    output <- c(leaveoutindex,predict, fit$optim$bestmem) # leaveoutindex, nLL, prameters....
    return(output)
  },
  error = function(e) {
    output<-rep(NA,length(lbound+1))
    # TEST SET
    return(output)
  }
  )
  return(output_catched)
}


##------------------------------------------------------------------------
##  2 e-greedy Q-Learning model with 2 learning rates (pos&neg) and social weight --
##------------------------------------------------------------------------
fit_2lr_3sw_softmax_egreedy <- function(d1,leaveoutindex) {
  # subselect participant, horizon and rounds not left out
  #which rounds to use
  rounds <- unique(d1$round)
  trainingSet <- rounds[!rounds == leaveoutindex] # remove round specified by leaveoutindex
  # test set
  testrialset <- leaveoutindex
  
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00000001,0.00000001,0.00000001,0.00000001,0,0,0,0) # first 2 are lr (pos, neg),tau, greedy, and social weight
  ubound <- c(5,5,20,0.99999999,50,50,50,50)                            # first 2 are lr (pos, neg),tau, greedy, and social weight
  
  #####
  # Begin cross validation routine
  output_catched<-tryCatch({#if sth crashes
    # TRAINING SET
    fit <- DEoptim(
      utility_2lr_3sw_softmax_egreedy, 
      lower = lbound, 
      upper = ubound, 
      dat = d1%>%filter(round %in% trainingSet),
      DEoptim.control(itermax = 200)
    )
    paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
    # TEST SET
    predict <- utility_2lr_3sw_softmax_egreedy(
      par = paramEstimates, 
      dat = d1%>%filter(round==leaveoutindex)
    )
    output <- c(leaveoutindex,predict, fit$optim$bestmem) # leaveoutindex, nLL, prameters....
    return(output)
  },
  error = function(e) {
    output<-rep(NA,length(lbound+1))
    # TEST SET
    return(output)
  }
  )
  return(output_catched)
}

# 
# ##
# ###
# ### Fit function to fit the utility model 1lr no gems with social weight
# ###
# 
# fitFun1lrsw <- function(d1, rounds) {
#   # subselect participant, horizon and rounds not left out
#   
#   #which rounds to use
#   rounds <- rounds
#   nParams <- 3
#   
#   # Set upper and lower bounds based on nParams
#   lbound <- c(0.00000001,0.00000001, 0) 
#   ubound <- c(1,10, 40)                           
#   
#   
#   #####
#   # Begin cross validation routine
#   # TRAINING SET
#   fit <- DEoptim(
#     soc_utility_model, 
#     lower = lbound, 
#     upper = ubound, 
#     dat = d1,
#     DEoptim.control(itermax = 100)
#   )
#   
#   paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
#   # TEST SET
#   predict <- soc_utility_model(
#     par = paramEstimates, 
#     dat = d1
#   )
#   output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
#   return(output) # return optimized value
# }
# 
# 

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
