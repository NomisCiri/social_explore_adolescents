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

fit_slr<- function(d1, environments) {
  # Set upper and lower bounds based on nParams
  lbound <- c(0.00001,0,0,-5) 
  ubound <- c(1,250,0.1,5)                           
  
  #####
  # TRAINING SET
  fit <- DEoptim(
    s_learn, 
    lower = lbound, 
    upper = ubound, 
    dat = d1,
    envs = environments,
    #verbose=F
    DEoptim.control(itermax = 200,NP=400)
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  
  # TEST SET
  predict <- s_learn(
    par = paramEstimates, 
    dat = d1,
    envs = environments
  )
  
  output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}

