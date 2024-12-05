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
  lbound <- c(0.00001,0,0.00001) 
  ubound <- c(1,100,1)                           
  
  #####
  # TRAINING SET
  fit <- DEoptim(
    s_value_shaping, 
    lower = lbound, 
    upper = ubound, 
    dat = d1,
    envs = environments,
    DEoptim.control(itermax = 200,NP=400,F=0.5,CR = 0.1)#attempt to make this robust. Defaults are NP=40;F=0.8;Cr=0.5
  )
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  
  # TEST SET
  predict <- s_value_shaping(
    par = paramEstimates, 
    dat = d1,
    envs = environments
  )
  
  output <- c(predict, fit$optim$bestmem) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}

