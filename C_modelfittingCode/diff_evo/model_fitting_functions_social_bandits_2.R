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
  lbound <- c(0.000001,0.00000001,0.00000001,0.00000001,0.00000001) 
  ubound <- c(1,2,1,1,1)                           
  
  # #####
  # # TRAINING SET
  # fit <- DEoptim(
  #   s_value_shaping,
  #   lower = lbound,
  #   upper = ubound,
  #   dat = d1,
  #   #envs = environments,
  #   DEoptim.control(itermax = 200)#,NP=400)#attempt to make this robust. Defaults are NP=40;F=0.8;Cr=0.5
  # )
  # browser()
  # TRAINING SET
  fit <- optim(
    par = c(0.5,0.5,0.5,0.5), # Provide an initial guess for the parameters
    fn = s_value_shaping,
    dat = d1,
    method = "BFGS" # Use a suitable method, e.g., "L-BFGS-B" for bounded optimization
    #lower = 0, # Lower bounds for parameters
   # upper = Inf  # Upper bounds for parameters
  )
  
  paramEstimates <- fit$par # MODEL DEPENDENT PARAMETER ESTIMATES
  #paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  
  # TEST SET
  g_sq <- fit$value
  
  output <- c(g_sq, paramEstimates) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}


##---------------------------------------------------------------
##                    basic Q-Learning model                   --
##---------------------------------------------------------------

fit_policy<- function(d1) {
  # Set upper and lower bounds based on nParams
  lbound <- c(0,0.01,0,0.01)#,0.00000001,0.00000001) 
  ubound <- c(1,5,1,5)                           
  
  # #####
  # # TRAINING SET
  fit <- DEoptim(
    s_policy_shaping,
    lower = lbound,
    upper = ubound,
    dat = d1,
    #envs = environments,
    DEoptim.control(itermax = 200,NP=400)#attempt to make this robust. Defaults are NP=40;F=0.8;Cr=0.5
  )
  ## browser()
  # TRAINING SET
  # fit <- optim(
  #   par = c(0.5,0.5,0.5),#,0.5,0.5), # Provide an initial guess for the parameters
  #   fn = s_policy_shaping,
  #   dat = d1,
  #   method = "L-BFGS-B", # Use a suitable method, e.g., "L-BFGS-B" for bounded optimization
  #   lower = 0, # Lower bounds for parameters
  #   upper = 1  # Upper bounds for parameters
  # )
  # 
  paramEstimates <- fit$optim$bestmem # MODEL DEPENDENT PARAMETER ESTIMATES
  g_sq <- fit$optim$bestval # MODEL DEPENDENT PARAMETER ESTIMATES
  #browser()
  # TEST SET
  #g_sq <- fit$value
  
  output <- c(g_sq, paramEstimates) # leaveoutindex, nLL, parameters....
  return(output) # return optimized value
}

