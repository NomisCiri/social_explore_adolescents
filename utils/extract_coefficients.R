###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###             FROM A MODEL FIT RETURN COEFFICIENTS AND CIS             ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

library(brms)
library(purrr)

## function to apply function to all elements of a list
map_list <- function(lst, func, ...) {
  if (is.list(lst)) {
    ## if lst is a list, apply the function recursively to each element
    map(lst, map_list, func = func, ...)
  } else {
    ## If lst is not a list, apply the function directly
    func(lst, ...)
  }
}

## function to summarize model coefficients with confidence intervals
summarize_model <- function(model, params, names, digits = 2) {
  
  ## get the summary of the model
  summary_model <- summary(model)
  
  ## function to extract fixed effects coefficients and confidence intervals for a given parameter
  extract_coefficients <- function(param) {
    ## ensure parameter exists in the summary
    if (param %in% rownames(summary_model$fixed)) {
      estimate <- summary_model$fixed[param, "Estimate"]
      ci_low <- summary_model$fixed[param, "l-95% CI"]
      ci_high <- summary_model$fixed[param, "u-95% CI"]
      list(estimate = estimate, ci_low = ci_low, ci_high = ci_high)
    } else {
      list(estimate = NA, ci_low = NA, ci_high = NA) # Handle missing parameters
    }
  }
  
  ## extract coefficients for all specified parameters
  coefficients <- lapply(params, extract_coefficients)
  names(coefficients) <- names  # Name the list entries according to the names provided to function
  
  ## return IRR if model is a poisson regression
  if (family(model)$family == "poisson") {
    coefficients <- map_list(coefficients, exp)
  }
  
  ## round all elements in the coefficients list
  coefficients <- map_list(coefficients, round, digits = digits) 
  
  return(coefficients)
}