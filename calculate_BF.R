###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###         TEST HYPOTHESES FROM A LIST AND RETURN BAYES FACTOR         ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

## load library
library(brms)

## define the function
test_hypotheses <- function(model_fit, hypotheses_list) {
  
  ## empty list to store Bayes Factor
  bayes_factors <- list()
  
  ## loop through each hypothesis in the list
  for (hyp in hypotheses_list) {
    
    ## test the hypothesis using the hypothesis function from brms
    hyp_result <- brms::hypothesis(model_fit, hyp)
    
    ## extract the Bayes Factor (Evid.Ratio) from the result
    bayes_factor <- round(hyp_result$hypothesis$Evid.Ratio, 2)
    
    ## store the Bayes Factor in the list
    bayes_factors[[hyp]] <- bayes_factor
  }
  
  ## return the list of Bayes Factors
  return(bayes_factors)
}