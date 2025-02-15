# Model comparison code
# Charley Wu and Anna Giron 2021

rm(list = ls()) # house keeping

# load packages
packages <- c("plyr", "jsonlite", "DEoptim", "matrixcalc", "fields")
# invisible(lapply(packages, install.packages, character.only = TRUE))
invisible(lapply(packages, require, character.only = TRUE)) # loads packages

# in#Source dependencies
# source('../models.R')
# source('../dataProcessing.R')

# if working in Windows
source("C_modelfittingCode/models.R")


##############################################################################################################
# Compile Experimental Data
##############################################################################################################
# only keep people who have completed the task
# data <- dataImport_Adolescent() #sourced from dataProcessing.R
# import preprocessed data
# data = read.csv('data/AdolescentGrid.csv')
data <- read.csv("Data/data_coord.csv")
# only Meder and Schulz data
# data = subset(data, experiment != 'Adolescent')
# Normalize data
# data$z <- (data$z - 25) / 50

nParticipants <- length(unique(data$player))

# nParticipants <- 150
# 151 participants, but id 35 is not available
##############################################################################################################
# Cluster configuration: (1 subject x model combination) per CPU
##############################################################################################################

# IMPORTANT: update batch name
batchName <- "batch1" # saves output csv files in 'modelResults/batchName/*.csv'

# create list of all kernel functions
# kernellist<-list(rbf, bayesianMeanTracker)
kernellist <- list(rbf)

# names of all kernel functions
# kernelnames<-c("RBF", "BMT")
kernelnames <- c("RBF")

# list of all acquisition functions
# acqlist<-list(greedyMean, greedyVar, ucb)
acqlist <- list(epsilonGreedy)

# names of all acquisition functions
# acqnames<-c("GM", "GV", 'UCB')
acqnames <- c("EG")

# all combinations of kernels and acquisition functions will be needed
combs <- expand.grid(1:length(kernellist), 1:length(acqlist))

# create a matrix with combinations of subjectIds and model combinations
subjectComb <- expand.grid(1:nParticipants, 1:(length(kernellist) * length(acqlist))) # 1:? defines the number of unique models to be analyzed

# Cluster id from qsub
clusterid <- as.integer(commandArgs(TRUE)[1]) # Cluster id, corresponds to an integer used to indicate which combination of kernel and acquisition function to simulate
if (is.na(clusterid)) {
  clusterid <- sample(1:nrow(subjectComb), 1)
} # sample a random number if not provided

subjectId <- subjectComb[clusterid, 1] # used to identify unique subjects
uid <- unique(data$id)[subjectId] # convert subjectId to uid

cat(paste0("age in years: ", subset(data, id == uid)$age[1])) # DEBUG: print age
combId <- subjectComb[clusterid, 2] # used to identify a unique model (i.e., combination of GP kernel and acquisition function)

# trim combs if cluster==TRUE
model <- combs[combId, ]


set.seed(clusterid) # set seed as the clusterid

##############################################################################################################
# Model Fitting
##############################################################################################################
# initialize Xtest
Xnew <- as.matrix(expand.grid(0:7, 0:7)) # do this outside the loop for better speed

# Negative Log Likelihood of model prediction
# parameters are distinct for each individual model
# subjD = subset of data for a specific subject
# kernel = any of fastKalmanFilter, standard GP (rbf, matern, or oru), or lingp
# acquisition = any of ucb, probofimp, expofimp, or pmu
# horizonLength
# rounds to be considered
modelFit <- function(par, subjD, acquisition, k, horizonLength, rounds) {
  # Extract and process parameters
  if (inherits(acquisition, "epsilonGreedy")) {
    epsilon <- 1 / (1 + exp(-(par[length(par)]))) # transform back from unbounded space; epsilon is the last parameter for epsilon greedy
  }
  par <- exp(par) # exponentiate parameters to make a non-negative and convex optimization surface
  # last parameter for all other models is always inverse temperature for softmax
  tau <- par[length(par)]
  # Which posterior function to use; therefore, which parameters to use
  if (inherits(k, "KalmanFilter")) { # null kernel indicates kalman filter model
    kNoise <- par[1]
    parVec <- c(kNoise) # Vector of parameters to send to the KF posterior function
  } else if (inherits(k, "GP")) { # lambda
    lambda <- par[1]
    parVec <- c(lambda, lambda, 1, .0001) # Vector of parameters to send to the GP posterior vector, where sF and sN are fixed
  }
  # Additional acquisition function dependent parameters
  if (inherits(acquisition, "UCB") | inherits(acquisition, "exploreCounts") | inherits(acquisition, "epsilonGreedy")) { # check if UCB is used
    beta <- par[length(par) - 1] # If UCB, beta is always 2nd last
    # refactor beta and tau into gamma and beta_star, where gamma = 1/tau and beta_star = beta/tau
  }
  # which rounds to consider?
  trainingSet <- subset(subjD, round %in% rounds)
  # Vector to store negative log likelihods
  nLL <- rep(0, length(rounds))
  for (r in unique(trainingSet$round)) { # Begin looping through each round
    # subset of data for round r
    roundD <- subset(subjD, round == r)
    horizon <- nrow(roundD)
    # Observations of subject choice behavior
    chosen <- roundD$chosen
    chosen <- chosen[2:length(chosen)] # trim first observation, since it wasn't a choice but a randomly revealed tile
    y <- roundD$z[0:(horizon - 1)] # trim off the last observation, because it was not used to inform a choice (round already over)
    x1 <- roundD$x[0:(horizon - 1)]
    x2 <- roundD$y[0:(horizon - 1)]
    # create observation matrix
    X <- as.matrix(cbind(x1, x2))
    # make sure X is a matrix
    X <- as.matrix(X)
    Xnew <- as.matrix(Xnew)
    # Utilties of each choice
    utilities <- NULL
    prevPost <- NULL # set the previous posterior computation to NULL for the kalman filter
    pMat <- NULL
    # loop through observations
    for (i in 1:(horizon - 1)) { # skip the last observation, because no choice was made based on that information
      # new observation
      X1 <- matrix(X[1:i, ], ncol = 2)
      y1 <- y[1:i]
      # Which posterior function to use
      if (inherits(k, "KalmanFilter")) { # kalman filter model
        out <- bayesianMeanTracker(x = X1[i, ], y = y[i], prevPost = prevPost, theta = parVec)
        # update prevPost for the next round
        prevPost <- out
      } else if (inherits(k, "GP")) { # GP with length-scale parameterized kernel
        out <- gpr(X.test = Xnew, theta = parVec, X = X1, Y = y1, k = k) # Mu and Sigma predictions for each of the arms; either GP or Kalman filter
      } else if (inherits(k, "Null")) { # null model
        out <- nullModel() # Mu and Sigma predictions for each of the arms; either GP or Kalman filter
      }
      # Slightly different function calls for each acquisition function
      if (inherits(acquisition, "UCB")) { # UCB takes a beta parameter
        utilityVec <- acquisition(out, c(beta))
      } else if (inherits(acquisition, "exploreCounts")) { # count-based exploration
        utilityVec <- exploreCounts(out, roundD$chosen[1:i], c(beta))
      } else if (inherits(acquisition, "epsilonGreedy")) {
        p <- epsilonGreedy(out, beta, epsilon)
        pMat <- rbind(pMat, t(p))
      } else { # any other
        utilityVec <- acquisition(out)
      }
      if (inherits(acquisition, "softmax")) {
        utilityVec <- utilityVec - max(utilityVec) # avoid overflow
        utilities <- rbind(utilities, t(utilityVec)) # build horizon_length x options matrix, where each row holds the utilities of each choice at each decision time in the search horizon
      }
    }
    # print(utilities)
    if (inherits(acquisition, "softmax")) {
      # Softmax rule
      p <- exp(utilities / tau)
      p <- p / rowSums(p)
      # avoid underflow by setting a floor and a ceiling
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      pMat <- p
    }
    # Calculate Negative log likelihood
    nLL[which(unique(trainingSet$round) == r)] <- -sum(log(pMat[cbind(c(1:(horizon - 1)), chosen)]))
  }
  # end loop through rounds
  return(sum(nLL)) # Return negative log likelihoods of all observations
}

