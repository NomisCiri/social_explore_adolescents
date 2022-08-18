#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(matrixcalc)
library(tidyverse)
library(cowplot)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Kalman Filter Exploration Parameters"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("exploration",
                        "Exploration Bonus:",
                        min = 0,
                        max = 100,
                        value = 0,
                        round=FALSE),
            sliderInput("priorMu",
                        "Prior Mus:",
                        min = -100,
                        max = 100,
                        value = 50,
                        round=FALSE),
            sliderInput("priorVar",
                        "Prior Variance:",
                        min = 0.01,
                        max = 5,
                        value = 5,
                        round=FALSE),
            sliderInput("beta",
                        "Confidence Bounds:",
                        min = -40,
                        max = 40,
                        value = 0),
            sliderInput("iterations",
                        "Rounds:",
                        min = 10,
                        max = 2000,
                        value = 750),
            sliderInput("temperature",
                        "Temperature (Random Exploration):",
                        min = 0.1,
                        max = 2,
                        value = 0.5,
                        round=FALSE)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("density")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #Models and sampling strategies
    #Eric Schulz, March 2018
    
    ##############################################################################################################
    #KERNELS
    ##############################################################################################################
    
    #Radial Basis Kernel
    rbf <- function(X1,X2,theta){
        #transfer to matrices
        X1 <- as.matrix(X1)
        X2 <- as.matrix(X2)
        #check dimensions
        if(ncol(X1) != ncol(X2)){
            stop("X1 and X2 must contain input values of the same dimension.")
        } else if(!all(theta>=0)){
            stop("All parameters must be >= 0.")
        }
        #get dimensions
        N1 <- nrow(X1)
        N2 <- nrow(X2)
        d <- ncol(X1)
        #initialize sigma
        sigma <-  matrix(rep(0, N1*N2),nrow=N1)
        #observational variance
        sf <- theta[d+1]
        #noise variance
        sn <- theta[d+2]
        #loop through
        for(i in 1:d){
            #length scale
            l <- theta[i] #Note: assumes a unique length scale for each dimension
            #x-diff
            xdiff <- (outer(X1[,i],X2[,i],function(x,y) x - y)/l)^2
            sigma <- sigma + xdiff
        }
        #RBF function
        if(identical(X1,X2)){
            id <- diag(rep(1,N1))
            sigma.final <- sf*exp(-0.5*sigma) + sn*id
        } else {
            sigma.final <- sf*exp(-0.5*sigma)
        }
        #return final covariance matrix
        return(sigma.final)
    }
    class(rbf)<- c(class(rbf), "GP") #identify the rbf kernel as a gp model
    
    
    ##############################################################################################################
    #MATRIX INVERSION
    ##############################################################################################################
    
    #calculate inverse of the cov-function using sigular value decomposition
    cov.inverse.svd <- function(X, tol = sqrt(.Machine$double.eps)){
        # Generalized Inverse of a Matrix
        dnx <- dimnames(X)
        if(is.null(dnx)) dnx <- vector("list", 2)
        #singular value decomposition
        s <- svd(X)
        nz <- s$d > tol * s$d[1]
        #inverse
        K.inv <- structure(
            if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
            dimnames = dnx[2:1])
        #logarithm of determinant.
        log.K.det <- sum(log(s$d))
        #return inverse plus log determinant
        return(list(Inv = K.inv,lDet = log.K.det))
    }
    
    #calculate inverse of the cov-function using Cholesky
    cov.inverse.chol <- function(X){
        #cholseky decomposition
        R <- chol(X)
        #complex conjugate
        Rt <- Conj(t(R))
        #invert
        R.inv <- solve(R)
        #invert
        Rt.inv <- solve(Rt)
        #multiply matrices
        X.inv <- R.inv %*% Rt.inv
        #log determinant
        log.X.det <- 2*sum(log(diag(R))) 
        #return both
        return(list(Inv = X.inv, lDet = log.X.det))
    }
    
    ##############################################################################################################
    #GAUSSIAN PROCESS
    ##############################################################################################################
    
    #Gaussian Process function
    #X.test: matrix for predcitions
    #theta: vector of hyper-parameters (lambda, Sf, Sn)
    #X; matrix of observations
    #y: vector of observed outcomes
    #kernel: used kernel function, can be "rbf", "oru", or "mat"
    gpr <- function(X.test, theta, X,Y, k){
        #make it a matrix
        Xstar <- as.matrix(X.test)
        #dimensions
        d <- ncol(as.matrix(X))
        #calculate capital K
        K <- k(X,X,theta) 
        #Check if matrix is positive semi-definite
        if (is.positive.definite(K)){
            KK <- cov.inverse.chol(K) #use Cholesky
        } else {
            KK <- cov.inverse.svd(K) #use SVD
        }
        #times y
        Ky <- KK$Inv %*% Y
        #apply the kernel
        result <- apply(Xstar, 1, function(x){
            XX <- matrix(x,nrow=1) 
            Kstar <- k(X, XX, theta)
            Kstarstar <- k(XX,XX,theta)
            #get mean vector
            mu <- t(Kstar) %*% Ky
            #get covariance
            cv <- Kstarstar - (t(Kstar) %*% KK$Inv %*% Kstar) #BUG: sometimes cv<0, leading to NaN when we return sqrt(cv)
            #DEBUG
            if (cv<0){ cv <- abs(cv)} #TEMPORARY SOLUTION: MANUALLY SET CV TO BE POSITIVE IF NEGATIVE
            #return means and variance
            return(c(mu, cv))
        })
        #as a data frame with names mu and sig
        prediction <- as.data.frame(t(result))
        prediction[is.na(prediction)] <- 0.01 #remove NaN items with the noise variance 
        colnames(prediction) <- c("mu", "sig")
        return(prediction)
    }
    
    
    
    ##############################################################################################################
    ##Mean Tracker
    ##############################################################################################################
    bayesianMeanTracker <- function(x, y, theta=c(80), prevPost=NULL,mu0Par,var0Par){ 
        #Updates the previous posterior based on a single observation
        #parameters
        mu0 <- mu0Par #prior mean
        var0 <- var0Par #prior variance
        vare <- theta[1] #error varriance
        if (is.null(prevPost)){#if no posterior prior, assume it is the first observation
            predictions <- data.frame(mu=rep(mu0,100), sig=rep(var0,100))
        }else{#if previous posterior is provided, update
            predictions <- prevPost
        }
        #Which of the 121 options were chosen at time?
        allopts<-expand.grid(1:10, 1:10)
        chosen <- which(allopts$Var1==x[1] & allopts$Var2==x[2])
        #Kalman gain
        kGain <- predictions$sig[chosen] / (predictions$sig[chosen] + vare^2)
        #update mean
        predictions$mu[chosen] <- predictions$mu[chosen] + (kGain * (y-predictions$mu[chosen]))
        #update variance for observed arm
        predictions$sig[chosen] <- predictions$sig[chosen] * (1 - kGain)
        #return output
        return(predictions)
    }
    class(bayesianMeanTracker)<- c(class(bayesianMeanTracker), "KalmanFilter")
    
    
    ##############################################################################################################
    #ACQUISITION FUNCTIONS
    ##############################################################################################################
    
    #Upper Confidence Bound Sampling
    ucb<-function(out, pars, refactor=F){
        if (refactor==TRUE){
            gamma <- pars[1]
            beta_star<-pars[2]
            #calulate all the upper confidence bounds
            outtotal<-(gamma*out$mu)#+(beta_star*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
            #avoid borderline cases
            outtotal[outtotal<=0]<-0.0001
            outtotal[outtotal>100]<-100
            outtotal<-matrix(outtotal, ncol=1, byrow=TRUE)
        }else{
            beta <- pars[1]
            #calulate all the upper confidence bounds
            outtotal<-out$mu + (beta*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
            #avoid borderline cases
            outtotal[outtotal<=0]<-0.0001
            outtotal[outtotal>99]<-99
            outtotal<-matrix(outtotal, ncol=1, byrow=TRUE)
        }
        #return them
        return(outtotal)
    }
    #add "UCB" to the class of the ucb function, so that modelFit can recognize that it has a longer parameter array
    class(ucb)<- c(class(ucb), "UCB")
    
    
    #Greedy Mean
    greedyMean <- function(out){
        outtotal<-out$mu #the value of each choice is solely based on the expectation of reward
        #avoid borderline cases
        outtotal[outtotal<0]<-0.0001
        outtotal[outtotal>50]<-50
        outtotal<-matrix(outtotal, nrow=1, byrow=TRUE)
        return(outtotal)
    }
    class(greedyMean)<- c(class(greedyMean), "greedyMean")
    
    #Greedy Variance
    greedyVar <- function(out){
        outtotal<- sqrt(out$sig) #the value of each choice is solely based on the expected uncertainty
        #avoid borderline cases
        outtotal[outtotal<0]<-0.0001
        outtotal[outtotal>50]<-50
        outtotal<-matrix(outtotal, nrow=nrow(out)/64, byrow=TRUE)
        return(outtotal)
    }
    class(greedyVar)<- c(class(greedyVar), "greedyVar")
    
    
    
    output$density<-renderPlot({
        set.seed(sample(seq(100,1000,length.out=100)))
        #define the range of adolescent environment
        envirionmentMeanAdol=seq(-100,100,length.out=10)
        envirionmentSigmaAdol=seq(1,40,length.out=10)
        EnvirionemntAdol=expand.grid(Mean=envirionmentMeanAdol,Sigma=envirionmentSigmaAdol)
        lambda<-input$lambda
        #get beta
        beta<-input$beta
        #get priorMu
        priorMu<-input$priorMu
        #get prior Variance
        priorVar<-input$priorVar
        #get tau
        tau<-input$temperature
        #Bonus
        Bonus<-input$exploration
        #Number if iterations
        Iteration<-input$iterations
        
        lambda<-input$lambda
        #create a parameter vector
        parVec <- c(lambda, lambda, 1, .0001) 
        # get sd of whole environemt
        #kernel is RBF
        k<-rbf
        #loop through trials
        out=NULL
        SD<-EnvirionemntAdol%>%dplyr::rowwise()%>%mutate(value=rnorm(1,mean = Mean,sd=Sigma))%>%
            ungroup()%>%dplyr::summarise(sd=sd(value))
        for (nround in 2:2){
            #get parameters for participant on that round
            if (nround==1){
                dat=expand.grid(x1=5:10,x2=1:5,id=1:160,round=1)
                ind<-sample(1:30,1)
            }else {
                dat=expand.grid(x1=1:10,x2=1:10,id=1:160,round=2:3)
                ind<-sample(1:100,1)
            }
            
            dp<-subset(dat, round==nround & id==1)
            dp$ExploreBonus=Bonus
            
            #random initialization as observation t=0
            #X matrix
            dp[ind,5]=dp[ind,5]-1#
            X<-as.matrix(dp[ind,1:2])
            #y matrix
            if (nround==1){
                y<-as.matrix(rnorm(1,mean = EnvirionemntKids[ind,]$Mean,sd=EnvirionemntKids[ind,]$Sigma))
            }else {
                y<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Sigma))
            }
            #X-start, i.e. all possible observations
            Xstart<-as.matrix(dp[,1:2])

            #loop through trials
            out=NULL
            
            for (trials in 1:Iteration)#what do you get done
            {
                #output by GP with particular parameter settings
                #don't forget mean centering and standardization.... mean is already 0 :)
                #out<-gpr(X.test=Xstar, theta=parVec, X=X, Y=(y)/SD$sd[1], k=k)
                 if(!is.null(out)){
                      out<-bayesianMeanTracker(x=X[trials,1:2], y=(y[trials]),prevPost = out,mu0Par=priorMu,var0Par=priorVar)
                  }else {
                      out<-bayesianMeanTracker(x=X[trials,1:2], y=(y[trials]),prevPost=NULL,mu0Par=priorMu,var0Par=priorVar)
                  }
                #utility vector. transpose if you use greedyMean
                utilityVec<-ucb(out,beta)
                
                utilityVecExplore=utilityVec+(dp[,5])# add expolation bonus on the utilities... also standardize it.
                #avoid overflow
                utilities <- utilityVecExplore - max(utilityVecExplore)
                #softmaximization
                p <- exp(utilities/tau)
                #probabilities
                p <- p/colSums(p)
                #numerical overflow
                p <- (pmax(p, 0.00001))
                p <- (pmin(p, 0.99999))
                #index is sampled proprotionally to softmaxed utitily vector
                if (nround==1){
                    dat=expand.grid(x1=5:10,x2=1:5,id=1:160,round=1)
                    ind<-sample(1:30,1,prob=p)
                }else {
                    ind<-sample(1:100, 1, prob=p)
                    #print(ind)
                }
                if(dp[ind,5]>0){# if the exploration bonus is greater than 0; subtract
                    dp[ind,5]=dp[ind,5]-1#if you samole it once, subtract one bonus unit
                }
                #bind X-observations
                X<-rbind(X, as.matrix(dp[ind,1:2]))
                #bind y-observations
                y<-rbind(y, as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Sigma)))# change this into a sample.

            }
                dummy<-data.frame(trial=0:Iteration, x=as.numeric(X[,1]), y=as.numeric(X[,2]),
                                  z=as.numeric(y),round=nround)
                #dummy data fram
        }# end trials
        
        #save csv of learning curves
        dummydat=expand.grid(x=1:10,y=1:10)
        dummydat$density=0
        
        dummy%>%dplyr::group_by(x,y)%>%
            summarize(density=n())->Agentsamples
        
        for (i in 1:length(Agentsamples$x)){
            Agentsamples$y[i]
            Agentsamples$x[i]
            dummydat[dummydat$x==Agentsamples$x[i] & dummydat$y==Agentsamples$y[i],]$density=Agentsamples$density[i]
        }
        dummydat%>%ggplot(aes(x=x,y=y,fill=density))+
            geom_tile()+
            scale_fill_viridis_c()+
            ylab("SD")+
            xlab("Mean")+
            ggtitle(paste0("Sampling Density ",trials))+theme_cowplot()
    })#endrounds
}


# Run the application 
shinyApp(ui = ui, server = server)
