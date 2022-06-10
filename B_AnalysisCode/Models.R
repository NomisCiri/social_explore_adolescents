
########
# BMT Model: maybe change into Q learning
#######
bayesianMeanTracker <- function(x, y, theta, prevPost=NULL,mu0Par,var0Par){ 
  #Updates the previous posterior based on a single observation
  #parameters
  mu0 <- mu0Par #prior mean
  var0 <- var0Par #prior variance
  vare <- theta[1] #error varriance
  if (is.null(prevPost)){#if no posterior prior, assume it is the first observation
    predictions <- data.frame(mu=rep(mu0,64), sig=rep(var0,64))
  }else{#if previous posterior is provided, update
    predictions <- prevPost
  }
  #Which of the 121 options were chosen at time?
  allopts<-expand.grid(1:8, 1:8)
  chosen <- which(allopts$Var1==x[1] & allopts$Var2==x[2])
  #Kalman gain
  kGain <- predictions$sig[chosen] / (predictions$sig[chosen] + 3600)# feed the uncertainty in here.
  #update mean
  predictions$mu[chosen] <- predictions$mu[chosen] + (kGain * (y-predictions$mu[chosen]))
  #update variance for observed arm
  predictions$sig[chosen] <- predictions$sig[chosen] * (1 - kGain)
  #return output
  #  browser()
  
  return(predictions)
}
class(bayesianMeanTracker)<- c(class(bayesianMeanTracker), "KalmanFilter")


########
## UCB sampling....
########
ucb<-function(out, pars, refactor=F){
  if (refactor==TRUE){
    gamma <- pars[1]
    beta_star<-pars[2]
    #calulate all the upper confidence bounds
    outtotal<-(gamma*out$mu)#+(beta_star*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    #avoid borderline cases
    #outtotal[outtotal<=0]<-0.0001
    #outtotal[outtotal>100]<-100
    outtotal<-matrix(outtotal, ncol=1, byrow=TRUE)
  }else{
    beta <- pars[1]
    #calulate all the upper confidence bounds
    outtotal<-out$mu+(beta*sqrt(out$sig)) #refactored parameters in combination with softmax tau, where gamma = 1/tau and beta_star = beta/tau
    #avoid borderline cases
    #outtotal[outtotal<=0]<-0.0001
    #outtotal[outtotal>99]<-99
    outtotal<-matrix(outtotal, ncol=1, byrow=TRUE)
  }
  #return them
  return(outtotal)
}





#########
######### Exploration Environmental change
#########
exploreEnv<-function(explore_func,choiceRule,env2,env1,cntrl,iter){
  #for (rep in 1:ntrialss){
  #unpack
  lambda=cntrl$lambda
  #get beta
  beta<-cntrl$beta# this scales risk attitude.
  #get tau
  tau<-cntrl$tau
  mu0<-cntrl$mu0#exploration bonus
  var0<-cntrl$var0
  #create a parameter vector
  parVec <- cntrl$parVec
  #
  ExploreBonus=cntrl$ExploreBonus
  #kernel is RBF
  #k<-rbf
  #loop through trials
  out=cntrl$out
  AllChoices=cntrl$AllChoices
  dummy=cntrl$dummy
  overallCnt=cntrl$overallCnt
  dat=cntrl$dat
  mu=list()
  sig=list()
  
  for (nround in 1:3){
    #get parameters for participant on that round
    if (nround==1){
      # define vectors that are used by the kalman filter
      lowestx=4
      highestx=9
      sampleVec=as.numeric(rownames(dat[dat$x1>=lowestx & dat$x1<=highestx  & dat$x2<7,]))# here you define where a child should sample from
      ind<-sample(sampleVec,1)
      nTrials=400
    }else {
      ind<-sample(1:64,1)
      nTrials=400
    }
    #random initialization as observation t=0
    #y matrix
    if (nround==1 & overallCnt==1){
      X<-as.matrix(dat[ind,1:2])# generate a new vector of Xs
      y<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))
    }else if(overallCnt==1) {
      print("Youre an adolescent now")
      X<-as.matrix(dat[ind,1:2])# generate a new vector of Xs
      y<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))
    }
    #X-start, i.e. all possible observations
    Xstar<-as.matrix(dat[,1:2])
    
    for (trial in 1:nTrials){
      #output by GP with particular parameter settings
      #don't forget mean centering and standardization.... mean is already 0 :)
      if (overallCnt>1){
        out<-bayesianMeanTracker(X[overallCnt,1:2],y[overallCnt], theta=lambda, prevPost=out,mu0Par=mu0,var0Par = var0)
      }else{
        out<-bayesianMeanTracker(X[overallCnt,1:2],y[overallCnt],theta=lambda, prevPost=NULL,mu0Par=mu0,var0Par=var0)
      }
      #browser()
      #utility vector. transpose if you use greedyMean
      #where is everybody?
      #here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
      #
      # browser()
      utilityVec<-ucb(out,beta)
      utilities <- utilityVec - max(utilityVec)
      #softmaximization
      p <- exp(utilities/tau)
      #probabilities
      p <- p/colSums(p)
      #numerical overflow
      p <- (pmax(p, 0.00001))
      p <- (pmin(p, 0.99999))
      #index is sampled proprotionally to softmaxed utitily vector
      if (nround==1){# subset the probability vector so that it corresponds to the right tiles.
        ind<-sample(sampleVec,1,prob=p[dat$x1>=lowestx & dat$x1<=highestx & dat$x2<7,])# sample from a childhood environemnt
        #this monster just scales exploration boni
      }else {
        ind<-sample(1:64, 1, prob=p)# sample from an adolescent environemnt
        # print(ind)
      }
      X<-rbind(X, as.matrix(dat[ind,1:2]))
      #bind y-observations
      y<-rbind(y, as.matrix(rnorm(n=1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance)))# change this into a sample.
      #if(y[overallCnt]<0){
      #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe. 
      #}
      
      dummy<-data.frame(trial=overallCnt, x=as.numeric(X[overallCnt,1]), y=as.numeric(X[overallCnt,2]),
                        z=as.numeric(y[overallCnt]),round=nround)
      
      AllChoices<-rbind(AllChoices,dummy)
      mu[[overallCnt]]<-out$mu
      sig[[overallCnt]]<-out$sig
      overallCnt=overallCnt+1
    }
    #dummy data frame
  }
  #}
  #This Here is for Plotting
  
  Plot_dat=expand.grid(x=1:8,y=1:8,trials=0:max(dummy$trial))
  Plot_dat$sample=0
  Plot_dat$out=0
  Plot_dat$mu=0
  Plot_dat$sig=40
  
  for (i in 1:length(AllChoices$x)){
    AllChoices$y[i]
    AllChoices$x[i]
    Plot_dat[Plot_dat$x==AllChoices$x[i] & Plot_dat$y==AllChoices$y[i] & Plot_dat$trials==AllChoices$trial[i],]$sample=1
    Plot_dat[Plot_dat$trials==AllChoices$trial[i],]$out=AllChoices$z[i]
    Plot_dat[Plot_dat$trials==AllChoices$trial[i],]$mu=mu[[i]]
    Plot_dat[Plot_dat$trials==AllChoices$trial[i],]$sig=sig[[i]]
  }
  # browser()
  
  Plot_dat$iter=iter
  return(Plot_dat)
}











####
#### Social exploration
####
WhereIsEverybody<-function(HowManyOthers,others,otherLoc,dat_social,X_oth,y_oth,diminishingSocial,trial,cntrl,sampleVec){
  lowestx=4
  highestx=9
  lambda=cntrl$lambda
  #get beta
  beta<-cntrl$beta# this scales risk attitude.
  #get tau
  tau<-cntrl$tau
  mu0<-cntrl$mu0#exploration bonus
  var0<-cntrl$var0
  #create a parameter vector
  parVec <- cntrl$parVec
  #
  ExploreBonus=cntrl$ExploreBonus
  #kernel is RBF
  #k<-rbf
  #loop through trials
  out=cntrl$out
  
  mu0=100
  if (HowManyOthers==1){
    return(list(Others=dat_social$others,LastSamples=X_oth,LastReturn=y_oth,OthersUtil=others))# basecase, terminate
  }
  else{
    ####
    ####Update_socialing process for each individual
    ####
    # browser()
    if(trial==1) {
      ind<-sample(1:64,1)
      print("Youre an adolescent now")
      X_oth[[HowManyOthers]]<-as.matrix(dat_social[ind,1:2])# generate a new vector of Xs
      y_oth[HowManyOthers]<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))
    }
    if (trial>1){
      others[[HowManyOthers]]<-bayesianMeanTracker(X_oth[[HowManyOthers]],y_oth[HowManyOthers], theta=lambda, prevPost=others[[HowManyOthers]],mu0Par=mu0,var0Par = var0)
    }else{
      others[[HowManyOthers]]<-bayesianMeanTracker(X_oth[[HowManyOthers]],y_oth[HowManyOthers],theta=lambda, prevPost=NULL,mu0Par=mu0,var0Par=var0)
    }     
    utilityVec<-ucb(others[[HowManyOthers]],beta)
    #browser()
    #if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    #} else{
    #social impact follows a power law starting in adolescnece. 
    # browser()
    utilityVec=utilityVec+otherLoc^diminishingSocial
    #}
    utilities <- utilityVec - max(utilityVec)
    # utilities=utilities
    #softmaximization
    p <- exp(utilities/tau)
    #probabilities
    p <- p/colSums(p)
    #numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    #index is sampled proprotionally to softmaxed utitily vector
    ind<-sample(1:64, 1, prob=p)# sample from an adolescent environemnt
    # print(ind)
    X_oth[[HowManyOthers]]<-as.matrix(dat_social[ind,1:2])
    #bind y-observations
    y_oth[HowManyOthers]<-as.matrix(rnorm(n=1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))# change this into a sample.
    dat_social[ind,]$others=dat_social[ind,]$others+1
    
    ####
    ####Now, let the others play
    ####
    WhereIsEverybody(HowManyOthers-1,others,otherLoc,dat_social,X_oth,y_oth,diminishingSocial,trial,cntrl,sampleVec)# recursion function returns itslef until no others are left
  }
}



















exploreEnv_Social<-function(explore_func,choiceRule,socialfunc,env2,env1,cntrl,iter){
  #for (rep in 1:ntrialss){
  #unpack
  EnvirionemntAdol=env2
  lambda=cntrl$lambda
  #get beta
  beta<-cntrl$beta# this scales risk attitude.
  #get tau
  tau<-cntrl$tau
  mu0<-cntrl$mu0#exploration bonus
  var0<-cntrl$var0
  #create a parameter vector
  parVec <- cntrl$parVec
  #
  ExploreBonus=cntrl$ExploreBonus
  #kernel is RBF
  #k<-rbf
  #loop through trials
  out=cntrl$out
  AllChoices_social=cntrl$AllChoices_social
  dummy=cntrl$dummy
  #overallCnt=cntrl$overallCnt
  dat_social=cntrl$dat_social
  dat_social$others=0#to get the right indices
  otherLoc=dat_social$others# location of others in last turn i need it to pass it to the social updating function to avoid social info to have a cumulative effect relative to trials.
  
  HowManyOthers=cntrl$HowManyOthers
  diminishingSocial=cntrl$diminishingSocial
  # info about the agents
  others = vector(mode = "list", length = HowManyOthers)# environment for everybody_needed for learning
  X_oth= vector(mode = "list",length = HowManyOthers)#new sample
  y_oth= vector(length = HowManyOthers)#new outcome
  AllOthers=NULL
  
  # for (nround in 1:3){
  
  #get parameters for participant on that round
  ind<-sample(1:64,1)
  nTrials=25
  X<-as.matrix(dat_social[ind,1:2])# generate a new vector of Xs
  y<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))
  #random initialization as observation t=0
  #y matrix
  Xstar<-as.matrix(dat_social[,1:2])
  
  for (trial in 1:nTrials){
    #dat_social$others=0
    #output by GP with particular parameter settings
    #don't forget mean centering and standardization.... mean is already 0 :)
    if (trial>1){
      out<-bayesianMeanTracker(X[trial,1:2],y[trial], theta=lambda, prevPost=out,mu0Par=mu0,var0Par = var0)
    }else{
      out<-bayesianMeanTracker(X[trial,1:2],y[trial],theta=lambda, prevPost=NULL,mu0Par=mu0,var0Par=var0)
    }
    #utility vector. transpose if you use greedyMean
    #where is everybody?
    #here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
    ####
    ####
    Out_Others=WhereIsEverybody(HowManyOthers,others,otherLoc,dat_social,X_oth,y_oth,diminishingSocial,trial,cntrl,sampleVec)# recursion.
    otherLoc=Out_Others$Others# how many others are in one spot
    X_oth=Out_Others$LastSamples
    y_oth=Out_Others$LastReturn
    others=Out_Others$OthersUtil
    #print(otherLoc)
    utilityVec<-ucb(out,beta)
    # if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    #} else{
    #social impact follows a power law starting in adolescnece. 
    browser()
    utilityVec=utilityVec+otherLoc^diminishingSocial
    #}
    #utilityVec=utilityVec+otherLoc^diminishingSocial#add social info
    utilities <- utilityVec - max(utilityVec)
    #softmaximization
    p <- exp(utilities/tau)
    #probabilities
    p <- p/colSums(p)
    #numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # browser()
    #index is sampled proprotionally to softmaxed utitily vectorelse {
    ind<-sample(1:64, 1, prob=p)# sample from an adolescent environemnt
    
    X<-rbind(X, as.matrix(dat_social[ind,1:2]))
    #bind y-observations
    y<-rbind(y, as.matrix(rnorm(n=1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance)))# change this into a sample.
    #if(y[overallCnt]<0){
    #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe. 
    #}
    AllOthers=rbind(AllOthers,data.frame(Loc=otherLoc,trial=trial))
    dummy<-data.frame(trial=trial, x=as.numeric(X[trial,1]), y=as.numeric(X[trial,2]),
                      z=as.numeric(y[trial]))
    
    AllChoices_social<-rbind(AllChoices_social,dummy)
    #trial=trial+1
  }
  #dummy data frame
  #}
  #This Here is for Plotting
  Plot_dat_social=expand.grid(x=1:8,y=1:8,trials=0:max(dummy$trial))
  Plot_dat_social$sample=0
  Plot_dat_social$out=0
  
  
  for (i in 1:length(AllChoices_social$x)){
    AllChoices_social$y[i]
    AllChoices_social$x[i]
    Plot_dat_social[Plot_dat_social$x==AllChoices_social$x[i] & Plot_dat_social$y==AllChoices_social$y[i] & Plot_dat_social$trials==AllChoices_social$trial[i],]$sample=1
    Plot_dat_social[Plot_dat_social$trials==AllChoices_social$trial[i],]$out=AllChoices_social$z[i]
  }
  
  ## here add the tally of "others".
  Plot_dat_social$Others=0
  for(k in unique(Plot_dat_social$trials)){
    if (k>0){
      Plot_dat_social[Plot_dat_social$trials==k,]$Others=AllOthers[AllOthers$trial==k,]$Loc
    }
  }
  Plot_dat_social$iter=iter
  return(Plot_dat_social)
}
















































####
#### Social exploration
####
WhereIsEverybodyProb<-function(HowManyOthers,others,otherLoc,dat_social,X_oth,y_oth,diminishingSocial,trial,cntrl,sampleVec){
  lowestx=4
  highestx=9
  lambda=cntrl$lambda
  #get beta
  beta<-cntrl$beta# this scales risk attitude.
  #get tau
  tau<-cntrl$tau
  mu0<-cntrl$mu0#exploration bonus
  var0<-cntrl$var0
  #create a parameter vector
  parVec <- cntrl$parVec
  #
  ExploreBonus=cntrl$ExploreBonus
  #kernel is RBF
  #k<-rbf
  #loop through trials
  out=cntrl$out
  
  mu0=100
  if (HowManyOthers==1){
    return(list(Others=dat_social$others,LastSamples=X_oth,LastReturn=y_oth,OthersUtil=others))# basecase, terminate
  }
  else{
    ####
    ####Update_socialing process for each individual
    ####
    # browser()
    if(trial==1) {
      ind<-sample(1:64,1)
      print("Youre an adolescent now")
      X_oth[[HowManyOthers]]<-as.matrix(dat_social[ind,1:2])# generate a new vector of Xs
      y_oth[HowManyOthers]<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))
    }
    if (trial>1){
      others[[HowManyOthers]]<-bayesianMeanTracker(X_oth[[HowManyOthers]],y_oth[HowManyOthers], theta=lambda, prevPost=others[[HowManyOthers]],mu0Par=mu0,var0Par = var0)
    }else{
      others[[HowManyOthers]]<-bayesianMeanTracker(X_oth[[HowManyOthers]],y_oth[HowManyOthers],theta=lambda, prevPost=NULL,mu0Par=mu0,var0Par=var0)
    }     
    utilityVec<-ucb(others[[HowManyOthers]],beta)
    #browser()
    #if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    #} else{
    #social impact follows a power law starting in adolescnece. 
     browser()
    #}
    utilities <- utilityVec - max(utilityVec)
    utilities<-utilities+otherLoc*diminishingSocial
    # utilities=utilities
    #softmaximization
    p <- exp(utilities/tau)
    #probabilities
    p <- p/colSums(p)
    #numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    #index is sampled proprotionally to softmaxed utitily vector
    ind<-sample(1:64, 1, prob=p)# sample from an adolescent environemnt
    # print(ind)
    X_oth[[HowManyOthers]]<-as.matrix(dat_social[ind,1:2])
    #bind y-observations
    y_oth[HowManyOthers]<-as.matrix(rnorm(n=1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))# change this into a sample.
    dat_social[ind,]$others=dat_social[ind,]$others+1
    
    ####
    ####Now, let the others play
    ####
    WhereIsEverybodyProb(HowManyOthers-1,others,otherLoc,dat_social,X_oth,y_oth,diminishingSocial,trial,cntrl,sampleVec)# recursion function returns itslef until no others are left
  }
}




exploreEnv_SocialProb<-function(explore_func,choiceRule,socialfunc,env2,env1,cntrl,iter){
  #for (rep in 1:ntrialss){
  #unpack
  EnvirionemntAdol=env2
  lambda=cntrl$lambda
  #get beta
  beta<-cntrl$beta# this scales risk attitude.
  #get tau
  tau<-cntrl$tau
  mu0<-cntrl$mu0#exploration bonus
  var0<-cntrl$var0
  #create a parameter vector
  parVec <- cntrl$parVec
  #
  ExploreBonus=cntrl$ExploreBonus
  #kernel is RBF
  #k<-rbf
  #loop through trials
  out=cntrl$out
  AllChoices_social=cntrl$AllChoices_social
  dummy=cntrl$dummy
  #overallCnt=cntrl$overallCnt
  dat_social=cntrl$dat_social
  dat_social$others=0#to get the right indices
  otherLoc=dat_social$others# location of others in last turn i need it to pass it to the social updating function to avoid social info to have a cumulative effect relative to trials.
  
  HowManyOthers=cntrl$HowManyOthers
  diminishingSocial=cntrl$diminishingSocial
  # info about the agents
  others = vector(mode = "list", length = HowManyOthers)# environment for everybody_needed for learning
  X_oth= vector(mode = "list",length = HowManyOthers)#new sample
  y_oth= vector(length = HowManyOthers)#new outcome
  AllOthers=NULL
  
  # for (nround in 1:3){
  
  #get parameters for participant on that round
  ind<-sample(1:64,1)
  nTrials=25
  X<-as.matrix(dat_social[ind,1:2])# generate a new vector of Xs
  y<-as.matrix(rnorm(1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance))
  #random initialization as observation t=0
  #y matrix
  Xstar<-as.matrix(dat_social[,1:2])
  
  for (trial in 1:nTrials){
    #dat_social$others=0
    #output by GP with particular parameter settings
    #don't forget mean centering and standardization.... mean is already 0 :)
    if (trial>1){
      out<-bayesianMeanTracker(X[trial,1:2],y[trial], theta=lambda, prevPost=out,mu0Par=mu0,var0Par = var0)
    }else{
      out<-bayesianMeanTracker(X[trial,1:2],y[trial],theta=lambda, prevPost=NULL,mu0Par=mu0,var0Par=var0)
    }
    #utility vector. transpose if you use greedyMean
    #where is everybody?
    #here i need a function that calls bayesianMeanTracker. n times and returns the values X for each n. Also, i need some kind of list, where i save the prior for each instance....
    ####
    ####
    Out_Others=WhereIsEverybodyProb(HowManyOthers,others,otherLoc,dat_social,X_oth,y_oth,diminishingSocial,trial,cntrl,sampleVec)# recursion.
    otherLoc=Out_Others$Others# how many others are in one spot
    X_oth=Out_Others$LastSamples
    y_oth=Out_Others$LastReturn
    others=Out_Others$OthersUtil
    #print(otherLoc)
    utilityVec<-ucb(out,beta)
    # if(nround==1){
    # no social impact in "kids environment"
    #  utilityVec=utilityVec#+otherLoc
    #} else{
    #social impact follows a power law starting in adolescnece. 
    #}
    #utilityVec=utilityVec+otherLoc^diminishingSocial#add social info
    utilities <- utilityVec - max(utilityVec)
    utilities=utilities+otherLoc*diminishingSocial
    
    #softmaximization
    p <- exp(utilities/tau)
    #probabilities
    p <- p/colSums(p)
    #numerical overflow
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    # browser()
    #index is sampled proprotionally to softmaxed utitily vectorelse {
    ind<-sample(1:64, 1, prob=p)# sample from an adolescent environemnt
    
    X<-rbind(X, as.matrix(dat_social[ind,1:2]))
    #bind y-observations
    y<-rbind(y, as.matrix(rnorm(n=1,mean = EnvirionemntAdol[ind,]$Mean,sd=EnvirionemntAdol[ind,]$Variance)))# change this into a sample.
    #if(y[overallCnt]<0){
    #  y[overallCnt]=-1*y[overallCnt]^2# make losses more severe. 
    #}
    AllOthers=rbind(AllOthers,data.frame(Loc=otherLoc,trial=trial))
    dummy<-data.frame(trial=trial, x=as.numeric(X[trial,1]), y=as.numeric(X[trial,2]),
                      z=as.numeric(y[trial]))
    
    AllChoices_social<-rbind(AllChoices_social,dummy)
    #trial=trial+1
  }
  #dummy data frame
  #}
  #This Here is for Plotting
  Plot_dat_social=expand.grid(x=1:8,y=1:8,trials=0:max(dummy$trial))
  Plot_dat_social$sample=0
  Plot_dat_social$out=0
  
  
  for (i in 1:length(AllChoices_social$x)){
    AllChoices_social$y[i]
    AllChoices_social$x[i]
    Plot_dat_social[Plot_dat_social$x==AllChoices_social$x[i] & Plot_dat_social$y==AllChoices_social$y[i] & Plot_dat_social$trials==AllChoices_social$trial[i],]$sample=1
    Plot_dat_social[Plot_dat_social$trials==AllChoices_social$trial[i],]$out=AllChoices_social$z[i]
  }
  
  ## here add the tally of "others".
  Plot_dat_social$Others=0
  for(k in unique(Plot_dat_social$trials)){
    if (k>0){
      Plot_dat_social[Plot_dat_social$trials==k,]$Others=AllOthers[AllOthers$trial==k,]$Loc
    }
  }
  Plot_dat_social$iter=iter
  return(Plot_dat_social)
}