//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  //number of participants
  int<lower=0> N;
  //how many trials and rewards
  vector[N] T_subj;
  int R_subj[N];
  
  //decisions and rewards
  int<lower=1,upper=64> choice[25,12,N];
  real rewards[25,12,N];
  
  //social info
  real<lower=1,upper=64> SI[25,12,N];//index of social info (1:64)
  int<lower=1,upper=12> env_rnd[N,12];//environment in which round
  // environments, defined by their mean and variance.
  real envs_mean[64,12];//12 environments that define mean and varaince of bandit
  real envs_var[64,12];//12 environments that define mean and varaince of bandit
}


transformed data{
  int n_params=2;
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_params] mus;
  vector<lower=0>[n_params] sigmas;
}

transformed parameters{
  //beliefs about environment
  real belief_means[64,25,12,N];
  real belief_vars[64,25,12,N];
  vector[64] utils[25,12,N];
  real pe[25,12,N];
  real k_gain;
  //model parameters
  real beta[N];//ucb
  real var_e[N];//error variance (scales kalman gain)
  
  // fill dummy values
  for (ppt in 1:N){
    for (env in 1:12){
      for (t in 1:25){
        for (option in 1:64){
          belief_means[option,t,env,ppt]=0;
          belief_vars[option,t,env,ppt]=10;
        }// end option
      }// end trial
    }// end env
  }//end ppt
  
  //belief update
  for (ppt in 1:N){
    for (r in 1:R_subj[ppt]){
      for (t in 1:25){
        //compute UCBs
        for (option in 1:64){
          utils[option][t][env_rnd[ppt,r]][ppt] = belief_means[option,t,env_rnd[ppt,r],ppt] + beta[ppt] * belief_vars[option,t,env_rnd[ppt,r],ppt];
        }
        //belief update: prediction error
        pe[t,r,ppt]=rewards[t,r,ppt]-belief_means[choice[t,r,ppt],t,env_rnd[ppt,r],ppt];
        //belief update: kalman gain
        k_gain =  belief_vars[choice[t,r,ppt],t+1,env_rnd[ppt,r],ppt] / ( belief_vars[choice[t,r,ppt],t+1,env_rnd[ppt,r],ppt] + var_e[N]);
        //new means
        belief_means[choice[t,r,ppt],t+1,env_rnd[ppt,r],ppt]+=k_gain*pe[t,r,ppt];
        //new variances
        belief_vars[choice[t,r,ppt],t+1,env_rnd[ppt,r],ppt]*=(1-k_gain);
      }// end trial
    }// end round
  }//end ppt
  
  
  
  //update
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (ppt in 1:N){
    for (r in 1:R_subj[ppt]){
      for (t in 1:25){
        choice[t,r,ppt] ~ categorical_logit(utils[:][r,t,ppt]);
      }
    }
  }
  
}// end mod.

