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
  int<lower=1> N;
  int<lower=1> T_max;
  int<lower=1> R_max;
  //how many trials and rewards
  int R_subj[N];
  
  //decisions and rewards
  int<lower=-1,upper=64> choices[T_max,R_max,N];
  real rewards[T_max,R_max,N];
  
  //social info
  real<lower=-1,upper=64> social_info[T_max,R_max,N];//index of social info (1:64)
  int<lower=-1,upper=R_max> env_rnd[R_max,N];//environment in which round
  // environments, i think i dont need that here
  // real envs_mean[64,R_max];//12 environments that define mean and varaince of bandit
  //real envs_var[64,R_max];//12 environments that define mean and varaince of bandit
}


transformed data{
  int n_params=2;
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_params] mus;
  vector<lower=0>[n_params] sigmas;
  vector[n_params] sub_lvl[N];
}

transformed parameters{
  //beliefs about environment
  real belief_means[64,T_max+1,R_max,N];
  real belief_vars[64,T_max+1,R_max,N];
  real utils[64,T_max+1,R_max,N];
  
  // model variables
  real pe[T_max,R_max,N];
  real k_gain;
  
  //model parameters
  real<lower=0> beta[N];//ucb
  real<lower=1> var_e[N];//error variance (scales kalman gain)
  
  // noncentered paramtetrization
  for (ppt in 1:N){
    beta[ppt]=Phi_approx(mus[1]+sigmas[1]*sub_lvl[ppt,1])*2;
    var_e[ppt]=1+Phi_approx(mus[2]+sigmas[2]*sub_lvl[ppt,2])*40;
  }
  
  // fill dummy values
  for (ppt in 1:N){
    for (env in 1:12){
      for (t in 1:(T_max+1)){
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
      for (t in 1:T_max){
        //compute UCBs
        for (option in 1:64){
          utils[option,t,env_rnd[r,ppt],ppt] = belief_means[option,t,env_rnd[r,ppt],ppt] + beta[ppt] * belief_vars[option,t,env_rnd[r,ppt],ppt];
        }
        //
        // BELIEF UPDATE BLOCK
        //
        // prediction error
        pe[t,env_rnd[r,ppt],ppt]=rewards[t,r,ppt] - belief_means[choices[t,r,ppt],t,env_rnd[r,ppt],ppt];
        //kalman gain
        k_gain =  belief_vars[choices[t,r,ppt],t,env_rnd[r,ppt],ppt] / ( belief_vars[choices[t,r,ppt],t,env_rnd[r,ppt],ppt] + var_e[N]);
        //new means
        belief_means[choices[t,r,ppt],t+1,env_rnd[r,ppt],ppt]+=k_gain*pe[t,env_rnd[r,ppt],ppt];
        //new variances
        belief_vars[choices[t,r,ppt],t+1,env_rnd[r,ppt],ppt]*=(1-k_gain);
      }// end trial
    }// end round
  }//end ppt
  
  
  }
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mus~normal(0,1);
  sigmas~cauchy(1,1);
  //subject level parameters (can do with colesky decomp later)
  to_vector(sub_lvl[1,:])~normal(0,1);
  to_vector(sub_lvl[2,:])~normal(0,1);
  for (ppt in 1:N){
    for (r in 1:R_subj[ppt]){
      for (t in 1:T_max){
        //print(to_vector(utils[:,t,r,ppt]))
        //print(r)
        //print(t)
        //print(N)
        choices[t,r,ppt] ~ categorical_logit(to_vector(utils[:,t,env_rnd[r,ppt],ppt]));
      }
    }
  }
}// end mod.

