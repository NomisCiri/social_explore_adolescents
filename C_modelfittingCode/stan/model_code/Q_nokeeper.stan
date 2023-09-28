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
  int<lower=-1,upper=64> social_info[T_max,R_max,N];//index of social info (1:64)
  int<lower=-1,upper=R_max> env_rnd[R_max,N];//environment in which round
  // environments, i think i dont need that here
  // real envs_mean[64,R_max];//12 environments that define mean and varaince of bandit
  //real envs_var[64,R_max];//12 environments that define mean and varaince of bandit
}


transformed data{
  int n_params=4;
  
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_params] mus;
  vector<lower=0>[n_params] sigmas;
  vector[n_params] sub_lvl[N];
  simplex[2] theta[N];
}

transformed parameters{
  
  //model parameters
  vector<lower=0>[N] lr;//error variance (scales kalman gain)
  vector<lower=0>[N] tau;//ucb
  vector<lower=0>[N] psi;
  
  vector[2] contributions[T_max,R_max,N];
  // model variables
  vector[64] belief_means;
  vector[64] utils_soc;
  
  real pe;

  simplex[64] ps;
  simplex[64] c_p[T_max,R_max,N];
  ps = rep_vector(1.0/64.0,64);
  c_p=rep_array(ps,T_max,R_max,N);
  
  
  // noncentered paramtetrization
 // for (ppt in 1:N){
    lr=exp(mus[1]+sigmas[1]*to_vector(sub_lvl[:,1]));
    tau=exp(mus[2]+sigmas[2]*to_vector(sub_lvl[:,2]));
    psi=exp(mus[3]+sigmas[3]*to_vector(sub_lvl[:,3]));
  
  //belief update
  for (ppt in 1:N){
    for (r in 1:R_subj[ppt]){
      // restart.
      belief_means=rep_vector(0,64);
      for (t in 1:T_max){
        //compute UCBs
        utils_soc=belief_means;
        utils_soc[social_info[t,r,ppt]]+=psi[ppt];
        c_p[t,r,ppt] = softmax(utils_soc / tau[ppt]);

        // BELIEF UPDATE BLOCK
        // prediction error
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //kalman gain
        belief_means[choices[t,r,ppt]]+=lr[ppt]*pe;
        //new variances

        contributions[t,r,ppt][1] = log(theta[ppt,1]) + categorical_lpmf(choices[t,r,ppt] | c_p[t,r,ppt]);
        contributions[t,r,ppt][2] = log(theta[ppt,2]) + categorical_lpmf(choices[t,r,ppt]  | ps);
      }// end trial
    }// end round
  }//end ppt
  
  
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mus~normal(0,1);
  sigmas~exponential(2);
  //subject level parameters (can do with colesky decomp later)
  to_vector(sub_lvl[:,1])~normal(0,1);
  to_vector(sub_lvl[:,2])~normal(0,1);
  to_vector(sub_lvl[:,3])~normal(0,1);

  for (ppt in 1:N){
    theta[ppt][:] ~ dirichlet(rep_vector(2,2));
    for (r in 1:R_subj[ppt]){
      for (t in 1:T_max){
        target += log_sum_exp(contributions[t,r,ppt][:]);      
      }
    }
  }
}// end mod.

