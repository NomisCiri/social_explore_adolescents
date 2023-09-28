//
// This Stan program defines a mixture model, 
// that is a kalman filter value maximizer in combination with a "trembling hand" error
// over 64 options. Reward expectations are scaled by uncertainty (UCB) and social 
// informaiton
// © Simon Ciranka 2023


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
  cholesky_factor_corr[n_params] l_omega;   // prior correlation of parameters
  matrix[n_params,N] scale; // prior scaling for each parameter
  
  simplex[2] theta[N];
}

transformed parameters{
  
  //model parameters
  vector<lower=0>[N] beta;//ucb
  vector<lower=0>[N] var_e;//error variance (scales kalman gain)
  vector<lower=0>[N] tau;//ucb
  vector<lower=0>[N] psi;
  matrix[N, n_params] params_phi;// for non-centered paramatrezation
  
  vector[2] contributions[T_max,R_max,N];
  
  // model variables
  real var_prior[N];
  vector[64] belief_means;
  vector[64] belief_vars;
  vector[64] utils;
  vector[64] utils_soc;
  
  real pe;
  real k_gain;
  
  simplex[64] ps;
  simplex[64] c_p[T_max,R_max,N];
  
  ps = rep_vector(1.0/64.0,64);
  c_p=rep_array(ps,T_max,R_max,N);
  
  
  // noncentered paramtetrization
  
  // for (ppt in 1:N){
    params_phi = (diag_pre_multiply(sigmas, l_omega) * scale)';
    
    beta=Phi_approx(mus[1]+params_phi[,1])*10;
    var_e=exp(mus[2]+params_phi[,2]);
    tau=exp(mus[3]+params_phi[,3]);
    psi=exp(mus[4]+params_phi[,4]);
    
    //belief update
    for (ppt in 1:N){
      for (r in 1:R_subj[ppt]){
        // restart.
        belief_means=rep_vector(0,64);
        belief_vars=rep_vector(10,64);
        
        for (t in 1:T_max){
          //compute UCBs
          utils = belief_means + beta[ppt] * belief_vars;
          utils_soc=utils;
          utils_soc[social_info[t,r,ppt]]+=psi[ppt];
          c_p[t,r,ppt] = softmax(utils_soc / tau[ppt]);
          
          // BELIEF UPDATE BLOCK
          // prediction error
          pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
          //kalman gain
          k_gain =  belief_vars[choices[t,r,ppt]] / ( belief_vars[choices[t,r,ppt]] + var_e[ppt]);
          //new means
          belief_means[choices[t,r,ppt]]+=k_gain*pe;
          //new variances
          belief_vars[choices[t,r,ppt]]*=(1-k_gain);
          
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
  sigmas~exponential(1);
  //subject level parameters (can do with colesky decomp later)
  to_vector(scale) ~ std_normal();
  // prior correlation of parameters
  l_omega~lkj_corr_cholesky(1);   
  
  for (ppt in 1:N){
    theta[ppt][:] ~ dirichlet(rep_vector(2,2));
    for (r in 1:R_subj[ppt]){
      for (t in 1:T_max){
        target += log_sum_exp(contributions[t,r,ppt][:]);      
      }
    }
  }
}// end mod.

