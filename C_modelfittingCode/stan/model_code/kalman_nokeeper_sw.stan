//
// that is a kalman filter value maximizer in combination with a "trembling hand" error
// over 64 options. Reward expectations are scaled by uncertainty (UCB) and social 
// informaiton
// © Simon Ciranka 2024


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
  int<lower=-1,upper=64> social_info[T_max,R_max,N];
  int<lower=-1,upper=4>  demo_type[T_max,R_max,N];
  
  real rewards[T_max,R_max,N];
}

transformed data{
  int  n_params=4;
}
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_params] mus;
  vector<lower=0>[n_params] sigmas;
  cholesky_factor_corr[n_params] l_omega;   // prior correlation of parameters
  matrix[n_params,N] scale; // prior scaling for each parameter
  // real<lower=0, upper=1> theta[N];
}

transformed parameters{
  
  //model parameters
  vector<lower=0>[N] var_e;//error variance (scales kalman gain)
  vector<lower=0>[N] beta;//ucb
  vector<lower=0>[N] tau;//ucb
  vector<lower=0>[N] sw;//social weight
  
  matrix[N, n_params] params_phi;// for non-centered paramatrezation
  
  params_phi = (diag_pre_multiply(sigmas, l_omega) * scale)';
  
  var_e=Phi_approx(mus[1]+params_phi[,1])*100;
  beta=Phi_approx(mus[2]+params_phi[,2])*2;
  tau=exp(mus[3]+params_phi[,3]);
  
  sw=Phi_approx(mus[4]+params_phi[,4])*50;
}

model {
  // beliefs after si
  real pe;
  real k_gain;
  vector[64] belief_means_sw;
  vector[64] belief_means;
  vector[64] belief_vars;

  mus~normal(0,2);
  sigmas~gamma(2,1);
  //subject level parameters (can do with colesky decomp later)
  to_vector(scale) ~ std_normal();
  // prior correlation of parameters
  l_omega~lkj_corr_cholesky(1);   
  //belief_means_sw=rep_vector(0,64);
  
  for (ppt in 1:N){  
    for (r in 1:R_subj[ppt]){
      //redefine beliefs after each round
      belief_means=rep_vector(0,64);
      belief_vars=rep_vector(10,64);
      for (t in 1:T_max){
        //compute UCBs
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //update
        k_gain =  belief_vars[choices[t,r,ppt]] / ( belief_vars[choices[t,r,ppt]] + var_e[ppt]);
        // updated means and variances
        belief_means[choices[t,r,ppt]]+=k_gain*pe;
        belief_vars[choices[t,r,ppt]]*=(1-k_gain);
        //weighted means for choice.
        belief_means_sw = belief_means+beta[ppt]*belief_vars;
        belief_means_sw[social_info[t,r,ppt]] += sw[ppt];
        // increment log probabiltiy
        choices[t,r,ppt] ~ categorical_logit(belief_means_sw/tau[ppt]);
      }
    }
  }
}// end mod.

generated quantities{
  
  real log_lik[T_max, R_max, N];
  real pe;
  real k_gain;

  vector[64] belief_means_sw;
  vector[64] belief_means;
  vector[64] belief_vars;
  
  // fill log liks
  for (ppt in 1:N){  
    for (r in 1:R_max){
      for (t in 1:T_max){
        log_lik[t,r,ppt] = 0;
      }
    }
  }
  // make logliks
  for (ppt in 1:N){  
    for (r in 1:R_subj[ppt]){
      belief_means=rep_vector(0,64);
      belief_vars=rep_vector(10,64);
      for (t in 1:T_max){
        //compute UCBs
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //update
        k_gain =  belief_vars[choices[t,r,ppt]] / ( belief_vars[choices[t,r,ppt]] + var_e[ppt]);
        // updated means
        belief_means[choices[t,r,ppt]]+=k_gain*pe;
        belief_vars[choices[t,r,ppt]]*=(1-k_gain);
        //weighted means for choice.
        belief_means_sw = belief_means+beta[ppt]*belief_vars;
        belief_means_sw[social_info[t,r,ppt]] += sw[ppt];
        // make loglik
        log_lik[t,r,ppt] = categorical_logit_lpmf(choices[t,r,ppt] | belief_means_sw/tau[ppt]);
      }
    }
  }
}// end mod.