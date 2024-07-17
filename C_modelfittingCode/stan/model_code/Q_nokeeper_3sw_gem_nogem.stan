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
  int<lower=-1,upper=64> social_info[T_max,R_max,N];
  int<lower=-1,upper=4>  demo_type[T_max,R_max,N];
  int<lower=-1,upper=64> gem_found[T_max,R_max,N];
  int<lower=-1,upper=64> gem_found_demo_type[T_max,R_max,N];
  real rewards[T_max,R_max,N];
}

transformed data{
  int  n_params=4;
}
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[n_params] mus;
  vector<lower=0>[n_params] sigmas;
  //cholesky_factor_corr[n_params] l_omega;   // prior correlation of parameters
  matrix[n_params+2,N] scale; // prior scaling for each parameter, add two additional ones for hyperprior of sw
  // real<lower=0, upper=1> theta[N];
}

transformed parameters{
  
  //model parameters
  vector<lower=0>[N] lr;//error variance (scales kalman gain)
  matrix<lower=0>[N,2] tau;//ucb
  
  matrix<lower=0>[N,3] sw;//social weight
 // matrix[N, n_params] params_phi;// for non-centered paramatrezation
  
  //params_phi = (diag_pre_multiply(sigmas, l_omega) * scale)';
  lr=Phi_approx(mus[1]+sigmas[1] * scale[1,]');
  
  // different taus for gem_nogem
  tau[,1]=exp(mus[2]+sigmas[2] * scale[2,]');
  tau[,2]=exp(mus[3]+sigmas[3] * scale[3,]');
  
  //fetch social weights from the same hyperprior
  sw[,1]=Phi_approx(mus[4]+sigmas[4] * scale[4,]')*30;
  sw[,2]=Phi_approx(mus[4]+sigmas[4] * scale[5,]')*30;
  sw[,3]=Phi_approx(mus[4]+sigmas[4] * scale[6,]')*30;

}

model {
  // beliefs after si
  real pe;
  vector[64] belief_means_sw;
  vector[64] belief_means;
  
  mus~normal(0,2);
  sigmas~gamma(2,1);
  //subject level parameters (can do with colesky decomp later)
  to_vector(scale) ~ std_normal();
  // prior correlation of parameters
  //l_omega~lkj_corr_cholesky(1);   
  //belief_means_sw=rep_vector(0,64);
  
  for (ppt in 1:N){  
    for (r in 1:R_subj[ppt]){
      belief_means=rep_vector(0,64);
      for (t in 1:T_max){
        //compute UCBs
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //update
        belief_means[choices[t,r,ppt]]+=lr[ppt]*pe;
        
        belief_means_sw = belief_means;
        belief_means_sw[social_info[t,r,ppt]] += sw[ppt,demo_type[t,r,ppt]];
        // increment log probabiltiy
        choices[t,r,ppt] ~ categorical_logit(belief_means_sw/tau[ppt,gem_found[t,r,ppt]]);
      }
    }
  }
}// end mod.

generated quantities{
  
  real log_lik[T_max, R_max, N];
  real pe;
  vector[64] belief_means_sw;
  vector[64] belief_means;
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
      
      for (t in 1:T_max){
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //update
        belief_means[choices[t,r,ppt]]+=lr[ppt]*pe;
        
        belief_means_sw = belief_means;
        belief_means_sw[social_info[t,r,ppt]] += sw[ppt,demo_type[t,r,ppt]];
        // increment log probabiltiy
        log_lik[t,r,ppt] = categorical_logit_lpmf(choices[t,r,ppt] | belief_means_sw/tau[ppt,gem_found[t,r,ppt]]);
      }
    }
  }
}// end mod.