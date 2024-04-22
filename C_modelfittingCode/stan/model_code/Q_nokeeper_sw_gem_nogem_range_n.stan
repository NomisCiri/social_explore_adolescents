//

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
  
  real rewards[T_max,R_max,N];
}

transformed data{
  int  n_params=3*2;
  
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
  matrix<lower=0>[N,2] lr;//error variance (scales kalman gain)
  matrix<lower=0>[N,2] tau;//ucb
  matrix<lower=0>[N,2] sw;//social weight
  
  matrix[N, n_params] params_phi;// for non-centered paramatrezation
  
  params_phi = (diag_pre_multiply(sigmas, l_omega) * scale)';
  lr[,1]=Phi_approx(mus[1]+params_phi[,1])*2;
  lr[,2]=Phi_approx(mus[2]+params_phi[,2])*2;
  
  tau[,1]=Phi_approx(mus[3]+params_phi[,3]);
  tau[,2]=Phi_approx(mus[4]+params_phi[,4]);
  
  
  sw[,1]=Phi_approx(mus[5]+params_phi[,5]);
  sw[,2]=Phi_approx(mus[4]+params_phi[,6]);
  
}

model {
  // beliefs after si
  real pe;
  vector[64] belief_means;
  
  mus~normal(0,2);
  sigmas~cauchy(1,1);
  //subject level parameters (can do with colesky decomp later)
  to_vector(scale) ~ std_normal();
  // prior correlation of parameters
  l_omega~lkj_corr_cholesky(1);   
  //belief_means_sw=rep_vector(0,64);
  
  for (ppt in 1:N){  
    for (r in 1:R_subj[ppt]){
      //first trial (no social info and rescaling not necessary)
      belief_means=rep_vector(0,64);
      pe = rewards[1,r,ppt] - belief_means[choices[1,r,ppt]];
      //update
      belief_means[choices[1,r,ppt]]+=lr[ppt,gem_found[1,r,ppt]]*pe;
      
      choices[1,r,ppt] ~ categorical_logit(belief_means/tau[ppt,gem_found[1,r,ppt]]);// turn into softmax again
      //after probability vector, add probability bonus to epsilon greedy, but epsilon is p to copy.
      for (t in 2:T_max){
        if ((gem_found[t,r,ppt] - gem_found[t-1,r,ppt])!=0){
          //rescale beleif means only once after gem was found
          belief_means*=0.002754821;
        }
        //reward_scaled=rewards[t,r,ppt]/norm_const[gem_found[t,r,ppt]];
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //update
        belief_means[choices[t,r,ppt]] += lr[ppt,gem_found[t,r,ppt]]*pe;
        belief_means[social_info[t,r,ppt]] += sw[ppt,gem_found[t,r,ppt]];
        // increment log probabiltiy
        choices[t,r,ppt] ~ categorical_logit(belief_means/tau[ppt,gem_found[t,r,ppt]]);// turn into softmax again
        
        belief_means[social_info[t,r,ppt]] += -1*sw[ppt,gem_found[t,r,ppt]];//social bonus gone
        //after probability vector, add probability bonus to epsilon greedy, but epsilon is p to copy.
      }
    }
  }
}// end mod.

generated quantities{
  
  real log_lik[T_max, R_max, N];
  real pe;
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
      //first trial (no social info and rescaling not necessary)
      pe = rewards[1,r,ppt] - belief_means[choices[1,r,ppt]];
      belief_means[choices[1,r,ppt]]+=lr[ppt,gem_found[1,r,ppt]]*pe;
      // increment log probabiltiy
      log_lik[1,r,ppt] = categorical_logit_lpmf(choices[1,r,ppt] | belief_means/tau[ppt,gem_found[1,r,ppt]]);
      
      for (t in 2:T_max){
        if ((gem_found[t,r,ppt] - gem_found[t-1,r,ppt])!=0){
          //rescale beleif means only once after gem was found
          belief_means*=0.002754821;
        }
        //reward_scaled=rewards[t,r,ppt]/norm_const[gem_found[t,r,ppt]];
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //update
        belief_means[choices[t,r,ppt]]+=lr[ppt,gem_found[t,r,ppt]]*pe;
        //social info
        belief_means[social_info[t,r,ppt]] += sw[ppt,gem_found[t,r,ppt]];
        // increment log probabiltiy
        log_lik[t,r,ppt] = categorical_logit_lpmf(choices[t,r,ppt] | belief_means/tau[ppt,gem_found[t,r,ppt]]);
        
        belief_means[social_info[t,r,ppt]] += -1*sw[ppt,gem_found[t,r,ppt]];//social bonus gone
      }
    }
  }
}// end mod.