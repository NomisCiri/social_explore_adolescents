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
  matrix<lower=0>[N,3] lr;//error variance (scales kalman gain)
  vector<lower=0.001>[N] tau;//ucb
  
  matrix[N, n_params] params_phi;// for non-centered paramatrezation
  
  params_phi = (diag_pre_multiply(sigmas, l_omega) * scale)';
  lr[,1]=Phi_approx(mus[1]+params_phi[,1]);
  lr[,2]=Phi_approx(mus[2]+params_phi[,2]);
  lr[,3]=Phi_approx(mus[3]+params_phi[,3]);

  tau=exp(mus[4]+params_phi[,4]);
}

model {
  mus~normal(0,2);
  sigmas~gamma(0.1,1);
  //subject level parameters (can do with colesky decomp later)
  to_vector(scale) ~ std_normal();
  // prior correlation of parameters
  l_omega~lkj_corr_cholesky(1);   
  
  // model variables: prediction error
  real pe;
  
  for (ppt in 1:N){  
    for (r in 1:R_subj[ppt]){
      vector[64] belief_means;
      belief_means=rep_vector(0,64);
      for (t in 1:T_max){
        //compute UCBs
        pe = rewards[t,r,ppt] - belief_means[choices[t,r,ppt]];
        //kalman gain
        belief_means[choices[t,r,ppt]]+=lr[ppt,demo_type[t,r,ppt]]*pe;
        // increment log probabiltiy
        choices[t,r,ppt] ~ categorical_logit(belief_means/tau[ppt]);
      }
    }
  }
}// end mod.

