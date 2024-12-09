data{
  int N_national_polls;    // Number of NATL polls
  int N_state_polls;    // Number of STATE polls
  int T;    // Number of distinct days
  int S;    // Number of distinct states (for which at least 1 poll available) + 1 [for natl]
  int P;    // Number of distinct pollsters
  int M;    // Number of distinct poll modes
  int Pop;    // Number of distinct poll populations
  array[N_state_polls] int<lower = 1, upper = S + 1> state; // State index [+1 is for natl polls, i.e. 52]
  array[N_state_polls] int<lower = 1, upper = T> day_state;
  array[N_national_polls] int<lower = 1, upper = T> day_national;   // Day index x STATE poll
  array[N_state_polls] int<lower = 1, upper = P> poll_state;   // Day index x NATL poll
  array[N_national_polls] int<lower = 1, upper = P> poll_national;  // Pollster index x STATE poll  // Pollster index x NATL poll
  array[N_state_polls] int<lower = 1, upper = M> poll_mode_state;    // Poll mode index x STATE poll
  array[N_national_polls] int<lower = 1, upper = M> poll_mode_national;    // Poll mode index x NATL poll
  array[N_state_polls] int<lower = 1, upper = Pop> poll_pop_state;    // Poll pop index x STATE poll
  array[N_national_polls] int<lower = 1, upper = Pop> poll_pop_national;    // Poll pop index x NATL pol	
   // count polled Harris supporters x NATL poll
  array[N_national_polls] int n_democrat_national;
  array[N_national_polls] int n_two_share_national;// count of polled declaring for two major parties x NATL poll 	
  array[N_state_polls] int n_democrat_state; // count polled Harris supporters x STATE poll
  array[N_state_polls] int n_two_share_state; // count of polled declaring for two major parties x STATE poll


  vector<lower = 0, upper = 1.0>[N_national_polls] unadjusted_national; // unadjusted indicator x NATL poll
  vector<lower = 0, upper = 1.0>[N_state_polls] unadjusted_state; // unadjusted indicator x STATE poll
  cov_matrix[S] ss_cov_mu_b_walk; // S x S mat of prior var-cov mat daily random walk evolution
  cov_matrix[S] ss_cov_mu_b_T; // S x S mat of prior var-cov mat latent state Harris vote intention
  cov_matrix[S] ss_cov_poll_bias; // S x S mat of prior var-cov mat polling bias
  
  //*** prior input
  vector[S] mu_b_prior; // prior on latent state Harris vote intention
  vector[S] state_weights; // state vote weights [vis-a-vis natl vote]
  real sigma_c; // prior on var of house effect dist.
  real sigma_m; // prior on var of poll mode effect dist.
  real sigma_pop; // prior on var of poll pop effect dist.
  real sigma_measure_noise_national; // prior on var NATL poll measurement error
  real sigma_measure_noise_state; // prior on var STATE poll measurement error
  real sigma_e_bias; // prior on var of day error
}

transformed data { // transformations of matrices for SPEED
  cholesky_factor_cov[S] cholesky_ss_cov_mu_b_T;
  cholesky_factor_cov[S] cholesky_ss_cov_mu_b_walk;
  cholesky_factor_cov[S] cholesky_ss_cov_poll_bias;
  cholesky_ss_cov_mu_b_T = cholesky_decompose(ss_cov_mu_b_T);
  cholesky_ss_cov_mu_b_walk = cholesky_decompose(ss_cov_mu_b_walk);
  cholesky_ss_cov_poll_bias = cholesky_decompose(ss_cov_poll_bias);
}

parameters { // the RAW parameters are the basic form of the priors -- look for their distributions below
  vector[S] raw_mu_b_T;
  matrix[S, T] raw_mu_b; 
  vector[P] raw_mu_c;
  vector[M] raw_mu_m;
  vector[Pop] raw_mu_pop;
  real<offset=0, multiplier=0.02> mu_e_bias; // daily error [multiplier = shrinkage constraint, can be modified]
  real<lower = 0, upper = 1> rho_e_bias; // cov of daily error -- assumed constant and small
  vector[T] raw_e_bias;
  vector[N_national_polls] raw_measure_noise_national;
  vector[N_state_polls] raw_measure_noise_state;
  vector[S] raw_polling_bias; 
  real mu_b_T_model_estimation_error; // model estimation error term for target day to predict
}

transformed parameters {
  //*** parameters
  matrix[S, T] mu_b; // latent 'true' state dem vote intention (daily)
  vector[P] mu_c; // pollster house effects
  vector[M] mu_m; // poll mode effects
  vector[Pop] mu_pop; // poll pop effects
  vector[T] e_bias; // day error
  vector[S] polling_bias = cholesky_ss_cov_poll_bias * raw_polling_bias; // S x S mat * S vec --> S vec (polling bias by state)
  vector[T] national_mu_b_average; // latent 'true' natl dem vote intention (daily)
  real national_polling_bias_average = transpose(polling_bias) * state_weights; // S vec * S vec --> scalar (nat polling bias as weighted state avg)
  real sigma_rho;
  
  //*** containers
  vector[N_state_polls] logit_pi_democrat_state; // indiv P(Harris support) x State [filled below]
  vector[N_national_polls] logit_pi_democrat_national; // indiv P(Harris support) natl [filled below]
  
  //*** construct parameters
  // last day of mu_b = prior modified by all the injected state info. [posterior is our estimate of "today's"" latent vote intention]
  mu_b[:,T] = cholesky_ss_cov_mu_b_T * raw_mu_b_T * mu_b_T_model_estimation_error + mu_b_prior;
  // daily mu_b = correlated over time with daily info & next day's mu_b
  for (i in 1:(T-1)) mu_b[:, T - i] = cholesky_ss_cov_mu_b_walk * raw_mu_b[:, T - i] + mu_b[:, T + 1 - i];

  national_mu_b_average = transpose(mu_b) * state_weights; // natl latent Harris support = wghtd avg over states
  mu_c = raw_mu_c * sigma_c; // dist. of house effects x var. of house effect distribution
  mu_m = raw_mu_m * sigma_m; // dist. of poll mode effects x var. of mode effect dist.
  mu_pop = raw_mu_pop * sigma_pop; // dist. of poll pop effects x var. of pop effect dist.

  // note: daily error/bias walks *forward* -- as does natural time
  e_bias[1] = raw_e_bias[1] * sigma_e_bias; // starting daily error term
  sigma_rho = sqrt(1-square(rho_e_bias)) * sigma_e_bias; // day-to-day covariance
  for (t in 2:T) e_bias[t] = mu_e_bias + rho_e_bias * (e_bias[t - 1] - mu_e_bias) + raw_e_bias[t] * sigma_rho;

  //*** fill logit_pi_democrat [state, then natl]
  for (i in 1:N_state_polls){
    logit_pi_democrat_state[i] = // STATE
      mu_b[state[i], day_state[i]] + 
      mu_c[poll_state[i]] + // pollster house effects
      mu_m[poll_mode_state[i]] + // poll mode effects
      mu_pop[poll_pop_state[i]] + // poll pop effects
      unadjusted_state[i] * e_bias[day_state[i]] + // basic error associated with day
      raw_measure_noise_state[i] * sigma_measure_noise_state + // basic measurement error in polls
      polling_bias[state[i]]; // specific state polling error 
  }
  
  logit_pi_democrat_national = // NATL
    national_mu_b_average[day_national] +  
    mu_c[poll_national] + // pollster house effects
    mu_m[poll_mode_national] + // poll mode effects
    mu_pop[poll_pop_national] + // poll pop effects
    unadjusted_national .* e_bias[day_national] + // basic error associated with day
    raw_measure_noise_national * sigma_measure_noise_national + // basic measurement error in polls
    national_polling_bias_average; // natl-specific polling error
}

model {
  //*** prior distributions
  raw_mu_b_T ~ std_normal();
  mu_b_T_model_estimation_error ~ scaled_inv_chi_square(7, 1);
  to_vector(raw_mu_b) ~ std_normal();
  raw_mu_c ~ std_normal();
  raw_mu_m ~ std_normal();
  raw_mu_pop ~ std_normal();
  mu_e_bias ~ normal(0, 0.02);
  rho_e_bias ~ normal(0.7, 0.1);
  raw_e_bias ~ std_normal();
  raw_measure_noise_national ~ std_normal();
  raw_measure_noise_state ~ std_normal();
  raw_polling_bias ~ std_normal();
  
  //*** likelihood [i.e. binomial function describing count of state/natl Harris voters]
  n_democrat_state ~ binomial_logit(n_two_share_state, logit_pi_democrat_state);
  n_democrat_national ~ binomial_logit(n_two_share_national, logit_pi_democrat_national);
}

generated quantities {
  matrix[T, S] predicted_score;
  for (s in 1:S){
    predicted_score[1:T, s] = inv_logit(to_vector(mu_b[s, 1:T])); // EV[indiv P(Harris)] x state, daily
  }
}
