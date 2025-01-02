mean_low_high <- function(draws, states, id){
  tmp <- draws
  draws_df <- data.frame(mean = inv.logit(apply(tmp, MARGIN = 2, mean)),
                         high = inv.logit(apply(tmp, MARGIN = 2, mean) + 1.96 * apply(tmp, MARGIN = 2, sd)),
                         low  = inv.logit(apply(tmp, MARGIN = 2, mean) - 1.96 * apply(tmp, MARGIN = 2, sd)),
                         state = states,
                         type = id)
  return(draws_df)
}

cov_matrix <- function(n, sigma2, rho) {
  m <- matrix(nrow = n,
              ncol = n)
  m[upper.tri(m)] <- rho
  m[lower.tri(m)] <- rho
  diag(m) <- 1
  (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

check_cov_matrix <- function(mat,wt=state_weights){
  # get diagonals
  s_diag <- sqrt(diag(mat))
  # output correlation matrix
  cor_equiv <- cov2cor(mat)
  diag(cor_equiv) <- NA
  # output equivalent national standard deviation
  nat_product <- sqrt(t(wt) %*% mat %*% wt) / 4

  # print & output
  hist(as_vector(cor_equiv),breaks = 10)

  hist(s_diag,breaks=10)

  print(sprintf('national sd of %s',round(nat_product,4)))
}

fit_rmse_day_x <- function(x) {
  0.03 +  (10 ^ -6.6) * (x) ^ 2
} # fit to error from external script

first_targets <- c(
  "n_chains",
  "n_cores",
  "n_warmup",
  "n_iter",
  "n_sampling",
  "n_refresh",
  "run_date",
  "election_day",
  "start_date",
  "sigma_measure_noise_national",
  "sigma_measure_noise_state",
  "sigma_c",
  "sigma_m",
  "sigma_pop",
  "sigma_e_bias",
  "poll_list_file",
  "poll_list",
  "potus_results_76_20",
  "state_abb_list",
  "polls",
  "elec_2020_file",
  "states2020",
  "state_name",
  "prior_diff_score",
  "state_weights",
  "ev_state",
  "vote_data_correlation",
  "census_file",
  "socio_dem",
  "urbanicity_file",
  "urbanicity",
  "white_evangel_pct_file",
  "median_income_file",
  "religious_unaffiliated_file",
  "religious_unaffiliated",
  "black_pct_without_file",
  "black_pct_without",
  "hispanics_without_cuban_file",
  "hispanics_without_cuban",
  "socio_dem_correlation",
  "region_file",
  "region_correlation",
  "vote_state_covariance",
  "socio_state_covariance",
  "regions_state_covariance",
  "C",
  "abramowitz_file",
  "consumer_sentiment_file",
  "issue_file",
  "tfc_prediction",
  "mu_b_prior",
  "national_mu_prior",
  "state_correlation_polling",
  "state_covariance_0",
  "adjusters",
  "N_state_polls",
  "N_national_polls",
  "N_state",
  "N_national",
  "T",
  "S",
  "P",
  "M",
  "Pop",
  "state",
  "day_national",
  "day_state",
  "poll_national",
  "poll_state",
  "poll_mode_national",
  "poll_mode_state",
  "poll_pop_national",
  "poll_pop_state",
  "n_democrat_national",
  "n_democrat_state",
  "n_two_share_national",
  "n_two_share_state",
  "unadjusted_national",
  "unadjusted_state",
  "polling_bias_scale",
  "expected_national_mu_b_T_error",
  "mu_b_T_scale",
  "random_walk_scale",
  "national_cov_matrix_error_sd",
  "ss_cov_mu_b_walk",
  "ss_cov_mu_b_T",
  "ss_cov_poll_bias",
  "stan_data"
)
