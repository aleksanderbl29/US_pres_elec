get_N_state_polls <- function(polls) {
  nrow(polls %>% filter(index_s != 52))
}

get_N_national_polls <- function(polls) {
  nrow(polls %>% filter(index_s == 52))
}

get_N_state <- function(polls) {
  nrow(polls %>% filter(index_s != 52))
}

get_N_national <- function(polls) {
  nrow(polls %>% filter(index_s == 52))
}

get_T <- function(election_day, start_date) {
  as.integer(round(difftime(election_day, start_date)))
}

get_S <- function() {
  51
}

get_P <- function(polls) {
  length(unique(polls$pollster))
}

get_M <- function(polls) {
  length(unique(polls$mode))
}

get_Pop <- function(polls) {
  length(unique(polls$polltype))
}

get_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    pull(index_s)
}

get_day_national <- function(polls) {
  polls %>%
    filter(index_s == 52) %>%
    pull(poll_day) %>%
    as.integer()
}

get_day_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    pull(poll_day) %>%
    as.integer()
}

get_poll_national <- function(polls) {
  polls %>%
    filter(index_s == 52) %>%
    pull(index_p)
}

get_poll_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    pull(index_p)
}

get_poll_mode_national <- function(polls) {
  polls %>%
    filter(index_s == 52) %>%
    pull(index_m)
}

get_poll_mode_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    pull(index_m)
}

get_poll_pop_national <- function(polls) {
  polls %>%
    filter(index_s == 52) %>%
    pull(index_pop)
}

get_poll_pop_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    pull(index_pop)
}

get_n_democrat_national <- function(polls) {
  polls %>%
    filter(index_s == 52) %>%
    pull(n_harris)
}

get_n_democrat_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    pull(n_harris)
}

get_n_two_share_national <- function(polls) {
  polls %>%
    filter(index_s == 52) %>%
    transmute(n_two_share = n_trump + n_harris) %>%
    pull(n_two_share)
}

get_n_two_share_state <- function(polls) {
  polls %>%
    filter(index_s != 52) %>%
    transmute(n_two_share = n_trump + n_harris) %>%
    pull(n_two_share)
}

get_unadjusted_national <- function(polls, adjusters) {
  polls %>%
    mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>%
    filter(index_s == 52) %>%
    pull(unadjusted)
}

get_unadjusted_state <- function(polls, adjusters) {
  polls %>%
    mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>%
    filter(index_s != 52) %>%
    pull(unadjusted)
}

get_polling_bias_scale <- function(x = 0.014) {
  as.numeric(x) * 4
}

get_mu_b_T_scale <- function(expected_national_mu_b_T_error) {
  as.numeric(expected_national_mu_b_T_error) * 4
}

get_random_walk_scale <- function(x = 0.05 / sqrt(300)) {
  as.numeric(x) * 4
}

get_expected_national_mu_b_T_error <- function(election_day, run_date) {
  difftime(election_day, run_date) %>%
    as.numeric() %>%
    fit_rmse_day_x()
}

get_national_cov_matrix_error_sd <- function(
    state_weights, state_covariance_0) {

  sqrt(t(state_weights) %*% state_covariance_0 %*% state_weights) %>% as.numeric()
}

get_ss_cov_mu_b_walk <- function(state_covariance_0,
                                 random_walk_scale,
                                 national_cov_matrix_error_sd) {

  state_covariance_0 * (random_walk_scale / national_cov_matrix_error_sd * 4) ^ 2

}

get_ss_cov_mu_b_T <- function(state_covariance_0,
                              mu_b_T_scale,
                              national_cov_matrix_error_sd) {

  state_covariance_0 * (mu_b_T_scale / national_cov_matrix_error_sd * 4) ^ 2

}

get_ss_cov_poll_bias <- function(state_covariance_0,
                                 polling_bias_scale,
                                 national_cov_matrix_error_sd) {

  state_covariance_0 * (polling_bias_scale / national_cov_matrix_error_sd * 4)^2

}



