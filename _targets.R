library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # Define packages
  packages = c(
    "tidyverse", "rstan", "stringr", "lubridate", "gridExtra", "pbapply",
    "parallel", "boot", "lqmm", "ggrepel", "Rcpp", "readxl", "corrplot"
  )
)

tar_source()

options(mc.cores = parallel::detectCores())

list(
  # Set options for runtime
  tar_target(n_chains, 4),
  tar_target(n_cores, getOption("mc.cores")),
  tar_target(n_warmup, 1000),
  tar_target(n_iter, 3500),
  tar_target(n_sampling, n_iter * 0.1),
  tar_target(n_refresh, n_sampling * 0.1),

  # Set date info - Useful before the election and to "go back in time"
  tar_target(run_date, today()),
  tar_target(election_day, ymd("2024-11-05")),
  # tar_target(start_date, ymd("2024-07-24")),
  tar_target(start_date, as.Date("2024-07-24")),

  # Setup priors
  tar_target(sigma_measure_noise_national, 0.05),
  tar_target(sigma_measure_noise_state, 0.05),
  tar_target(sigma_c, 0.06),
  tar_target(sigma_m, 0.04),
  tar_target(sigma_pop, 0.04),
  tar_target(sigma_e_bias, 0.02),

  # Download and wrangle polls
  tar_download(poll_list_file,
               "https://projects.fivethirtyeight.com/2024-general-data/538_2024_election_forecast_data.zip",
               "data/fivethirtyeight.zip",
               error = "continue"),
  tar_target(poll_list, extract_polls(poll_list_file)),

  # Get list of state abbreviations
  tar_target(potus_results_76_20, "data/potus_results_76_20.csv", format = "file"),
  tar_target(state_abb_list, get_state_abb(potus_results_76_20)),

  # Add index of states to polls
  tar_target(polls, create_index(poll_list, state_abb_list)),

  # Read 2020 election data
  tar_target(elec_2020_file, "data/2020.csv", format = "file"),
  tar_target(states2020, import_2020_data(elec_2020_file, state_abb_list)),
  tar_target(state_name, add_state_names(states2020, state_abb_list)),
  tar_target(prior_diff_score, get_prior_diff_score(states2020, state_abb_list)),
  tar_target(state_weights, get_state_weights(states2020, state_abb_list)),
  tar_target(ev_state, get_electoral_votes(states2020, state_abb_list)),

  # Create state correlation matrix
  ## Vote correlation
  tar_target(vote_data_correlation, get_vote_returns(potus_results_76_20)),
  ## Sociodemographics
  tar_target(census_file,
             "data/acs_2023_variables.csv",
             format = "file"),
  tar_target(socio_dem, get_census_data(census_file)),
  ## Urbanicity
  tar_target(urbanicity_file,
             "data/urbanicity_index.csv",
             format = "file"),
  tar_target(urbanicity, get_urbanicity(urbanicity_file)),
  ## Pct of white evangelists
  tar_target(white_evangel_pct_file,
             "data/white_evangel_pct.csv",
             format = "file"),
  tar_target(white_evangel_pct, get_white_evangel_pct(white_evangel_pct_file)),
  ## Median income
  tar_target(median_income_file,
             "data/MedianHouseholdIncome2023_Abbrev.csv",
             format = "file"),
  tar_target(median_income, get_median_income(median_income_file)),
  ## Amount of religiously unaffiliated
  tar_target(religious_unaffiliated_file,
             "data/ReligiousUnaffiliated_Abbrev.csv",
             format = "file"),
  tar_target(religious_unaffiliated, get_religious_unaffiliated(religious_unaffiliated_file)),
  ## Blacks that are not young men between 18-29
  tar_target(black_pct_without_file,
             "data/share_18-29_blacks_1.xlsx",
             format = "file"),
  tar_target(black_pct_without, get_black_pct_without(black_pct_without_file,
                                                      elec_2020_file)),
  ## Hispanics that are non-cuban
  tar_target(hispanics_without_cuban_file,
             "data/noncubanhispanics.csv",
             format = "file"),
  tar_target(hispanics_without_cuban, get_hispanics_without_cuban(hispanics_without_cuban_file)),
  ## Sociodem correlation
  tar_target(socio_dem_correlation, combine_sociodem(socio_dem,
                                                     urbanicity,
                                                     white_evangel_pct,
                                                     median_income,
                                                     religious_unaffiliated,
                                                     black_pct_without,
                                                     hispanics_without_cuban)),
  ## Regions
  tar_target(region_file, "data/US_States_by_Region.csv", format = "file"),
  tar_target(region_correlation, get_region_correlation(region_file)),

  # Coerce correlations into proper covariance-matrix
  tar_target(vote_state_covariance, cor(vote_data_correlation)),
  tar_target(socio_state_covariance, cor(socio_dem_correlation)),
  tar_target(regions_state_covariance, cor(region_correlation)),
  tar_target(C, construct_matrix(vote_state_covariance,
                                 socio_state_covariance,
                                 regions_state_covariance)),

  # Calculate TFC prior
  tar_target(abramowitz_file,
             "data/abramowitz_data_2024.csv",
             format = "file"),
  tar_target(consumer_sentiment_file,
             "data/Con_Sen.xls",
             format = "file"),
  tar_target(issue_file,
             "data/most_important_issue.csv",
             format = "file"),
  tar_target(construction_file,
             "data/construction_season_adj.csv",
             format = "file"),
  tar_target(tfc_prediction, tfc(abramowitz_file, consumer_sentiment_file,
                                    issue_file, construction_file,
                                    prior_diff_score, state_weights)),
  tar_target(mu_b_prior, get_mu_b_prior(tfc_prediction, prior_diff_score)),
  tar_target(national_mu_prior, get_national_mu_prior(mu_b_prior, state_weights)),

  # Scaling
  tar_target(state_correlation_polling, get_new_C(C) %>% make.positive.definite()),
  tar_target(state_covariance_0,
             cov_matrix(51, 0.078 ^ 2, 0.29) * state_correlation_polling),

  # List adjusters
  tar_target(adjusters, c(
    "Marquette", "Change Research", "Data for Progress", "Fabrizio",
    "Big Village","Florida Atlantic","SoCal Research", "Echelon Insights",
    "co/efficient", "Trafalgar Group", "J.L. Partners", "Survey Center",
    "YouGov/SNF Agora", "Emerson College")),

  # Prepare data to pass into stan
  tar_target(N_state_polls, get_N_state_polls(polls)),
  tar_target(N_national_polls, get_N_national_polls(polls)),
  tar_target(N_state, get_N_state(polls)),
  tar_target(N_national, get_N_national(polls)),
  tar_target(T, get_T(election_day, start_date)),
  tar_target(S, get_S()),
  tar_target(P, get_P(polls)),
  tar_target(M, get_M(polls)),
  tar_target(Pop, get_Pop(polls)),
  tar_target(state, get_state(polls)),
  tar_target(day_national, get_day_national(polls)),
  tar_target(day_state, get_day_state(polls)),
  tar_target(poll_national, get_poll_national(polls)),
  tar_target(poll_state, get_poll_state(polls)),
  tar_target(poll_mode_national, get_poll_mode_national(polls)),
  tar_target(poll_mode_state, get_poll_mode_state(polls)),
  tar_target(poll_pop_national, get_poll_pop_national(polls)),
  tar_target(poll_pop_state, get_poll_pop_state(polls)),
  tar_target(n_democrat_national, get_n_democrat_national(polls)),
  tar_target(n_democrat_state, get_n_democrat_state(polls)),
  tar_target(n_two_share_national, get_n_two_share_national(polls)),
  tar_target(n_two_share_state, get_n_two_share_state(polls)),
  tar_target(unadjusted_national, get_unadjusted_national(polls, adjusters)),
  tar_target(unadjusted_state, get_unadjusted_state(polls, adjusters)),
  tar_target(polling_bias_scale, get_polling_bias_scale(0.014)),
  tar_target(
    expected_national_mu_b_T_error,
    get_expected_national_mu_b_T_error(election_day, run_date)
  ),
  tar_target(mu_b_T_scale, get_mu_b_T_scale(expected_national_mu_b_T_error)),
  tar_target(random_walk_scale, get_random_walk_scale(0.05 / sqrt(300))),
  tar_target(
    national_cov_matrix_error_sd,
    get_national_cov_matrix_error_sd(state_weights, state_covariance_0)
  ),
  tar_target(
    ss_cov_mu_b_walk,
    get_ss_cov_mu_b_walk(
      state_covariance_0,
      random_walk_scale,
      national_cov_matrix_error_sd
    )
  ),
  tar_target(ss_cov_mu_b_T, get_ss_cov_mu_b_T(state_covariance_0,
                                              mu_b_T_scale,
                                              national_cov_matrix_error_sd)),
  tar_target(ss_cov_poll_bias, get_ss_cov_poll_bias(state_covariance_0,
                                                    polling_bias_scale,
                                                    national_cov_matrix_error_sd)),

  # Compile stan data
  tar_target(stan_data, list(
    N_national_polls = N_national, # count of national polls
    N_state_polls = N_state, # count of state polls
    T = T, # days from analysis start to election day
    S = S, # count of distinct states
    P = P, # count of distinct pollsters
    M = M, # count of distinct poll modes
    Pop = Pop, # count of distinct poll types [i.e. voter populations, likely/registered/etc.]
    state = state, # indexes of poll states
    state_weights = state_weights, # state weight (for taking natl weighted avgs)
    day_state = day_state, # day of STATE polls
    day_national = day_national, # day of NATL polls
    poll_state = poll_state, # pollster index for STATE polls
    poll_national = poll_national, # pollster index for NATL polls
    poll_mode_national = poll_mode_national, # mode index for NATL polls
    poll_mode_state = poll_mode_state, # mode index for STATE polls
    poll_pop_national = poll_pop_national, # pop index for NATL polls
    poll_pop_state = poll_pop_state, # pop index for STATE polls
    unadjusted_national = unadjusted_national, # indicator for 'unadjusted' NATL polls
    unadjusted_state = unadjusted_state, # indicator for 'unadjusted' STATE polls
    n_democrat_national = n_democrat_national, # dem vote intention count NATL polls
    n_democrat_state = n_democrat_state, # dem vote intention count STATE polls
    n_two_share_national = n_two_share_national, # two-party vote intention count NATL polls
    n_two_share_state = n_two_share_state, # two-party vote intention count STATE polls
    sigma_measure_noise_national = sigma_measure_noise_national,
    sigma_measure_noise_state = sigma_measure_noise_state,
    mu_b_prior = mu_b_prior, # prior on latent state Harris vote intention
    sigma_c = sigma_c, # prior on var of house effect dist.
    sigma_m = sigma_m, # prior on var of poll mode effect dist.
    sigma_pop = sigma_pop, # prior on var of poll pop effect dist.
    sigma_e_bias = sigma_e_bias, # prior on var of day error
    state_covariance_0 = state_covariance_0,
    polling_bias_scale = polling_bias_scale,
    mu_b_T_scale = mu_b_T_scale,
    ss_cov_mu_b_walk = ss_cov_mu_b_walk,
    ss_cov_mu_b_T = ss_cov_mu_b_T,
    ss_cov_poll_bias = ss_cov_poll_bias,
    random_walk_scale = random_walk_scale
  )),

  # Define stan model
  tar_target(model_file, "stan/model.stan"),

  # Run stan model
  # tar_target(stan_run, stan(
  #   file = model_file,
  #   data = stan_data,
  #   chains = n_chains,
  #   warmup = n_warmup,
  #   iter = n_iter,
  #   cores = n_cores,
  #   refresh = n_refresh
  # ), deployment = "main", memory = "persistent"),
  tar_target(model, run_model(stan_data, model_file, n_chains, n_warmup,
                              n_iter, n_cores, n_refresh)),
  tar_target(draws_path, "_output/draws.rds", format = "auto"),

  # Process stan results
  # tar_target(draws, get_draws(stan_run), deployment = "main", memory = "persistent"),

  # Render synopsis
  tar_quarto(render, "synopsis/synopsis.qmd")
)
