############################################################
########################## Voting ##########################
############################################################
get_vote_returns <- function(file) {
  read_csv(file) %>%
    select(year, state, dem) %>%
    group_by(state) %>%
    mutate(dem = dem) %>%
    select(state, variable = year, value = dem)  %>%
    ungroup() %>%
    na.omit() %>%
    filter(variable >= 1996) %>%
    group_by(variable) %>%
    # filter(state != "DC") %>%
    # scale all variables (value - minimum) / original range --> varies b/w 0 and 1
    mutate(value = (value - min(value, na.rm = T)) /
             (max(value, na.rm = T) - min(value, na.rm = T))) %>%
    # now spread
    spread(state, value) %>%
    na.omit() %>%
    ungroup() %>%
    select(-variable)
}

############################################################
#################### Sociodemographics #####################
############################################################
get_census_data <- function(file) {
  read_csv(file) %>%
    filter(!is.na(state)) %>%
    # filter(state != "DC") %>%
    select(-c(state_fips, pop_total, pop_density, black_pct, hisp_pct)) %>%
    group_by(state) %>%
    gather(variable, value, 1:(ncol(.) - 1)) %>%
    ungroup()
}

get_urbanicity <- function(file) {
  read_csv(file) %>%
    select(state, pop_density = average_log_pop_within_5_miles) %>%
    gather(variable, value, 2:(ncol(.)))
}

get_white_evangel_pct <- function(file) {
  read_csv(file) %>%
    gather(variable, value, 2:(ncol(.)))
}

get_median_income <- function(file) {
  read_csv(file) %>%
    gather(variable, value, 2:(ncol(.)))
}

get_religious_unaffiliated <- function(file) {
  read_csv(file) %>%
    gather(variable, value, 2:(ncol(.))) %>%
    add_row(state = "DC",
            variable = "Unaffiliated (%)",
            value = 24) %>%
    arrange(state)
  # source ->
  # https://www.pewresearch.org/religious-landscape-study/database/metro-area/washington-dc-metro-area/
}

get_black_pct_without <- function(file, elec_2020_file) {
  blacks <- read_excel(file, sheet = "Without")
  states <- read_csv(elec_2020_file) %>%
    select(c(state, state_name))

  blacks %>%
    left_join(states, by = c("state" = "state_name")) %>%
    select('state.y', 'black_pct_without') %>%
    rename('state' = 'state.y') %>%
    gather(variable, value, 2:(ncol(.)))
}

get_hispanics_without_cuban <- function(file) {
  read.csv(file) %>%
    gather(variable, value, 2:(ncol(.)))
}

combine_sociodem <- function(socio_dem,
                             urbanicity,
                             white_evangel_pct,
                             median_income,
                             ReligiousUnaffiliated,
                             black_pct_without,
                             hispanics_without_cuban) {
  socio_dem %>%
    bind_rows(urbanicity) %>%
    bind_rows(white_evangel_pct) %>%

    # This is our modification
    bind_rows(median_income) %>%
    # This is also our modification
    bind_rows(ReligiousUnaffiliated) %>%
    bind_rows(black_pct_without) %>%
    bind_rows(hispanics_without_cuban) %>%
    group_by(variable) %>%
    # filter(state != "DC") %>%
    # scale all variables (value - minimum) / original range --> varies b/w 0 and 1
    mutate(value = (value - min(value, na.rm = T)) /
             (max(value, na.rm = T) - min(value, na.rm = T))) %>%
    # now spread
    spread(state, value) %>%
    na.omit() %>%
    ungroup() %>%
    select(-variable)
}

############################################################
######################### Regions ##########################
############################################################
get_region_correlation <- function(file) {
  data <- read_csv(file) %>%
    rename(state = State, region = Region) %>%
    select(state = state, region) %>%       # Vælg staten og regionskolonnerne
    mutate(value = 1) %>%                   # Opret en dummy for hver region
    spread(region, value)                   # Konverter region til brede dummy-variabler

  # Erstat NA'er med 0
  data[is.na(data)] <- 0

  data %>%
    gather(variable, value, 2:(ncol(.))) %>%
    add_row(state = "DC",
            variable = "District of Columbia",
            value = 1) %>%
    group_by(variable) %>%
    # filter(state != "DC") %>%
    # scale all variables (value - minimum) / original range --> varies b/w 0 and 1
    mutate(value = (value - min(value, na.rm = T)) /
             (max(value, na.rm = T) - min(value, na.rm = T))) %>%
    # now spread
    spread(state, value) %>%
    na.omit() %>%
    ungroup() %>%
    select(-variable)
}

############################################################
################### Matrix construction ####################
############################################################
construct_matrix <- function(vote_state_covariance,
                             socio_state_covariance,
                             regions_state_covariance) {

  # Replace NA with 0
  vote_state_covariance[is.na(vote_state_covariance)] <- 0
  socio_state_covariance[is.na(socio_state_covariance)] <- 0
  regions_state_covariance[is.na(regions_state_covariance)] <- 0

  # Baseline cor
  baseline_cor <- matrix(data = 1, nrow = 51, ncol = 51)

  # Defining weights estimated in seperate model
  baseline_w <- 0.04175
  vote_w <- 0.25825
  region_w <- 0.1
  demographic_w <- 0.6

  C <- (baseline_w * baseline_cor) +
    (vote_w * vote_state_covariance) +
    (region_w * regions_state_covariance) +
    (demographic_w * socio_state_covariance)

  # Ensure no correlations are above 1
  C[C > 1] <- 1

  return(C)

}

get_new_C <- function(C) {
  # Formula is
  # a*(lambda*C + (1-lambda)*C_1)
  # where C is our correlation matrix with min .3
  # and C_1 is a sq matrix with all 1's
  # lambda=0 is 100% correlation, lambda=1 is our corr matrix
  # C[C < 0.3] <- 0.3 # baseline correlation for national poll error
  lambda <- 0.5
  C_1 <- matrix(data = 1, nrow = 51, ncol = 51)
  C <- C[1:nrow(C_1), 1:ncol(C_1)]
  a <- 1
  #nat_vote_scale <- 0.01
  nat_vote_scale <- 1
  new_C <- nat_vote_scale * (lambda * C + (1 - lambda) * C_1) %>% make.positive.definite()
}

