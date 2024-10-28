## econ2024_DB_Harris.R
## Building on 'final_2016.R' run Econ. model on the 2024 data
## adaptation by Derek Beach, Nicholas Beach Johansen and Nicholas Haas

#Set directory#
# setwd("~/Dropbox/Teaching Aarhus/Forecasting/Session 9")

#Clear environment#
rm(list = ls())

###############################################################
## Setup for runtime
###############################################################

tictoc::tic()

options(mc.cores = parallel::detectCores())
n_chains <- 4
# n_cores <- 4
n_cores <- parallel::detectCores()
n_sampling <- 1000
n_warmup <- 1000
n_refresh <- n_sampling*0.1
n_iter <- 1500

###############################################################
## Libraries
###############################################################
library(pacman)
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

{
  library(tidyverse, quietly = TRUE)
  library(rstan, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(gridExtra, quietly = TRUE)
  library(pbapply, quietly = TRUE)
  library(parallel, quietly = TRUE)
  library(boot, quietly = TRUE)
  library(lqmm, quietly = TRUE)
  library(gridExtra, quietly = TRUE)
  library(ggrepel, quietly = TRUE)
  #  library(cmdstanr, quietly = TRUE)
  library(Rcpp)
  library(fredr)
}

###############################################################
## Setup for dates
###############################################################
# RUN_DATE <- ymd("2024-07-14") # Grab today's date as "run date" (alter depending on date)
RUN_DATE <- today() # Grab today's date as "run date" (alter depending on date)
election_day <- ymd("2024-11-05") # Date of election (do not modify)
start_date <- as.Date("2024-07-24") # Keep all polls after July 24 - day when Harris became candidate (can be modified)

###############################################################
## Setup for priors (adjust model uncertainty)
###############################################################
sigma_measure_noise_national <- 0.05
sigma_measure_noise_state <- 0.05
sigma_c <- 0.06
sigma_m <- 0.04
sigma_pop <- 0.04
sigma_e_bias <- 0.02

###############################################################
## Helper functions
###############################################################
# function to create a covariance matrix of size n (rows & cols)
# rho = off-diagonal elements (i.e. covariances)
# sigma2 = diagonal elements (i.e. variances)
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

  hist(s_diag,breaks = 10)

  print(sprintf('national sd of %s',round(nat_product,4)))
}

poll_download <- function() {
  old_data <- read_csv("data/poll_list.csv")
  url <- "https://projects.fivethirtyeight.com/polls/data/president_polls.csv"
  new_data <- read_csv(url)

  if (old_data == new_data) {
    print("No new data")
  } else if (!old_data == new_data) {
    print("The data on disk is out-dated")
    print("Updating data...")
    if (!dir.exists("data/old_data/poll_list/")) {
      print("Creating old_data directory")
      dir.create("data/old_data/poll_list/", recursive = TRUE)
    }
    write_csv(old_data, paste0("data/old_data/poll_list/", today(), ".csv"))
    print("Archiving data on disk")
    write_csv(new_data, "data/poll_list.csv")
    print("Writing new data to poll_list.csv")
  }
}

# Function to assign weights and compute weighted average
weighted_avg_4_years <- function(data, election_years) {
  # Initialize an empty data frame to store results
  result <- data.frame()

  for (election in election_years) {
    # Select the 4-year block ending at the election year
    df_block <- data %>%
      filter(year >= (election - 3) & year <= election)

    # Define the weights based on proximity to the election year
    weights <- c(1, 2, 3, 4)  # Weights increase toward election year

    # Calculate weighted average
    weighted_avg <- weighted.mean(df_block$total_value, weights)

    # Store the result with the corresponding election year
    result <- rbind(result, data.frame(election_year = election, weighted_avg = weighted_avg))
  }

  return(result)
}

###############################################################
## wrangle polls ------------------------------------------
###############################################################

# poll_download()

# select relevant columns from all polls 2024 (post-Harris), create one row per poll#
polls <- read_csv("data/poll_list.csv") #Recall: you can update polls from 538
# polls <- read_csv("data/poll_list-1.csv") #Recall: you can update polls from 538

# read_csv("data/state-info.csv") %>% colnames() %>% sort()
#
# state_info <- read_csv("data/state-info.csv") %>%
#   select(state, state_abb, region, region_computed, pollster, pollster_wt) %>%
#   unique()
#
# polls <- polls %>%
#   left_join(state_info, by = c("state"))

polls <- polls %>%
  mutate(grp = paste(poll_id,pollster_rating_id),
         pollpollsterID = match(grp, unique(grp)))

polls <- filter(polls, candidate_name == "Kamala Harris" |  candidate_name == "Donald Trump")

polls <- polls %>%
  group_by(pollpollsterID) %>%
  mutate(trump = case_when(candidate_name == "Donald Trump" ~ pct),
         harris = case_when(candidate_name == "Kamala Harris" ~ pct)) %>%
  fill(trump, .direction = "up") %>%
  fill(harris)  %>%
  filter(row_number(pollpollsterID) == 1)  %>%
  ungroup()

polls <- polls %>%
  dplyr::select(state_abb, pollster_rating_name,
                sample_size,
                population, methodology,
                start_date, end_date, region, region_computed, pollster_wt,
                harris, trump) # pick variables to keep (do not have data on undecided and other - could include later)

# basic mutations
df <- polls %>%
  mutate(begin = ymd(start_date),
         end = ymd(end_date),
         t = begin + (as.numeric(end-begin)/2))  # Assign poll date to middle of field period

#Rename variables#
df <- df %>%
  dplyr::rename(mode = methodology, n = sample_size, pollster = pollster_rating_name, state = state_abb)

#Clean mode variable#
df$mode <- case_when(
  df$mode == "IVR/Online Panel" ~ "IVR",
  df$mode == "IVR/Text-to-Web" ~ "IVR",
  df$mode == "IVR/Online Panel/Text-to-Web" ~ "IVR",
  df$mode == "IVR/Live Phone/Online Panel/Text-to-Web" ~ "IVR",
  df$mode == "IVR/Text" ~ "IVR",
  df$mode == "Live Phone/Text-to-Web" ~ "Live Phone",
  df$mode == "Live Phone/Text-to-Web/Email/Mail-to-Web/Mail-to-Phone" ~ "Live Phone",
  df$mode == "Live Phone/Text" ~ "Live Phone",
  df$mode == "Live Phone/Email" ~ "Live Phone",
  df$mode == "Live Phone/Probability Panel" ~ "Live Phone",
  df$mode == "Live Phone/Online Panel/Text-to-Web" ~ "Live Phone",
  df$mode == "Live Phone/Online Panel/App Panel" ~ "Live Phone",
  df$mode == "IVR/Live Phone/Text/Online Panel/Email" ~ "IVR",
  df$mode == "Text-to-Web/Online Ad" ~ "Text-to-Web",
  df$mode == "Text-to-Web/Email" ~ "Text-to-Web",
  df$mode == "Online Panel/Text-to-Web" ~ "Online Panel",
  df$mode == "Mail-to-Web/Mail-to-Phone" ~ "Mail-to-Web",
  df$mode == "Live Phone/Online Panel" ~ "Live Phone",
  df$mode == "Live Phone/Online Panel/Text" ~ "Live Phone",
  TRUE ~ as.character(df$mode)
)

#Clean pollster variable#
df$pollster <- case_when(
  df$pollster == "The New York Times/Siena College" ~ "NYT",
  df$pollster == "Harris Insights & Analytics" ~ "Harris",
  df$pollster == "Fabrizio, Lee & Associates/GBAO" ~ "Fabrizio",
  df$pollster == "Marquette University Law School" ~ "Marquette",
  df$pollster == "YouGov/Johns Hopkins University SNF Agora Institute" ~ "YouGov",
  df$pollster == "Florida Atlantic University PolCom Lab/Mainstreet Research" ~ "Florida Atlantic",
  df$pollster == "University of Massachusetts Department of Political Science/YouGov" ~ "Uni Mass",
  df$pollster == "Hart Research Associates/Public Opinion Strategies" ~ "Hart",
  df$pollster == "Beacon Research/Shaw & Co. Research" ~ "Beacon",
  TRUE ~ as.character(df$pollster)
)

# vote shares#
df <- df %>%
  mutate(two_party_sum = harris + trump, # two-party sum
         polltype = population, # poll population
         n_respondents = round(n), # poll sample size (rounding should not be needed, but in case)
         # harris
         n_harris = round(n * harris / 100), # approximate Harris supporters
         p_harris = harris / two_party_sum, # % of Harris voters
         # trump
         n_trump = round(n * trump / 100), # approximate Trump supporters
         p_trump = trump / two_party_sum) # % of Trump voters

##Set starting day#
first_day <- min(df$t) # first day of poll start dates

## get state abbreviations
state_abb_list <- read.csv("data/potus_results_76_20.csv") %>%
  pull(state) %>% unique()

## Removing polls for congressional districts (NE-CD2 and ME-CD1 and CD2)
df <- df[!grepl('ME-02', df$state),]
df <- df[!grepl('ME-01', df$state),]
df <- df[!grepl('NE-02', df$state),]

#Subset to polls for which data on polls is not missing#
df <- subset(df,!is.na(df$mode))

#Relabel national polls#
df$state[is.na(df$state)] <- "--"

## creating index
df <- df %>%
  arrange(state) %>%
  mutate(poll_day = t - min(t) + 1,
         # Factors are alphabetically sorted: 1 = --, 2 = AL, 3 = AK, 4 = AZ...
         # NB: the 'index' variables are numbered identifiers for each category
         index_s = as.numeric(
           factor(
             as.character(state),
             levels = c('--', state_abb_list) #1 for national polls (--), then count for each other state#
           )
         ),
         index_s = ifelse(index_s == 1, 52, index_s - 1), # Make national polls index state 52, separate indicator for each state#
         index_t = 1 + as.numeric(t) - min(as.numeric(t)),
         index_p = as.numeric(as.factor(as.character(pollster))),
         index_m = as.numeric(as.factor(as.character(mode))),
         index_pop = as.numeric(as.factor(as.character(polltype)))) %>%
  # selections and organization
  arrange(state, t, polltype, two_party_sum) %>%
  distinct(state, t, pollster, polltype, .keep_all = TRUE) %>%
  select(
    # poll information
    state, t, begin, end, pollster, polltype, mode, n_respondents,
    # vote shares
    p_harris, n_harris,
    p_trump, n_trump,
    poll_day, index_s, index_p, index_m, index_pop, index_t)

###############################################################
## Reading in 2020 election data
###############################################################
library("readxl")
states2020 <- read_csv("data/2020.csv") %>%
  # states2020 <- read_excel("data/2020.xlsx") %>%
  mutate(score = biden_count / (biden_count + trump_count), # state-level dem % of 2-party vote 2020
         national_score = sum(biden_count) / sum(biden_count + trump_count), # nat. dem % of 2-party vote 2020
         delta = score - national_score, # state diffs vs. national
         share_national_vote = (total_count * (1 + adult_pop_growth_2020_23)) # two-party vote weighted by pop growth
         / sum(total_count * (1 + adult_pop_growth_2020_23))) %>% # divided by total voters weighted by pop growth
  arrange(state) # alphabetize by state code

# get state indices
rownames(states2020) <- state_abb_list # set row names
state_name <- states2020$state_name  # get state names
names(state_name) <- state_abb_list

# set prior differences
prior_diff_score <- states2020$delta
names(prior_diff_score) <- state_abb_list # label prior diff scores w/state abbreviation

# set state weights relative to national vote (for weighted averages)
state_weights <- states2020$share_national_vote / sum(states2020$share_national_vote)
names(state_weights) <- state_abb_list # label state weights w/state abbreviation

# electoral votes, by state:
ev_state <- states2020$ev
names(ev_state) <- state_abb_list # label electoral votes w/ state abbreviation

###############################################################
#create state correlation matrix#
###############################################################
#Basic state data -- vote returns in previous elections#
state_data <- read_csv("data/potus_results_76_20.csv")
state_data <- state_data %>%
  select(year, state, dem) %>%
  group_by(state) %>%
  mutate(dem = dem) %>%
  select(state,
         variable = year,
         value = dem)  %>%
  ungroup() %>%
  na.omit %>%
  filter(variable == 2020)

#Census data#
census <- read_csv("data/acs_2023_variables.csv")
census <- census %>%
  filter(!is.na(state)) %>%
  select(-c(state_fips, pop_total, pop_density)) %>%
  group_by(state) %>%
  gather(variable,
         value,
         1:(ncol(.)-1)) %>%
  ungroup()

#Combine with state vote returns from 2020#
state_data <- state_data %>%
  mutate(variable = as.character(variable)) %>%
  bind_rows(census)

# add urbanicity
urbanicity <- read.csv("data/urbanicity_index.csv") %>%
  dplyr::select(state,pop_density = average_log_pop_within_5_miles) %>%
  gather(variable,
         value,
         2:(ncol(.)))

#Combine#
state_data <- state_data %>%
  bind_rows(urbanicity)

#Add percentage white evangelical#
white_evangel_pct <- read_csv("data/white_evangel_pct.csv") %>%
  gather(variable,
         value,
         2:(ncol(.)))

#Combine#
state_data <- state_data %>%
  bind_rows(white_evangel_pct)

###############################################################
## An option: add region#
#
# Add a dummy for each region
# regions <- read_csv('state_region_crosswalk.csv') %>%
#   select(state = state_abb, variable=region) %>%
#   mutate(value = 1) %>%
#   spread(variable,value)
#
# regions[is.na(regions)] <- 0
#
# regions <- regions %>%
#   gather(variable, value, 2:ncol(.))
#
#state_data <- state_data %>%
#  bind_rows(regions)
###############################################################

# Scale and spread (see materials for session 6)#
# this normalizes all variables for state cov matrix to fall between 0 and 1#
# then, it spreads them out so that states are in columns
# and variables are along rows
# the final cov matrix gets the covariance between columns (states)
state_data_long <- state_data %>%
  group_by(variable) %>%
  # scale all variables (value - minimum) / original range --> varies b/w 0 and 1
  mutate(value = (value - min(value, na.rm=T)) /
           (max(value, na.rm=T) - min(value, na.rm=T))) %>%
  # now spread
  spread(state, value) %>%
  na.omit() %>%
  ungroup() %>%
  select(-variable)

# ###############################################################
# # test of the associations our selected variables return
# # try entering other states here to see how your choices pan out
#Recall from session 6 how to make a map#
ggplot(state_data_long,
       aes(x = PA,
           y = OH)) +
  geom_point() +
  geom_smooth(method = 'lm')

# make cor matrix
state_covariance <- cor(state_data_long)
state_covariance

# Formula is
# a*(lambda*C + (1-lambda)*C_1)
# where C is our correlation matrix with min .3
# and C_1 is a sq matrix with all 1's
# lambda=0 is 100% correlation, lambda=1 is our corr matrix
C <-  cor(state_data_long)
C[C < 0.3] <- 0.3 # baseline correlation for national poll error

tmp_C <- C
diag(tmp_C) <- NA
mean(tmp_C, na.rm = T)

lambda <- 0.5
C_1 <- matrix(data = 1, nrow = 51, ncol = 51)
a <- 1
nat_vote_scale <- 0.01
new_C <- nat_vote_scale * (lambda * C + (1 - lambda) * C_1) %>% make.positive.definite()

tmp <- new_C
diag(tmp) <- NA
mean(tmp, na.rm = T)

state_correlation_polling <- new_C

# make pos definite, and save#
state_correlation_polling <- make.positive.definite(state_correlation_polling)

# covariance matrix for polling error
state_covariance_polling_bias <- cov_matrix(51, 0.078^2, 0.9) #3.4% on elec day
state_covariance_polling_bias <- state_covariance_polling_bias * state_correlation_polling

sqrt(t(state_weights) %*% state_covariance_polling_bias %*% state_weights) / 4
mean(apply(MASS::mvrnorm(1000, rep(0, 51), state_covariance_polling_bias), 2, sd) / 4)

# covariance for prior e-day prediction
state_covariance_mu_b_T <- cov_matrix(n = 51, sigma2 = 0.17^2, rho = 0.9) # 6% on elec day
state_covariance_mu_b_T <- state_covariance_mu_b_T * state_correlation_polling

sqrt(t(state_weights) %*% state_covariance_mu_b_T %*% state_weights) / 4
mean(apply(MASS::mvrnorm(1000, rep(0, 51), state_covariance_mu_b_T), 2, sd) / 4)

# covariance matrix for random walks
state_covariance_mu_b_walk <- cov_matrix(51, (0.016)^2, 0.9)
state_covariance_mu_b_walk <- state_covariance_mu_b_walk * state_correlation_polling # we want the demo correlations for filling in gaps in the polls

(sqrt(t(state_weights) %*% state_covariance_mu_b_walk %*% state_weights) / 4) * sqrt(300)
mean(apply(MASS::mvrnorm(100,rep(0,51),state_covariance_mu_b_walk),2,sd) /4) * sqrt(300)

## MAKE DEFAULT COV MATRICES
# we're going to pass 3 scaling values to stan, where the 3 values are:
# (1) the national sd on the polls,
# (2) the national sd on the prior and
# (3) the national sd of the random walk
# make initial covariance matrix (using specified correlation)
state_covariance_0 <- cov_matrix(51, 0.078^2, 0.9)
state_covariance_0 <- state_covariance_0 * state_correlation_polling # we want the demo correlations for filling in gaps in the polls

# save the initial scaling factor
national_cov_matrix_error_sd <- sqrt(t(state_weights) %*% state_covariance_0 %*% state_weights) %>% as.numeric()

# save the other scales for later
fit_rmse_day_x <- function(x){0.03 +  (10^-6.6)*(x)^2} # fit to error from external script
fit_rmse_day_x(0:300)
days_til_election <- as.numeric(difftime(election_day, RUN_DATE))
expected_national_mu_b_T_error <- fit_rmse_day_x(days_til_election)

polling_bias_scale <- 0.014 # on the probability scale -- we convert later down
mu_b_T_scale <- expected_national_mu_b_T_error # on the probability scale -- we convert later down
random_walk_scale <- 0.05 / sqrt(300) # on the probability scale -- we convert later down

# gen fake matrices, check the math (this is recreated in stan)
national_cov_matrix_error_sd <- sqrt(t(state_weights) %*% state_covariance_0 %*% state_weights) %>% as.numeric()

ss_cov_poll_bias = state_covariance_0 * (polling_bias_scale / national_cov_matrix_error_sd * 4)^2
ss_cov_mu_b_T = state_covariance_0 * (mu_b_T_scale / national_cov_matrix_error_sd * 4)^2
ss_cov_mu_b_walk = state_covariance_0 * (random_walk_scale / national_cov_matrix_error_sd * 4)^2

sqrt(t(state_weights) %*% ss_cov_poll_bias %*% state_weights) / 4
sqrt(t(state_weights) %*% ss_cov_mu_b_T %*% state_weights) / 4
sqrt(t(state_weights) %*% ss_cov_mu_b_walk %*% state_weights) / 4 * sqrt(300)

#Checking#
check_cov_matrix(state_covariance_polling_bias)
check_cov_matrix(state_covariance_mu_b_T)
check_cov_matrix(state_covariance_mu_b_walk)

###############################################################
##### Creating priors --------------
###############################################################
# read in basic abramowitz data
# abramowitz <- read_excel("abramowitz_data_2024.xlsx") %>%
abramowitz <- read_csv("data/abramowitz_data_2024.csv") %>%
  filter(year < 2024)

election_years <- abramowitz %>%
  filter(year >= 1960) %>%
  pull(year)

election_years <- c(election_years, 2024)

real_inc <- fredr::fredr(series_id = "DSPIC96") %>%
  mutate(year = year(date)) %>%     # Extract year from date
  group_by(year) %>%                # Group by year
  summarise(total_value = sum(value)) %>%
  filter(year != 1959)

abramowitz <- abramowitz %>%
  left_join(real_inc, join_by(year)) %>%
  rename(real_inc = total_value)

# weighted_avg_4_years(real_inc, election_years)

# run TFC model
prior_model <- lm(
  formula = incvote ~  juneapp + q2gdp + term1 + real_inc, # model equation
  data = abramowitz # source data for TFC model
)

# make predictions from TFC (see session 3)
national_mu_prior <- predict(object = prior_model, # your TFC model
                             newdata = tibble(q2gdp = 3.0, # these values are for 2024 predictions
                                              juneapp = 0,
                                              term1 = 1,
                                              real_inc = 140234.5))
# pur predictions on correct scale
national_mu_prior <- national_mu_prior / 100

# Mean of mu_b_prior PER STATE (from TFC model)
mu_b_prior <- logit(national_mu_prior + prior_diff_score)
all(names(mu_b_prior) == names(prior_diff_score)) # check all mu_b_prior names in correct order

# use state mu_b_prior values to set national_mu_prior
national_mu_prior <- weighted.mean(inv.logit(mu_b_prior), state_weights)

# declare on screen the national prior + SE of the prior on Harris vote share
cat(sprintf('Prior Harris two-party vote is %s\nWith a standard error of %s',
            round(national_mu_prior,3),
            round(median(mu_b_T_scale),3)
)
)

###############################################################
## --- Adjustment national v state polls
###############################################################
## adjusting polling houses ##
adjusters <- c(
  "NYT",
  "YouGov",
  "Uni Mass",
  "Emerson College"
)


df %>%
  filter((pollster %in% adjusters)) %>%
  pull(pollster) %>%
  unique()

###############################################################
# Passing the data to Stan and running the model ---------
###############################################################
N_state_polls <- nrow(df %>% filter(index_s != 52))
N_national_polls <- nrow(df %>% filter(index_s == 52))

N_state <- nrow(df %>% filter(index_s != 52))
N_national <- nrow(df %>% filter(index_s == 52))
T <- as.integer(round(difftime(election_day, first_day)))
S <- 51
P <- length(unique(df$pollster))
M <- length(unique(df$mode))
Pop <- length(unique(df$polltype))
state <- df %>% filter(index_s != 52) %>% pull(index_s)

day_national <- df %>% filter(index_s == 52) %>% pull(poll_day) %>% as.integer
day_state <- df %>% filter(index_s != 52) %>% pull(poll_day) %>% as.integer

poll_national <- df %>% filter(index_s == 52) %>% pull(index_p)
poll_state <- df %>% filter(index_s != 52) %>% pull(index_p)

poll_mode_national <- df %>% filter(index_s == 52) %>% pull(index_m)
poll_mode_state <- df %>% filter(index_s != 52) %>% pull(index_m)

poll_pop_national <- df %>% filter(index_s == 52) %>% pull(index_pop)
poll_pop_state <- df %>% filter(index_s != 52) %>% pull(index_pop)

# data ---
n_democrat_national <- df %>% filter(index_s == 52) %>% pull(n_harris)
n_democrat_state <- df %>% filter(index_s != 52) %>% pull(n_harris)

n_two_share_national <- df %>% filter(index_s == 52) %>% transmute(n_two_share = n_trump + n_harris) %>% pull(n_two_share)
n_two_share_state <- df %>% filter(index_s != 52) %>% transmute(n_two_share = n_trump + n_harris) %>% pull(n_two_share)

unadjusted_national <- df %>% mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% filter(index_s == 52) %>% pull(unadjusted)
unadjusted_state <- df %>% mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% filter(index_s != 52) %>% pull(unadjusted)

polling_bias_scale <- as.numeric(polling_bias_scale) * 4
mu_b_T_scale <- as.numeric(mu_b_T_scale) * 4
random_walk_scale <- as.numeric(random_walk_scale) * 4
###############################################################
# data ---
data <- list(
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
)

###############################################################
### Run the model ###
###############################################################
out <- stan(file = "stan/poll_model_2024_oldcovmats.stan", # this is stan model declared above
            data = data, # data list declared above
            chains  = n_chains, # number of simultaneous Markov chains
            warmup = n_warmup, # number of iterations spent on burn in
            iter = n_iter, # number of iterations per chain (more -> + time, better convergence)
            cores = n_cores,
            refresh = n_refresh
)

saveRDS(out, file = "_output/new_output1.rds")

tictoc::toc()
