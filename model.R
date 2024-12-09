## econ2024_DB_Harris.R
## Building on 'final_2016.R' run Econ. model on the 2024 data
## adaptation by Derek Beach, Nicholas Beach Johansen and Nicholas Haas
## further adapted by Aleksander Bang-Larsen

###############################################################
## Setup for runtime
###############################################################

options(mc.cores = parallel::detectCores())
n_chains <- 4
n_cores <- getOption("mc.cores")
n_warmup <- 1000
n_iter <- 3500
n_sampling <- n_iter * 0.1
n_refresh <- n_sampling * 0.1

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
}

###############################################################
## Setup for dates
###############################################################
RUN_DATE <- today() # Grab today's date as "run date" (varies depending on today's date)
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

  hist(s_diag,breaks=10)

  print(sprintf('national sd of %s',round(nat_product,4)))
}

###############################################################
## wrangle polls ------------------------------------------
###############################################################


# select relevant columns from all polls 2024 (post-Harris), create one row per poll#
sample_data <- read_csv("data/sample_poll_list.csv") # Recall: you can update polls from 538

sample_data %>%
  select(state, region) %>%
  distinct() %>%
  write_csv("data/regions.csv")

sample_data %>% select(pollster_rating_name, pollster_wt) %>%
  distinct() %>%
  write_csv("data/pollster_ratings.csv")

# Get regions
regions <- read_csv("data/regions.csv")

# Get pollster ratings
pollster_ratings <- read_csv("data/pollster_ratings.csv")

rm(sample_data)

if (!exists("sourced")) {
  url <- "https://projects.fivethirtyeight.com/2024-general-data/538_2024_election_forecast_data.zip"
  destfile <- "fivethirtyeight_data"
  download_folder_name <- "data/downloads/538_2024_election_forecast_data"
  filename <- "data/poll_list.csv"
  download.file(url, destfile)
  unzip(destfile, overwrite = TRUE)
  file.copy(from = paste0(download_folder_name, "/", filename),
            to = filename,
            overwrite = TRUE)
  file.remove(destfile)
}

polls <- read_csv("data/poll_list.csv") %>%
  mutate(
    grp = paste(poll_id, pollster_rating_id),
    pollpollsterID = match(grp, unique(grp))
  ) %>%
  filter(
    candidate_name == "Kamala Harris" | candidate_name == "Donald Trump"
  ) %>%
  group_by(pollpollsterID) %>%
  mutate(
    trump = case_when(candidate_name == "Donald Trump" ~ pct),
    harris = case_when(candidate_name == "Kamala Harris" ~ pct)
  ) %>%
  fill(trump, .direction = "up") %>%
  fill(harris) %>%
  filter(row_number(pollpollsterID) == 1) %>%
  ungroup() %>%
  dplyr::select(
    state_abb,
    pollster_rating_name,
    sample_size,
    population,
    methodology,
    start_date,
    end_date,
    region,
    region_computed,
    pollster_wt,
    harris,
    trump
  )# %>% # pick variables to keep (do not have data on undecided and other - could include later)

write_csv(polls, file = glue::glue("data/old_data/poll_list/poll_list_{today()}.csv"))

rm(regions, pollster_ratings, polls)

# basic mutations
df <- read_csv(file = glue::glue("data/old_data/poll_list/poll_list_{today()}.csv")) %>%
  mutate(
    begin = ymd(start_date),
    end = ymd(end_date),
    t = begin + (as.numeric(end - begin) / 2)
  )  # Assign poll date to middle of field period


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
  TRUE ~ "Other"
  #TRUE ~ as.character(df$mode)
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
vote_data <- read_csv("data/potus_results_76_20.csv")
vote_data <- vote_data %>%
  select(year, state, dem) %>%
  group_by(state) %>%
  mutate(dem = dem) %>%
  select(state,
         variable = year,
         value = dem)  %>%
  ungroup() %>%
  na.omit %>%
  filter(variable >= 1996)

vote_data_correlation <- vote_data %>%
  group_by(variable) %>%
  filter(state != "DC") %>%
  # scale all variables (value - minimum) / original range --> varies b/w 0 and 1
  mutate(value = (value - min(value, na.rm=T)) /
           (max(value, na.rm=T) - min(value, na.rm=T))) %>%
  # now spread
  spread(state, value) %>%
  na.omit() %>%
  ungroup() %>%
  select(-variable)

#Census data#
census <- read_csv("data/acs_2023_variables.csv")
socio_dem <- census %>%
  filter(!is.na(state)) %>%
  filter(state != "DC") %>%
  select(-c(state_fips, pop_total, pop_density, black_pct, hisp_pct)) %>%
  group_by(state) %>%
  gather(variable,
         value,
         1:(ncol(.)-1)) %>%
  ungroup()

#Combine with state vote returns from 2020#
socio_dem <- socio_dem %>%
  mutate(variable = as.character(variable))

# add urbanicity
urbanicity <- read.csv("data/urbanicity_index.csv") %>%
  dplyr::select(state,pop_density = average_log_pop_within_5_miles) %>%
  gather(variable,
         value,
         2:(ncol(.)))

#Combine#
socio_dem <- socio_dem %>%
  bind_rows(urbanicity)

#Add percentage white evangelical#
white_evangel_pct <- read_csv("data/white_evangel_pct.csv") %>%
  gather(variable,
         value,
         2:(ncol(.)))

#Combine#
socio_dem <- socio_dem %>%
  bind_rows(white_evangel_pct)


##### our modification
#add Median income
Median_income <- read.csv("data/MedianHouseholdIncome2023_Abbrev.csv") %>%
  gather(variable,
         value,
         2:(ncol(.)))

#Combine#
socio_dem <- socio_dem %>%
  bind_rows(Median_income)


##### our modification
#add religious unaffiliated
ReligiousUnaffiliated <- read_csv("data/ReligiousUnaffiliated_Abbrev.csv") %>%
  gather(variable, value, 2:(ncol(.))) %>%
  add_row(state = "DC",
          variable = "Unaffiliated (%)",
          value = 24) %>%
  arrange(state)
# source ->
# https://www.pewresearch.org/religious-landscape-study/database/metro-area/washington-dc-metro-area/

socio_dem <- socio_dem %>%
  bind_rows(ReligiousUnaffiliated)

#Black_pct without black men, age 18-29
library(readxl)
black_pct_without <- read_excel("data/share_18-29_blacks_1.xlsx",
                                sheet = "Without")
library("readxl")
states_2020 <- read_excel("data/2020.xlsx") %>%
  select(c(state, state_name))

black_pct_without <- black_pct_without %>%
  left_join(states_2020,
            by = c("state" = "state_name")) %>%
  select('state.y', 'black_pct_without') %>%
  rename('state' = 'state.y')

black_pct_without <- black_pct_without %>%
  gather(variable,
         value,
         2:(ncol(.)))

socio_dem <- socio_dem %>%
  bind_rows(black_pct_without)

#add hispanics witout Cubans
hispanics_without_cuban <- read.csv("data/noncubanhispanics.csv") %>%
  gather(variable,
         value,
         2:(ncol(.)))

#Combine#
socio_dem <- socio_dem %>%
  bind_rows(hispanics_without_cuban)

#Correlation socio
socio_dem_correlation <- socio_dem %>%
  group_by(variable) %>%
  filter(state != "DC") %>%
  # scale all variables (value - minimum) / original range --> varies b/w 0 and 1
  mutate(value = (value - min(value, na.rm=T)) /
           (max(value, na.rm=T) - min(value, na.rm=T))) %>%
  # now spread
  spread(state, value) %>%
  na.omit() %>%
  ungroup() %>%
  select(-variable)

###############################################################
## An option: add region#
# Læs regionsdata ind og vælg relevante kolonner
library(readxl)
regions <- read_csv("data/US_States_by_Region.csv") %>%
  rename(state = State,
         region = Region)

regions <- regions %>%
  select(state = state, region) %>%       # Vælg staten og regionskolonnerne
  mutate(value = 1) %>%                       # Opret en dummy for hver region
  spread(region, value)                       # Konverter region til brede dummy-variabler

# Erstat NA'er med 0
regions[is.na(regions)] <- 0

# Konverter data tilbage til lang form
regions <- regions %>%
  gather(variable, value, 2:ncol(.))          # Saml igen for at få variabel-kolonnen

###############################################################

regions_correlation <- regions %>%
  group_by(variable) %>%
  filter(state != "DC") %>%
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

# make cor matrix
vote_state_covariance <- cor(vote_data_correlation)
vote_state_covariance %>%
  corrplot::corrplot(type = "upper")

socio_state_covariance <- cor(socio_dem_correlation)
socio_state_covariance%>%
  corrplot::corrplot(type = "upper")

regions_state_covariance <- cor(regions_correlation)
regions_state_covariance%>%
  corrplot::corrplot(type = "upper")

# Baseline cor
# baseline_cor <- matrix(data = 1, nrow = 51, ncol = 51)
baseline_cor <- matrix(data = 1, nrow = 50, ncol = 50)

# Defining weights estimated in seperate model
baseline_w <- 0.04175
vote_w <- 0.25825
region_w <- 0.1
demographic_w <- 0.6

# Constructing matrix
C <- (baseline_w * baseline_cor) +
  (vote_w * vote_state_covariance) +
  (region_w * regions_state_covariance) +
  (demographic_w * socio_state_covariance)

# baseline <- baseline_w * baseline_cor
# vote <- vote_w * vote_state_covariance
# region <- region_w * regions_state_covariance
# demographic <- demographic_w * socio_state_covariance

#Fix correlations < 1
C[C > 1] <- 1

# Adding DC
C <- cbind(C, c(rep(0, 50), 1))
C <- rbind(C, c(rep(0, 50), 1))
rownames(C)[51] <- "DC"
colnames(C)[51] <- "DC"

# Formula is
# a*(lambda*C + (1-lambda)*C_1)
# where C is our correlation matrix with min .3
# and C_1 is a sq matrix with all 1's
# lambda=0 is 100% correlation, lambda=1 is our corr matrix
#C[C < 0.3] <- 0.3 # baseline correlation for national poll error

tmp_C <- C
diag(tmp_C) <- NA
mean(tmp_C, na.rm = T)

lambda <- 0.5
C_1 <- matrix(data = 1, nrow = 51, ncol = 51)
C <- C[1:nrow(C_1), 1:ncol(C_1)]
a <- 1
#nat_vote_scale <- 0.01
nat_vote_scale <-1
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
###############################################################
# read in basic abramowitz data
abramowitz_2024 <- read.csv("data/abramowitz_data_2024.csv") %>%
  filter(year < 2024)

library(readxl)
Consumer_sentiment <- read_excel("data/Con_Sen.xls") %>%
  filter(year < 2024)

abramowitz <- abramowitz_2024  %>%
  left_join(Consumer_sentiment, by = "year")

library(readr)
most_important_issue <- read_csv("data/most_important_issue.csv")

abramowitz <- abramowitz %>%
  left_join(most_important_issue, by = "year")

abramowitz$issue_diff[abramowitz$year == 2000] <- median(abramowitz$issue_diff, na.rm = TRUE)

#Construction
construction <- read_csv("data/construction_season_adj.csv") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%                # Group by year
  summarise(total_value = sum(value)) %>%
  mutate(construction_rate = 100 * (total_value - lag(total_value))/lag(total_value)) %>%
  filter(year != 1970, # remove first year as rate cannot be calculated without data from prev. year
         year <= 2020)

construction_monthly <- read_csv("data/construction_season_adj.csv") %>%
  mutate(year = year(date),
         month_year = glue::glue("{year}_{month(date)}")) %>%     # Extract year from date
  group_by(month_year) %>%            # Group by month_year
  summarise(total_value = sum(value)) %>%
  mutate(construction_rate = 100 * (total_value - lag(total_value))/lag(total_value)) %>%
  filter(month_year != "1970_1") %>% # remove first year as rate cannot be calculated without data from prev. year
  mutate(year = as.double(substr(month_year, 1, 4))) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(mean_construction_rate = mean(construction_rate),
            median_construction_rate = median(construction_rate))

construction_median_2024 <- construction_monthly$median_construction_rate[construction_monthly$year == max(construction_monthly$year)]

construction <- construction %>%
  left_join(construction_monthly, join_by(year)) %>%
  select(-total_value)

abramowitz <- abramowitz %>%
  left_join(construction, by = "year")

# run  TFC model --> here we can make a change
prior_model <- lm(
  formula = incvote ~  juneapp + q2gdp + Con_Sen + issue_diff + construction_rate, # model equation
  data = abramowitz # source data for  model
)

# make predictions from  (see session 3)
national_mu_prior <- predict(object = prior_model, # your  model
                             newdata = tibble(q2gdp = 3, # these values are for 2024 predictions
                                              juneapp = -20, ## Bruger bidens model
                                              issue_diff = -5, ## forskel i issue - hvem de anser som værende bedst til at løse det issue de synes er bedst
                                              Con_Sen = 74.467,
                                              construction_rate = construction_median_2024)) ## Consumer sentiment i år

# put predictions on correct scale
national_mu_prior <- national_mu_prior / 100

# Make prior diff score smaller for swing states
swing_states <- c(
  "AZ", "GA", "MI", "NV", "NC", "PA", "WI"
)

options(scipen = 999)

for (state in swing_states) {
  cat("Old diff for ", state, prior_diff_score[state], "\n")
  prior_diff_score[state] <- prior_diff_score[state] * 0.25
  cat("New diff for ", state, prior_diff_score[state], "\n")
}

# Mean of mu_b_prior PER STATE (from  model)
mu_b_prior <- logit(national_mu_prior + prior_diff_score)
all(names(mu_b_prior) == names(prior_diff_score)) # check all mu_b_prior names in correct order

# use state mu_b_prior values to set national_mu_prior
national_mu_prior <- weighted.mean(inv.logit(mu_b_prior), state_weights)

# declare on screen the national prior + SE of the prior on Harris vote share
cat(sprintf('Prior Harris two-party vote is %s\nWith a standard error of %s',
            round(national_mu_prior,3),
            round(median(mu_b_T_scale),3)
))

## MAKE PRIOR CONFIDENCE WIDER

###############################################################
## --- Adjustment national v state polls
###############################################################
## adjusting polling houses ##
adjusters <- c(
  "Marquette",
  "Change Research",
  "Data for Progress",
  "Fabrizio",
  "Big Village",
  "Florida Atlantic",
  "SoCal Research",
  "Echelon Insights",
  "co/efficient",
  "Trafalgar Group",
  "J.L. Partners",
  "Survey Center",
  "YouGov/SNF Agora",
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
T <- as.integer(round(difftime(election_day, start_date)))
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

if (!exists("sourced")) {

  ###############################################################
  ### Run the model ###
  ###############################################################
  out <- stan(file = "stan/poll_model_2024_2.stan", # this is stan model declared above
              data = data, # data list declared above
              chains  = n_chains, # number of simultaneous Markov chains
              warmup = n_warmup, # number of iterations spent on burn in
              iter = n_iter, #number of iterations per chain
              cores = n_cores,
              refresh = n_refresh
  )

  saveRDS(out, file = "_output/Model output.rds")

}
