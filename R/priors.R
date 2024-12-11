tfc <- function(abramowitz_file,
                consumer_sentiment_file,
                issue_file,
                construction_file,
                prior_diff_score,
                state_weights) {
  abramowitz <- read_csv(abramowitz_file) %>%
    filter(year < 2024)

  consumer_sentiment <- read_excel(consumer_sentiment_file) %>%
    filter(year < 2024)

  abramowitz <- abramowitz %>%
    left_join(consumer_sentiment, by = "year")

  most_important_issue <- read_csv(issue_file)

  abramowitz <- abramowitz %>%
    left_join(most_important_issue, by = "year")

  abramowitz$issue_diff[abramowitz$year == 2000] <- median(abramowitz$issue_diff, na.rm = TRUE)

  construction <- read_csv(construction_file) %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%                # Group by year
    summarise(total_value = sum(value)) %>%
    mutate(construction_rate = 100 * (total_value - lag(total_value))/lag(total_value)) %>%
    filter(year != 1970, # remove first year as rate cannot be calculated without data from prev. year
           year <= 2020)

  construction_monthly <- read_csv(construction_file) %>%
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
  predict(object = prior_model, # your  model
                               newdata = tibble(q2gdp = 3, # these values are for 2024 predictions
                                                juneapp = -20, ## Bruger bidens model
                                                issue_diff = -5, ## forskel i issue - hvem de anser som værende bedst til at løse det issue de synes er bedst
                                                Con_Sen = 74.467,
                                                construction_rate = construction_median_2024)) ## Consumer sentiment i år
}

get_mu_b_prior <- function(tfc_prediction, prior_diff_score) {

  # put predictions on correct scale
  national_mu_prior <- tfc_prediction / 100

  # Make prior diff score smaller for swing states
  swing_states <- c(
    "AZ", "GA", "MI", "NV", "NC", "PA", "WI"
  )

  for (state in swing_states) {
    cat("Old diff for ", state, prior_diff_score[state], "\n")
    prior_diff_score[state] <- prior_diff_score[state] * 0.25
    cat("New diff for ", state, prior_diff_score[state], "\n")
  }

  # Mean of mu_b_prior PER STATE (from  model)
  mu_b_prior <- logit(national_mu_prior + prior_diff_score)
  all(names(mu_b_prior) == names(prior_diff_score)) # check all mu_b_prior names in correct order
  return(mu_b_prior)
}

get_national_mu_prior <- function(mu_b_prior, state_weights) {
  # use state mu_b_prior values to set national_mu_prior
  national_mu_prior <- weighted.mean(inv.logit(mu_b_prior), state_weights)

  return(national_mu_prior)
}
