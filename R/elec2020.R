import_2020_data <- function(file, state_abb_list) {
  states <- read_csv(file) %>%
    mutate(score = biden_count / (biden_count + trump_count), # state-level dem % of 2-party vote 2020
           national_score = sum(biden_count) / sum(biden_count + trump_count), # nat. dem % of 2-party vote 2020
           delta = score - national_score, # state diffs vs. national
           share_national_vote = (total_count * (1 + adult_pop_growth_2020_23)) # two-party vote weighted by pop growth
           / sum(total_count * (1 + adult_pop_growth_2020_23))) %>% # divided by total voters weighted by pop growth
    arrange(state) # alphabetize by state code

  rownames(states) <- state_abb_list
  return(states)
}

add_state_names <- function(states, state_abb_list) {
  state_name <- states$state_name
  names(state_name) <- state_abb_list
  return(state_name)
}

get_prior_diff_score <- function(states, state_abb_list) {
  # set prior differences
  prior_diff_score <- states$delta
  names(prior_diff_score) <- state_abb_list # label prior diff scores w/state abbreviation
  return(prior_diff_score)
}

get_state_weights <- function(states, state_abb_list) {
  # set state weights relative to national vote (for weighted averages)
  state_weights <- states$share_national_vote / sum(states$share_national_vote)
  names(state_weights) <- state_abb_list # label state weights w/state abbreviation
  return(state_weights)
}

get_electoral_votes <- function(states, state_abb_list) {
  # electoral votes, by state:
  ev_state <- states$ev
  names(ev_state) <- state_abb_list # label electoral votes w/ state abbreviation
  return(ev_state)
}
