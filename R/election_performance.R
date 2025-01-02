get_election_results <- function() {
  # URL of the Wikipedia page
  url <- "https://en.wikipedia.org/wiki/2024_United_States_presidential_election#Results"

  # Read the webpage content
  webpage <- read_html(url)

  tables <- webpage %>%
    html_nodes("table") %>%
    html_table()

  table26 <- tables[[26]][2:(nrow(tables[[26]])-2), c(1, 6)]
  colnames(table26) <- c("state_name", "pct_dem")

  results <- table26 %>%
    mutate(
      state_name = gsub("\\[\\d+\\]", "", state_name),
      pct_dem_pretty = pct_dem,
      pct_dem = as.numeric(gsub("%", "", pct_dem))
    ) %>%
    mutate(
      state_name = sub("-.*", "", state_name)
    ) %>%
    mutate(
      state_name = gsub(" †", "", state_name)
    ) %>%
    filter(!state_name %in% c("NE", "ME")) %>%
    mutate(
      state = usdata::state2abbr(state_name)
    )

  return(results)

}

get_model_election_performance <- function(forecast, election_results) {
  swing_states <- c(
    "AZ", "GA", "MI", "NV", "NC", "PA", "WI"
  )

  forecast %>%
    filter(!is.na(t)) %>%
    filter(t == max(t), state != "--") %>%
    left_join(election_results, by = join_by(state)) %>%
    mutate(pct_dem = pct_dem / 100) %>%
    mutate(model_diff = mean_dem - pct_dem,
           model_normalized = mean_dem - .5,
           actual_normalized = pct_dem - .5) %>%
    mutate(model_right = sign(model_normalized) == sign(actual_normalized),
           swing_state = state %in% swing_states)

}

