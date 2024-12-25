plot_state_votes <- function(data, election_results) {

  states <- data %>%
    filter(t == max(t),
           state != "--") %>%
    arrange(desc(mean_dem)) %>%
    pull(state)

  swing_states <- c(
    "AZ", "GA", "MI", "NV", "NC", "PA", "WI"
  )

  get_model_election_performance(data, election_results) %>%
    mutate(
      label = factor(state),
      winner = ifelse(mean_dem >= .5, 'Democratic', 'Republican')) %>%
    pivot_longer(cols = c(mean_dem, pct_dem), names_to = "value_from",
                 values_to = "votes") %>%
    mutate(pretty_text = if_else(label %in% swing_states &
                                   value_from == "mean_dem", votes, NA)) %>%
    ggplot(aes(x = label, y = votes, ymax = high_dem, ymin = low_dem,
               color = winner, shape = value_from)) +
    geom_point(size = 1, fill = "black") +
    geom_linerange(linewidth = 0.5,
                   alpha = .4) +
    geom_hline(yintercept = .5,
               linetype = "dashed") +
    scale_x_discrete(limits = states) +
    scale_color_manual(
      values = c('Democratic' = '#3A4EB1',
                 'Republican' = '#E40A04')) +
    scale_shape_manual(values = c(5, 21), labels = c("Our model", "Actual")) +
    geom_text(aes(label = round(pretty_text, digits = 2)),
              nudge_y = .08, size = 2, na.rm = TRUE) +
    labs(color = NULL, x = NULL, y = NULL, shape = NULL) +
    coord_flip() +
    theme_classic() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.8),
      axis.text.y = element_text(size = 5)
    )
}

plot_map_state_votes <- function(data, election_results) {
  states <- data %>%
    filter(t == max(t),
           state != "--") %>%
    arrange(desc(mean_dem)) %>%
    pull(state)

  swing_states <- c(
    "AZ", "GA", "MI", "NV", "NC", "PA", "WI"
  )

  us_median_age <- tidycensus::get_acs(
    geography = "state",
    variables = "B01002_001",
    year = 2019,
    survey = "acs1",
    geometry = TRUE,
    resolution = "20m"
  ) %>%
    tigris::shift_geometry()

  data <- us_median_age %>%
    mutate(abbr = usdata::state2abbr(NAME)) %>%
    select(abbr) %>%
    left_join(data, by = c("abbr" = "state")) %>%
    mutate(state = abbr)

  get_model_election_performance(data, election_results) %>%
    ggplot(aes(fill = model_diff, color = model_right)) +
    geom_sf() +
    scale_fill_gradient(low = "#E40A04", high = "#3A4EB1") +
    theme_map()
}

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





















