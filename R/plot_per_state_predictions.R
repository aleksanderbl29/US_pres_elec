plot_state_votes <- function(election_performance) {

  states <- election_performance %>%
    filter(t == max(t),
           state != "--") %>%
    arrange(desc(mean_dem)) %>%
    pull(state)

  swing_states <- c(
    "AZ", "GA", "MI", "NV", "NC", "PA", "WI"
  )

  election_performance %>%
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
              nudge_y = c(.07, .06, .08), size = 2, na.rm = TRUE) +
    labs(color = NULL, x = NULL, y = NULL, shape = NULL) +
    coord_flip() +
    theme_classic() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.8),
      axis.text.y = element_text(size = 5)
    )
}

plot_map_state_votes <- function(election_performance) {
  states <- election_performance %>%
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
    left_join(election_performance, by = c("abbr" = "state")) %>%
    mutate(state = abbr) %>%
    filter(!is.na(model_right))

  right_wrong <- data %>%
    st_centroid() %>%
    select(model_right, model_diff) %>%
    filter(!model_right) %>%
    mutate(model_right = "Wrong winner \nprediction")

  dem_dif <- max(data$model_diff * 100) %>% ceiling()
  rep_dif <- min(data$model_diff * 100) %>% floor()

  data %>%
    mutate(model_diff = model_diff * 100) %>%
    ggplot(aes(fill = model_diff)) +
    geom_sf(color = "black") +
    geom_sf(data = right_wrong, aes(color = model_right, shape = model_right),
            size = 3) +
    scale_fill_steps(low = "#E40A04", high = "#3A4EB1",
                     show.limits = TRUE,
                     breaks = c(rep_dif, seq(-2, 2, 2), dem_dif),
                     labels = function(x) round(x)) +
    scale_color_manual(values = c("red")) +
    scale_shape_manual(values = c(20)) +
    labs(fill = "Model \ndifference",
         shape = NULL,
         color = NULL) +
    cowplot::theme_map() +
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
}

