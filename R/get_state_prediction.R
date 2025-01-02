get_state_prediction <- function(election_performance, input_state, type = "prediction") {
  state_data <- election_performance %>%
    filter(t == max(t)) %>%
    filter(state_name == input_state)

  if (type == "prediction") {
    state_data %>%
      pull(mean_dem) %>%
      format_num(big_mark = ",", decimal_mark = ".")
  } else if (type == "error") {
    state_data %>%
      pull(model_diff) %>%
      format_num(big_mark = ",", decimal_mark = ".")
  }
}
