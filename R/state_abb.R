get_state_abb <- function(file) {
  read_csv(file) %>%
    pull(state) %>%
    unique()
}
