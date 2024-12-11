create_index <- function(df, state_abb_list) {
  df %>%
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
}
