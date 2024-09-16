library(tidyverse)

poll2016_file <- "../4 - Data input & opinion polls_ Sampling and weighting, polling mode, likely-voter models/polls2016.csv"
polls <- read_csv(poll2016_file)

polls_sub <- polls %>%
  group_by(state) %>%
  arrange(state, days_to_election) %>%  # Sorts by days to election in ascending order (smallest first)
  slice_head(n = 3) %>%  # Keeps the 3 rows with the smallest days to election
  ungroup()

nrow(polls_sub)
range(polls_sub$days_to_election)

average_support <- polls_sub %>%
  group_by(state) %>%
  summarise(avg_Trump = mean(Trump),
            avg_Clinton = mean(Clinton),
            electoral_votes = electoral_votes) %>%
  ungroup() %>% as_tibble()

average_support %>%
  ggplot(aes(x = avg_Clinton, avg_Trump, label = state)) +
  geom_point(size = 0.5) +
  geom_abline(slope = 1,
              color = "black") +
  lims(x = c(25,65),
       y = c(25,65)) +
  geom_smooth(method = "lm") +
  geom_text(vjust = 2.5,
            size = 2.5) +
  theme_bw()

#Predict winner based on average support
average_support <- average_support %>%
  mutate(winner = case_when(
    avg_Clinton > avg_Trump ~ "Clinton",
    avg_Trump > avg_Clinton ~ "Trump",
    TRUE ~ "Tie"  # Handle ties
  ))

#Create data with only 1 observation pr state
polls_sub2 <- polls %>%
  group_by(state) %>%
  arrange(state, days_to_election) %>%  # Sorts by days to election in ascending order (smallest first) %
  slice_head(n = 1) %>%  # Keeps the row with the smallest days to election
  ungroup()

#Join polls_sub2 with average data to get electoral votes
polls_full <- polls_sub2 %>%
  left_join(average_support, by = c("state" = "state", "electoral_votes" = "electoral_votes"))

#Allocate electoral votes
polls_full <- polls_full %>%
  mutate(clinton_votes = ifelse(winner == "Clinton", electoral_votes,
                                ifelse(winner == "Tie", electoral_votes / 2, 0)),
         trump_votes = ifelse(winner == "Trump", electoral_votes,
                              ifelse(winner == "Tie", electoral_votes / 2, 0)))

# Summarize the total electoral votes for each candidate --> Clinton wins with 334 electoral votes (Trump gets 204)
total_votes <- polls_full %>%
  summarise(
    clinton_total = sum(clinton_votes),
    trump_total = sum(trump_votes)
  )


winner <- average_support %>%
  mutate(winner = case_when(
    avg_Clinton > avg_Trump ~ "Clinton",
    avg_Trump > avg_Clinton ~ "Trump",
    TRUE ~ "Tie"  # Handle ties
  ))

polls_full2 <- polls_sub2 %>%
  left_join(average_support, by = c("state" = "state", "electoral_votes" = "electoral_votes"))

polls_full2 <- polls_full2 %>%
  mutate(clinton_votes = ifelse(winner == "Clinton", electoral_votes,
                                ifelse(winner == "Tie", electoral_votes / 2, 0)),
         trump_votes = ifelse(winner == "Trump", electoral_votes,
                              ifelse(winner == "Tie", electoral_votes / 2, 0)))

total_votes <- polls_full %>%
  summarise(
    clinton_total = sum(clinton_votes),
    trump_total = sum(trump_votes)
  )

# Ensure electoral_votes is retained from the original 'polls' data
average_support <- polls_sub %>%
  group_by(state) %>%
  summarise(avg_Trump = mean(Trump),
            avg_Clinton = mean(Clinton)) %>%
  left_join(polls %>% select(state, electoral_votes) %>% distinct(), by = "state") %>%
  ungroup()

# Predict winner based on average support
average_support <- average_support %>%
  mutate(winner = case_when(
    avg_Clinton > avg_Trump ~ "Clinton",
    avg_Trump > avg_Clinton ~ "Trump",
    TRUE ~ "Tie"  # Handle ties
  ))

# Create data with only 1 observation per state
polls_sub2 <- polls %>%
  group_by(state) %>%
  arrange(state, days_to_election) %>%  # Sort by days to election
  slice_head(n = 1) %>%  # Keep the row with the smallest days to election
  ungroup()

# Join polls_sub2 with average data to get electoral votes
polls_full <- polls_sub2 %>%
  left_join(average_support, by = c("state" = "state", "electoral_votes" = "electoral_votes"))

# Summarize the total electoral votes for each candidate
total_votes <- polls_full %>%
  summarise(
    clinton_total = sum(clinton_votes),
    trump_total = sum(trump_votes)
  )

# Print the total votes
print(total_votes)

