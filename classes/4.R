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

# Allocate electoral votes
polls_full <- polls_full %>%
  mutate(clinton_votes = ifelse(winner == "Clinton", electoral_votes,
                                ifelse(winner == "Tie", electoral_votes / 2, 0)),
         trump_votes = ifelse(winner == "Trump", electoral_votes,
                              ifelse(winner == "Tie", electoral_votes / 2, 0)))

# Summarize the total electoral votes for each candidate
total_votes <- polls_full %>%
  summarise(
    clinton_total = sum(clinton_votes),
    trump_total = sum(trump_votes)
  )

# Print the total votes
print(total_votes)



## Q3
rm(list = ls())
library(tidyverse)

# Load the original dataset
poll2016_file <- "../4 - Data input & opinion polls_ Sampling and weighting, polling mode, likely-voter models/polls2016.csv"
polls <- read_csv(poll2016_file)

# 1. Function to compute average support within 30 days of the most recent poll
compute_average_support <- function(polls_data) {
  # Create data with the most recent poll and those within 30 days of it
  polls_sub_voter <- polls_data %>%
    group_by(state) %>%
    arrange(state, days_to_election) %>%
    mutate(recent_days_to_election = min(days_to_election)) %>%
    filter(days_to_election <= recent_days_to_election + 30) %>%
    ungroup()

  # Compute average support per state
  average_support_voter <- polls_sub_voter %>%
    group_by(state) %>%
    summarise(
      avg_Trump = mean(Trump, na.rm = TRUE),
      avg_Clinton = mean(Clinton, na.rm = TRUE),
      electoral_votes = first(electoral_votes)
    ) %>%
    ungroup()

  # Manually assign Washington D.C. to Clinton
  average_support_voter <- average_support_voter %>%
    mutate(
      winner = case_when(
        state == "District of Columbia" ~ "Clinton",  # Clinton wins DC
        avg_Clinton > avg_Trump ~ "Clinton",
        avg_Trump > avg_Clinton ~ "Trump",
        TRUE ~ "Tie"
      )
    )

  # Return the final dataset
  return(average_support_voter)
}

# Since there's no "voter type" column, we'll run the same calculation assuming all are either likely voters or registered voters
# 2. Likely voters subset (Assume polls represent likely voters)
likely_voters_avg <- compute_average_support(polls)

# 3. Registered voters subset (Also assume for this case)
registered_voters_avg <- compute_average_support(polls)

# 4. Function to allocate electoral votes based on winner
allocate_electoral_votes <- function(average_support_data) {
  average_support_data %>%
    mutate(
      clinton_votes = ifelse(winner == "Clinton", electoral_votes,
                             ifelse(winner == "Tie", electoral_votes / 2, 0)),
      trump_votes = ifelse(winner == "Trump", electoral_votes,
                           ifelse(winner == "Tie", electoral_votes / 2, 0))
    ) %>%
    summarise(
      clinton_total = sum(clinton_votes, na.rm = TRUE),
      trump_total = sum(trump_votes, na.rm = TRUE)
    )
}

# 5. Allocate electoral votes for likely voters (assuming likely voters)
total_votes_likely <- allocate_electoral_votes(likely_voters_avg)

# 6. Allocate electoral votes for registered voters (assuming registered voters)
total_votes_registered <- allocate_electoral_votes(registered_voters_avg)

# 7. Print the results
print("Total votes (likely voters):")
print(total_votes_likely)

print("Total votes (registered voters):")
print(total_votes_registered)


## Q4
rm(list = ls())
library(tidyverse)

# Load the original dataset
poll2016_file <- "../4 - Data input & opinion polls_ Sampling and weighting, polling mode, likely-voter models/polls2016.csv"
polls <- read_csv(poll2016_file)

# Function to compute average support based on the 3 latest polls per state for a given day
compute_average_support_daily <- function(polls_data, days_before_election) {
  # Filter polls up to the current day
  polls_daily <- polls_data %>%
    filter(days_to_election <= days_before_election) %>%
    group_by(state) %>%
    arrange(state, days_to_election) %>%
    slice_head(n = min(3, n())) %>%  # Handle fewer than 3 polls
    ungroup()

  # Compute average support per state
  average_support_daily <- polls_daily %>%
    group_by(state) %>%
    summarise(
      avg_Trump = mean(Trump, na.rm = TRUE),
      avg_Clinton = mean(Clinton, na.rm = TRUE),
      electoral_votes = first(electoral_votes)
    ) %>%
    ungroup()

  # Manually assign Washington D.C. to Clinton
  average_support_daily <- average_support_daily %>%
    mutate(
      winner = case_when(
        state == "District of Columbia" ~ "Clinton",  # Clinton wins DC
        avg_Clinton > avg_Trump ~ "Clinton",
        avg_Trump > avg_Clinton ~ "Trump",
        TRUE ~ "Tie"
      )
    )

  return(average_support_daily)
}

# Function to allocate electoral votes based on winner
allocate_electoral_votes <- function(average_support_data) {
  average_support_data %>%
    mutate(
      clinton_votes = ifelse(winner == "Clinton", electoral_votes,
                             ifelse(winner == "Tie", electoral_votes / 2, 0)),
      trump_votes = ifelse(winner == "Trump", electoral_votes,
                           ifelse(winner == "Tie", electoral_votes / 2, 0))
    ) %>%
    summarise(
      clinton_total = sum(clinton_votes, na.rm = TRUE),
      trump_total = sum(trump_votes, na.rm = TRUE)
    )
}

# Generate time series data: start from 60 days before the election
days_before_election <- 60
time_series_data <- tibble(
  days_before_election = numeric(),
  clinton_total = numeric(),
  trump_total = numeric()
)

for (day in 0:days_before_election) {
  # For each day, compute the average support and electoral votes
  average_support <- compute_average_support_daily(polls, day)
  total_votes <- allocate_electoral_votes(average_support)

  # Save results for plotting
  time_series_data <- time_series_data %>%
    add_row(
      days_before_election = day,
      clinton_total = total_votes$clinton_total,
      trump_total = total_votes$trump_total
    )
}

# Plot the time series of electoral votes
time_series_data %>%
  ggplot(aes(x = days_before_election)) +
  geom_line(aes(y = clinton_total, color = "Clinton")) +
  geom_line(aes(y = trump_total, color = "Trump")) +
  geom_hline(yintercept = 270, linetype = "dashed", color = "black", size = 0.8) +  # 270-vote winning line
  scale_x_reverse() +  # Reverse the x-axis to go from 60 days before to Election Day
  labs(
    title = "Predicted Electoral Votes Over Time (60 Days Before Election)",
    x = "Days Before Election",
    y = "Electoral Votes",
    color = "Candidate"
  ) +
  theme_minimal()


