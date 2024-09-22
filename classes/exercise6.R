library(tidyverse)
library(corrplot)
library(mapdata)

dataset_file <- "../6 - Sociodemographics/States.Rda"
load(dataset_file)

States


# Question 1 --------------------------------------------------------------

# Discuss what vars are imported and if any are missing that could be important.

# Question 2 --------------------------------------------------------------

pallettes <- names(wesanderson::wes_palettes)

set.seed(42)
sample(pallettes, 1)

plt <- wesanderson::wes_palette("Chevalier1")

States %>%
  ggplot(aes(x = pct_white_evangel, y = dem)) +
  geom_point(color = plt[1]) +
  geom_smooth(method = "lm", color = plt[2]) +
  geom_text(vjust = 2.5,
            size = 2.5, aes(label = state), color = plt[1]) +
  theme_minimal()

States %>%
  ggplot(aes(x = average_log_pop_within_5_miles, y = dem)) +
  geom_point(color = plt[1]) +
  geom_smooth(method = "lm", color = plt[2]) +
  geom_text(vjust = 2.5,
            size = 2.5, aes(label = state), color = plt[1]) +
  theme_minimal()

States %>%
  ggplot(aes(x = college_pct, y = dem)) +
  geom_point(color = plt[1]) +
  geom_smooth(method = "lm", color = plt[2]) +
  geom_text(vjust = 2.5,
            size = 2.5, aes(label = state), color = plt[1]) +
  theme_minimal()

# Question 3 --------------------------------------------------------------
# Using the corrplot package, explore correlations between the variables
# (you can exclude variables such as state name or identifier).
# What do you observe?

States %>%
  select(-state, -state_name) %>%
  cor() %>%
  corrplot()
States %>%
  select(-state, -state_name) %>%
  cor() %>%
  corrplot(method = "number")
States %>%
  select(-state, -state_name) %>%
  cor() %>%
  corrplot(method = "color")

# Question 4 --------------------------------------------------------------
# Next, we are going to begin unpacking how we might expect states to covary
# depending on these variables of interest. First, create a new dataset containing
# only the following variables: state, dem, other, pct_white_evangel, pop,
# average_log_pop_within_5_miles, white_pct, black_pct, hisp_other_pct,
# college_pct, and median_age.
# Second, convert the dataframe from wide to long format,
# such that each row captures the value for a different variable for a given state.
# (Thus, there should be 10 rows per country, one for each variable.)

subset <- States %>%
  select(state, dem, other, pct_white_evangel, pop,
         average_log_pop_within_5_miles, white_pct, black_pct,
         hisp_other_pct, college_pct, median_age) %>%
  pivot_longer(cols = -state, names_to = "variable", values_to = "value")

head(subset, 15)
summary(subset)

# Rescale each variable using min-max scaling.
subset_min_max <- subset %>%
  group_by(variable) %>%
  mutate(value = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup()

# Convert the dataframe from long to wide format, where each row is a variable, each
# column is a state, and each cell is a rescaled value.
subset_wide <- subset_min_max %>%
  pivot_wider(names_from = state, values_from = value) %>%
  select(-variable)

# Calculate between-state correlations, and map correlations (use Illinois as a reference
# state). Hint: you can use the maps package, and run the command
# (usmap <- map_data("state")) to get state coordinates

usmap <- map_data("state")

# Calculate correlations with Illinois
illinois_value <- subset_wide %>% select(IL)

correlations_il <- cor(subset_wide, illinois_value) %>%
  as.data.frame(rownames(.)) %>%
  rownames_to_column("state") %>%
  as_tibble()

# Merge correlation data with map data
map_data <- usmap %>%
  mutate(abbr = usdata::state2abbr(region)) %>%
  filter(abbr %in% correlations_il$state) %>%
  left_join(correlations_il, by = c("abbr" = "state")) %>%
  rename(corr = IL)

# Plot the map
ggplot(map_data, aes(long, lat, group = group, fill = corr)) +
  geom_polygon(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation with Illinois", fill = "Correlation")

# Calculate correlations with Texas
texas_value <- subset_wide %>% select(TX)

correlations_tx <- cor(subset_wide, texas_value) %>%
  as.data.frame(rownames(.)) %>%
  rownames_to_column("state") %>%
  as_tibble()

# Merge correlation data with map data
map_data <- usmap %>%
  mutate(abbr = usdata::state2abbr(region)) %>%
  filter(abbr %in% correlations_tx$state) %>%
  left_join(correlations_tx, by = c("abbr" = "state")) %>%
  rename(corr = TX)

# Plot the map
ggplot(map_data, aes(long, lat, group = group, fill = corr)) +
  geom_polygon(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation with Illinois", fill = "Correlation")
