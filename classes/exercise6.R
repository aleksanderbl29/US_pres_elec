library(tidyverse)
library(corrplot)
library(mapdata)
library(usdata)
library(patchwork)

dataset_file <- "../6 - Sociodemographics/States.Rda"
load(dataset_file)

old_data <- States

updated_dataset_file <- "../6 - Sociodemographics/ACS_2023.Rda"
load(updated_dataset_file)

States <- ACS_2023 %>%
  rename(pct_white_evangel = white_evangel_pct,
         pop = pop_total) %>%
  mutate(state_name = usdata::abbr2state(state),
         average_log_pop_within_5_miles = NA)

colnames(old_data)
colnames(States)

rm(ACS_2023, dataset_file, updated_dataset_file)

testthat::expect_equal(
  sort(colnames(old_data)),
  sort(colnames(States))
)


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
  ggplot(aes(x = hisp_other_pct, y = dem)) +
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

corrplot_data <- States %>%
  filter(state != "AK",
         state != "DC") %>%
  select(-state, -state_name, -state_fips, -average_log_pop_within_5_miles) %>%
  select(order(colnames(.)))

colnames(corrplot_data) <- c(
  "Black", "College", "Democrat", "Other eth",
  "Median Age", "Other", "White evang", "Population",
  "Density", "Republican", "Total Votes", "White",
  "White Working Class"
)


corrplot_data %>%
  cor(use = 'complete.obs') %>%
  corrplot(type = "upper", order = "original", tl.col = "black", tl.srt = 45)

# Question 4 --------------------------------------------------------------
# Next, we are going to begin unpacking how we might expect states to covary
# depending on these variables of interest. First, create a new dataset containing
# only the following variables: state, dem, other, pct_white_evangel, pop,
# average_log_pop_within_5_miles, white_pct, black_pct, hisp_other_pct,
# college_pct, and median_age.
# Second, convert the dataframe from wide to long format,
# such that each row captures the value for a different variable for a given state.
# (Thus, there should be 10 rows per country, one for each variable.)

# Create new dataset with only the selected vars
subset <- States %>%
  filter(!is.na(state)) %>%
  arrange(state) %>%
  # select(state, dem, other, pct_white_evangel, pop,
  #        #average_log_pop_within_5_miles,
  #        white_pct, black_pct,
  #        hisp_other_pct, college_pct, median_age) %>%
  select(c(state, pct_white_evangel, average_log_pop_within_5_miles, white_pct,
           black_pct, hisp_other_pct, college_pct, median_age, wwc_pct)) %>%
  pivot_longer(cols = -state, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  arrange(state, variable)

head(subset, 15)
summary(subset)

# Rescale each variable using min-max scaling.
subset_min_max <- subset %>%
  group_by(variable) %>%
  mutate(value = (value - min(value, na.rm = TRUE)) /
           (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
  ungroup()

# Convert the dataframe from long to wide format, where each row is a variable, each
# column is a state, and each cell is a rescaled value.
subset_wide <- subset_min_max %>%
  pivot_wider(names_from = state, values_from = value) # %>% select(-variable)

# Calculate between-state correlations, and map correlations (use Illinois as a reference
# state). Hint: you can use the maps package, and run the command
# (usmap <- map_data("state")) to get state coordinates

usmap <- map_data("state")

names <- States %>%
  mutate(region = tolower(state_name)) %>%
  rename(abbr = state) %>%
  select(abbr, region)

# Calculate state correlations for all states
correlations <- subset_wide %>%
  select(-variable) %>%
  cor() %>%
  as_tibble() %>%
  mutate(state = colnames(.)) %>%
  relocate(state, .before = AK)

correlations_il <- correlations %>%
  mutate(IL = NA) %>%
  pivot_longer(cols = -state, names_to = "abbr", values_to = "cor") %>%
  filter(state == "IL") %>%
  left_join(names)

# Merge correlation data with map data
map_data_il <- usmap %>%
  filter(region %in% correlations_il$region) %>%
  left_join(correlations_il, by = "region")

# Plot the map
il_map <- ggplot(map_data_il, aes(long, lat, group = group, fill = cor)) +
  geom_polygon(color = "black") +
  scale_fill_gradient2(low = "red", high = "lightgreen", mid = "white",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation with Illinois", fill = "Correlation")
il_map

# Calculate correlations with Texas
texas_value <- subset_wide %>% select(TX)

correlations_tx <- subset_wide %>%
  select(-variable) %>%
  mutate(TX = NA) %>%
  cor(., texas_value) %>%
  as.data.frame(rownames(.)) %>%
  rownames_to_column("state") %>%
  as_tibble()

# Merge correlation data with map data
map_data_tx <- usmap %>%
  mutate(abbr = usdata::state2abbr(region)) %>%
  filter(abbr %in% correlations_tx$state) %>%
  left_join(correlations_tx, by = c("abbr" = "state")) %>%
  rename(corr = TX)

# Plot the map
tx_map <- ggplot(map_data_tx, aes(long, lat, group = group, fill = corr)) +
  geom_polygon(color = "black") +
  scale_fill_gradient2(low = "red", high = "lightgreen", mid = "white",
                       midpoint = 0, limits = c(-1, 1)) +
  # geom_label(aes(label = region)) +
  theme_minimal() +
  labs(title = "Correlation with Texas", fill = "Correlation")
tx_map

(il_map + plot_spacer()) / (plot_spacer() + tx_map)

design <- "AA#
           BB#"
wrap_plots(A = il_map, B = tx_map, design = design) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        legend.justification = "right")





