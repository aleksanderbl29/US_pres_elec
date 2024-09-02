# Dataset info ------------------------------------------------------------
## `intrade`
### Each row represents daily trading information about the contracts for
### either the Democratic or Republican Party nominee’s victory in
### a particular state.

# Name       | Description
# -----------|------------------------------------------------------------------
# day        | Date of the session
# statename  | Full name of each state (including District of Columbia)
# state      | Abbreviation of each state (including District of Columbia)
# PriceD     | Closing price (predicted vote share) of Democratic Nominee’s market
# PriceR     | Closing price (predicted vote share) of Republican Nominee’s market
# VolumeD    | Total session trades of Democratic Party Nominee’s market
# VolumeR    | Total session trades of Republican Party Nominee’s market

## `election`
### Election outcome data

# Name       | Description
# -----------|------------------------------------------------------------------
# state.name | Full name of state
# state      | Two letter state abbreviation
# Obama      | Vote percentage for Obama
# McCain     | Vote percentage for McCain
# EV         | Number of electoral college votes for this state

# Clear env
rm(list = ls())

# Load pkgs.
library(tidyverse)
library(tinytable)
library(patchwork)

# Question 1 --------------------------------------------------------------
## Read csv files to objects `intrade` and `election`
intrade_08_file <- "../2 - Structural fundamentals (economics, incumbency) or, It's the economy stupd/intrade08.csv"
intrade <- read.csv(intrade_08_file)

pres08_file <- "../2 - Structural fundamentals (economics, incumbency) or, It's the economy stupd/pres08.csv"
election <- read.csv(pres08_file)

## Joining dataframes into `df` by the state abbrv. & rename vars to snake_case
df <- election %>%
  mutate(state.name = if_else(state.name == "D.C.",
                              "District of Columbia",
                              state.name)) %>%
  inner_join(intrade, by = c("state" = "state", "state.name" = "statename")) %>%
  rename(obama = Obama,
         mccain = McCain,
         votes = EV,
         statename = state.name,
         close_price_dem = PriceD,
         close_price_rep = PriceR,
         volume_dem = VolumeD,
         volume_rep = VolumeR) %>%
  arrange(day)

## Create DaysToElection by subtracting the day of the election from each day
df <- df %>%
  mutate(days_to_election = ymd("2008-11-04") - ymd(day))

## Create state margin of victory variable and a betting market margin variable
df <- df %>%
  mutate(vict_margin = obama - mccain,
         bet_margin = close_price_dem - close_price_rep)


# Question 2 --------------------------------------------------------------
## Predict electoral margins from trading margins using a linear model
### Filter data
one_day_out <- df %>%
  filter(days_to_election == 1)

### Perform linear model
linear <- lm(vict_margin ~ bet_margin, one_day_out)
summary(linear)
summary(linear)$r.squared

### Predict election outcomes based on linear model
predictions <- predict(linear, se.fit = FALSE)

## Does the linear model predict well?
### Pull the actual winning margins for comparison
victory_margins <- one_day_out %>%
  pull(vict_margin)

### Calculate the predicion errors and getting the range
prediction_error <- predictions - victory_margins
range(prediction_error)

### Assign vectors their state names
names(predictions) <- one_day_out$statename
names(victory_margins) <- one_day_out$statename
names(prediction_error) <- one_day_out$statename

### Create tibble for presentation and calculations
pred_diffs <- tibble(
  one_day_out$statename,
  victory_margins,
  predictions,
  prediction_error,
  correct = (victory_margins < 0 & predictions < 0) | (victory_margins > 0 & predictions > 0)
)

#### Assign proper column names to tibble
colnames(pred_diffs) <- c(
  "State",
  "Victory Margin",
  "Betting Prediction",
  "Difference",
  "Correct prediction"
)

#### Summarising the tibble
summary(pred_diffs)

### I conclude that it does not predict it very well, numerically. We will
### discuss following the visualisations below.

## Visualize the prediction and variables together
### Present predictions and actuals in `{tinytable}`
pred_diffs %>%
  tt() %>%
  style_tt(j = 2:4, align = "r")

### Only show states where the prediction would assign the electoral college
### votes to the wrong candidate
pred_diffs %>%
  filter(!`Correct prediction`) %>%
  tt() %>%
  style_tt(j = 2:4, align = "r")

### Plot the predictions and actuals in a `{ggplot2}`
pred_diffs %>%
  ggplot(aes(x = `Betting Prediction`,
             y = `Victory Margin`)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.1) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.1) +
  geom_point(aes(shape = `Correct prediction`),
             color = ifelse(pred_diffs$`Victory Margin` > 0, "blue", "red"),
             fill = ifelse(pred_diffs$`Victory Margin` > 0, "blue", "red"),
             size = 3) +
  geom_abline(slope = linear$coefficients[2]) +
  # xlim(-max(abs(pred_diffs[3])), max(abs(pred_diffs[3]))) +
  ylim(min(pred_diffs[2:3]), max(abs(pred_diffs[2]))) +
  xlim(min(pred_diffs[2:3]), min(pred_diffs[2:3]) * -1) +
  scale_shape_manual(values = c(8, 1)) +
  theme_minimal() +
  theme(legend.position = "none")

### Get the ranges of margins from betting and the election as well as
### the prediction
range(predictions)
range(one_day_out$bet_margin)
range(one_day_out$vict_margin)


# Question 3 --------------------------------------------------------------
## Look at price evolution across states
## Fit a linear model to Obama's margin for different states over 20 days
## before the election

## Filtering data and running linear model for both states
wv_price_evolution <- df %>%
  filter(days_to_election >= 0 & days_to_election <= 20,
         statename == "West Virginia") %>%
  lm(bet_margin ~ days_to_election, data = .)

ma_price_evolution <- df %>%
  filter(days_to_election >= 0 & days_to_election <= 20,
         statename == "Massachusetts") %>%
  lm(bet_margin ~ days_to_election, data = .)

summary(wv_price_evolution)
summary(ma_price_evolution)

## Predicting votes
predict(wv_price_evolution)
predict(ma_price_evolution)

## Plotting market evolutions in `{ggplot2}` for comparison

### Creating function for calculating the amount of times the price data
### are updated in the dataset
price_update_calc <- function(col) {
  as.factor(cumsum(col != lag(col, default = first(col))))
}


### Plotting the trend for West Virginia
wv_price_trend <- df %>%
  filter(days_to_election >= 0 & days_to_election <= 20,
         statename == "West Virginia") %>%
  mutate(price_updates = price_update_calc(bet_margin)) %>%
  ggplot(aes(x = days_to_election, y = bet_margin)) +
  geom_point() +
  # geom_line() +
  geom_abline(slope = -wv_price_evolution$coefficients[2],
              intercept = wv_price_evolution$coefficients[1],
              color = "#E8171E") +
  lims(x = c(20, 0),
       y = c(-100, 0)) +
  labs(title = "West Virginia",
       x = "Days from election",
       y = "Betting market margins for Obama") +
  theme_bw()
wv_price_trend

### Plotting the trend for Massachusetts
ma_price_trend <- df %>%
  filter(days_to_election >= 0 & days_to_election <= 20,
         statename == "Massachusetts") %>%
  mutate(price_updates = price_update_calc(bet_margin)) %>%
  ggplot(aes(x = days_to_election, y = bet_margin)) +
  geom_point() +
  geom_abline(slope = ma_price_evolution$coefficients[2],
              intercept = ma_price_evolution$coefficients[1],
              color = "#0045CA") +
  geom_line(aes(group = price_updates)) +
  lims(x = c(20, 0),
       y = c(90, 100)) +
  labs(title = "Massachusetts",
       x = "Days from election",
       y = "Betting market margins for Obama") +
  theme_bw()
ma_price_trend

### Patching the two plots together and creating nice titles
wv_price_trend + ma_price_trend +
  plot_annotation(
    title = "Betting market price corrections election day -20 for WV and MA",
    subtitle = "Dotted line = Constant price \nNote the difference between y-axes"
  ) +
  plot_layout(axis_titles = "collect")


# Question 4 --------------------------------------------------------------
# Evaluate the prediction by looking at election outcome (electoral college votes)
# as opposed to simply vote share.

## As previously graphed we have shown that only two (the stars) states have the
## *wrong* election winner even though the voter shares are very off.
pred_diffs %>%
  ggplot(aes(x = `Betting Prediction`,
             y = `Victory Margin`)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.1) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.1) +
  geom_point(aes(shape = `Correct prediction`),
             color = ifelse(pred_diffs$`Victory Margin` > 0, "blue", "red"),
             fill = ifelse(pred_diffs$`Victory Margin` > 0, "blue", "red"),
             size = 3) +
  geom_abline(slope = linear$coefficients[2]) +
  # xlim(-max(abs(pred_diffs[3])), max(abs(pred_diffs[3]))) +
  ylim(min(pred_diffs[2:3]), max(abs(pred_diffs[2]))) +
  xlim(min(pred_diffs[2:3]), min(pred_diffs[2:3]) * -1) +
  scale_shape_manual(values = c(8, 1)) +
  theme_minimal() +
  theme(legend.position = "none")

## These two states were wrong
pred_diffs %>%
  filter(!`Correct prediction`) %>%
  tt() %>%
  style_tt(j = 2:4, align = "r")

## How is the votes from the electoral college distributed?
### First we will extract the amount of votes from each state
votes_df <- df %>%
  filter(days_to_election == 0) %>%
  select(statename, votes)

### Join votes into predictions
voting_predictions <- pred_diffs %>%
  inner_join(votes_df, by = c("State" = "statename")) %>%
  select(State, `Betting Prediction`, votes)

### Present votes in a `{tinytable}`
voting_predictions %>%
  select(State, `Betting Prediction`, votes) %>%
  mutate(Party = if_else(
    `Betting Prediction` > 0,
    "Democrats",
    "Republicans"
  )) %>%
  select(State, votes, Party) %>%
  group_by(Party) %>%
  summarise(Votes = sum(votes)) %>%
  tt() # %>%
  # print("markdown")    # Use this to print a nice table in the console

## If the betting markets were to trust, then the dems would have won 364
## votes from the electoral college - This calc may be wrong.


# Question 5 --------------------------------------------------------------










