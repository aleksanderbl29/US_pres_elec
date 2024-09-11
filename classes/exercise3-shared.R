## load data
library(readxl)
library(tidyverse)
library(patchwork)
library(modelsummary)
abramowitz_data_file <- "../3 - Developing fundamentals-based model/abramowitz_data_2024.xlsx"
abramowitz_data_2024 <- read_excel(abramowitz_data_file)
head(abramowitz_data_2024)

summary(abramowitz_data_2024$year) ## look at the years covered

## Create a histogram for GDP growth rate with density curve and mean line
gdp_plot <- ggplot(abramowitz_data_2024, aes(x = q2gdp)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 10,
    fill = "skyblue",
    color = "black",
    alpha = 0.7
  ) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(
    aes(xintercept = mean(q2gdp, na.rm = TRUE)),
    color = "blue",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(title = "Histogram of GDP Growth Rate in the Second Quarter of Election Years", x = "GDP Growth Rate", y = "Density") +
  theme_minimal()

## Create a histogram for June presidential approval rating with density curve and mean line
approval_plot <- ggplot(abramowitz_data_2024, aes(x = juneapp)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 10,
    fill = "lightgreen",
    color = "black",
    alpha = 0.7
  ) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(
    aes(xintercept = mean(juneapp, na.rm = TRUE)),
    color = "blue",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(title = "Histogram of June Presidential Approval Rating", x = "Approval Rating", y = "Density") +
  theme_minimal()

# Print the plots
print(gdp_plot)
print(approval_plot)

## combine the plots
if (!require(patchwork)) install.packages("patchwork")
library(patchwork)
combined_plot <- gdp_plot + approval_plot + plot_layout(ncol = 2) +
  plot_layout(axis_titles = "collect")

# Print the combined plot
print(combined_plot)


# Question 2 --------------------------------------------------------------
## Run fundamentals model
fundamentals <- lm(incvote ~ q2gdp + juneapp + term2, data = abramowitz_data_2024)

summary(fundamentals)

#regression med Stargazer pakke
if (!require(stargazer)) install.packages("stargazer")
library(stargazer)

reg_funda <- abramowitz_data_2024 %>%
  lm(formula = incvote ~ q2gdp + juneapp + term2) %>%
  stargazer(type = "text")

#LM model - just eco
reg_eco <- abramowitz_data_2024 %>%
  filter(year <= 2004) %>%
  lm(formula = incvote ~ q2gdp) %>%
  stargazer(type = "text")

#LM model - just app
reg_app <- abramowitz_data_2024 %>%
  filter(year <= 2004) %>%
  lm(formula = incvote ~ juneapp) %>%
  stargazer(type = "text")

#LM model - just term
reg_term <- abramowitz_data_2024 %>%
  filter(year <= 2004) %>%
  lm(formula = incvote ~ term2) %>%
  stargazer(type = "text")



## Scatterplot of the relationship between Q2_GDP and Incumbent vote
ggplot(data = abramowitz_data_2024, mapping = aes(x = q2gdp, y = incvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## Scatterplot of the relationship between June approval and Incumbent vote
ggplot(data = abramowitz_data_2024, mapping = aes(x = juneapp, y = incvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## Bud på Nicholas’ code:
ggplot(data = abramowitz_data_2024, aes(x = q2gdp, ## make days - to have election day on right
                                        y = incvote)) +
  geom_point(
    size = 0.8,
    col = "black") +
  geom_smooth(
      formula = y ~ x,
      method = "lm",
      color = "darkgreen"
  ) +
  theme_minimal() +
  labs(x = "GDP growth rate", y = "Incumbent vote share", title =
         "Relationship between GDP growth rate and Incumbent vote share")



# Our own data in fundamentals model --------------------------------------
source("classes/Week2.R")  #Kører kode der henter data fra hjemmesiden

our_data <- abramowitz_data_2024 %>%
  left_join(rvest_data, by = c("year" = "years"))


our_model <- our_data %>%
  lm(incvote ~ q2gdp + juneapp + term2 + num, data = .)
summary(our_model)

####### -------- Loop Question 5 -------
# Assuming election_data contains columns: 'year', 'incumbent_vote_share',
# 'predictor1', 'predictor2', 'predictor3', and is already loaded in the environment.

# Initialize a list to store results
results <- vector("list", length = length(seq(1964, 2020, by = 4)))

# Define the election years to loop through
election_years <- seq(1964, 2020, by = 4)

# Loop through each election year
for (i in seq_along(election_years)) {
  year <- election_years[i]
  # Create the train dataset: all elections prior to the given year
  train_data <- abramowitz_data_2024[abramowitz_data_2024$year < year, ]
  # Create the test dataset: only the given election year
  test_data <- abramowitz_data_2024[abramowitz_data_2024$year == year, ]
  # Check if there is enough data to train the model
  if (nrow(train_data) > 0 && nrow(test_data) > 0) {
    # Run the TFC linear model on the train dataset
    # The model uses three predictors; replace these with actual column names from your dataset
    model <- lm(incvote ~ q2gdp + juneapp + term2, data = train_data)
    # Predict the incumbent vote share for the given election year
    predicted_vote_share <- predict(model, newdata = test_data)

    # Store the results as a data frame in the list
    results[[i]] <- data.frame(year = year,
                               actual = test_data$incvote,
                               predicted = predicted_vote_share)
  } else {
    # If not enough data, store NA
    results[[i]] <- data.frame(year = year,
                               actual = NA,
                               predicted = NA)
  }
}

# Combine all results into a single data frame
final_results <- do.call(rbind, results)

# Print the final results
print(final_results)

### Plot the results
# Assuming final_results is your data frame containing the year, actual, and predicted values
# and that it was created in the previous steps.

library(ggplot2)
# Plot actual vs. predicted vote share
plot <- ggplot(final_results, aes(x = actual, y = predicted)) +
  geom_point(color = "blue") +                  # Plot the data points
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red"
  ) +  # Add a linear model line
  labs(title = "Actual vs. Predicted Incumbent Vote Share (1964-2020)", x = "Actual Vote Share", y = "Predicted Vote Share") +
  theme_minimal() +
  geom_text(
    aes(label = year),
    hjust = 0.5,
    vjust = -0.5,
    size = 3
  ) # Optional: Label points with years

# Fit a linear model to get R-squared
model_fit <- lm(predicted ~ actual, data = final_results)
r_squared <- summary(model_fit)$r.squared

# Display R-squared on the plot
plot + annotate(
  "text",
  x = min(final_results$actual),
  y = max(final_results$predicted),
  label = paste("R² =", round(r_squared, 2)),
  hjust = 0,
  vjust = 1,
  size = 5,
  color = "black"
)

# Print the plot
print(plot)


#### ------ Question 6 2024 prediction ------
Prediction2024 <- vector("list", length = length(seq(1964, 2024, by = 4)))

# Define the election years to loop through
election_years2024 <- seq(1964, 2024, by = 4)

# Loop through each election year
for (i in seq_along(election_years2024)) {
  year <- election_years2024[i]
  # Create the train dataset: all elections prior to the given year
  train_data <- abramowitz_data_2024[abramowitz_data_2024$year < year, ]
  # Create the test dataset: only the given election year
  test_data <- abramowitz_data_2024[abramowitz_data_2024$year == year, ]
  # Check if there is enough data to train the model
  if (nrow(train_data) > 0 && nrow(test_data) > 0) {
    # Run the TFC linear model on the train dataset
    # The model uses three predictors; replace these with actual column names from your dataset
    model <- lm(incvote ~ q2gdp + juneapp + term2, data = train_data)
    # Predict the incumbent vote share for the given election year
    predicted_vote_share <- predict(model, newdata = test_data)

    # Store the results as a data frame in the list
    Prediction2024[[i]] <- data.frame(year = year,
                                      actual = test_data$incvote,
                                      predicted = predicted_vote_share)
  } else {
    # If not enough data, store NA
    Prediction2024[[i]] <- data.frame(year = year,
                                      actual = NA,
                                      predicted = NA)
  }
}


# Combine the predictions into a single data frame
final_Prediction <- do.call(rbind, Prediction2024)

# Print the final results
print(final_Prediction)

