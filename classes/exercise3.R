# Clean the environment
rm(list = ls())

# Load packages
library(tidyverse)
library(readxl)
library(patchwork)
library(modelsummary)
library(ggpmisc)


# Question 1 --------------------------------------------------------------
## Load Abramowitz data
abramowitz_data <- "../3 - Developing fundamentals-based model/abramowitz_data_2024.xlsx"
abr_data <- read_xlsx(abramowitz_data,
                      na = c("", "NA"))

range(abr_data$year)

abr_data %>%
  ggplot(aes(x = q2gdp)) +
  geom_histogram()


## Create a histogram for GDP growth rate with density curve and mean line
gdp_plot <- ggplot(abr_data, aes(x = q2gdp)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = mean(q2gdp, na.rm = TRUE)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "GDP Growth Rate in Q2 of Election Years",
       x = "GDP Growth Rate",
       y = "Density") +
  theme_minimal()

## Create a histogram for June presidential approval rating with density curve and mean line
approval_plot <- ggplot(abr_data, aes(x = juneapp)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(aes(xintercept = mean(juneapp, na.rm = TRUE)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "June Presidential Approval Rating",
       x = "Approval Rating",
       y = "Density") +
  theme_minimal()

gdp_plot + approval_plot +
  plot_layout(axis_titles = "collect")

# Question 2 --------------------------------------------------------------
## Run fundamentals model to replicate Abr. analysis
fundamentals_abr <- abr_data %>%
  filter(year <= 2004) %>%
  lm(
    incvote ~ q2gdp + juneapp + term2,
    data = .
  )

summary(fundamentals_abr)

## Filtering data up to 2004
early_data <- abr_data %>%
  filter(year <= 2004)

## Running the model for each predictor separately and with all data until 2004
fundamentals_full <- lm(incvote ~ q2gdp + juneapp + term2, data = early_data)
summary(fundamentals_full)

fundamentals_gdp <- lm(incvote ~ q2gdp, data = early_data)
summary(fundamentals_gdp)

fundamentals_app <- lm(incvote ~ juneapp, data = early_data)
summary(fundamentals_app)

fundamentals_term <- lm(incvote ~ term2, data = early_data)
summary(fundamentals_term)

modelsummary(list(
  fundamentals_gdp,
  fundamentals_app,
  fundamentals_term,
  fundamentals_full), stars = TRUE)

## Running the model for each predictor separately and with all data until 2020
fundamentals_all <- lm(incvote ~ q2gdp + juneapp + term2, data = abr_data)
summary(fundamentals_all)

fundamentals_gdp <- lm(incvote ~ q2gdp, data = abr_data)
summary(fundamentals_gdp)

fundamentals_app <- lm(incvote ~ juneapp, data = abr_data)
summary(fundamentals_app)

fundamentals_term <- lm(incvote ~ term2, data = abr_data)
summary(fundamentals_term)

modelsummary(list(
  fundamentals_gdp,
  fundamentals_app,
  fundamentals_term,
  fundamentals_full), stars = TRUE)

# Question 4 --------------------------------------------------------------
## Visualize the relationship
abr_data %>%
  ggplot(aes(x = q2gdp, y = incvote)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm") +
  theme_bw() +
  stat_poly_eq()

abr_data %>%
  ggplot(aes(x = juneapp, y = incvote)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm") +
  theme_bw() +
  stat_poly_eq()

abr_data %>%
  ggplot(aes(x = term2, y = incvote)) +
  geom_point(color = "pink") +
  geom_smooth(method = "lm") +
  theme_bw() +
  stat_poly_eq()

abr_data %>%
  ggplot(aes(x = juneapp, y = incvote)) +
  geom_point(aes(), color = "lightblue")



# Our own data in fundamentals model --------------------------------------
source("classes/Week2.R")

rvest_data <- rvest_data %>%
  mutate(prev_yr = lag(num))

our_data <- abr_data %>%
  left_join(rvest_data, by = c("year" = "years"))

our_model <- our_data %>%
  lm(incvote ~ q2gdp + juneapp + term2 + num,
     data = .)
summary(our_model)
our_model_lag <- our_data %>%
  lm(incvote ~ q2gdp + juneapp + term2 + prev_yr,
     data = .)
summary(our_model_lag)
modelsummary(list(our_model,
                  our_model_lag),
             stars = TRUE)


