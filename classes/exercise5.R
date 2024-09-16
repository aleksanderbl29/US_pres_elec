rm(list = ls())

library(tidyverse)
# library(tinytable)
library(modelsummary)

file <- "../5 - Developing a polls-only model/blackturnout.csv"
bturnout <- read.csv(file) %>%
  mutate(candidate = as.factor(candidate))

head(bturnout)


# Question 1 --------------------------------------------------------------

years <- unique(bturnout$year)
states <- unique(bturnout$state)

years
states
length(states)


# Question 2 --------------------------------------------------------------

levels(bturnout$candidate) <- c("Non-ethnic", "Co-ethnic")

plt <- wesanderson::wes_palette("Darjeeling2")

bturnout %>%
  ggplot(aes(x = turnout, y = candidate, fill = candidate)) +
  geom_boxplot() +
  labs(title = "Turnout rate in black voting-age population",
       # subtitle = "Depending on the ethnic combination of candidacy",
       # fill = "Candidacy \n ethnic \n combination") +
       fill = "Candidacy") +
  lims(x = c(0, 1)) +
  scale_fill_manual(values = c(plt[1], plt[2])) +
  ggthemes::theme_fivethirtyeight()


# Question 3 --------------------------------------------------------------

model <- lm(turnout ~ candidate,
            data = bturnout)
summary(model)

modelsummary(list("Model" = model),
             coef_map = c("(Intercept)" = "Intercept",
                          "candidateCo-ethnic" = "Co-ethnic candidacy"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE)

stargazer::stargazer(model,
                     type = "text")

marginaleffects::plot_predictions(model, by = "candidate") +
  ggthemes::theme_fivethirtyeight()


# Question 4 --------------------------------------------------------------

c1 <- bturnout %>%
  filter(candidate == "Co-ethnic")

c0 <- bturnout %>%
  filter(candidate == "Non-ethnic")

ggplot() +
  geom_point(data = c0, aes(x = CVAP, y = turnout, color = candidate, shape = candidate)) +
  geom_smooth(data = c0, method = "lm", se = FALSE,
              aes(x = CVAP, y = turnout, color = candidate)) +
  geom_point(data = c1, aes(x = CVAP, y = turnout, color = candidate, shape = candidate)) +
  geom_smooth(data = c1, method = "lm", se = FALSE,
              aes(x = CVAP, y = turnout, color = candidate)) +
  labs(title = "Turnout rate in black voting-age population",
       color = "Candidacy",
       shape = "Candidacy",
       x = "Turnout rate",
       y = "Black share of VAP") +
  lims(x = c(0, 1),
       y = c(0, 1)) +
  scale_color_manual(values = c(plt[1], plt[2])) +
  theme_minimal()

# Question 5 --------------------------------------------------------------

model2 <- lm(turnout ~ candidate + CVAP,
            data = bturnout)
summary(model2)

modelsummary(list("Simple" = model,
                  "With VAP" = model2),
             coef_map = c("(Intercept)" = "Intercept",
                          "candidateCo-ethnic" = "Co-ethnic candidacy",
                          "CVAP" = "Colored Voting Age Population"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE)

stargazer::stargazer(model2,
                     type = "text")

modelsummary::modelplot(model2,
          coef_map = c("(Intercept)" = "Intercept",
                       "candidateCo-ethnic" = "Co-ethnic candidacy",
                       "CVAP" = "Colored Voting Age Population")) +
  theme_bw()
