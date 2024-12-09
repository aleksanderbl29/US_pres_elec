# Code by Nicholas Haas and Derek Beach.

# Clear environment
rm(list = ls())

#Libraries
library(tidyverse)

#Load data
out <- read_rds("_output/Model output.rds") #Load your output, this is just an example using 2020 predictions/results#
predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]
state_abb_list <- read.csv("data/potus_results_76_20.csv") %>%
  pull(state) %>% unique()

## ERRORS ##

#Pull predicted score for each state for election day, take the mean of all simulations#
prs <- predicted_score %>%
  `[`(, 245, ) %>%
  apply(2, mean)

#Create a dataframe with prediction (in percentage terms) for each state#
pred_state <- data.frame(
  state = state_abb_list,
  pred = (prs * 100)
)

#Pull the results for each state#
result <- read_csv("data/2024a_results.csv") %>%
  mutate(Biden = (Biden / (Biden + Trump)) * 100) %>%
  select(state, Biden) %>%
  arrange(state)

#Create a merged dataset with predicted and actual results, and their difference (error)#
pred_state <- pred_state %>%
  left_join(result) %>%
  mutate(error = pred - Biden,
         label = factor(state))

#Calculate mean squared error, round to three decimal places#
mse <- round(sum(pred_state$error^2) / nrow(pred_state), digits = 3)

#Plot differences by state, color predicted/actual winner (if over 50%, Democratic, blue), draw lines between
#predicted and observed values (if error under 0, make blue), add a dashed line at 50% to demarcate threshold,
#order states by threshold, add error label next to predicted value, add title#
ggplot(pred_state,
       aes(x = label,
           y = Biden,
           ymin = Biden,
           ymax = pred)) +
  geom_point(data = pred_state,
             aes(y = pred,
                 color = ifelse(pred >= 50,
                                'Democratic',
                                'Republican')),
             size = 1.5, alpha = .6) +
  geom_linerange(alpha = .4,
                 size = 1.2,
                 color = ifelse(pred_state$error < 0, 'blue', 'red')) +
  geom_point(aes(color = ifelse(Biden >= 50,
                                'Democratic',
                                'Republican')),
             size = 2.2) +
  geom_hline(yintercept = 50,
             linetype = "dashed") +
  scale_color_manual(name = 'State prediction/\nwinner',
                     values = c('Democratic' = '#3A4EB1',
                                'Republican' = '#E40A04')) +
  scale_x_discrete("",
                   limits = pred_state$label[order(pred_state$pred)]) +
  scale_y_continuous("Biden vote share prediction/result") +
  geom_text(aes(y = pred,
                label = round(error, digits = 2)),
            nudge_y = 4,
            color = ifelse(pred_state$error < 0, 'blue', 'red'),
            size = 2) +
  coord_flip() +
  ggtitle(paste("Total predictive performance, MSE =", mse)) +
  theme_minimal()

#ggsave(filename = "performance.pdf", width = 8, height = 6)
