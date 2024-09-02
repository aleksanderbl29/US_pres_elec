library(tidyverse)
library(rvest)

page <- "https://www.slickcharts.com/sp500/returns"
tags_num <- "td+ td"
tags_yr <- "td:nth-child(1)"

num <- read_html(page) %>%
  html_nodes(tags_num) %>%
  html_text() %>%
  as.double()

years <- read_html(page) %>%
  html_nodes(tags_yr) %>%
  html_text() %>%
  as.double()

rvest_data <- tibble(num, years)

head(rvest_data)

# write_csv(rvest_data, "classes/data-w-2.csv")
