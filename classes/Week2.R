library(tidyverse)
library(rvest)

page <- "https://www.slickcharts.com/sp500/returns"
tags_num <- "td+ td"
tags_yr <- "td:nth-child(1)"

num <- read_html(page) %>%
  html_nodes(tags_num) %>%
  html_text()

years <- read_html(page) %>%
  html_nodes(tags_yr) %>%
  html_text()

data <- tibble(num, years)

write_csv(data, "classes/data-w-2.csv")
