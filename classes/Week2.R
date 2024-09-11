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

first_read <- read.csv("classes/data-w-2.csv")

testthat::expect_equal(mean(rvest_data$num), mean(first_read$num))
testthat::expect_equal(min(rvest_data$num), min(first_read$num))
testthat::expect_equal(max(rvest_data$num), max(first_read$num))
testthat::expect_equivalent(rvest_data, first_read)

# write_csv(rvest_data, "classes/data-w-2.csv")
