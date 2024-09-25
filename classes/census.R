library(tidycensus)
library(tidyverse)

census_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(census_key)

age20 <- get_decennial(geography = "state",
                       variables = "P13_001N",
                       year = 2020,
                       sumfile = "dhc")

deaths <- get_estimates(geography = "county",
                     variables = "DEATHS")

head(deaths)
