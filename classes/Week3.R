library(dotenv)
library(httr2)

api_url <- "http://api.marketstack.com/v1/eod"

cache_dir <- tempdir()

resp <- request(api_url) %>%
  req_url_query(!!!list(access_key = Sys.getenv("marketstack_api"),
                     symbols = "AAPL")) %>%
  req_cache(cache_dir) %>%
  req_perform() %>%
  resp_body_json()


df <- resp$data %>%
  do.call(rbind.data.frame, .)

df
