library(tidyverse)
library(jsonlite)

countries <- c("BX", "CB", "ID", "LA", "MY", "BM", "RP", "SN", "TH", "VM")

ROOT <- "https://api.joshuaproject.net/"
API <- "4cc6200ae202"
LIMIT <- 10000

raw_data <- tibble()
# Loop through fetch data for every country
for (c in countries){
  url <- paste0(ROOT, "/v1/people_groups.json?api_key=", API, "&countries=", c, "&limit=", LIMIT)
  message(paste0("Fetching data for: ", c))
  fetched_data <- fromJSON(url)
  message(paste0("Number of rows: ", nrow(fetched_data)))
  raw_data <- bind_rows(raw_data, fetched_data)
}
raw_data$ROG3 %>% unique()

desired_data <- raw_data %>%
  top_n(500, PopulationPercentUN)

toJSON(desired_data) %>%
  write("data/output/aseanEthnicGroup.json")