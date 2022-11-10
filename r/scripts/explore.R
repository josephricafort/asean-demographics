library(tidyverse)
library(jsonlite)
library(ggmap)

countries <- c("BX", "CB", "ID", "LA", "MY", "BM", "RP", "SN", "TH", "VM")

ROOT <- "https://api.joshuaproject.net/"
API_KEY <- "4cc6200ae202"
LIMIT <- 10000

GOOGLE_MAP_API_KEY <- "AIzaSyDLuH3fAvlTJg-XPX_Ng_-4acr6FEShLfw"

raw_data <- tibble()
# Loop through fetch data for every country
for (c in countries){
  url <- paste0(ROOT, "/v1/people_groups.json?api_key=", API_KEY, "&countries=", c, "&limit=", LIMIT)
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

# Visualize the distribution across Southeast Asia
register_google(GOOGLE_MAP_API_KEY)

location <- c(lon = 118.03717513509697, lat = 7.4778495323582765)
asean_map <- get_map(location, 
                     zoom = 4,
                     source = "google",
                     maptype = "roadmap",
                     color = "bw",
                     )

ggmap(asean_map, darken = c(0.5, "white")) +
  geom_point(data = desired_data, 
             aes(Longitude, Latitude, 
                 size = Population, 
                 fill = PrimaryReligion, # PrimaryReligion, 
                 alpha = 0.75),
             shape = 21,
             color = "transparent",
             )
