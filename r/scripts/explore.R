library(tidyverse)
library(jsonlite)
library(ggmap)
library(rworldmap)
library(googlesheets4)

sea_countries <- c("BX", "CB", "ID", "LA", "MY", "BM", "RP", "SN", "TH", "VM")
sea_countries_full <- c("Brunei", "Cambodia", "Indonesia", "Laos",
  "Malaysia", "Myanmar (Burma)", "Philippines", "Singapore", "Thailand",
  "Vietnam")
western_countries <- c("britain", "france", "netherlands", "portugal", "spain")

ROOT <- "https://api.joshuaproject.net/"
API_KEY <- "4cc6200ae202"
LIMIT <- 10000

GOOGLE_MAP_API_KEY <- "AIzaSyDLuH3fAvlTJg-XPX_Ng_-4acr6FEShLfw"
COLDAT_COLONIES_URL <- "https://docs.google.com/spreadsheets/d/1Qu8UmA-wJU9vohV6wmvDk0RcS7_4MvIz8OviELQ1dkw/edit#gid=1614706879"
COLDAT_DYADS_URL <- "https://docs.google.com/spreadsheets/d/1uXKX4b8CySp8E_W2B8KeBJknTeYppDjljv6VF2qLc4A/edit#gid=1350853870"

raw_data <- tibble()
# Loop through fetch data for every country
for (c in sea_countries){
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

# Read colonial dataset from Google sheets
gs4_deauth()
raw_coldat_data <- read_sheet(COLDAT_DYADS_URL)

desired_colz_data <- raw_colz_data %>%
  select(contains(c("country", western_countries))) %>%
  filter(country %in% sea_countries_full) %>%
  bind_cols(tibble(acronym = sea_countries))

# Visualize the distribution across Southeast Asia
register_google(GOOGLE_MAP_API_KEY)

location <- c(lon = 118.03717513509697, lat = 7.4778495323582765)
asean_map <- get_map(location,
                     zoom = 4,
                     source = "google",
                     maptype = "roadmap",
                     color = "bw")

ggmap(asean_map, darken = c(0.5, "white")) +
  geom_point(data = desired_data,
             aes(Longitude, Latitude,
                 size = Population,
                 color = PrimaryReligion # PeopleCluster # AffinityBloc,
                 ),
             fill = "transparent") # +
  # theme(legend.position="none")
