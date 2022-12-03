library(tidyverse)
library(jsonlite)
library(ggmap)
library(googlesheets4)
library(lingtypology)

sea_countries <- c("BX", "CB", "ID", "LA", "MY", "BM", "RP", "SN", "TH", "VM")
sea_countries_full <- c("Brunei", "Cambodia", "Indonesia", "Laos",
  "Malaysia", "Myanmar (Burma)", "Philippines", "Singapore", "Thailand",
  "Vietnam")
western_countries <- c("britain", "france", "netherlands", "portugal", "spain")
sea_countries_tbl <- tibble(country_code = sea_countries,
                            country = sea_countries_full)

ROOT <- "https://api.joshuaproject.net/"
API_KEY <- "4cc6200ae202"
LIMIT <- 10000

GOOGLE_MAP_API_KEY <- "AIzaSyDLuH3fAvlTJg-XPX_Ng_-4acr6FEShLfw"
COLDAT_COLONIES_URL <- "https://docs.google.com/spreadsheets/d/1Qu8UmA-wJU9vohV6wmvDk0RcS7_4MvIz8OviELQ1dkw/edit#gid=1614706879"
COLDAT_DYADS_URL <- "https://docs.google.com/spreadsheets/d/1uXKX4b8CySp8E_W2B8KeBJknTeYppDjljv6VF2qLc4A/edit#gid=1350853870"

ALL_OTHER_CLUSTERS = "All Other Clusters"
ALL_OTHER_RELIGIONS = "All Other Religions"
ALL_OTHER_GROUPS = "All Other Groups"
ALL_OTHER_LANGFAMS = "All Other Language Families"

#--- FETCH DATASETS --- #

# Colonial data
# Read colonial dataset from Google sheets
gs4_deauth()
raw_coldat_data <- read_sheet(COLDAT_DYADS_URL)

desired_coldat_data <- raw_coldat_data %>%
  filter(country %in% sea_countries_full) %>%
  filter(col != 0) %>%
  left_join(sea_countries_tbl, by = c("country")) %>%
  mutate_at(vars(colstart_max:colend_mean), as.double) # %>%
  # pivot_longer(colstart_max:colend_mean, names_to = "col_marker",
  #              values_to = "year")

# Demographics Data
raw_data <- tibble()
# Loop through fetch data for every country
for (c in sea_countries){
  url <- paste0(ROOT, "/v1/people_groups.json?api_key=", API_KEY, "&countries=", c, "&limit=", LIMIT)
  message(paste0("Fetching data for: ", c))
  fetched_data <- fromJSON(url)
  message(paste0("Number of rows: ", nrow(fetched_data)))
  raw_data <- bind_rows(raw_data, fetched_data)
}

desired_data <- raw_data %>%
  # top_n(500, PopulationPercentUN) %>%
  left_join(desired_coldat_data, by = c("ROG3" = "country_code")) %>%
  select(ROG3, Ctry, PeopNameInCountry,
         PeopNameAcrossCountries, PeopleCluster,
         AffinityBloc,
         PrimaryReligion, colonizer,
         Population, Longitude, Latitude,
         ROL3, PrimaryLanguageName,
         PCIslam:PCOtherChristian,
         PCBuddhism:PCRomanCatholic)

toJSON(desired_data) %>%
  write("data/output/aseanEthnicGroup.json")

# Language Families
glotto_data <- as_tibble(glottolog) %>%
  filter(iso %in% unique(desired_data$ROL3)) %>%
  filter(affiliation != "Bookkeeping" & 
           affiliation != "NA") %>%
  filter(affiliation != "Sign Language") %>%
  select(iso, level, area, affiliation) %>%
  rename(lang_iso = iso,
         lang_level = level,
         lang_area = area,
         lang_affiliation = affiliation) %>%
  mutate(lang_family = str_extract(lang_affiliation, '[\\w\\s\\-]*'))

combined_data <- desired_data %>%
  left_join(glotto_data, by = c("ROL3" = "lang_iso")) %>%
  mutate(Ctry = fct_relevel(desired_data$Ctry, sea_countries_full))
