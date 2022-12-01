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

OTHER_CLUSTERS = "Other Clusters"
OTHER_RELIGIONS = "Other Religions"
OTHER_GROUPS = "Other Groups"
OTHER_LANGFAMS = "Other Language Families"

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


#--- REPRESENT COMMUNITIES ---#

# Trial and error constant number to identify the divider thousands that
# will make the sum of all population into a thousand
N_THOU = 978.7 

strat_data <- combined_data %>%
  filter(!is.na(Population)) %>%
  rename(langFamily = lang_family) %>%
  select(Ctry, 
         PrimaryReligion, 
         langFamily,
         PeopNameInCountry,
         PeopNameAcrossCountries,
         PeopleCluster,
         Population) %>%
  group_by(Ctry, PrimaryReligion, PeopleCluster, PeopNameAcrossCountries) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou))

majority_data <- strat_data %>%
  filter(popThou != 0)

total_maj_data <- majority_data %>%
  summarize(Population = sum(Population))

strat_min_data <- strat_data %>%
  filter(popThou == 0) %>%
  # Introduce the majority population to compute for popThou
  bind_rows(tibble( 
    total_maj_data,
    PeopNameAcrossCountries = OTHER_GROUPS,
    PeopleCluster = "All Clusters"
    )) %>%
  arrange(Ctry) %>%
  group_by(Ctry, PeopleCluster) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou)) %>%
  # Remove majority population
  filter(!is.na(Ctry)) %>%
  print(n = Inf)

minority_data <- strat_min_data %>% 
  mutate(PeopNameAcrossCountries = paste("Other", PeopleCluster, "Groups"),
         PrimaryReligion = OTHER_RELIGIONS) %>%
  filter(popThou > 0 & !is.na(Ctry))

total_majmin_data <- full_join(majority_data, minority_data) %>%
  summarize(Population = sum(Population))

superminority_data <- strat_min_data %>%
  filter(popThou == 0) %>%
  group_by(Ctry) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  # Introduce the majority + minority population to compute for popThou
  bind_rows(total_majmin_data) %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou)) %>%
  mutate(PeopNameAcrossCountries = "All Other Groups",
         PeopleCluster = paste("Other", Ctry, "Clusters"),
         PrimaryReligion = OTHER_RELIGIONS) %>%
  # Remove majority + minority population
  filter(!is.na(Ctry))

get_popsum <- function(data) { data %>% summarize(sum(popThou)) }

majmin_data <- bind_rows(majority_data, 
                         minority_data,
                         superminority_data) %>%
  left_join(combined_data %>%
              select(Ctry, PeopNameAcrossCountries, lang_family)) %>%
  rename(langFamily = lang_family) %>%
  mutate(langFamily = if_else(is.na(langFamily), "All Other Languages", langFamily)) %>%
  unique() %>% arrange(Ctry) %>%
  select(Ctry, PeopNameAcrossCountries, PeopleCluster,
         langFamily, PrimaryReligion, perthou, popThou, Population) %>%
  arrange(Ctry)

# Generate data for secondary religions and locations,
# join with the distrib_data
religion_longer_data <- combined_data %>% 
  select(Ctry, 
         PeopNameAcrossCountries, 
         PCBuddhism, PCEthnicReligions,
         PCHinduism, PCIslam,
         PCNonReligious, PCOtherSmall,
         PCRomanCatholic,
         -PCDblyProfessing) %>%
  pivot_longer(cols = c(PCBuddhism:PCRomanCatholic),
               names_to = "religion",
               values_to = "pc") %>%
  filter(pc >= 30) %>%
  group_by(Ctry, PeopNameAcrossCountries) %>%
  mutate(rank = paste0("religion", row_number(-pc)))

religion_data <- religion_longer_data %>%
  # select(-pc) %>%
  mutate(religion = paste0(religion, "-", pc)) %>%
  pivot_wider(names_from = rank,
              values_from = c("religion")) %>%
  select(Ctry, PeopNameAcrossCountries,
         religion1, religion2, religion3) %>%
  separate(col = c("religion1"), into = c("religion1", "pc1"), sep="-") %>%
  separate(col = c("religion2"), into = c("religion2", "pc2"), sep="-") %>%
  separate(col = c("religion3"), into = c("religion3", "pc3"), sep="-")

loc_cluster_data <- combined_data %>%
  select(Ctry, PeopleCluster, Longitude, Latitude) %>%
  group_by(Ctry, PeopleCluster) %>%
  summarize(PeopNameAcrossCountries = paste("Other", PeopleCluster, "Groups"),
            Longitude = mean(Longitude, na.rm = T),
            Latitude = mean(Latitude, na.rm = T)) %>%
  unique() %>% ungroup() %>%
  semi_join(majmin_data, by = c("Ctry", "PeopleCluster"))

# Final distribution data
distrib_data <- majmin_data %>%
  arrange(desc(Population), Ctry, PrimaryReligion, langFamily) %>%
  arrange(desc(Population)) %>%
  # Inject the location for every people group (PeopNameAcrossCountries)
  left_join(bind_rows(combined_data, loc_cluster_data) %>%
              select(Ctry, PeopNameAcrossCountries, Longitude, Latitude)) %>%
  unique() %>%
  print(n = Inf)

distrib_data %>% summarize(sum(popThou))


# Quickly visualize
ggplot(distrib_data %>% mutate(popThouLog = log10(popThou))) +
  geom_tile(aes(x = langFamily,
                y = PrimaryReligion,
                fill = popThouLog)) +
  facet_wrap(vars(Ctry), scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # scale_fill_continuous(palette = "RdYlBu")
  # scale_fill_gradient2(
  #   low = "white", 
  #   mid = "grey", 
  #   high = "brown", 
  #   midpoint = 1
  # )
  scale_fill_viridis_c(option = "magma",
                       direction = -1,
                       na.value = "white")

# Checking names if read the same between countries
names_data <- distrib_data %>%
  left_join(combined_data %>%
              select(Ctry, PeopNameInCountry, PeopNameAcrossCountries),
            by = c("Ctry", "PeopNameAcrossCountries")) %>%
  filter(PeopNameInCountry != PeopNameAcrossCountries) %>%
  select(Ctry, PeopNameAcrossCountries, PeopNameInCountry) %>%
  arrange(Ctry) %>%
  print(n = Inf)

distrib_data_json <- distrib_data %>%
  rename(country = Ctry,
         primaryReligion = PrimaryReligion,
         peopleNames = PeopNameAcrossCountries,
         population = Population)

toJSON(distrib_data_json) %>%
  write("data/output/aseanDistributionData.json")


# To plot per country
distrib_data_json %>% 
  # select(-perthou, -popTenThou, -population) %>%
  # filter(country == "Indonesia") %>%
  # summarize(sum(popThou))
  # left_join(
  #   raw_data %>%
  #     select(Ctry, PeopNameAcrossCountries, LocationInCountry), 
  #   by = c(country = "Ctry",
  #                    peopleNames = "PeopNameAcrossCountries")) %>%
  # select(-country) %>%
  print(n = Inf)# %>%


# Review the religion breakdown for popThou more than 10


distrib_data_json %>% filter(country == "Malaysia") %>%
  print(n = Inf)

distrib_data %>%
  select(Ctry, PeopNameAcrossCountries, PeopleCluster, PrimaryReligion, popThou) %>%
  # left_join(religion_longer_data %>%
  #             select(-religion)) %>%
  # filter(rank %in% c("religion1", "religion2", "religion3")) %>%
  group_by(PeopNameAcrossCountries, Ctry) %>%
  arrange(Ctry, PeopleCluster, desc(popThou), PeopNameAcrossCountries) %>%
  print(n = Inf) 

religion_longer_data
