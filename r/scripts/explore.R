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
  select(ROG3, Ctry, PeopleCluster, AffinityBloc,
         PrimaryReligion, colonizer,
         Population, Longitude, Latitude,
         ROL3, PrimaryLanguageName,
         PeopNameAcrossCountries)

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


#--- VISUALIZE --- #

# Visualize the distribution across Southeast Asia
register_google(GOOGLE_MAP_API_KEY)

location <- c(lon = 118.03717513509697, lat = 7.4778495323582765)
asean_map <- get_map(location,
                     zoom = 4,
                     source = "google",
                     maptype = "roadmap",
                     color = "bw")

ggmap(asean_map, darken = c(0.5, "white")) +
  geom_point(data = combined_data,
             aes(Longitude, Latitude,
                 size = Population,
                 color = lang_family, # PrimaryReligion # PeopleCluster # AffinityBloc,
                 shape= PrimaryReligion
                 ),
             fill = "transparent") #+ theme(legend.position="none")

# Visualize the timeline of colonization per country
coldat_malaysia_tbl <- desired_coldat_data %>%
  filter(country == "Malaysia")

coldat_malaysia_tbl$country[1] = "Malaysia (Britain)"
coldat_malaysia_tbl$country[2] = "Malaysia (Portugal)"

toplot_coldat_data <- desired_coldat_data %>%
  filter(country != "Malaysia") %>%
  bind_rows(coldat_malaysia_tbl) %>%
  arrange(colstart_max)

ggplot(toplot_coldat_data) +
  geom_linerange(aes(
      x = fct_reorder(country, sort(colstart_max, decreasing = T)),
      ymin = colstart_max,
      ymax = colend_max,
      color = colonizer),
    size = 5,
    alpha = 0.95) +
  coord_flip() +
  labs(x = "Country", y = "Year")

# Facet visualize
ggplot(combined_data, aes(
    x = lang_family,
    y = Population)) +
  # geom_point(aes(size = Population, 
  #                color = lang_family,
  #                shape = PrimaryReligion)) +
  geom_col(aes(fill = PrimaryReligion)) +
  facet_grid(rows = vars(Ctry), scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()

ggplot(combined_data, aes(
    x = reorder(Ctry, desc(Ctry)),
    y = Population)) +
  geom_col(aes(fill = lang_family)) +
  coord_flip()

ggplot(combined_data, aes(
    x = reorder(Ctry, desc(Ctry)),
    y = Population)) +
  geom_col(aes(fill = PrimaryReligion)) +
  coord_flip()


#--- REPRESENT COMMUNITIES ---#
OTHER_CLUSTERS = "Other Clusters"
OTHER_RELIGIONS = "Other Religions"
OTHER_LANGFAMS = "Other Language Families"

strat_data <- combined_data %>%
  filter(!is.na(Population)) %>%
  rename(langFamily = lang_family) %>%
  select(Ctry, PrimaryReligion, langFamily, PeopleCluster, Population) %>%
  group_by(Ctry, PrimaryReligion, langFamily, PeopleCluster) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(percent = (Population / sum(Population) * 100),
         popThou = round(percent * 10),
         popTenThou = round(percent * 100))

majority_data <- strat_data %>%
  filter(popThou != 0)

total_maj_data <- majority_data %>%
  summarize(Population = sum(Population))

strat_min_data <- strat_data %>%
  filter(popThou == 0) %>%
  bind_rows(tibble(
    total_maj_data,
    PeopleCluster = OTHER_CLUSTERS
    )) %>%
  arrange(Ctry) %>%
  group_by(Ctry, langFamily) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(percent = (Population / sum(Population) * 100),
         popThou = round(percent * 10),
         popTenThou = round(percent * 100))

minority_data <- strat_min_data %>% 
  mutate(PeopleCluster = OTHER_CLUSTERS,
         PrimaryReligion = OTHER_RELIGIONS) %>%
  filter(popThou > 0 & !is.na(Ctry))

superminority_data <- strat_min_data %>%
  filter(popThou == 0) %>%
  anti_join(minority_data, by = "Ctry", "langFamily") %>%
  group_by(Ctry) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  bind_rows(total_maj_data) %>%
  mutate(percent = (Population / sum(Population) * 100),
         popThou = round(percent * 10),
         popTenThou = round(percent * 100)) %>%
  mutate(PeopleCluster = OTHER_CLUSTERS,
         PrimaryReligion = OTHER_RELIGIONS,
         langFamily = OTHER_LANGFAMS)

# Brunei special case - since all of Brunei's population
# is represented by a single unit, we need to recode
# the wordings of 'Other' to 'All' (Ex.: Other Clusters
# will become All Clusters)
replaceOtherAll <- 
brunei_data <- superminority_data %>%
  filter(Ctry == "Brunei") %>%
  mutate(across(PeopleCluster:langFamily, 
                function (c){ 
                  return (str_replace(c, "Other", "All")) 
                }))
  
distrib_data <- majority_data %>%
  bind_rows(minority_data, superminority_data) %>%
  ungroup() %>%
  filter(!is.na(Ctry)) %>%
  filter(Ctry != "Brunei") %>% # Replace with the tweaked one above
  bind_rows(brunei_data) %>%
  arrange(Ctry, desc(Population), PrimaryReligion, langFamily)

print(distrib_data, n = nrow(distrib_data))
distrib_data %>% colnames

# Quickly visualize
ggplot(distrib_data) +
  geom_col(aes(x = fct_rev(as.factor(PeopleCluster)),
               y = popThou,
               fill = langFamily)) +
  facet_wrap(vars(Ctry), scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()
  # coord_polar()

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

distrib_data_json <- distrib_data %>%
  rename(country = Ctry,
         primaryReligion = PrimaryReligion,
         peopleCluster = PeopleCluster,
         population = Population)

toJSON(distrib_data_json) %>%
  write("data/output/aseanDistributionData.json")
