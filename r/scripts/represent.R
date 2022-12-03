#--- REPRESENT COMMUNITIES ---#

# Trial and error constant number to identify the divider thousands that
# will make the sum of all population into a thousand
source("scripts/combine.R")
combined_data

N_THOU = 976.5 

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
  group_by(Ctry, PeopleCluster, PeopNameAcrossCountries,
           PrimaryReligion, langFamily) %>%
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
    PeopNameAcrossCountries = ALL_OTHER_GROUPS,
    PeopleCluster = "All Clusters"
  )) %>%
  arrange(Ctry) %>%
  group_by(Ctry, PeopleCluster, PrimaryReligion, langFamily) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou)) %>%
  # Remove majority population
  filter(!is.na(Ctry)) %>%
  print(n = Inf)

minority_data <- strat_min_data %>% 
  mutate(PeopNameAcrossCountries = paste("Other", PeopleCluster, "Groups")) %>%
  # PrimaryReligion = ALL_OTHER_RELIGIONS) %>%
  filter(popThou > 0 & !is.na(Ctry)) %>%
  print(n = Inf)

total_majmin_data <- bind_rows(majority_data, minority_data) %>%
  summarize(Population = sum(Population))

strat_smin_data <- strat_min_data %>%
  filter(popThou == 0) %>%
  group_by(Ctry, PrimaryReligion, langFamily) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  # Introduce the majority + minority population to compute for popThou
  bind_rows(total_majmin_data) %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou)) %>%
  mutate(PeopNameAcrossCountries = ALL_OTHER_GROUPS,
         PeopleCluster = paste("Other", Ctry, "Clusters")) %>%
  # PrimaryReligion = ALL_OTHER_RELIGIONS) %>%
  # Remove majority + minority population
  filter(!is.na(Ctry)) %>%
  print(n = Inf)

superminority_data <- strat_smin_data %>%
  filter(popThou != 0)

total_majminsmin_data <- bind_rows(total_majmin_data,
                                   superminority_data) %>%
  summarize(Population = sum(Population))

strat_langfam_data <- strat_smin_data %>%
  filter(popThou == 0) %>%
  group_by(Ctry, langFamily) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  # Inject the 'not others' data: maj + min + smin
  bind_rows(total_majminsmin_data) %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou)) %>%
  mutate(PeopNameAcrossCountries = ALL_OTHER_GROUPS,
         PeopleCluster = paste("Other", Ctry, "Clusters"),
         PrimaryReligion = ALL_OTHER_RELIGIONS) %>%
  # Remove 'not others' population
  filter(!is.na(Ctry))

langfam_data <- strat_langfam_data %>%
  filter(popThou != 0)

ctry_data <- strat_langfam_data %>%
  filter(popThou == 0) %>%
  group_by(Ctry) %>%
  summarize(Population = sum(Population)) %>%
  ungroup() %>%
  # Inject the 'not others' data: maj + min + smin
  bind_rows(bind_rows(total_majminsmin_data,
                      langfam_data) %>%
              summarize(Population = sum(Population))) %>%
  mutate(perthou = (Population / sum(Population) * N_THOU),
         popThou = round(perthou)) %>%
  mutate(PeopNameAcrossCountries = ALL_OTHER_GROUPS,
         PeopleCluster = paste("All Other", Ctry, "Clusters"),
         PrimaryReligion = ALL_OTHER_RELIGIONS,
         langFamily = ALL_OTHER_LANGFAMS) %>%
  # Remove 'not others' population
  filter(!is.na(Ctry)) %>%
  filter(popThou != 0)

get_popsum <- function(data) { data %>% summarize(sum(popThou)) }

majmin_data <- bind_rows(majority_data, 
                         minority_data,
                         superminority_data,
                         langfam_data,
                         ctry_data) %>%
  left_join(combined_data %>%
              rename(langFamily = lang_family) %>%
              select(Ctry, PeopNameAcrossCountries, langFamily)) %>%
  mutate(langFamily = if_else(is.na(langFamily), ALL_OTHER_LANGFAMS, langFamily)) %>%
  unique() %>% arrange(Ctry) %>%
  select(Ctry, PeopNameAcrossCountries, PeopleCluster,
         langFamily, PrimaryReligion, perthou, popThou, Population) %>%
  arrange(Ctry) %>%
  print(n = Inf)

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
  rename(langFamily = lang_family) %>%
  select(Ctry, PeopleCluster, langFamily, Longitude, Latitude) %>%
  group_by(Ctry, PeopleCluster, langFamily) %>%
  summarize(PeopNameAcrossCountries = paste("Other", PeopleCluster, "Groups"),
            langFamily = if_else(!is.na(langFamily), langFamily, ALL_OTHER_RELIGIONS),
            Longitude = mean(Longitude, na.rm = T),
            Latitude = mean(Latitude, na.rm = T)) %>%
  unique() %>% ungroup() %>%
  semi_join(majmin_data, by = c("Ctry", "PeopleCluster")) %>%
  print(n = Inf)

loc_group_data <- combined_data %>%
  rename(langFamily = lang_family) %>%
  select(Ctry, PeopNameAcrossCountries, PeopleCluster, 
         langFamily, Longitude, Latitude) %>%
  group_by(Ctry, PeopNameAcrossCountries, langFamily) %>%
  summarize(langFamily = if_else(!is.na(langFamily), langFamily, ALL_OTHER_RELIGIONS),
            Longitude = mean(Longitude, na.rm = T),
            Latitude = mean(Latitude, na.rm = T)) %>%
  print(n = Inf)


# Final distribution data
distrib_data <- majmin_data %>%
  arrange(desc(Population), Ctry, PrimaryReligion, langFamily) %>%
  arrange(desc(Population)) %>%
  # # Inject the location for every people group (PeopNameAcrossCountries)
  # left_join(bind_rows(combined_data, loc_cluster_data) %>%
  #             unique() %>%
  #             select(Ctry, PeopleCluster, PeopNameAcrossCountries,
  #                    Longitude, Latitude)) %>%
  unique() %>%
  print(n = Inf)

COUNTRY <- "Philippines"

distrib_data %>% arrange(Ctry, PeopleCluster,
                         desc(popThou),
                         PeopNameAcrossCountries,
                         PrimaryReligion, langFamily) %>%
  filter(Ctry == COUNTRY) %>%
  print(n = Inf)

distrib_data %>%
  filter(Ctry == COUNTRY) %>%
  group_by(Ctry, PeopleCluster) %>%
  summarize(sum(popThou)) %>%
  print(n = Inf)


distrib_data %>% summarize(sum(popThou))



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
         peopleCluster = PeopleCluster,
         peopleNames = PeopNameAcrossCountries,
         population = Population,
         long = Longitude,
         lat = Latitude)

toJSON(distrib_data_json) %>%
  write("data/output/aseanDistributionData.json")


# Review the religion breakdown for popThou more than 10

distrib_data_json %>% filter(country == "Indonesia") %>%
  arrange(peopleCluster) %>%
  print(n = Inf)

distrib_data %>%
  select(Ctry, PeopleCluster, PeopNameAcrossCountries, PrimaryReligion, popThou) %>%
  # left_join(religion_longer_data %>%
  #             select(-religion)) %>%
  # filter(rank %in% c("religion1", "religion2", "religion3")) %>%
  group_by(PeopNameAcrossCountries, Ctry) %>%
  arrange(Ctry, PeopleCluster, desc(popThou), PeopNameAcrossCountries) %>%
  print(n = Inf) 

religion_longer_data