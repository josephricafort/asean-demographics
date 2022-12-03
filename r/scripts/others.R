#-- IDENTIFY PEOPLE GROUPS FROM OTHERS --#
source("represent.R")
strat_data
distrib_data

strat_data %>%
  filter(popThou == 0) %>%
  anti_join(distrib_data %>%
              select(Ctry, PeopleCluster, PeopNameAcrossCountries)) %>%
  group_by(Ctry, PeopleCluster, PeopNameAcrossCountries) %>%
  summarize(Population = sum(Population)) %>%
  group_by(Ctry) %>%
  mutate(percent = Population / sum(Population) * 100) %>%
  arrange(Ctry, desc(Population)) %>%
  filter(percent >= 1) %>%
  print(n = Inf)
  
