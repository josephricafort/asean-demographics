#-- IDENTIFY PEOPLE GROUPS FROM OTHERS --#
source("represent.R")
strat_data
distrib_data

others_data <- strat_data %>%
  # anti_join(distrib_data %>% select(Ctry, PeopleCluster),
  #           by = c("Ctry", "PeopleCluster")) %>%
  # filter(popThou == 0) %>%
  filter(Ctry == "Singapore") %>%
  filter(PeopleCluster == "Chinese") %>%
  arrange(PeopleCluster, desc(Population), PrimaryReligion, langFamily) %>%
  print(n = Inf)
  
