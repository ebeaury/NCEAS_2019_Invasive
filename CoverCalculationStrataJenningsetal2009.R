### script to recalculate cover for datatsets that we aggregated strata following Jennings et al 2009 ###

library(tidyverse)
library(readr)
WV_species_data <- read_csv("/home/shares/neon-inv/raw_VegBank_data/WV_species_data.csv")
View(WV_species_data)

WVdata <- WV_species_data %>% select(`Plot Code`, `Plant Symbol`, `Scientific Name`, `Stratum`, `Real Cover`) %>% 
  #T = total cover (which is summing in the original data)
  filter(`Stratum`!="T") %>% 
  mutate(Step1=1-(`Real Cover`/100)) %>% 
  group_by(`Plot Code`, `Scientific Name`, `Plant Symbol`) %>% 
  summarize(PctCov_100=(1-(prod(Step1)))*100)

WVdataStrata <- WV_species_data %>% select(`Plot Code`, `Plant Symbol`, `Scientific Name`, `Stratum`, `Real Cover`) %>%
  #T = total cover (which is summing in the original data)
  filter(`Stratum`!="T") %>%
  group_by(`Plot Code`, `Stratum`) %>% 
  mutate(StrataTotCov = sum(`Real Cover`)) %>% 
  add_tally() %>% 
  group_by(`Plot Code`, `Stratum`,`Plant Symbol`, `Scientific Name`) %>%
  mutate(IndvSppCov_perStrata = sum(`Real Cover`))
  select(-`Plant Symbol`, -`Scientific Name`, -`Real Cover`) %>% distinct()
