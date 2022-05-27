### script to recalculate cover for datatsets that we aggregated strata following Jennings et al 2009 ###

library(tidyverse)
library(readr)

#### WVNHP ####
WV_species_data <- read_csv("/home/shares/neon-inv/raw_VegBank_data/WV_species_data.csv")
View(WV_species_data)

WVdata <- WV_species_data %>% select(`Plot Code`, `Plant Symbol`, `Scientific Name`, `Stratum`, `Real Cover`) %>% 
  #T = total cover (which is summing in the original data)
  filter(`Stratum`!="T") %>% 
  mutate(Step1=1-(`Real Cover`/100)) %>% 
  group_by(`Plot Code`, `Scientific Name`, `Plant Symbol`) %>% 
  summarize(PctCov_100=(1-(prod(Step1)))*100) %>% 
  ungroup() %>% select(-`Plant Symbol`) %>% rename(Original.Plot=`Plot Code`, Original.TaxonName=`Scientific Name`)
range(WVdata$PctCov_100, na.rm = T)
write.csv(WVdata, "/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_WVNHP/WVNHPPct_100.csv", row.names=F)

WVdataStrata <- WV_species_data %>% select(`Plot Code`, `Plant Symbol`, `Scientific Name`, `Stratum`, `Real Cover`) %>%
  #T = total cover (which is summing in the original data)
  filter(`Stratum`!="T") %>%
  group_by(`Plot Code`, `Stratum`) %>% 
  mutate(StrataTotCov = sum(`Real Cover`)) %>% 
  add_tally() %>% 
  group_by(`Plot Code`, `Stratum`,`Plant Symbol`, `Scientific Name`) %>%
  mutate(IndvSppCov_perStrata = sum(`Real Cover`))
  select(-`Plant Symbol`, -`Scientific Name`, -`Real Cover`) %>% distinct()
  
#### NPS ####
  # site CAVE already has cover for a stratum called "All"
  # importing all other files

  setwd("/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_NPS/NPS_raw-cover")
  #importing all files and stacking as a single tibble
  NPSrawdata <-
    list.files(pattern = "*.csv") %>% 
    map_df(~read_csv(., col_types = cols(.default = "c"))) 
  glimpse(NPSrawdata)
  #changing column type from character to numeric
  NPSrawdata <- NPSrawdata %>% mutate(Pct_Cov=as.numeric(Pct_Cov),
                                      Stratum=ifelse(is.na(Stratum), "NA", Stratum),
                                      Species=ifelse(is.na(Species), "UNKNOWN", Species))

  unique(NPSrawdata$Stratum) #as Eve mentioned, hard to know what these different strata mean
  
  NPSdata <- NPSrawdata %>% select(Site, Plot, Pct_Cov, Stratum, Species) %>% 
    filter(Site!="OZAR",
           Stratum!="All") %>% 
    mutate(Step1=1-(Pct_Cov/100),
           Plot=paste0(Site,"_", Plot),
           Stratum=ifelse(is.na(Stratum), "NA", Stratum)) %>% 
    group_by(Plot, Species) %>% 
    summarize(PctCov_100=(1-(prod(Step1)))*100) %>% 
    rename(SpCode.Original=Species)
  write.csv(NPSdata, "/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_NPS/NPSPct_100.csv", row.names=F)
  
  range(NPSdata$PctCov_100)
     
  
## importing final file, testing join ##
## WVNHP ##
FULLDatabase_05232022 <- read_csv("/home/shares/neon-inv/data_paper/final_data/FULLDatabase_05232022.csv")
glimpse(FULLDatabase_05232022)
FULLDatabase_05232022<-FULLDatabase_05232022 %>% left_join(WVdata, by = c("Original.Plot", "Original.TaxonName")) %>% 
  mutate(PctCov_100=ifelse(Dataset=="WVNHP" & RecordedStrata=="N", PctCov, PctCov_100))
# okay, for WVHNHP --> it basically works for all entries, joining by Original.TaxonName and Original.Plot. 
# The very few observations that do not work, do not have recorded strata, so I can mutate and get them blanks filled.  

## NPS ##
FULLDatabase_05232022 <- read_csv("/home/shares/neon-inv/data_paper/final_data/FULLDatabase_05232022.csv")
glimpse(FULLDatabase_05232022)
DataBase2<-DataBase2 %>% left_join(NPSdata, by = c("Plot", "SpCode.Original")) %>% # A LOT OF NAs
  mutate(PctCov_100=ifelse(Dataset=="NPS" & Site=="CAVE", PctCov, PctCov_100))
         
                           # ifelse(is.na(SpCode), PctCov, PctCov_100)))

NPSNAs<-DataBase2 %>% filter(Dataset=="NPS", is.na(PctCov_100)) ## no more missing values
DataBase2 %>% filter(Dataset=="NPS") %>% group_by(Dataset) %>%
  summarize(min100 = min(PctCov_100, na.rm = TRUE),
            mean100 = mean(PctCov_100, na.rm = TRUE),
            max100 = max(PctCov_100, na.rm = TRUE),
            min = min(PctCov, na.rm = TRUE),
            mean = mean(PctCov, na.rm = TRUE),
            max = max(PctCov, na.rm = TRUE)) 
