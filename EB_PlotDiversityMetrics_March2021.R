###  Abundance-Impacts plot summary
##  Generate plot-level diversity indices from species cover data
#  E.B. Building from H.S.'s code
#  Generates summaries based on PAINLES combined species cover level data
#  Last modified 5/26/2021

## Clean working directory
rm(list=ls())

## Load packages
library(tidyverse)
library(dplyr)
library(codyn)
library(stringr)

## Load combined data
setwd("~/Desktop/National Parks Temp/Continuous cover data")
data <- read.csv("PAINLES_18March2021.csv", stringsAsFactors = FALSE)
glimpse(data)
# subset to relevant columns (removed year because neon year is funky)
AllSp.Final <- data %>% select(UniqueID, Dataset, Site, Plot, Long, Lat, SpCode, ExoticStatus,
                        PctCov) %>%
  rename(Exotic=ExoticStatus, Species=SpCode, Pct_Cov = PctCov)
glimpse(AllSp.Final)
# change BLM dataset
AllSp.Final$Dataset[AllSp.Final$Dataset=="BLM_LMF" | AllSp.Final$Dataset=="BLM_TerrADat"] <- "BLM"
table(AllSp.Final$Dataset)
# how many plots in each dataset?
AllSp.Final %>% select(Dataset, Plot, Long, Lat) %>% distinct() %>%
  group_by(Dataset) %>% summarise(nplot = n())
# are there any duplicate plot codes?
AllSp.Final %>% select(Dataset, Plot) %>% distinct() %>%
  group_by(Dataset, Plot) %>% summarise(nplot = n()) %>% filter(nplot > 1)


## Calculate community metrics
# Set up dataframe with a unique ID by which to calcualte metrics
sum(grepl("__", AllSp.Final$Plot)) # make sure no [double] underscores in plot name
All.Sp <- AllSp.Final %>%
  # Unite all columns for which separate estimate is desired (site, growth habit, etc)
  unite(PlotByStatus, UniqueID, Dataset, Site, Plot, Exotic, sep = '__')
glimpse(All.Sp)

##  Diversity metrics:
H.df <- community_diversity(All.Sp, metric = "Shannon",
                            abundance.var = "Pct_Cov", replicate.var = "PlotByStatus")
InvSimp.df <- community_diversity(All.Sp, metric = "InverseSimpson",
                                  abundance.var = "Pct_Cov", replicate.var = "PlotByStatus")
##  Richness and evenness: using Evar following Avolio etal 2019 Ecosphere
Even.df <- community_structure(All.Sp, metric = "Evar", 
                               abundance.var = "Pct_Cov", replicate.var = "PlotByStatus")
head(Even.df)
tail(Even.df)
## Combine to one dataframe
codynMetrics <- H.df %>%
  inner_join(InvSimp.df) %>%
  inner_join(Even.df) %>%
  separate(PlotByStatus, c("UniqueID", "Dataset", "Site", "Plot", "Exotic"), sep = "__")
head(codynMetrics)
tail(codynMetrics)
codynMetrics$Site[codynMetrics$Site=="NA"] <- NA

## Summarize & join
All.PlotDiv <- AllSp.Final %>%
  group_by(UniqueID, Dataset, Site, Plot, Long, Lat, Exotic) %>%
  summarise(TotalPctCover = sum(Pct_Cov),
            # get 'richness' of unk sp codes too:
            Richness = n_distinct(Species)) %>%
  # Reshape
  pivot_longer(c(TotalPctCover, Richness), names_to = "variable", values_to = "value") %>%
  unite(Var_SpType, variable, Exotic, sep = "_") %>%
  pivot_wider(names_from = Var_SpType, names_sep = "_",
              values_from = value, values_fill = list(value = 0)) %>%
  # Calculate relative cover
  mutate(RelCov_I = (TotalPctCover_I / (TotalPctCover_I + TotalPctCover_N + TotalPctCover_NA + TotalPctCover_NI))*100,
         RelCov_N = (TotalPctCover_N / (TotalPctCover_I + TotalPctCover_N+ TotalPctCover_NA + TotalPctCover_NI))*100,
         RelCov_Unk = ((TotalPctCover_NA + TotalPctCover_NI) / 
                         (TotalPctCover_I + TotalPctCover_N+ TotalPctCover_NA + TotalPctCover_NI))*100) %>%
  # Drop plots with Relative Unk Cover > 10
  filter(RelCov_Unk < 10) %>%
  # Gather I and N for joining:
  pivot_longer(c(TotalPctCover_I, Richness_I, RelCov_I, 
                TotalPctCover_N, Richness_N, RelCov_N),
               names_to = c("variable", "Exotic"), names_sep = "_", values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value, values_fill = list(value = 0)) %>%
  # the above creates a dataset with a row for I or N cover of 0
  # make sure NAs for site join
  left_join(codynMetrics) %>%
  mutate(EvennessPielou = Shannon / log(Richness)) %>%
  select(-richness) %>%
  # rename:
  rename(EvennessEvar = Evar, DiversityShannon = Shannon, DiversityInvSimpson = InverseSimpson) %>%
  # Combine unknowns and drop leftover categories
  mutate(TotalPctCover_Unk = TotalPctCover_NI + TotalPctCover_NA,
         Richness_Unk = Richness_NI + Richness_NA) %>%
  select(-TotalPctCover_NI,-TotalPctCover_NA,-Richness_NI,-Richness_NA)
glimpse(All.PlotDiv)

## Double check everything worked
# Change missing values to NAs
All.PlotDiv[All.PlotDiv=="NaN"] = NA
# Add UniqueID to match enviro data (FIA first)
#fia_keys <- read.csv("FIA_PlotKeys_EB.csv")
#fia_keys <- fia_keys %>% mutate(Plot = as.character(Plot))
#glimpse(fia_keys)
#All.PlotDiv <- All.PlotDiv %>% left_join(fia_keys)
#All.PlotDiv %>% filter(Dataset=="FIA") %>% glimpse()
# Other datasets
#All.PlotDiv$UniqueID <- ifelse(All.PlotDiv$Dataset == "FIA", as.character(All.PlotDiv$UniqueID),
 #                               ifelse(All.PlotDiv$Dataset=="NPS", 
  #                             paste0(All.PlotDiv$Site, All.PlotDiv$Plot),
   #                    as.character(All.PlotDiv$Plot)))
#All.PlotDiv %>% filter(Dataset=="NPS") %>% glimpse()
# woo!

## Export combined data
# setwd("/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data")
# write.csv(All.PlotDiv, "Alldata_PlotDiv_18March2021.csv", row.names=FALSE)


