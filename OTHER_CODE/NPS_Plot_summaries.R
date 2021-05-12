rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(forcats)

#
#
#
#
#


# Get site level stats
  # For every Site + Plot + Exotic + Growth_habit combination

# Function to calculate Shannon's diversity
##  Works on relative abundance, NOT raw abundance like vegan diversity()
shannonH <- function(p_i) {
  # drop 0s and NAs:
  p_i <- p_i[!is.na(p_i) & p_i != 0]
  # H:
  -sum(p_i*log(p_i))
}

# Summarize cover, shannon's diversity, evenness, and richness
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/formatted cover data")
fileNames <- Sys.glob("clean_*")
length(fileNames) # 123 sites

for (inp_file in fileNames) {
  raw_data <- read.csv(inp_file, stringsAsFactors = FALSE)
  
  # make sure the dataframe is structured correctly
  raw_data$Site <- as.factor(raw_data$Site)
  raw_data$Plot <- as.character(raw_data$Plot)
  raw_data$Growth_habit <- as.factor(raw_data$Growth_habit)
  raw_data$Stratum <- as.factor(raw_data$Stratum)
  raw_data$Pct_Cov <- as.numeric(raw_data$Pct_Cov)
  
  # Consider Exotic NAs and Growth Habit NAs as unknowns
  raw_data$Growth_habit <- raw_data$Growth_habit %>% fct_explicit_na(na_level = "Unknown")
  raw_data$Exotic <- raw_data$Exotic %>% fct_explicit_na(na_level = "Unknown")
  
  # Collapse by strata first, then calculate community level metrics
  PlotSummary <- raw_data %>%
    group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit, Species) %>%
    summarize(Pct_Cov = sum(Pct_Cov, na.rm=T))
  # Now there should only be one measure of cover per species per plot
  
  # Estimate total cover, richness, eveness, and diversity (modified from Helen's code, see below)
  PlotSummary2 <- PlotSummary %>%
    # summarize separately for each native status - and also by Growth_habit (not Stratum)
    group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit) %>%
    # calculate p_i (relative abundance) within exotic status (ignoring other sp):
    mutate(pi = Pct_Cov / sum(Pct_Cov, na.rm = T),
           pi = replace_na(pi, 0)) %>%
    summarize(Diversity_Shannon = shannonH(pi),
              Richness = n_distinct(Species),
              Evenness_Pielou = Diversity_Shannon / log(Richness),
              TotalAbsCov = sum(Pct_Cov, na.rm = T))
  write.csv(PlotSummary2, file=paste("summary_", unique(PlotSummary$Site), ".csv", sep=""), row.names = F)
}
warnings()

# Now do the same thing for special parks with strata = all
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/formatted cover data/special parks")
fileNames <- Sys.glob("clean_*") # 10 sites

for (inp_file in fileNames) {
  raw_data <- read.csv(inp_file, stringsAsFactors = FALSE)
  
  # make sure the dataframe is structured correctly
  raw_data$Site <- as.factor(raw_data$Site)
  raw_data$Growth_habit <- as.factor(raw_data$Growth_habit)
  raw_data$Stratum <- as.factor(raw_data$Stratum)
  raw_data$Pct_Cov <- as.numeric(raw_data$Pct_Cov)
  
  # Consider Exotic NAs and Growth Habit NAs as unknowns
  raw_data$Growth_habit <- raw_data$Growth_habit %>% fct_explicit_na(na_level = "UNK")
  raw_data$Exotic <- raw_data$Exotic %>% fct_explicit_na(na_level = "UNK")
  
  # Select stratum = all, then calculate community level metrics
  PlotSummary <- raw_data %>%
    filter(Stratum=="All") %>%
    group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit, Species) %>%
    summarize(Pct_Cov = sum(Pct_Cov, na.rm=T))
  # Now there should only be one measure of cover per species per plot
  
  # Estimate total cover, richness, eveness, and diversity (modified from Helen's code, see below)
  PlotSummary2 <- PlotSummary %>%
    # summarize separately for each native status - and also by Growth_habit (not Stratum)
    group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit) %>%
    # calculate p_i (relative abundance) within exotic status (ignoring other sp):
    mutate(pi = Pct_Cov / sum(Pct_Cov, na.rm = T),
           pi = replace_na(pi, 0)) %>%
    summarize(Diversity_Shannon = shannonH(pi),
              Richness = n_distinct(Species),
              Evenness_Pielou = Diversity_Shannon / log(Richness),
              TotalAbsCov = sum(Pct_Cov, na.rm = T))
  
  write.csv(PlotSummary2, file=paste("summary_", unique(PlotSummary$Site), ".csv", sep=""), row.names = F)
}

# NOTE - summarise did not maintain rows with 0 pct cover (i.e., if there were no vines in plot 1, there's no row)
# Code doesn't count multiple NAs if there are multiple unknown species per plot. Not sure there is a way around this.
# GRTE didn't retain lat/long/year. Not sure why ('parsing failure'), added in excel for now.
# For island parks - native status is a little funky because we only pulled status for lower 48 states
  # These show up as unknown native status with a growth form

#
#
#
#
#
#
# NTOES AND CHANGES TO ORIGINAL CODE ARE BELOW
#
#
#
#
#
#

# Problem with the the original code - if the species itself is unknown (Species=NA), it doesn't get counted in richness/diversity
# Example is ZION plot 24, try again with taking out species NA
str(raw_data)
raw_data <- read.csv(file.choose())
PlotSummary <- raw_data %>%
  # summarize separately for each native status - and also by Growth_habit (not Stratum)
  group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit) %>%
  # calculate p_i (relative abundance) within exotic status (ignoring other sp):
  mutate(pi = Pct_Cov / sum(Pct_Cov, na.rm = T),
         pi = replace_na(pi, 0)) %>%
  summarize(Diversity_Shannon = shannonH(pi),
            Richness = sum(!is.na(Pct_Cov)),
            Evenness_Pielou = Diversity_Shannon / log(Richness),
            TotalAbsCov = sum(Pct_Cov, na.rm = T)) 

# Fixed by removing filter(!is.na(Species))
# Another problem - will it double count species in different strata with this method?
  # Yes - double counting species codes if the species occurs in different strata
  # Def a problem with richness, not sure if it's a problem with estimate of diversity since that uses cover..?

PlotSummary <- raw_data %>%
  # summarize separately for each native status - and also by Growth_habit (not Stratum)
  group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit) %>%
  # calculate p_i (relative abundance) within exotic status (ignoring other sp):
  mutate(pi = Pct_Cov / sum(Pct_Cov, na.rm = T),
         pi = replace_na(pi, 0)) %>%
  summarize(Diversity_Shannon = shannonH(pi),
            Richness = n_distinct(Species),
            Evenness_Pielou = Diversity_Shannon / log(Richness),
            TotalAbsCov = sum(Pct_Cov, na.rm = T)) 

# This fixes the problem of richness double counting species in different strata within a plot
# But doesn't solve problem of counting NAs if the species is unknown. Not sure there is a way around this.
  # We want to count NAs differently if they NAs are in the same strata
    # Same strata implies different species
    # Different strata could be the same species or different species, but we would have to solve these as individual cases

# Another problem - why is diversity sometimes calculated when there is only a single species in a plot...?
  # Looks like the diversity calculation is messed up when species have multiple measures of cover by strata
  # I wonder for all of these, if we should summarise strata first, and then do all these calculations...?

for (inp_file in fileNames) {
  raw_data <- read_csv(inp_file)
  
  # make sure the dataframe is structured correctly
  raw_data$Site <- as.factor(raw_data$Site)
  raw_data$Growth_habit <- as.factor(raw_data$Growth_habit)
  raw_data$Stratum <- as.factor(raw_data$Stratum)
  raw_data$Pct_Cov <- as.numeric(raw_data$Pct_Cov)
  
  # Consider Exotic NAs and Growth Habit NAs as unknowns
  raw_data$Growth_habit <- raw_data$Growth_habit %>% fct_explicit_na(na_level = "UNK")
  raw_data$Exotic <- raw_data$Exotic %>% fct_explicit_na(na_level = "UNK")
  
  # Collapse by strata first, then calculate community level metrics
  PlotSummary <- raw_data %>%
    group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit, Species) %>%
    summarize(Pct_Cov = sum(Pct_Cov, na.rm=T))
  # Now there should only be one measure of cover per species per plot
  
  # Estimate total cover, richness, eveness, and diversity (modified from Helen's code, see below)
  PlotSummary2 <- PlotSummary %>%
    # summarize separately for each native status - and also by Growth_habit (not Stratum)
    group_by(Dataset, Site, Plot, Long, Lat, Year, Exotic, Growth_habit) %>%
    # calculate p_i (relative abundance) within exotic status (ignoring other sp):
    mutate(pi = Pct_Cov / sum(Pct_Cov, na.rm = T),
           pi = replace_na(pi, 0)) %>%
    summarize(Diversity_Shannon = shannonH(pi),
              Richness = n_distinct(Species),
              Evenness_Pielou = Diversity_Shannon / log(Richness),
              TotalAbsCov = sum(Pct_Cov, na.rm = T))
  # write.csv(PlotSummary, file=paste("summary_", unique(PlotSummary$Site), ".csv", sep=""), row.names = F)
}

# Fixed!

