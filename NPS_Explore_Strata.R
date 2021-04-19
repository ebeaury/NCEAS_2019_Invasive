rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

#
#
#
#
#

# Add growth habit to raw data sheets, maybe update invasive status

# Change all raw data sheet column names to match mock data sheet (so Helen can do plot level diversity, evenness, etc.)

# Change summarized column names to match mock data sheet

# set working directory to find datasheets with species cover
getwd()

fileNames <- Sys.glob("annotated_*.csv")
length(fileNames)# 142 sites with continuous cover estimates

# Add column to all data sheets so that there is a common 'Abundance' column
for (inp_file in fileNames) {
  raw_data <- read_csv(inp_file)
  
  # need to rename all continuous cover columns to Abundance
  if ("Real Cover" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$'Real Cover')
  } else if ("Continuous Cover Per Stratum" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$"Continuous Cover Per Stratum")
  } else if ("Real_Cover" %in% colnames(raw_data))  
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$Real_Cover)
  }  else if ("PercentCover" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$PercentCover)
  } else if ("Continuous.Cover.Per.Stratum" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$Continuous.Cover.Per.Stratum)
  } else if ("Cover" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$Cover)
  } else if ("%_Cover" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$"%_Cover")
  } else if ("Real_Stratum_Cover" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$Real_Stratum_Cover)
  } else if("Continuous Cover per Stratum" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$"Continuous Cover per Stratum")
  } else
  {
    clean_data <- raw_data %>% mutate(Abundance = raw_data$Total_Plot_Cover)
  }
  
  write.csv(clean_data, file=paste("clean_", inp_file, sep=""), row.names = F)
}

# Add column to all data sheets so that there is a common 'Stratum' column
fileNames <- Sys.glob("clean_*")

for (inp_file in fileNames) {
  raw_data <- read_csv(inp_file)
  
  # need to rename all strata columns to Stratum
  if ("Stratum" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Stratum = raw_data$Stratum)
  } else if ("Strata" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Stratum = raw_data$Strata)
  } else if ("Stratum Sort" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Stratum = raw_data$`Stratum Sort`)
  } else
  {
    clean_data <- raw_data %>% mutate(Stratum = raw_data$Layer)
  }
  write.csv(clean_data, paste(inp_file), row.names = F)
}
    

# Clean and summarize data for plot level stats (DONE, no need to repeat)
fileNames <- Sys.glob("clean_*.csv")
length(fileNames)

for (inp_file in fileNames) {
  raw_data <- read_csv(inp_file)
  
  # Drop any species information that we don't have abundance data for, and change low cover to trace amounts
   # if cover=<1, change to 0.5 (trace amounts)
  new_data <- raw_data %>% filter(!is.na(Abundance)) %>% 
    mutate(Abundance = ifelse(Abundance =="<1",0.5,Abundance))
  # drop 0s
  new_data <- new_data[!new_data$Abundance==0,]
  write.csv(new_data, file=paste(inp_file), row.names = F)
  
}

# Figure out which parks have strata = all so that we do plot summaries differently

setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/formatted cover data")
fileNames <- Sys.glob("clean_*")

test <- data.frame(Dataset=NA, Site=NA, Plot=NA, Long=NA, Lat=NA, Species=NA, Pct_Cov=NA,
                   Stratum=NA, Exotic=NA, Year=NA, Growth_habit=NA)

for (inp_file in fileNames) {
  raw_data <- read.csv(inp_file, stringsAsFactors = FALSE)
  test <- rbind(test, raw_data)
}

str(test)

test$Stratum <- as.factor(test$Stratum)
levels(test$Stratum)
print(test %>% filter(Stratum=="All") %>% select(Site) %>% distinct())

#
#
#
#

