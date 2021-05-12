rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

# Coding raw data sheets to match Bethany's sample format

# Site	Plot	Species	Pct_Cov	Stratum	Exotic	Growth_habit
  # Site = Park
  # Plot = Plot
  # Species = CODE
  # Pct_Cov = Abundance
  # Stratum = ????
  # Exotic = STATUS
  # Growth_habit = need to append

setwd("/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data")

fileNames <- Sys.glob("clean_*.csv")
df <- data.frame(Site=NA, Plot=NA, Species=NA, Pct_Cov=NA, Exotic=NA)

for (inp_file in fileNames) {
  raw_data <- read_csv(inp_file)
  new_data <- raw_data %>% select(Park, Plot, CODE, Abundance, STATUS, Stratum)
  colnames(new_data) <- c("Site", "Plot", "Species", "Pct_Cov", "Stratum","Exotic")
  new_data <- na.omit(new_data)
  # write.csv(new_data, paste(unique(new_data$Site), "spcov.csv"), row.names=F)
}

# Maybe need to collapse if there are repeat reports of cover per plot+species+strata? 
# Not sure why there would be but can double check?

#
#
#

# Add environmental information (location, community type, and area sampled) to cover sheets
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Environmental Data")
envirofiles <- Sys.glob("split*")

enviro <- data.frame(Park=NA, Plot=NA, Lat=NA, Lon=NA, Area_sampled=NA, com_type=NA)

for(inp_file in envirofiles){
  raw_data <- read.csv(inp_file, stringsAsFactors=FALSE)
  new_data <- raw_data %>% select(Park, Plot, Lat, Lon, Area_sampled, Com_type)
  new_data <- unique(new_data)
  colnames(new_data) <- c("Park", "Plot", "Lat", "Lon", "Area_sampled", "com_type")
  enviro <- rbind(enviro, new_data)
}
enviro <- enviro[-1,]
head(enviro)
str(enviro) # Earlier versions, problems occur when I make plot a factor. Keep as a character and it should be OKAY

# Loop through cover sheets to left_join
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/archived")
fileNames <- Sys.glob("clean*")

test <- data.frame(Park=NA, Plot=NA)
for(inp_file in fileNames){
  raw_data <- read.csv(inp_file, stringsAsFactors=FALSE)
  new_data <- raw_data %>% select(Park, Plot)
  new_data <- unique(new_data)
  test <- rbind(test, new_data)
}

head(test)
sum(is.na(test$Plot))


fileNames <- "clean_annotated_plots_agfo_spcov.csv"


for(inp_file in fileNames){
  raw_data <- read.csv(inp_file, stringsAsFactors=FALSE)
  raw_data$Park <- as.factor(raw_data$Park)
  raw_data$Plot <- as.character(raw_data$Plot)
  
  new_data <- left_join(raw_data, enviro, by=c("Park", "Plot"))
  write.csv(new_data, paste(unique(levels(raw_data$Park)),"_cover.csv", sep=""), row.names=F)
}
warnings()

# Double check that worked

# Now make the files look like formatted datasheet

setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/raw data with enviro attached")
fileNames <- Sys.glob("*cover.csv")

for(inp_file in fileNames){
  raw_data <- read.csv(inp_file, stringsAsFactors=FALSE)
  raw_data$Dataset <- "NPS"
  raw_data$Year <- NA
  raw_data$Growth_habit <- NA
  clean_data <- raw_data %>% select(Dataset, Park, Plot, Lon, Lat, Year, CODE, Abundance, Stratum, STATUS, Growth_habit)
  colnames(clean_data) <- c("Dataset", "Site", "Plot", "Long", "Lat", "Year", "Species", "Pct_Cov", "Stratum", "Exotic", "Growth_habit")
  write.csv(clean_data, paste("clean",inp_file,sep="_"), row.names=F)
}

# Woohoo!

# Now I need to get growth form binded in
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data")
growths <- read.csv("taxonomy_temp9.csv")
head(growths)
  # Columns I need are Accepted.Symbol (I think) and GrowthForm
growth2 <-growths %>% select(Accepted.Symbol, GrowthForm)
head(growth2)
colnames(growth2)[1] <- c("Species")
growth2 <- unique(growth2)
df <- data.frame(table(growth2$Species)) # no duplicates!

# Add synonymous codes to Ian's taxonomy sheet
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability")
usda <- read.csv("usda_plants.csv")

head(usda)
usda2 <- usda %>% filter(!Synonym=="")
head(usda2)

usda3 <- left_join(usda2, growth2, by="Species")
head(usda3)
syns <- usda3 %>% select(Synonym, GrowthForm) %>% rename(Species = Synonym)
head(syns)
syns <- unique(syns)
df <- data.frame(table(syns$Species)) # no duplicates
syns <- syns %>% drop_na(GrowthForm)

new_growth <- rbind(growth2, syns)
head(new_growth)
tail(new_growth)
new_growth <- unique(new_growth)
df <- data.frame(table(new_growth$Species)) # one weird duplicate

print(new_growth %>% filter(Species=="CABR42"))
new_growth <- new_growth %>% filter(!(Species=="CABR42" & is.na(GrowthForm)))
new_growth <- droplevels(new_growth)
str(new_growth)

levels(new_growth$GrowthForm)
new_growth <- new_growth %>% mutate(Growth_habit=recode(GrowthForm, Subshrub="Shrub"))
head(new_growth)
new_growth <- new_growth %>% select(Species, Growth_habit)
levels(new_growth$Growth_habit)

print(new_growth %>% filter(Species=="Unknown"))
# Fixed! Added data for a lot of synonyms (I hope)

setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/formatted cover data")

fileNames <- Sys.glob("clean*")

for(inp_file in fileNames){
  raw_data <- read.csv(inp_file, stringsAsFactors=FALSE)
  raw_data <- raw_data %>% select(-Growth_habit)
  new_data <- left_join(raw_data, new_growth, by="Species")
  new_data <- unique(new_data)
  
  # drop NAs in park column
  clean_data <- new_data %>% drop_na(Site)
  write.csv(clean_data, paste(inp_file), row.names=F)
}

# Worked!

# Adding dates is in "Cleaning_envirodata.R#

#
#

