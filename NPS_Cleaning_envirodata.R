rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

# Need to make sure all environmental data sheets match species cover data sheets
  # Column names are uniform
  # Plot locations are ready to go
  # Any other environmental data is easily matched

# Relevant environmental data recorded by NPS
  # Plot locations
  # Classified community type from biotic resistance
  # Area sampled

# Use enviro datasheets that have been cleaned for biotic resistance
  # Make master sheet of Site+Plot+Enviro characteristics to make sure we can fill in holes

# location of species cover files
setwd("/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data")
fileNames <- Sys.glob("clean_*")

parks <- data.frame(Park=NA)

for(inp_file in fileNames){
  raw_data <- read_csv(inp_file)
  park <- unique(raw_data$Park)
  parks <- rbind(parks, park)
}

parks <- parks[2:136,]
parks <- as.character(parks)
parks <- tolower(parks)

# location of current status
setwd("/Users/ebeaury/Box/National Parks/Datasheets/enviro_data/clean/split plots")
envirdata <- Sys.glob("split_*")

# Make a master sheet of site+plot for the parks we have abundance data
# Make a master environmental sheet with site+plot+plot location+com_type+area sampled

# If site+plot combination is in richness site+plot combination, make a new df with
  # site, plot, com_type, area sampled, and maybe wait on other enviro data?
  # then I can rbind this info in from new sheets as well
  # and for whatever enviro datahseets I don't have

richness <- read.csv(file.choose())
str(richness)

setwd("/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data")
fileNames <- Sys.glob("clean*")

master <- data.frame(Park=NA, Plot=NA)

for(inp_file in fileNames){
  raw_data <- read_csv(inp_file)
  raw_data$Park <- as.factor(raw_data$Park)
  raw_data$Plot <- as.factor(raw_data$Plot)
  
  data <- raw_data %>% select(Park, Plot)
  data <- unique(data)
  data <- na.omit(data)
  master <-rbind(master, data)
}

master <- master[2:27189,]
head(master)
str(master)

master$Park <- as.factor(master$Park)
levels(master$Park)
table(master$Park)

sum(is.na(master$Plot))

# Sample size = 27,188 plots across 134 sites
hist(table(master$Park))
sort(table(master$Park))
# Some of the big plots sampled a tonnnnn of plots (Grand Canyon, Grand Tetons, Lake Mead)
# Most parks sampld < 250 plots
  # Min = 3-4 plots at several sites (Forts)
  mean(table(master$Park)) #200 plots per site
  median(table(master$Park)) # median is 74 plots

# Number of plots per site
ggplot(master, aes(x=Park)) + geom_histogram(stat="count")

print(master[master$Park=="CRLA",])
  # Funky plot names are easily read by R

setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability")
richness  <- read.csv("Plot_totals_all_parks_w_area.csv")
str(richness)
richness2 <- richness %>% select(Park, Plot, Lat, Lon, Area_sampled, com_type)
head(richness2)

# match richness file to park and plot from cover data to see what we have
all_data <- left_join(master, richness2,by=c("Park", "Plot"))
sum(is.na(all_data$Lat))
  # missing environmental data for 7,132 plots

# Realizing not all sheets have the com_type, lat and lon columns. These were subsetted in a different cleaning step with richness data
# Need to update all sheets to match what's in the Richness master sheet.
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Environmental Data")
envirofiles <- Sys.glob("split*")

richness2 <- richness %>% select(Park, Plot, Lat, Lon, Area_sampled, com_type)
head(richness2)
str(richness2)

# for(inp_file in envirofiles){
  raw_data <- read_csv(inp_file)
  raw_data$Park <- as.factor(raw_data$Park)
  raw_data$Plot <- as.factor(raw_data$Plot)
  park <- unique(levels(raw_data$Park))
  
  if(park %in% levels(richness2$Park)){
    new_data <- left_join(raw_data, richness2, by=c("Park", "Plot"))
    write.csv(new_data, paste(inp_file), row.names = F)
  }
}

# Now add new park enviro data to richness2
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Environmental Data")
envirofiles <- Sys.glob("split*")

richness3 <- data.frame(Park=NA, Plot=NA, Lat=NA, Lon=NA, Area_sampled=NA, com_type=NA)

for(inp_file in envirofiles){
  raw_data <- read_csv(inp_file)
    new_data <- raw_data %>% select(Park, Plot, Lat, Lon, Area_sampled, Com_type)
    new_data <- unique(new_data)
    colnames(new_data) <- c("Park", "Plot", "Lat", "Lon", "Area_sampled", "com_type")
    richness3 <- rbind(richness3, new_data)
}

head(richness3)
richness3$Park <- as.factor(richness3$Park)
table(richness3$Park)

# See how much data we fill in with this
all_data <- left_join(master, richness3,by=c("Park", "Plot"))
  # Some random plots have been duplicated
  # check which ones
    cover <- data.frame(table(master$Park))
    merged <- data.frame(table(all_data$Park))
    df <- merge(cover, merged, by="Var1")
    df$diff <- df$Freq.x - df$Freq.y
  # Need to fiz OZAR - 17 plots have contradictory information
    print(all_data %>% filter(Park=="OZAR"))
    all_data$Plot <- as.factor(all_data$Plot)
    ugh <- data.frame(table(all_data$Plot[all_data$Park=="OZAR"]))

# Fixed OZAR - go back and run again
    # Now the sample sizes between the merged data (all_data) and cover data (master) agree

# Look to see how much environmental data is missing
sum(is.na(all_data$Lat))
  # Now we're only missing data for 4505 rows

#
#
#
#
# WORKING STARTING HERE
#
#
#
#
#
#

    
head(richness3)
richness3$Park <- as.factor(richness3$Park)
table(richness3$Park)
# ^ This is my master environmental data sheet. But contains different values (probs coordinates) for some parks
# In other words, some parks and plots are repeated if we throw them together

# Now rejoin to see if we added any missing pieces
all_data <- left_join(master, richness3,by=c("Park", "Plot"))
head(master)
  # Sample should be 27,252. Somewhere adding 1500 plots of duplicates

# Which are repeated?
size <- data.frame(table(richness3$Park)) # sample size from enviro
match <- data.frame(table(master$Park)) # sample size from cover
binded <- data.frame(table(all_data$Park))
df <- merge(size, match, by="Var1")
df <- merge(df, binded, by="Var1")

# So it's keeping all the extra plots from richness3. 
print(all_data %>% filter(Park=="ACAD"))
  # Looks like it's when there are similar, but not the same coordinates...

all_data <- unique(all_data)
head(all_data)
sum(is.na(all_data$Lat))
# still missing environmental data for 4505 plots

head(all_data %>% filter(Park=="GRBA"))
head(richness3 %>% filter(Park=="GRBA"))

# Filled in a lot of gaps, but missing data for many plots

# Fixing stuff in sheets and rerunning
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Environmental Data")
envirofiles <- Sys.glob("split*")

richness3 <- data.frame(Park=NA, Plot=NA, Lat=NA, Lon=NA, Area_sampled=NA, com_type=NA)

for(inp_file in envirofiles){
  raw_data <- read_csv(inp_file)
  new_data <- raw_data %>% select(Park, Plot, Lat, Lon, Area_sampled, Com_type)
  new_data <- unique(new_data)
  colnames(new_data) <- c("Park", "Plot", "Lat", "Lon", "Area_sampled", "com_type")
  richness3 <- rbind(richness3, new_data)
}
all_data <- left_join(master, richness3,by=c("Park", "Plot"))

head(richness3)
richness3$Park <- as.factor(richness3$Park)
table(richness3$Park)
head(richness3 %>% filter(Park=="HOCU"))
head(all_data %>% filter(Park=="HOCU"))

# Now how many NAs?
sum(is.na(all_data$Lat))
  # ~ 3699 but everything that's filled in is done
all_data_complete <- na.omit(all_data)
  # ~21,000 plots with everything filled in

# Now - I want to use this environmental data to bind back to the raw data sheets

# Extract sampling year and combine with datasheets
setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Environmental Data")
envirofiles <- Sys.glob("split*")

dates <- data.frame(Park=NA, Plot=NA, Date=NA)

for (inp_file in envirofiles) {
  raw_data <- read_csv(inp_file)
  
  # need to rename all continuous cover columns to Abundance
  if ("Event_Date" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$Event_Date)
  } else if ("Survey Date" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$`Survey Date`)
  } else if ("Survey_Date" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$Survey_Date)
  } else if ("Surv_Date" %in% colnames(raw_data)) 
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$Surv_Date)
  } else if ("DATE_" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$DATE_)
  } else if ("SurveyDate" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$SurveyDate)
  } else if ("DATE" %in% colnames(raw_data))
  {
    clean_data <- raw_data %>% mutate(Date = raw_data$DATE)
    
  }
  else {
    clean_data <- raw_data %>% mutate(Date = raw_data$Date)
    
  }
  new_data <- clean_data %>% select(Park, Plot, Date)
  dates <- rbind(new_data, dates)
  
}

dates2 <- dates %>% separate(Date, c("day", "month", "year"), sep="/")
str(dates2)
dates2$year <- as.factor(dates2$year)                             
levels(dates2$year)

levels(dates2$year) <- c("2000","2003","2004","2005","2006","2007","2008","2009","2010","2011","1195","2012","1976",
                         "1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007",
                         "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015" ,"2016" ,"2018" ,"2020", "1999")
head(dates2)

# Join back to raw data sheets
dates3 <- dates2 %>% select(Park, Plot, year)
colnames(dates3) <- c("Site", "Plot", "year")
head(dates3)
str(dates3)
dates3$Site <- as.factor(dates3$Site)

setwd("C:/Users/ebeaury/Box/National Parks/Vulnerability/Continuous cover data/formatted cover data")
fileNames <- Sys.glob("clean*")

for(inp_file in fileNames){
  raw_data <- read.csv(inp_file, stringsAsFactors = FALSE)
  raw_data$Site <- as.factor(raw_data$Site)
  raw_data$Plot <- as.character(raw_data$Plot)
  new_data <- left_join(raw_data, dates3, by=c("Site", "Plot"))
  new_data <- unique(new_data)
  
  new_data <- new_data %>% select(-(Year))
  new_data <- new_data %>% rename("Year" = year)
  
  # drop NAs in park column
  clean_data <- new_data %>% drop_na(Site)
  write.csv(clean_data, paste(inp_file), row.names = F)
}

# Woohoo!

