#NCEAS FIA Database analysis
#Read in Phase3 vegetation data for each site and add species names

library (dplyr)
library (plyr)
setwd ("~/Documents/National Parks Project/FIA Database for species abundances/AllStates")

#VegData <- read.csv ("FIA P3 Veg.csv")
VegData <- read.csv ("FIA P3 Veg Layers.csv")

DataUnique <- unique (VegData$PLT_CN)
length (DataUnique)
#Use our latest species list
PLANTSData <- read.csv ("taxonomy_temp10_revised.csv")
Vars <- c("Accepted.Symbol", "Symbol", "Scientific.Name", "Growth.Habit", "genus", "epithet", "inv_L48")
PLANTSData <- PLANTSData[, Vars]
colnames(PLANTSData) <- c("VEG_SPCD", "SynCode", "SciName", "Growth.Habit", "genus", "epithet", "inv_L48")

NewData <- join (VegData, PLANTSData, by = "VEG_SPCD", match = "first") #Used join instead of dplyr's left_join to coerce to first observation


#This takes care of species with Accepted Names. But not Codes that are the old sci names
WithNames <- subset (NewData, !is.na(SciName))
GenusOnly <- subset (WithNames, is.na(epithet))
NoNames <- subset (NewData, is.na(SciName))
NoNames <- NoNames [, 1:13]


##Check the Synonym codes
PLANTSSyn <- read.csv ("taxonomy_temp10_revised.csv")
SynVars <- c("Synonym.Symbol", "Symbol", "Scientific.Name", "Growth.Habit", "genus", "epithet", "inv_L48")
PLANTSSyn <- PLANTSSyn[, SynVars]
PLANTSSyn$Symbol <- as.character(PLANTSSyn$Symbol)
colnames (PLANTSSyn) <- c("AccCode", "VEG_SPCD", "SciName", "Growth.Habit", "SYNgenus", "SYNepithet", "inv_L48")

#Synonyms <- left_join (NoNames, PLANTSSyn, by = "VEG_SPCD")
Synonyms <- join (NoNames, PLANTSSyn, by = "VEG_SPCD", match = "first")


SynName <- subset (Synonyms, !is.na(SciName))
NoName2 <- subset (Synonyms, is.na(SciName))
#colnames(SynName) <- c("VEG_SPCD", "X", "STATECD", "PLOT", "PLT_CN", "LAT",
#                       "LON", "INVYR", "PlotCover", "DESIGNCD",
#                       "SynCode", "SciName", "Growth.Habit", "genus", "epithet", "inv_L48"  )
colnames(SynName) <- c("VEG_SPCD", "X", "STATECD", "PLOT", "PLT_CN", "LAT",
                       "LON", "INVYR", "TotalCover", "Layer1_2Cover", "Layer3Cover", "Layer4Cover", "DESIGNCD",
                       "SynCode", "SciName", "Growth.Habit", "genus", "epithet", "inv_L48"  )

#rbind WithNames and SynNames to have SciName associated with both accepted names and synonyms 
VegDataWithNames <- rbind (WithNames, SynName)

##Add in species that are NA
#colnames(NoName2) <- c("VEG_SPCD", "X", "STATECD", "PLOT", "PLT_CN", "LAT",
#                       "LON", "INVYR", "TotalCover", "Layer1_2Cover", "Layer3Cover", "Layer4Cover", "DESIGNCD",
#                       "SynCode", "SciName", "Growth.Habit", "genus", "epithet", "inv_L48"  )
colnames(NoName2) <- c("VEG_SPCD", "X", "STATECD", "PLOT", "PLT_CN", "LAT",
                       "LON", "INVYR", "TotalCover", "Layer1_2Cover", "Layer3Cover", "Layer4Cover","DESIGNCD",
                       "SynCode", "SciName", "Growth.Habit", "genus", "epithet", "inv_L48"  )

AllVegData <- rbind (VegDataWithNames, NoName2)
AllVegData <- subset (AllVegData, select = -c(X, SynCode))
AllVegData <- AllVegData [order(AllVegData$STATECD, AllVegData$PLT_CN, AllVegData$VEG_SPCD), ]
length (unique (AllVegData [["PLT_CN"]])) #How many plots
#Export
write.csv (AllVegData, "FIA Veg data Layers 4-15-20.csv")

NIcat <- subset (AllVegData, inv_L48 == "NI")
write.csv (NIcat, "NI category.csv")

#Is there more than one year per plot?

Resample <- aggregate(INVYR ~ PLOT+STATECD+LAT+LON, data=unique(AllVegData[c("PLOT","INVYR", "STATECD", "LAT", "LON")]), FUN=length)
Resample <- Resample [order(Resample$INVYR, decreasing = TRUE), ]
Twice <- subset (Resample, INVYR == 2)
write.csv (Twice, "Resampled plots.csv")

table (Resample$INVYR)
# Of the 2714 plots, 2221 were sampled once, 493 sampled twice.
#Only the most recent sampling date
Latest <- aggregate (AllVegData$INVYR, list (AllVegData$STATECD, AllVegData$PLOT, AllVegData$LAT, AllVegData$LON), max)
colnames (Latest) <- c("STATECD", "PLOT", "LAT", "LON", "INVYR")
LatestPlots <- left_join(Latest, AllVegData, by = c("STATECD", "PLOT", "LAT", "LON", "INVYR"))
length (unique (LatestPlots [["PLT_CN"]])) #How many plots

write.csv (LatestPlots, "FIA Veg data Layers Latest only 4-15-20.csv")

#Only plots that were resampled
ResampledList <- read.csv ("Resampled plots.csv")
Resampled <- left_join(ResampledList, AllVegData, by = c("PLOT", "STATECD", "LAT", "LON"))
Resampled <- subset (Resampled, select = -c(INVYR.x))
colnames (Resampled)[8] <- "INVYR"

write.csv(Resampled, "FIA Veg Data resampled plots 4-15-20.csv")





                       