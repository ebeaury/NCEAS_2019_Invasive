#NCEAS FIA Database analysis
#Read in Phase3 vegetation data and match species ID code with PLOTS database names

library (dplyr)
setwd ("~/Documents/National Parks Project/FIA Database for species abundances/AllStates")
#VegData <- read.csv ("FIA P3 Veg.csv")
VegData <- read.csv ("FIA P3 Veg Layers.csv")
PLOTSData <- read.csv ("taxonomy_temp10_revised.csv")
Vars <- c("Accepted.Symbol", "Symbol", "Scientific.Name", "genus", "epithet")
PLOTSData <- PLOTSData[, Vars]
colnames(PLOTSData) <- c("VEG_SPCD", "SynCode", "SciName", "genus", "epithet")

NewData <- left_join (VegData, PLOTSData, by = "VEG_SPCD")

write.csv (NewData, "P3 FIA Species names.csv")

WithNames <- subset (NewData, !is.na(SciName))
NoNames <- subset (NewData, is.na(epithet)) #Species not in PLANTS and species only ID'd to Genus
#Vars <- c("STATECD", "PLOT", "PLT_CN", "LAT", "LON", "INVYR", "VEG_SPCD", "PlotCover")
Vars <- c("STATECD", "PLOT", "PLT_CN", "LAT", "LON", "INVYR", "VEG_SPCD", "TotalCover", "Layer1_2Cover", "Layer3Cover", "Layer4Cover")
NoNames <- NoNames [, Vars]
NANames <- unique (NoNames$VEG_SPCD)

#Check the Synonym codes
PLOTSDataAlt <- PLOTSData
colnames (PLOTSDataAlt) <- c("AccCode", "VEG_SPCD", "SciName", "SYNgenus", "SYNepithet")
Synonyms <- left_join (NoNames, PLOTSDataAlt, by = "VEG_SPCD")
SynName <- subset (Synonyms, !is.na(SYNepithet))
NoName2 <- subset (Synonyms, is.na(SciName))
NANames <- unique (NoName2$VEG_SPCD)
write.csv (NANames, "Missing Species Codes.csv")

WithNames <- subset ()
#Check known:unknown ratio
#Add up all the knowns by plot
Known <- WithNames %>%
  group_by (PLT_CN) %>%
  summarize (KnownCover = sum(PlotCover))
#Add up total cover
AllCover <- NewData  %>%
  group_by (PLT_CN) %>%
  summarize (AllCover = sum(PlotCover))
#Add up all the unknowns by plot
#Unknown <- WithNames
UnKnown <- NoNames %>%
  group_by (PLT_CN) %>%
  summarize (UnknownCover = sum(PlotCover))
#Merge AllCover with Unknown
AllUnknown <- full_join (AllCover, UnKnown, by = "PLT_CN")
AllUnknown[is.na(KnownUnknown)] <- 0 
#Get ratio
AllUnknown$Ratio <- KnownUnknown$UnknownCover/KnownUnknown$AllCover

#How many unknowns are >10%
TenPercent <- subset (KnownUnknown, Ratio > .10)
TwentyFivePercent <- subset (KnownUnknown, Ratio > .25)
hist (KnownUnknown$Ratio)

##################################################

#What are the major species ID'd to Genus but not species?
GenusOnly <- subset (WithNames, is.na(epithet))
#Get basic stats by Genus
GenusOnlySummaries <- GenusOnly %>%
  group_by(genus) %>%
  summarize (mean = mean (PlotCover), min = min(PlotCover), max = max(PlotCover), n=n())

GenusOnlySummaries <- GenusOnlySummaries[order(-GenusOnlySummaries$mean),]
write.csv (GenusOnlySummaries, "GenusOnlySummaries.csv")

#Check the Synonym codes
PLOTSDataAlt <- PLOTSData
colnames (PLOTSDataAlt) <- c("AccCode", "VEG_SPCD", "SciName", "genus", "epithet")
Synonyms <- left_join (NoNames, PLOTSDataAlt, by = "VEG_SPCD")
NoName2 <- subset (Synonyms, is.na(SciName.x))

write.csv (NoName2, "Missing codes.csv")

#Next, add up all the unknowns by plot
UnknSum <- NoName2 %>%
  group_by (PLT_CN) %>%
  summarize (UnknCover = sum())

PlotSum<- VegSubpSppTable %>%
  group_by(PLT_CN, PLOT, VEG_SPCD) %>%
  summarize (PlotSumCover = sum (SP_CANOPY_COVER_TOTAL))
Test <- subset (PlotSum, PLOT == 61)

