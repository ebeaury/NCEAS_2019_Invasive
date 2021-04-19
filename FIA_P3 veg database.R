#FIA P3 Veg Subplot analysis for NCEAS project
#install.packages ("rFIA")
library (rFIA)
library (dplyr)
library (tidyr)
library (plyr)
#install.packages('bit')
#install.packages('rFIA')

setwd ("~/Documents/National Parks Project/FIA Database for species abundances/AllStates")
#dirpath <- c("~/Documents/National Parks Project/FIA Database for species abundances")

#Read in just NY's plots
#NYdata <- getFIA (states = c('NY'), common = FALSE,
#                             tables = c('PLOT', 'VEG_VISIT', 'VEG_SUBPLOT', 'VEG_SUBPLOT_SPP'))
#PlotTable <- as.data.frame (NYdata$PLOT)
#PlotTable <- subset (PlotTable, DESIGNCD ==1)
#Test <- subset (PlotTable, PLOT == 61)
#Vars <- c('CN', "INVYR", "PLOT", "STATECD", "LAT", "LON")
#PlotTable <- PlotTable[Vars]
#colnames(PlotTable) <- c('PLT_CN', "INVYR", "PLOT", "STATECD", "LAT", "LON")
#VegVisitTable <- as.data.frame (NYdata$VEG_VISIT)
#VegSubplotTable <- as.data.frame (NYdata$VEG_SUBPLOT)
#VegSubpSppTable <- as.data.frame (NYdata$VEG_SUBPLOT_SPP)
#Test <- subset (VegSubpSppTable, PLOT == 61)

#Read in all data
States <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", 
           "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
           "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
           "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR", "VI") 
#Alldata <- getFIA (states, common = FALSE,
#                  tables = c('PLOT', 'VEG_VISIT', 'VEG_SUBPLOT', 'VEG_SUBPLOT_SPP'))
#Read in downloaded data
PlotTable <- read.csv("PLOT.csv")
Test <- subset (PlotTable, CN == 217595060020004)
#Test <- subset (PlotTable, DESIGNCD ==1)
Test <- subset (PlotTable, PLOT == 61)
Test <- subset (PlotTable, CN == 217595060020004)
Vars <- c('CN', "INVYR", "PLOT", "STATECD", "LAT", "LON", "DESIGNCD")
PlotTable <- PlotTable[Vars]
colnames(PlotTable) <- c('PLT_CN', "INVYR", "PLOT", "STATECD", "LAT", "LON", "DESIGNCD")
VegVisitTable <- read.csv ("VEG_VISIT.csv")
print (unique (VegVisitTable$VEG_MANUAL))
Manual1_7 <- subset (VegVisitTable, TRACE_COVER_ALLOWED == 0) 
VegSubplotTable <- read.csv ("VEG_SUBPLOT.csv")
VegSubpSppTable <- read.csv ("VEG_SUBPLOT_SPP.csv")
Test <- subset (VegSubpSppTable, PLT_CN == 66944222010661)
Test <- subset (PlotTable, STATECD == 78)
Test <- subset (VegSubpSppTable, STATECD == 72)
Test <- subset (VegSubpSppTable, PLOT== 61)
Test <- subset (VegSubplotTable, PLT_CN == 66944222010661)
Test <- subset (PlotTable, PLT_CN == 217595060020004)
print (unique (VegSubpSppTable$STATECD))
Test <- subset (VegVisitTable, STATECD == 17)
write.csv (Test, "Resample17.csv")
#Just most recent data
#NYdata2 <- clipFIA (NYdata, mostRecent = TRUE, mask = NULL, matchEval = FALSE,
#                   evalid = NULL, designCD = NULL, nCores = 1)


#How many subplots per plot were sampled - VEG_SUBP_STATUS_CD = 1
PlotCount <- VegSubplotTable %>%
  group_by (PLT_CN, PLOT) %>%
  summarise (SubplotNum = sum(VEG_SUBP_STATUS_CD == 1))
Test <- subset (PlotCount, PLOT == 61)

#To get plot total cover, sum by PLOT and Species
PlotSum<- VegSubpSppTable %>%
  group_by(PLT_CN, PLOT, VEG_SPCD) %>%
  summarize (PlotSumCover = sum (SP_CANOPY_COVER_TOTAL),
             PlotSumLayer1_2 = sum (SP_CANOPY_COVER_LAYER_1_2),
             PlotSumLayer3 = sum (SP_CANOPY_COVER_LAYER_3),
             PlotSumLayer4 = sum (SP_CANOPY_COVER_LAYER_4))
Test <- subset (PlotSum, PLOT == 61)

ByPlot <- merge (PlotSum, PlotCount, by = c("PLT_CN", "PLOT"))
#Divide PlotSumCover by SubplotNum to account for number of subplots sampled
ByPlot$TotalCover <- with (ByPlot, PlotSumCover/SubplotNum)
ByPlot$Layer1_2Cover <- with (ByPlot, PlotSumLayer1_2/SubplotNum)
ByPlot$Layer3Cover <- with (ByPlot, PlotSumLayer3/SubplotNum)
ByPlot$Layer4Cover <- with (ByPlot, PlotSumLayer4/SubplotNum)
Test <- subset (ByPlot, PLOT == 61)
#Add in Lat/Long
Test <- subset (PlotTable, PLOT == 61)
Test <- subset (PlotTable, PLT_CN == 217595060020004)
Output <- merge (ByPlot, PlotTable, by = c("PLT_CN", "PLOT"))
print (unique (Output$DESIGNCD))
Test <- subset (Output, PLOT == 61)
Vars <- c("PLOT", "LAT", "LON", "INVYR", "TotalCover", "Layer1_2Cover",
          "Layer3Cover", "Layer4Cover", "STATECD", "VEG_SPCD", "PLT_CN", "DESIGNCD")
Output <- Output[Vars]
Output <- Output[c(9, 1, 11, 2, 3, 4, 10, 5, 6, 7, 8, 12)]
#Write to file
#write.csv (Output, "NY P3 Veg.csv")
write.csv (Output, "FIA P3 Veg Layers.csv")


