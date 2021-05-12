#################################################################################
##### Compile locations of all plots #####
##### Written by Regan Early
##### Written on 20th May 2020
##### Modified on 21st August 2020
#################################################################################

# library(dplyr)
library(maps)
library(mapdata)
library(sp)
library(rgdal)
library(maptools)

##### Compile sample locations #####
wd <- "E:\\NON_PROJECT\\NCEAS2\\"

## NEON plots are all very close to each other, so each location will be repeated multiple times
neon <- read.csv(paste0(wd, "NEON\\PLANTS\\ABUNDANCE_IMPACT_ANALYSES\\NEON_InesData20thAug.csv"), as.is=T)
neon <- neon[,c("plotID","decimalLongitude","decimalLatitude","year")]
colnames(neon) <- c("Plot", "Long", "Lat", "Year")
neon <- unique(neon)
neon$source <- "neon"
neon$joinCol <- "Plot" ## Name of the column in the original data table to which the env data can be joined.
neon.shp <- neon
coordinates(neon.shp) = ~Long + Lat ## Make neon a spatial points shapefile
writeOGR(neon.shp, dsn=paste0(wd, "NEON\\PLANTS\\ABUNDANCE_IMPACT_ANALYSES"), layer="NEON_InesData20thAug_pts", driver="ESRI Shapefile")

## FIA locations are not accurate - do not use for modelling
fia <- read.csv(paste0(wd, "FIA\\FIA_PlotKeys.csv"), as.is=T) ## Picked as it contained the most data. There are four alternative versions of the data that could be used.
# lg <- unlist(lapply(fia$Long, function(x) {substr(x, nchar(x)-2, nchar(x))}))
# lt <- unlist(lapply(fia$Lat, function(x) {substr(x, nchar(x)-2, nchar(x))}))  
# options(scipen = 999)
# fia$plotUnique <- paste0(fia$Plot,"_",lg,"_",lt) ## Add a unique identifier for each plot
# options(scipen = 6)
# write.csv(fia, file=paste0(wd, "FIA\\FIA_PlotDiv_6July2020_uniqueID.csv"), row.names=F)

fia <- fia[,c("UniqueID","Long","Lat", "Year")]
colnames(fia) <- c("Plot", "Long", "Lat", "Year")
fia <- unique(fia)
fia$source <- "fia"
fia$joinCol <- "UniqueID" ## Name of the column in the original data table to which the env data can be joined.

fia.shp <- fia
coordinates(fia.shp) = ~Long + Lat ## Make fia a spatial points shapefile
writeOGR(fia.shp, dsn=paste0(wd, "FIA"), layer="FIA_PlotDiv_18Aug2020_uniqueID_pts", driver="ESRI Shapefile")

## BLM
blm <- read.csv(paste0(wd, "BLM\\AIM_PlotDiv_25June2020.csv"), as.is=T)
blm <- blm[,c("Plot","Long","Lat","Year")] ## Note that some plot locations appear multiple times with different IDs - they were sampled in different years. Helen will sort out.
blm <- unique(blm)
blm$source <- "blm"
blm$joinCol <- "Plot" ## Name of the column in the original data table to which the env data can be joined.

blm.shp <- blm
coordinates(blm.shp) = ~Long + Lat ## Make blm a spatial points shapefile
writeOGR(blm.shp, dsn=paste0(wd, "BLM"), layer="AIM_PlotDiv_25June2020_pts", driver="ESRI Shapefile")

## NPS 
nps <- read.csv(paste0(wd, "NPS\\NPS_PlotDiv_30June2020.csv"), as.is=T) 
nps$plotUnique <- paste0(nps$Site, nps$Plot) ## Add a unique identifier for each plot
# write.csv(nps, file=paste0(wd, "NPS\\NPS_PlotDiv_19Aug2020_uniqueID.csv"), row.names=F)
nps <- nps[,c("plotUnique", "Long","Lat","Year")]
colnames(nps) <- c("Plot", "Long", "Lat", "Year")
nps <- unique(nps)
nps$source <- "nps"
nps$joinCol <- "plotUnique" ## Name of the column in the original data table to which the env data can be joined.
nps$Year[is.na(nps$Year)] <- -9999
nps <- na.omit(nps)

nps.shp <- nps
coordinates(nps.shp) = ~Long + Lat ## Make blm a spatial points shapefile
writeOGR(nps.shp, dsn=paste0(wd, "NPS"), layer="NPS_PlotLocations_19Aug2020_uniqueID", driver="ESRI Shapefile")

## Merge all location data
names(neon) <- names(fia) <- names(blm) <- names(nps) <- c("plot", "lon", "lat", "year", "source","joinCol")
dat <- rbind(neon, fia, blm, nps)
write.csv(dat, "E:\\NON_PROJECT\\NCEAS2\\plot_locs_12stAug2020.csv", row.names=F)

dat.shp <- dat
coordinates(dat.shp) = ~lon + lat ## Make a spatial points shapefile
writeOGR(dat.shp, dsn="E:\\NON_PROJECT\\NCEAS2", layer="plot_locs_21stAug2020_pts", driver="ESRI Shapefile")

##### Plot geographic locations #####
data(stateMapEnv)
map('state')
colrs <- data.frame(source=c("neon", "fia", "blm", "nps"), colrs=c("hotpink","darkgreen","purple", "blue"), stringsAsFactors=F)
dat.plot <- merge(dat, colrs)
points(dat.plot$lon, dat.plot$lat, pch=16, cex=0.5, col=dat.plot$colrs)

legend(-126, 35, legend=colrs$source,
       col=colrs$colrs, pch=16, cex=0.7, bty="n")

# par(mfrow=c(2,2))
map('usa', main="NEON")
points(dat$lon[dat$source=="neon"], dat$lat[dat$source=="neon"], pch=16, cex=0.5, col="hotpink")

map('usa', main="FIA")
points(dat$lon[dat$source=="fia"], dat$lat[dat$source=="fia"], pch=16, cex=0.5, col="darkgreen")

map('usa', main="BLM")
points(dat$lon[dat$source=="blm"], dat$lat[dat$source=="blm"], pch=16, cex=0.5, col="purple")

map('usa', main="NPS")
points(dat$lon[dat$source=="nps"], dat$lat[dat$source=="nps"], pch=16, cex=0.5, col="blue")



