#################################################################################
##### Compile locations of all plots #####
##### Written by Regan Early
##### Written on 30th September 2020
##### Modified on ...
#################################################################################

# library(maps)
library(mapdata)
library(sp)
library(rgdal)
# library(maptools)

##### Extract sample locations only #####
wd <- "E:\\NON_PROJECT\\NCEAS2\\LOCATION_DATA\\"

dat <- read.csv(paste0(wd, "PLOT_DIVERSITY\\Alldata_PlotDiv_30Sept.csv"), as.is=T) 
dat <- dat[,c("UniqueID", "Long","Lat","Year", "Dataset")]
colnames(dat) <- c("plot", "lon", "lat", "year", "source")
dat <- unique(dat)
dat$year[is.na(dat$year)] <- -9999
dat <- na.omit(dat) ## remove plots without long/lats. 
write.csv(dat, paste0(wd, "AllPlotLocations_30Sept2020.csv"), row.names=F)

dat.shp <- dat
coordinates(dat.shp) = ~lon + lat ## make a spatial points shapefile
writeOGR(dat.shp, dsn="E:\\NON_PROJECT\\NCEAS2\\LOCATION_DATA", layer="AllPlotLocations_30Sept2020", driver="ESRI Shapefile")

##### Plot geographic locations #####
data(stateMapEnv)
map('state')
colrs <- data.frame(source=c("NEON", "FIA", "BLM", "NPS"), colrs=c("hotpink","darkgreen","purple", "blue"), stringsAsFactors=F)
dat.plot <- merge(dat, colrs)
points(dat.plot$lon, dat.plot$lat, pch=16, cex=0.5, col=dat.plot$colrs)

legend(-126, 35, legend=colrs$source,
       col=colrs$colrs, pch=16, cex=0.7, bty="n")

# par(mfrow=c(2,2))
map('usa', main="NEON")
points(dat$lon[dat$source=="NEON"], dat$lat[dat$source=="NEON"], pch=16, cex=0.5, col="hotpink")

map('usa', main="FIA")
points(dat$lon[dat$source=="FIA"], dat$lat[dat$source=="FIA"], pch=16, cex=0.5, col="darkgreen")

map('usa', main="BLM")
points(dat$lon[dat$source=="BLM"], dat$lat[dat$source=="BLM"], pch=16, cex=0.5, col="purple")

map('usa', main="NPS")
points(dat$lon[dat$source=="NPS"], dat$lat[dat$source=="NPS"], pch=16, cex=0.5, col="blue")



