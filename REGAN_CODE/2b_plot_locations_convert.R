#################################################################################
##### Compile locations of all plots #####
##### Written by Regan Early
##### Written on 30th September 2020
##### Modified on 11th May 2021
#################################################################################

# library(maps)
library(mapdata)
library(sp)
library(rgdal)
# library(maptools)

##### Extract sample locations only #####
wd <- "E:\\NON_PROJECT\\NCEAS2\\LOCATION_DATA\\"

dat <- read.csv(paste0(wd, "Alldata_PlotDiv_18March2021.csv"), as.is=T) 

dat <- dat[,c("UniqueID", "Long","Lat", "Dataset")] ## there's no ,"Year" column in this dataset
colnames(dat) <- c("plot", "lon", "lat", "source") ##  "year",
dat <- unique(dat)
# dat$year[is.na(dat$year)] <- -9999
dat <- na.omit(dat) ## remove plots without long/lats. 

# blm.extra.25Oct <- read.csv("E:\\NON_PROJECT\\NCEAS2\\BLM\\missing_env_Oct25.csv", as.is=T)
# nps.extra.25Oct <- read.csv("E:\\NON_PROJECT\\NCEAS2\\NPS\\missing_env_Oct25.csv", as.is=T)
# dat <- rbind(dat, blm.extra.25Oct, nps.extra.25Oct)

write.csv(dat, paste0(wd, "AllPlotLocations_18March2021.csv"), row.names=F)

dat.shp <- dat
coordinates(dat.shp) = ~lon + lat ## make a spatial points shapefile
writeOGR(dat.shp, dsn="E:\\NON_PROJECT\\NCEAS2\\LOCATION_DATA", layer="AllPlotLocations_18March2021", driver="ESRI Shapefile")

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



