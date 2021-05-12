#################################################################################
##### Compile environmental data everyone is using and save as raster stack #####
##### Written by Regan Early
##### Written on 20th May 2020
##### Modified on 21st August 2020
#################################################################################

library(raster)
library(tigris) ## states function
library(rgdal)
library(prism) # To download PRISM climate data

out.wd <- "E:/NON_PROJECT/NCEAS2/ENV_DATA/ENV_LAYERS"

proj.wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

s <- states(cb=T, resolution="500k", year=2018) ## highest resolution
l48 <- s[!(s$NAME %in% c("Alaska", "Hawaii", "Puerto Rico", "American Samoa", "Commonwealth of the Northern Mariana Islands", "Guam", "United States Virgin Islands")),] ## Reduce to conterminous states only
proj4string(l48)
l48 <- spTransform(l48, proj.wgs)
# writeOGR(l48, "E:\\GIS_DATA\\USA\\Geog_proj_data", "l48_500k", driver="ESRI Shapefile", overwrite=T)
l48 <- readOGR("E:\\GIS_DATA\\USA\\Geog_proj_data", "l48_500k")

##### NPP - http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/MODIS_250/modis-250-npp/ ##### 
npp.wd <- "E:\\GIS_DATA\\NPP\\USA\\"
npp <- raster(paste0(npp.wd,"modis-250-npp-2018.tif"))
proj4string(npp)
npp <- crop(npp, l48) ## Trim to extent of l48. A few secs.
npp <- mask(npp, l48) ## Trim to outline of l48. Lengthy.
names(npp) <- "npp"
writeRaster(npp, paste0(out.wd, "/npp_l48.tif"))

##### Aridity - https://doi.org/10.6084/m9.figshare.7504448.v3 #####
ar.wd <- "E:\\GIS_DATA\\WORLD\\ARIDITY\\"
ar <- raster(paste0(ar.wd,"ai_et0.tif"))
proj4string(ar)
ar <- crop(ar, l48) ## Trim to extent of l48. A few secs.
ar <- mask(ar, l48) ## Trim to outline of l48. About 20 secs with 5m. About 1 minute with 500k.
names(ar) <- "ar"
writeRaster(ar, paste0(out.wd,"/ar_l48.tif"))

##### HYDROLOGY - CGIAR #####
### SOIL-WATER DEFICIT
swd.wd <- "E:\\GIS_DATA\\HYDROLOGY\\CGIAR_SOIL_WATER_BALANCE_WORLD\\swc_fr\\"
swd <- stack(as.list(paste0(swd.wd,"swc_fr",paste0("_",c(1:12)))))
swd <- calc(swd, fun=mean)
proj4string(swd) ## in wgs format already
swd <- crop(swd, l48) ## Trim to extent of l48
swd <- mask(swd, l48) ## Trim to outline of l48
names(swd) <- "swd"
writeRaster(swd, paste0(out.wd,"/swd_l48.tif"))

### Priestley-Taylor alpha coefficient
pt.wd <- "E:\\GIS_DATA\\HYDROLOGY\\CGIAR_SOIL_WATER_BALANCE_WORLD\\ALPHA\\"
pt <- raster(paste0(pt.wd,"alpha"))
proj4string(pt) ## in wgs format already
pt <- crop(pt, l48) ## Trim to extent of l48
pt <- mask(pt, l48) ## Trim to outline of l48
names(pt) <- "pt"
writeRaster(pt, paste0(out.wd,"/pt_l48.tif"))

##### HYDROLOGY - GRIDMET #####
### Potential water deficit
pwd.wd <- "E:\\GIS_DATA\\HYDROLOGY\\GRIDMET_USA\\"
pwd <- raster(paste0(pwd.wd,"mean_PotDef_l48.wb.tif"))
proj4string(pwd) ## in wgs format already
pwd <- crop(pwd, l48) ## Trim to extent of l48
pwd <- mask(pwd, l48) ## Trim to outline of l48
names(pwd) <- "pwd"
writeRaster(pwd, paste0(out.wd,"/pwd_l48.tif"))


##### Human modification - https://doi.org/10.6084/m9.figshare.7283087.v1#####
hm.wd <- "E:\\GIS_DATA\\WORLD\\HUMAN_MODIFICATION\\"
hm <- raster(paste0(hm.wd,"gHM.tif"))
proj4string(hm)
hm <- projectRaster(hm, crs=proj.wgs)
hm <- crop(hm, l48) ## Trim to extent of l48
hm <- mask(hm, l48) ## Trim to outline of l48
names(hm) <- "hm"
writeRaster(hm, paste0(out.wd, "/hm_l48.tif"))

##### Climate - worldclim ~1km climate 1950-2000 (https://www.worldclim.org/data/index.html) #####
### Note these are probably worldclim1 data (superceded worldclim2). Including only for interest.
clim.wd <- "E:\\GIS_DATA\\CLIMATE\\WORLDCLIM\\bio_30s\\"
clim <- stack(as.list(paste0(clim.wd,"bio_",c(5,6,13,14))))
names(clim) <- c("maxTemp","minTemp","wetPrecip","dryPrecip")
proj4string(clim)
clim <- crop(clim, l48) ## Trim to extent of l48.
clim <- mask(clim, l48) ## Trim to outline of l48. 
writeRaster(clim, paste0(out.wd,"/clim_wc_l48.grd")) ## grd extension preserves layer names

##### Climate - PRISM (https://climatedataguide.ucar.edu/climate-data/prism-high-resolution-spatial-climate-data-united-states-maxmin-temp-dewpoint) #####
prism.wd <- "E:/GIS_DATA/CLIMATE/USA/PRISM/NORMALS"
options(prism.path = prism.wd)

### Download data for 30 year normals (i.e. monthly means over 30 years) at 4km or 800m grid cell resolution (see/change filenames)
get_prism_normals(type="tmean",resolution = "800m", mon = 1:12, keepZip=F)
get_prism_normals(type="tmax",resolution = "800m", mon = 1:12, keepZip=F)
get_prism_normals(type="tmin",resolution = "800m", mon = 1:12, keepZip=F) # Downloading maxes...?
get_prism_normals(type="ppt",resolution = "800m", mon = 1:12, keepZip=F)

### Identify the 12 monthly files for each variable
tmean_norms <- as.list(list.files(path=prism.wd, pattern=glob2rx("*tmean*800m*.bil"), full.names=T, recursive=T))
tmax_norms <- as.list(list.files(path=prism.wd, pattern=glob2rx("*tmax*800m*.bil"), full.names=T, recursive=T))
tmin_norms <- as.list(list.files(path=prism.wd, pattern=glob2rx("*tmin*800m*.bil"), full.names=T, recursive=T))
ppt_norms <- as.list(list.files(path=prism.wd, pattern=glob2rx("*ppt*800m*.bil"), full.names=T, recursive=T))

### Import monthly files as rasters and calculate annual means
tmean <- stack(tmean_norms)
tmean <- tmean / 100
tmean <- calc(tmean, fun=mean)

tmax <- stack(tmax_norms)
tmax <- tmax / 100
tmax <- calc(tmax, fun=mean)

tmin <- stack(tmin_norms)
tmin <- tmin / 100
tmin <- calc(tmin, fun=mean)

psum <- stack(ppt_norms)
psum <- psum / 100
psum <- calc(psum, fun=sum)

### Stack and project
clim.prism <- stack(tmean, tmax, tmin, psum)
names(clim.prism) <- c("tmean", "tmax", "tmin", "psum")

proj4string(clim.prism)
clim.prism <- projectRaster(clim.prism, crs=crs(l48))

clim.prism <- crop(clim.prism, l48) ## Trim to extent of l48.
clim.prism <- mask(clim.prism, l48) ## Trim to outline of l48. 
writeRaster(clim.prism, paste0(out.wd,"/prism800m_l48.grd")) ## .grd format preserves layer names, .tif allows the extent of the data to be visualised in ArcMAP

##### Disturbance #####
### This is the 10-year composite disturbance layer up till 2014
### Commented out lines were run on a cluster - file too big for desktop computer
### However, reprojection mangled the data. Instead of this step, reprojected plot locations and extracted from original vdist file - see code 3_extract_data_to_points
# vdist.wd <- "E:\\GIS_DATA\\USA\\DISTURBANCE\\US_VegDIST2014_10312018\\Grid"
# vdist <- raster(paste0(vdist.wd,"\\us_vdist2014"), RAT=F)
# proj4string(vdist)
# rasterOptions(chunksize = 1e+04, maxmemory = 1e+06) ## reprojection requires a cluster
# # chunksize: Maximum number of cells to read/write in a single chunk while processing (chunk by chunk) disk based Raster* objects.
# # maxmemory: Maximum number of cells to read into memory. I.e., if a Raster* object has more than this number of cells, canProcessInMemory will return FALSE.
# vdist_wgs <- projectRaster(vdist, crs=proj.wgs) ## Project fragment to new projection
# 
# vdist <- crop(vdist, l48) ## Trim to extent of l48.
# vdist <- mask(vdist, l48) ## Trim to outline of l48. 
# writeRaster(vdist, paste0(out.wd,"/vdist_l48.tif"))

##### Hardiness #####
### These are the average annual minimum winter temperature values used to calculate hardiness
hard.wd <- "E:\\GIS_DATA\\CLIMATE\\USA\\HARDINESS"
hard <- raster(paste0(hard.wd,"phm_us.asc"))
proj4string(hard)
hard <- projectRaster(hard, crs=proj.wgs)
hard <- crop(hard, l48) ## Trim to extent of l48.
hard <- mask(hard, l48) ## Trim to outline of l48. 
writeRaster(hard, paste0(out.wd,"/hard_l48.tif")) 

### These are the derived hardiness zones.
hard <- readOGR(hard.wd, "phm_us_shp")
hard$ID <- hard$GRIDCODE <- NULL
hard <- spTransform(hard, proj.wgs)
hard <- crop(hard, extent(l48))

writeOGR(hard, out.wd, "hard_l48", driver="ESRI Shapefile")

##### Ecoregions - https://www.epa.gov/eco-research/ecoregions-north-america #####
ecoR.wd <- "E:/GIS_DATA/USA/ECOREGIONS/us_eco_l4"
ecoR <- readOGR(ecoR.wd, "us_eco_l4_no_st")
ecoR <- spTransform(ecoR, proj.wgs)
ecoR$Shape_Leng <- ecoR$Shape_Area <- NULL
ecoR <- crop(ecoR, extent(l48))

writeOGR(ecoR, out.wd, "ecoR_l48", driver="ESRI Shapefile")

##### NLCD ...




