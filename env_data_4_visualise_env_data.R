#################################################################################
##### Visualise environmental data at plot locations #####
##### Written by Regan Early
##### Written on 20th May 2020
##### Modified on 29th September 2020
#################################################################################

library(ggplot2)
library(GGally)

out.wd <- "F:/NON_PROJECT/NCEAS2/ENV_DATA/ENV_LAYERS"
dat <- read.csv("E:\\NON_PROJECT\\NCEAS2\\LOCATION_DATA\\AllPlotLocations_15Sept2020.csv")

##### Visualise #####
## Group by source (use PRISM climate data)
jpeg(paste0(out.wd, "/env_by_source.jpg"), width=600, height=600)
ggpairs(dat, columns=c("npp","ar","hm","swd", "pt", "pwd", "vdist", "tmean","tmax","tmin","psum", "ecoR", "hard"),
        ggplot2::aes(colour=source),
        lower=list(continuous=wrap("points", alpha = 0.1,    size=0.1), 
                   combo=wrap("dot", alpha=0.4, size=0.1)))
dev.off()

## Group by ecoregion (use PRISM climate data)
jpeg(paste0(out.wd, "/env_by_ecoregion.jpg"), width=600, height=600)
ggpairs(dat, columns=c("npp","ar","hm","swd", "pt", "pwd", "vdist", "tmean","tmax","tmin","psum", "ecoR", "hard"),
        ggplot2::aes(colour=NA_L3NAME),
        lower=list(continuous=wrap("points", alpha = 0.1,    size=0.1), 
                   combo=wrap("dot", alpha=0.4, size=0.1)))
dev.off()


