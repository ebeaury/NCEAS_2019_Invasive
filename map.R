# world map

# map of the countries of the entire world and choosing scale, respectively
library(sf)
library(cowplot) # to arrange inset maps
# library(rcartocolor)
library(tidyverse)
library(spData)
library(sp)
library(rgdal)
library(ggsn) #add north arrow
library(viridis)
library(scales)
library(inlmisc) #another color blind pallete to get qualitative colors

### Data preparation
#### Base maps
# data("us_states", package = "spData") ## does not have hawaii, pr and alaska
library(tigris)
options(tigris_use_cache = TRUE)
us_states = states(cb = FALSE, class = "sf") # this one comes with all the "extra" territories

us_states2 = subset(us_states, 
                   !NAME %in% c(
                     "United States Virgin Islands",
                     "Commonwealth of the Northern Mariana Islands",
                     "Guam",
                     "American Samoa",
                     "Puerto Rico",
                     "Alaska",
                     "Hawaii"
                   ))

us_states_2163 = st_transform(us_states2, crs = 2163)

hawaii = subset(us_states, 
                   NAME %in% c(
                     "Hawaii"
                   ))

#I'll need to crop Hawaii, to exclude the little islands
ggplot() + geom_sf(data = hawaii) + theme_bw()
hawaii_2 <- st_crop(hawaii, xmin = -161, xmax = -153,
                            ymin = 18, ymax = 23)
ggplot() + geom_sf(data = hawaii_2) + theme_bw()

hawaii_2163 = st_transform(hawaii_2, crs = 2163)

alaska = subset(us_states, 
                NAME %in% c(
                  "Alaska"
                ))

alaska_2163 = st_transform(alaska, crs = 2163)

puerto_rico = subset(us_states, 
                NAME %in% c(
                  "Puerto Rico"
                ))

puerto_rico_2163 = st_transform(puerto_rico, crs = 2163)

#### Point data
# SPCIS_plant_taxa <- readRDS("/home/shares/neon-inv/data_paper/final_data/SPCIS_plant_taxa_04212022.rds")
# SPCIS_plots <- readRDS("/home/shares/neon-inv/data_paper/final_data/SPCIS_plots_04212022.rds")
SPCIS_plant_taxa <- read_csv("/home/shares/neon-inv/data_paper/final_data/SPCIS_plant_taxa_09212022.csv")
SPCIS_plots <- read_csv("/home/shares/neon-inv/data_paper/final_data/SPCIS_plots_09212022.csv")
SPCIS <- SPCIS_plant_taxa %>% left_join(SPCIS_plots)
glimpse(SPCIS)
SPCIS <- SPCIS %>% 
  ungroup() %>% 
  filter(FuzzedCoord != "Missing") %>% 
  select(Dataset, Plot, Long, Lat, Zone) %>% 
  distinct() %>% 
  st_as_sf(coords = c("Long", "Lat"), crs=4326)

SPCIS_2163 = st_transform(SPCIS, crs = 2163)

#creating a box around the Conterminous US
us_states_2163_bb = st_as_sfc(st_bbox(us_states_2163))
us_states_2163_bb = st_buffer(us_states_2163_bb, dist = 120000)

#creating a box around the Alaska
alaska_2163_bb = st_as_sfc(st_bbox(alaska_2163))
alaska_2163_bb = st_buffer(alaska_2163_bb, dist = 100000)

#creating a box around the HI
hawaii_2163_bb = st_as_sfc(st_bbox(hawaii_2163))
hawaii_2163_bb = st_buffer(hawaii_2163_bb, dist = 10000)

#creating a box around the PR
puerto_rico_2163_bb = st_as_sfc(st_bbox(puerto_rico_2163))
puerto_rico_2163_bb = st_buffer(puerto_rico_2163_bb, dist = 10000)

# showing pallete of color
show_col(viridis_pal(option = "plasma")(9)) 
inlmisc::GetColors(9, scheme = "muted") # try this one instead

### Creating maps
#### Conterminous US
ggm1 <- ggplot() + 
  geom_sf(data = us_states_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(SPCIS_2163, Zone == "L48"), aes(color = Dataset), #fill = NA, color = "blue", size = 1.2
          alpha = .2) +
  scale_colour_manual(values=c("#CC6677", "#332288", "#DDCC77", "#117733",
                               "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499")) +
  theme_void() +
  # theme(legend.position = "none") +
  theme(legend.position = c(0.93, 0.25),
        legend.direction = "vertical",
        #aligns legend title to the middle
        legend.title.align=0.5) +
  # theme(legend.position = c(0.5, 0.07),
  #       legend.direction = "horizontal") +
  coord_sf(xlim = st_bbox(us_states_2163_bb)[c(1, 3)],
           ylim = st_bbox(us_states_2163_bb)[c(2, 4)]) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 

ggm1

#### Hawaii
ggm2 <- ggplot() + 
  geom_sf(data = hawaii_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(SPCIS_2163, Zone == "HI"), aes(color = Dataset), #fill = NA, color = "blue", size = 1.2
          alpha = .2) +
  scale_colour_manual(values=c("#88CCEE", "#882255")) +
  geom_sf(data = hawaii_2163_bb, fill = NA, color = "black", size = 0.1) +
  theme_void() +
# the colors of the datasets are not matching to the master graph
  theme(legend.position="none") #,
        #legend.direction = "horizontal")

ggm2

#### Alaska
ggm3 <- ggplot() + 
  geom_sf(data = alaska_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(SPCIS_2163, Zone == "AK"), aes(color = Dataset), #fill = NA, color = "blue", size = 1.2
          alpha = .2) +
  scale_colour_manual(values=c("#DDCC77", "#88CCEE")) +
  geom_sf(data = alaska_2163_bb, fill = NA, color = "black", size = 0.1) +
  theme_void()  +
# the colors of the datasets are not matching to the master graph
  theme(legend.position = "none") #,
 #       legend.direction = "horizontal")

ggm3

#### Puerto Rico
ggm4 <- ggplot() + 
  geom_sf(data = puerto_rico_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(SPCIS_2163, Zone == "PR"), aes(color = Dataset), #fill = NA, color = "blue", size = 1.2
          alpha = .2) +
  geom_sf(data = puerto_rico_2163_bb, fill = NA, color = "black", size = 0.1) +
  scale_colour_manual(values=c("#DDCC77", "#88CCEE")) +
  theme_void() +
  # the colors of the datasets are not matching to the master graph
  theme(legend.position = "none")

ggm4

#### Putting all the maps together

gg_inset_map = ggdraw() +
  #Conterminous US
  draw_plot(ggm1) +
  #Alaska
  draw_plot(ggm3, x = 0.09, y = 0, width = 0.2, height = 0.27) +
  #HI
  draw_plot(ggm2, x = 0.19, y = 0, width = 0.25, height = 0.27) +
  #PR
  draw_plot(ggm4, x = 0.55, y = 0, width = 0.15, height = 0.2) 

gg_inset_map 

#adding the north arrow
# north2(gg_inset_map, .93, .55, symbol = 10)


# The line below is not working, so I am manually saving the plot
# ggsave("/home/shares/neon-inv/data_paper/figures/MapAllPlots09212022.png", dpi = 300)


## Ecoregions map
library(sf)
# import the vector into R as an sf object
ecoreg1 <- st_read("/home/shares/neon-inv/data_paper/env_code_data/gis_layers/ecoregionlevel1/NA_CEC_Eco_Level1.shp") %>% rename(Ecoregion = NA_L1NAME)
glimpse(ecoreg1)

ecoreg1_2163 = st_transform(ecoreg1, crs = 2163)

#creating a box around 
ecoreg1_2163_bb = st_as_sfc(st_bbox(ecoreg1_2163))
ecoreg1_2163_bb = st_buffer(ecoreg1_2163_bb, dist = 120)

### Creating maps
# finding color
inlmisc::GetColors(3, scheme = "light") # graphs

eco1<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "EASTERN TEMPERATE FORESTS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  geom_sf(data = ecoreg1_2163_bb, fill = NA, color=NA, size = 0.1) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoEASTERN-TEMPERATE-FORESTS05272022.png", dpi = 300)

eco2<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "GREAT PLAINS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoGREAT-PLAINS05272022.png", dpi = 300)

eco3<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "MARINE WEST COAST FOREST"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoMARINE-WEST-COAST-FOREST05272022.png", dpi = 300)

eco4<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "MEDITERRANEAN CALIFORNIA"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoMEDITERRANEAN-CALIFORNIA05272022.png", dpi = 300)

eco5<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "NORTH AMERICAN DESERTS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoNORTH-AMERICAN-DESERTS05272022.png", dpi = 300)

eco6<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "NORTHERN FORESTS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoNORTHERN-FORESTS05272022.png", dpi = 300)

eco7<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "NORTHWESTERN FORESTED MOUNTAINS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoNORTHWESTERN-FORESTED-MOUNTAINS05272022.png", dpi = 300)

eco8<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "SOUTHERN SEMIARID HIGHLANDS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoSOUTHERN-SEMIARID-HIGHLANDS05272022.png", dpi = 300)

eco9<-ggplot() + 
  geom_sf(data = ecoreg1_2163, fill = "white", size = 0.2) + 
  geom_sf(data = filter(ecoreg1_2163, Ecoregion == "TEMPERATE SIERRAS"), 
          aes(fill=Ecoregion), #fill = NA, color = "blue", size = 1.2
          color="grey20",fill="grey20", alpha = .2) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes= list(alpha = 1, size=3))) 
# ggsave("/home/shares/neon-inv/data_paper/figures/mapEcoregions/EcoTEMPERATE-SIERRAS05272022.png", dpi = 300)