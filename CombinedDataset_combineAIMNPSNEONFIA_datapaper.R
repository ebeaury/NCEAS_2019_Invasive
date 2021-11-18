###  PAINLES: join all datasets 

##  Helen Sofaer
##  11 May 2020
##  4 Aug 2020: all datasets including traits
##  11 Sept 2020: dropped repeated AIM plots; updated to consistent NI codes, separate Site Plot
##  16 Oct 2020: update codes for few sp Jeff C suggested; add CWM for all other sp
##  21 Oct 2020: update NEON to include original code for unk sp; moved CWM calcs into another file
##  8 March 2021: updated NEON data - some changes in sp status
##  11 March 2021: consistent exotic status - including NAs

##  Laís Petri (LP)
##  04/21/2021: changes in code to accommodate the files from NCEAS server and the dataset Kristen Pearch worked on 
##  08/21/2021: including new datasets and merging into a full database
##  10/28/2021: included IL_CTAP and made modifications on the accuracy of coordinates of a few datasets
##  11/01/2021: included "OriginalPlot" column. This allow that the combination of Plot*Year to find resampled plots.
##  11/18/2021: updated the "SampledArea" column for AIM according to info Helen gave me

library(tidyverse)
theme_set(theme_classic())

setwd('/home/shares/neon-inv/data_paper')


###   Columns to carry forward in combined dataset
DesiredColumns <- c("Dataset", "Site", #AIM data does not have Site
                    "Plot", "OriginalPlot", "Long", "Lat", "Year", 
                    "SpCode", "AcceptedSpeciesName", "OriginalSpeciesName", "NativeStatus", "PctCov",
                    "FuzzedCoord", "SampledArea" 
                    #"GrowthForm", "Duration", 
                    #LP: removing traits for the data paper
                    #"TraitNRow s", "SLA", "LDMC", "Leaf.area",
                    #"Leaf.N.mass", "Leaf.P.mass", "Plant.height", "Seed.dry.mass", 
                    #"StemSpecificDensity", "C3.C4", "Woodiness"
                    )

###
####   NEON   ####
###

NEON <- read.csv("data_by_dataset/NEONdata_flatted20210810.csv")
glimpse(NEON) 
# Accepted.Symbol has NA where code couldn't be matched to an accepted one; neon_sp_code has the original (can be synonym)

# total of unique plots = 1413
# length(table(NEON$plotID))

NEON <- NEON %>%
  mutate(FuzzedCoord = "N",
         # those are sites in which diversity and vegetation structure data are from different years
         year = ifelse(siteID == "ABBY" | siteID == "SJER" | siteID == "WREF", 2017, year),
         OriginalPlot = as.character(plotID)) %>%
  rename(Dataset = dataset,
         Site = siteID,
         Plot = plotID,
         Year = year,
         AcceptedSpeciesName = bestname,
         SpCode = Accepted.Symbol,
         Long = decimalLongitude,
         Lat = decimalLatitude,
         NativeStatus = Native.Status,
         PctCov = plotCover,
         OriginalSpeciesName = scientificName,
         SampledArea = SampledArea) %>%
  select(one_of(DesiredColumns), 
         #next line of code carries whether cover of a particular species in a plot was based on basal areas and/or canopy cover (sum)
         # if 0 in both, it came from the 1m2 plots
         # NEONvst_status = if a plot should have veg structure data, but it was not collected yet
         contains("NEON", ignore.case = FALSE))

setdiff(DesiredColumns, names(NEON))
setdiff(names(NEON), DesiredColumns)

###
#####   FIA    ####
###

FIA <- read_csv("data_by_dataset/FIA_DataPaper_10282021.csv") #ALL data including repeated samples (3207 plots)
glimpse(FIA)

#file with unique plot IDs
NewPlotIDFIA <- read_csv("code_by_dataset/extra_csv_FIA/NewPlotIDFIA.csv")

FIA <- FIA %>% 
  left_join(NewPlotIDFIA, by = c("PLOT", "STATECD", "LAT", "LON"))

# all.equal(FIA$SciName, FIA$bestname)
# FIA %>%
#   select(SciName, bestname) %>%
#   distinct() %>%
#   filter(SciName != bestname) %>%
#   head() # looks like SciName has ssp and var info, so I'll go with bestname

# total of unique plots = 2715
# length(table(FIA$PLT_CN))

# checking whether my new plot code worked out. I need to have 492 plots that were resampled
# x<-FIA %>% select(OriginalPlot, INVYR) %>% group_by(OriginalPlot) %>% distinct() %>% 
#   add_tally() %>% select(-INVYR) %>% filter(n ==2) %>% distinct()
# dim(x)
# yes!


FIA <- FIA %>%
  mutate(FuzzedCoord = "Y",
         OriginalPlot = as.character(PLT_CN)) %>%
  # I think I need to create a new code for Plot so we can identify by Plot which ones are resampled. 
  # unsure if there is a column we could actually use as Site, maybe State 
  rename(Site = PLOT,
         ## Jeff C will provide new data soon, so check which changes will be necessary (join Area sampled?)
         Plot = NewPlot,
         SpCode = VEG_SPCD, 
         PctCov = PlotCover,
         NativeStatus = inv_L48,
         Long = LON,
         Lat = LAT,
         Year = INVYR,
         SampledArea = SampledArea,
         AcceptedSpeciesName = bestname,
         OriginalSpeciesName = SciName) %>%
  mutate(Dataset = "FIA",
         Site = as.character(Site),
         Plot = as.character(Plot)) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(FIA))
setdiff(names(FIA), DesiredColumns)

###
####   AIM data    ####
###

AIM <- read_csv("data_by_dataset/AIM_AllSpTax_LP_20Aug2021.csv") 
glimpse(AIM)

# making sure plotID is unique to plots with the same coordinate across years
renamedPlotIDAIM <- AIM %>% select(PLOTKEY,NAD83.X,NAD83.Y, VisitYear) %>% group_by(NAD83.X,NAD83.Y) %>% distinct() %>%
  add_tally() %>% select(-VisitYear) %>% filter(n >1) %>% distinct() %>% mutate(NewPlotID = paste0("AIM_", cur_group_id()))

AIM <- AIM  %>%
  left_join(renamedPlotIDAIM, by = c("PLOTKEY", "NAD83.X", "NAD83.Y")) %>% 
  # drop earlier visits for resampled plots:
  # filter(!PLOTKEY %in% AIMdrop$PLOTKEY) %>%
  # convert from proportion to percent cover:
  mutate(PctCov = 100*prop.cover,
         FuzzedCoord = "N",
         Site = NA,
         SampledArea = ifelse(DataSet == "BLM_LMF", 2000, 10000),
         # Dataset = ifelse(DataSet == "BLM_LMF", "AIM_LMF", "AIM_TerrADat"),
         Dataset = "AIM",
         OriginalPlot = as.character(PLOTKEY),
         Plot = ifelse(is.na(NewPlotID), PLOTKEY, NewPlotID)) %>%
  rename(Long = NAD83.X,
         Lat = NAD83.Y,
         Year = VisitYear, 
         SpCode = code,
         NativeStatus = inv_L48,
         AcceptedSpeciesName = bestname, 
         # SampledArea = nMark,
         OriginalSpeciesName = OriginalName) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(AIM)) # Site
setdiff(names(AIM), DesiredColumns)

#code to summarize the number of distinct plots
AIM %>% select(Plot) %>% distinct() %>% nrow()

###
####   NPS    ####
###

NPS <- read.csv("data_by_dataset/NPS_DataPaper_27July2021.csv", 
                na = c('', 'NA'))
glimpse(NPS)

# total of unique plots = 24150
# length(table(NPS$UniqueID))
# multiple years of data for a single plot?
# NPS2 <- NPS %>% group_by(UniqueID) %>% filter(Year == max(Year))

NPS <- NPS %>%
  mutate(OriginalPlot = as.character(Plot),
         Year = ifelse(UniqueID =="ASIS_2", 1995, Year)) %>% 
  select(-Plot) %>%
  rename(NativeStatus = Exotic,
         SpCode = Species,
         PctCov = Pct_Cov,
         SampledArea =  Area_sampled,
         Plot = UniqueID,
         FuzzedCoord = RealCoords,
         AcceptedSpeciesName = bestname,
         OriginalSpeciesName = OriginalName) %>%
  select(one_of(DesiredColumns)) %>%
  mutate(SampledArea = as.double(SampledArea))

setdiff(DesiredColumns, names(NPS))
setdiff(names(NPS), DesiredColumns)

###
####   VEGBANK    ####
###

VEGBANK <- read.csv("data_by_dataset/All_VegBank_KPEACH_EB_LP_reduced.csv", 
                    na = c('', 'NA')) # this data includes repeated samples
glimpse(VEGBANK)

#updated information on plot area for WVNHP/NCVS
WVplotArea <- read_excel("code_by_dataset/extra_csv_WVNHP/WVplotArea.xlsx") %>% 
  rename(UniqueID = `Plot Code`, 
         PlotArea = `Plot Area`) %>% 
  distinct()

#plots to be removed, duplicate info with VNHP
PlotsRemoveVegBank <- c("094-01-0032", "094-01-0053", "094-01-0056")

VEGBANK <- VEGBANK %>% 
  left_join(select(WVplotArea, UniqueID, PlotArea), by="UniqueID") %>% 
  #extracting the first letters from the plotID to use as site info for WVNHP
  mutate(Site = ifelse(Dataset == "NCVS_WV", gsub("\\..*", "", UniqueID), NA),
         SampledArea = ifelse(Dataset == "NCVS_WV", PlotArea, Taxon.Observation.Area),
         SampledArea = ifelse(Dataset == "NCVS_WV" & SampledArea == 0, NA, SampledArea),
         OriginalPlot = ifelse(grepl("VEGBANK", Dataset), VegBankUniqueID, as.character(UniqueID))) %>%
  #line below removes the 3 plots that have the same information in VNHP dataset
  filter(!VegBankUniqueID %in% PlotsRemoveVegBank) %>% 
  rename(Plot = UniqueID,
         Lat_Original = Lat,
         Long_Original = Long,
         AcceptedSpeciesName = bestname,
         OriginalSpeciesName = OriginalName,
         NativeStatus = ExoticStatus) %>%
  mutate(Lat = ifelse(Dataset=="NCVS_WV", round(Lat_Original, 1), Public.Latitude),
         Long = ifelse(Dataset=="NCVS_WV", round(Long_Original, 1), Public.Longitude),
         Dataset = ifelse(grepl("VEGBANK", Dataset), "CVS", "WVNHP"),
         FuzzedCoord = "Y") %>%
  # removing entire plots with threatened and endangered species or targeted for poaching. Request from Jim
  group_by(OriginalPlot) %>%
  filter(is.na(OriginalSpeciesName)| # code to keep the plot with NA in the original species name column
           !any(OriginalSpeciesName == "Arabis serotina Steele" & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Ptilimnium nodosumRose Mathias" & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Scirpus ancistrochaetus Schuyler" & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Trifolium stoloniferum Muhl. ex Eat." & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Isotria medeoloidesPursh Raf." & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Spiraea virginiana Britt." & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Cypripedium parviflorum Salisb. var. pubescensWilld. Knight" & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Cypripedium parviflorum Salisb." & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Cypripedium reginae Walt." & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Panax quinquefolius" & Dataset == "WVNHP"),
         is.na(OriginalSpeciesName)|
           !any(OriginalSpeciesName == "Hydrastis canadensis" & Dataset == "WVNHP")) %>%
  ungroup() %>% 
  dplyr::select(one_of(DesiredColumns))

#code to summarize the number of distinct plots
# total # of plots is 14735
# total # of plots removing the ones with targeted species is 14519
# 216 plots removed
VEGBANK %>% select(Plot) %>% distinct() %>% nrow()

setdiff(DesiredColumns, names(VEGBANK))
setdiff(names(VEGBANK), DesiredColumns)

###
####   VANHP    ####
###

VANHP <- read.csv("data_by_dataset/VANHP_datapaper_06_16_21_EB.csv", 
                    na = c('', 'NA')) 
glimpse(VANHP)

VANHP <- VANHP %>%
  mutate(FuzzedCoord = "Y", 
         Site = "NA", 
         Long = round(Long, 1),
         Lat = round(Lat, 1),
         OriginalPlot = as.character(Plot),
         Dataset = "VNHP") %>%
  rename(OriginalSpeciesName = VASpeciesName,
         AcceptedSpeciesName = bestname,
         SampledArea = Plot.Size, 
         NativeStatus = ExoticStatus) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(VANHP))
setdiff(names(VANHP), DesiredColumns)

###
####   NWCA    ####
###

NWCA <- read.csv("data_by_dataset/NWCA2011-2016_08202021.csv", 
                  na = c('', 'NA')) 
glimpse(NWCA)

NWCA <- NWCA %>%
  mutate(FuzzedCoord = "N",
         Dataset = "NWCA",
         OriginalPlot = as.character(PLOT)) %>%
  rename(Site = SITE_ID,
         Plot = UniquePlotID,
         OriginalSpeciesName = OriginalName,
         AcceptedSpeciesName = bestname,
         SampledArea = PlotArea_m2,
         NativeStatus = inv_L48,
         SpCode = Accepted.Symbol,
         PctCov = COVER) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(NWCA))
setdiff(names(NWCA), DesiredColumns)

###
####   IL_CTAP    ####
###

IL_CTAP <- read.csv("data_by_dataset/IL_CTAP_DataPaper10262021.csv", 
                 na = c('', 'NA')) 
glimpse(IL_CTAP)

IL_CTAP <- IL_CTAP %>%
  mutate(FuzzedCoord = "Y",
         Lat = round(Lat, 2),
         Long = round(Long, 2),
         OriginalPlot = as.character(Transect)) %>%
  rename(Site = SiteID,
         AcceptedSpeciesName = bestname,
         NativeStatus = inv_L48,
         SpCode = Accepted.Symbol,
         Year = PYear) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(IL_CTAP))
setdiff(names(IL_CTAP), DesiredColumns)

###
#####    Join    ####
###


DataBase <- bind_rows(AIM, NPS, FIA, NEON, VEGBANK, VANHP, NWCA, IL_CTAP)
glimpse(DataBase)

DataBase %>% select(Plot) %>% distinct() %>% nrow()

###
####   Update exotic status to be consistent    ####
###
# Some legit variation in exotic status for plots in AK and HI, but fix w/in L48

status <- read_csv("code_by_dataset/taxonomy/multStatusSpL48.csv")
head(status)

status <- status %>%
  select(SpCode, L48status = `FINAL DECISION (L48)`) %>%
  filter(!is.na(SpCode))
anyDuplicated(status$SpCode)

DataBase <- DataBase %>%
  mutate(Zone = case_when(Lat > 50 ~ "AK",
                          Lat < 50 & Lat > 19 & Long > -130 ~ "L48",
                          Long < -150 & Lat < 25 ~ "HI",
                          Lat < 20 & Long < -65 & Long > -70 ~ "PR",
                          TRUE ~ "MissingCoords")) %>%
  left_join(status) %>%
  mutate(NativeStatus = ifelse(Zone == "L48" & !is.na(L48status), L48status, NativeStatus)) %>%
  select(-L48status)

DataBase %>%
  select(Dataset, Site, Plot, Zone) %>%
  distinct() %>%
  count(Zone) #number of plots per zone 

DataBase %>%
  filter(Zone == "MissingCoords",
         !is.na(Long),
         !is.na(Lat)) %>%
  nrow() # yes, all missing coords 
table(DataBase$Zone) #missing coordinates = 308199 records

DataBase %>%
  filter(Zone == "PR") %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset)

###
####checking missing data for Year column####
DataBase %>% 
  filter(is.na(Year)) %>% 
  nrow() # missing Year = 65368 records 

table(DataBase$Year) # checks the range of years

DataBase%>%
  filter(Year == 1195) %>%
  select(Dataset, Plot) %>%
  distinct() # ASIS_2 --> FIXED!

DataBase%>%
  filter(Year == 2063) %>%
  select(Dataset, Plot) %>%
  distinct() # ROBI.XX = 10 plots

DataBase%>%
  filter(Year == 2064) %>%
  select(Dataset, Plot) %>%
  distinct() # ROBI.XX = 24 plots

#removes the data entries with Years to come
DataBase <- DataBase %>%
  filter(Year < 2063)
###

###
####   Fix taxonomy errors and inconsistencies  ####
###

DataBase <- DataBase %>%
  mutate(AcceptedSpeciesName = recode(AcceptedSpeciesName, 
                           "Pyrrocoma uniflora var. uniflora" = "Pyrrocoma uniflora",
                           "Antennaria rosea ssp. confinis" = "Antennaria rosea",
                           "Notholithocarpus densiflorus" = "Lithocarpus densiflorus"))

DataBase <- DataBase %>%
  mutate(NativeStatus = ifelse(SpCode == "RHYNC3" & Zone == "L48",
                               "NI",
                               NativeStatus),
         NativeStatus = ifelse(SpCode == "FIMBR" & Zone == "L48",
                               "NI",
                               NativeStatus),
         # based on USDA plants:
         NativeStatus = ifelse(SpCode == "ACMI2" & Zone == "AK",
                               "N",
                               NativeStatus))

# Species with multiple exotic status:
DataBase %>%
  group_by(Zone, SpCode, AcceptedSpeciesName) %>%
  summarize(nStatus = n_distinct(NativeStatus)) %>%
  filter(nStatus > 1)

###  FIX species with multiple exotic status  ####
# By zone
DataBase.fill <- DataBase %>%
  group_by(SpCode, AcceptedSpeciesName, Zone) %>%
  fill(NativeStatus, .direction = "downup") 

# Creates a summarized table in which identifies whether a species code has 
# more than a particular "bestname", "NativeStatus" attributed to it.
# I see inconsistencies when adding the VEGBANK data. 
countFilled <- DataBase.fill %>%
  group_by(SpCode) %>%
  # non-numeric variables:
  summarise(across(one_of("AcceptedSpeciesName", "NativeStatus"), 
                   ~ n_distinct(.x[!is.na(.x)]))) %>%
  filter(AcceptedSpeciesName>1) # filter species codes with more than one matching bestname
head(countFilled)

IanTaxonomy <- read_csv("code_by_dataset/taxonomy/taxonomy_temp10_revised.csv")

IanTaxonomy <- IanTaxonomy %>%
  select(Accepted.Symbol, bestname) %>%
  filter(Accepted.Symbol %in% countFilled$SpCode) %>%
  distinct() %>%
  group_by(Accepted.Symbol) %>%
  add_tally() %>%
  rename(SpCode = Accepted.Symbol)

#it seems that even Ian's taxonomy has multiple species bestname for a particular code. 
#So here, I filtered the species codes that had a single match for bestname (and I couldn't figure out why
#they were different across datasets) and redefined their bestname. The remaining species codes, that
#have more than one bestname, I am going to manually modify based on the USDA accepted name on 08/20/2021 and 10/28/2021
DataBase.fill <- DataBase.fill %>%
  left_join(IanTaxonomy %>% filter(n == 1)) %>%
  mutate(AcceptedSpeciesName = ifelse(!is.na(bestname), bestname, AcceptedSpeciesName)) %>%
  select(-bestname, -n)
glimpse(DataBase.fill)

#checking again
#now it's better, only 49 species codes have multiple bestname
countFilled <- DataBase.fill %>%
  group_by(SpCode) %>%
  # non-numeric variables:
  summarise(across(one_of("AcceptedSpeciesName", "NativeStatus"), 
                   ~ n_distinct(.x[!is.na(.x)]))) %>%
  filter(AcceptedSpeciesName>1) # filter species codes with more than one matching bestname
head(countFilled)
 
## species codes with different bestname associated to them
## Not really using this dataframe for anything in the code, just to help me check the inconsistencies
UnmatchData <- DataBase.fill %>% 
  ungroup() %>%
  filter(SpCode %in% countFilled$SpCode) %>%
  select(Dataset, SpCode, AcceptedSpeciesName, NativeStatus) %>%
  distinct()

#manually modify based on the USDA accepted name on 08/20/2021 and 10/28/2021
DataBase.fill <- DataBase.fill %>%
  mutate(SpCode = ifelse(SpCode == "7-Feb", "FEBR7", SpCode),
    AcceptedSpeciesName = ifelse(SpCode == "ACFL", "Acer floridanum",
                    ifelse(SpCode == "ANGLP", "Andropogon glomeratus var. pumilus",
                    ifelse(SpCode == "ANGYS", "Andropogon gyrans var. stenophyllus",
                    ifelse(SpCode == "ANMI3", "Antennaria microphylla",
                    ifelse(SpCode == "ARLAB", "Arabis laevigata var. burkii",
                    ifelse(SpCode == "ARLUI2", "Artemisia ludoviciana ssp. incompta",
                    ifelse(SpCode == "ASTUR", "Asclepias tuberosa ssp. rolfsii",
                    ifelse(SpCode == "BOON", "Botrychium oneidense",
                    ifelse(SpCode == "BRAR5", "Bromus arvensis",
                    ifelse(SpCode == "CACAD2", "Carex canescens ssp. disjuncta",
                    ifelse(SpCode == "CEPU10", "Celtis pumila",
                    ifelse(SpCode == "CLHA", "Cleome hassleriana",
                    ifelse(SpCode == "CRMIE", "Croton michauxii var. ellipticus",
                    ifelse(SpCode == "DRGO3", "Dryopteris goldieana",
                           ifelse(SpCode == "ELMI2", "Eleocharis microcarpa",
                           ifelse(SpCode == "ELPAP", "Eleocharis palustris var. palustris",
                           ifelse(SpCode == "ERHIH2", "Erechtites hieraciifolius var. hieraciifolius",
                           ifelse(SpCode == "EUATA2", "Euonymus atropurpureus var. atropurpureus",
                           ifelse(SpCode == "EUMAM4", "Eutrochium maculatum var. maculatum",
                           ifelse(SpCode == "EURE18", "Eubotrys recurvus",
                           ifelse(SpCode == "FEBR7", "Festuca brevipila",
                           ifelse(SpCode == "HASU3", "Hasteola suaveolens",
                           ifelse(SpCode == "HEAMH2", "Heuchera americana var. hispida",
                           ifelse(SpCode == "KYGR", "Kyllinga gracillima",
                           ifelse(SpCode == "LEFR5", "Lespedeza frutescens",
                           ifelse(SpCode == "LIPUM2", "Liatris punctata var. mucronata",
                           ifelse(SpCode == "MEOF", "Melilotus officinalis",
                           ifelse(SpCode == "MOFIB", "Monarda fistulosa var. brevis",
                                  ifelse(SpCode == "NEAR5", "Nekemias arborea",
                                  ifelse(SpCode == "OEFRT", "Oenothera fruticosa ssp. tetragona",
                                  ifelse(SpCode == "OSAM", "Osmanthus americanus",
                                  ifelse(SpCode == "PAPSP2", "Packera pseudaurea var. pseudaurea",
                                  ifelse(SpCode == "PARI4", "Panicum rigidulum",
                                  ifelse(SpCode == "PARIE2", "Panicum rigidulum var. elongatum",
                                  ifelse(SpCode == "PHLA13", "Phlox latifolia",
                                  ifelse(SpCode == "PIASA", "Pityopsis aspera var. adenolepis",
                                  ifelse(SpCode == "PIGRT", "Pityopsis graminifolia var. tenuifolia",
                                  ifelse(SpCode == "PIMI", "Piptatheropsis micrantha",
                                  ifelse(SpCode == "PLAQ2", "Platanthera aquilonis",
                                  ifelse(SpCode == "POAR21", "Poa arnowiae",
                                  ifelse(SpCode == "RHMAV", "Rhexia mariana var. ventricosa",
                                         ifelse(SpCode == "RUSAM", "Rumex salicifolius var. mexicanus",
                                         ifelse(SpCode == "RUTRR", "Rudbeckia triloba var. rupestris",
                                         ifelse(SpCode == "SACA19", "Saxifraga careyana",
                                         ifelse(SpCode == "SOPUP", "Solidago puberula var. pulverulenta",
                                         ifelse(SpCode == "VEHY", "Veratrum hybridum",
                                         ifelse(SpCode == "VIBE5", "Viola ×bernardii",
                                         ifelse(SpCode == "VIES3", "Viola ×esculenta",
                                         ifelse(SpCode == "ZILE", "Zigadenus leimanthoides",
                                         AcceptedSpeciesName))))))))))))))))))))))))))))))))))))))))))))))))))

## it seems to have a limit on how many ifelse we can have in a mutate, that's why I broke it down here.
DataBase.fill <- DataBase.fill %>%
  mutate(AcceptedSpeciesName = ifelse(SpCode == "DIACA", "Dichanthelium acuminatum", AcceptedSpeciesName))

# checking if the code worked:
countFilled <- DataBase.fill %>%
  group_by(SpCode) %>%
  # non-numeric variables:
  summarise(across(one_of("AcceptedSpeciesName", "NativeStatus"), 
                   ~ n_distinct(.x[!is.na(.x)])),
            # round the numeric ones:
            #across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4)))
  ) %>%
  filter(AcceptedSpeciesName>1) # filter species codes with more than one matching bestname
head(countFilled)
nrow(countFilled) # the issues were fixed!

##fixing an error on code FEBR7 that came as 7-Feb before
DataBase.fill <- DataBase.fill %>%
  mutate(NativeStatus = ifelse(SpCode == "FEBR7" & is.na(NativeStatus), "I", NativeStatus))

##  Update few species/genera following suggestion from Jeff Corbin:
DataBase.fill %>%
  filter(SpCode %in% c("CASTI", "POLYG4", "RUMEX", "PLANT", "ELMA7")) %>%
  select(SpCode, AcceptedSpeciesName, NativeStatus, Zone) %>%
  distinct() %>%
  arrange(SpCode)

DataBase.fill <- DataBase.fill %>%
  mutate(NativeStatus = ifelse(SpCode == "CASTI",
                               "I",
                               NativeStatus),
         NativeStatus = ifelse(SpCode == "ELMA7", "N", NativeStatus),
         NativeStatus = ifelse(SpCode == "PLANT", "NI", NativeStatus),
         NativeStatus = ifelse(SpCode == "POLYG4" & Zone != "AK",
                               "NI", NativeStatus),
         NativeStatus = ifelse(SpCode == "RUMEX" & Zone != "AK", 
                               "NI", NativeStatus))

###
#### Checks PctCover column and drop plot with missing info ####
###

##  Check that all datasets are percent cover, not proportion:
DataBase.fill %>%
  group_by(Dataset) %>%
  summarize(min = min(PctCov, na.rm = TRUE),
            mean = mean(PctCov, na.rm = TRUE),
            max = max(PctCov, na.rm = TRUE)) 

# why are there any rows with NA cover
DataBase.fill %>%
  ungroup() %>%
  filter(is.na(PctCov)) %>%
  select(Site, Plot) %>%
  distinct()

DataBase.fill %>%
  filter(Plot == "24023611010478",
         !is.na(PctCov)) # all NA for this plot
#all others belong to NWCA dataset. The plots have only one or a few species that there was no cover recorded, 
#although the species were recorded. for example
DataBase.fill %>%
  filter(Plot == "NWCA_1",
         !is.na(PctCov)) # there are other species with cover recorded

# I am keeping NWCA plots with some missing cover (LAIS)

# drop this FIA plot with all NAs
DataBase.fill <- DataBase.fill %>%
  filter(Plot != "24023611010478")

DataBase.fill %>%
  filter(is.na(PctCov)) %>%
  nrow()

## sampled area has some wierd values (-40, and 0)
DataBase.fill %>% 
  ungroup() %>% 
  select(Dataset, SampledArea, Plot) %>% 
  distinct() %>% 
  filter(SampledArea <= 0) %>% 
  group_by(Dataset, SampledArea) %>% 
  count(Plot) %>%
  nrow()
## 34 plots from NPS with zeros.
## I still need to fix this!


write_rds(DataBase.fill, "final_data/Database_11182021.rds")
write_csv(DataBase.fill, "final_data/Database_11182021.csv")


##### Summaries #####

# number of records #
dim(DataBase.fill) #2041440 records

DataBase.fill %>%
  ungroup() %>%
  filter(Zone!="MissingCoords", # no coordinates
         !is.na(Year)) %>% # no year
  nrow() #1778351 records

#number of records per database
w<-DataBase.fill %>%
  ungroup() %>%
  count(Dataset) %>%
  mutate(status = "All",
         pct = round((n/sum(n))*100,2))

# number of plots #
DataBase.fill %>%
  ungroup() %>%
  select(Plot) %>%
  distinct() %>%
  nrow() 
#77333 plots

# number of invaded plots #
DataBase.fill %>%
  ungroup() %>%
  filter(NativeStatus=="I") %>%
  select(Plot) %>%
  distinct() %>%
  nrow() #42722 plots (55.24% of the total)

# number of plots  per Dataset#
x<-DataBase.fill %>%
  ungroup() %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset) %>%
  mutate(status = "All",
         pct = round((n/sum(n))*100,2))
# Dataset          n   status pct
# 1 AIM_LMF      12304 All    15.8 
# 2 AIM_TerrADat 14931 All    19.2 
# 3 FIA           2715 All     3.49
# 4 IL_CTAP       1040 All     1.34
# 5 NCVS          4594 All     5.9 
# 6 NEON          1413 All     1.82
# 7 NPS          19768 All    25.4 
# 8 NWCA          6163 All     7.92
# 9 VA_NHP        4773 All     6.13
# 10 VEGBANK      10110 All    13.0 
ggplot(x, aes(x=Dataset, y=n, fill=Dataset)) +
  geom_bar(stat="identity", width=1) +
 # coord_polar("y", start=0)+ 
  scale_fill_viridis_d(option="B") +
  theme_minimal() +
  ylab("number of plots per dataset") + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.position = "none")

# number of invaded plots per dataset#
y<-DataBase.fill %>%
  ungroup() %>%
  filter(NativeStatus=="I") %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset) %>%
  rename(invaded = n)
y
# Dataset          invaded
# 1 AIM_LMF         7374
# 2 AIM_TerrADat   10037
# 3 FIA             1929
# 4 IL_CTAP          879
# 5 NCVS            1723
# 6 NEON             806
# 7 NPS             9143
# 8 NWCA            5132
# 9 VA_NHP          2392
# 10 VEGBANK        3307

xx<-x %>%
  left_join(y) %>%
  mutate(uninvaded = n-invaded) %>%
  select(-n, -status) %>%
  gather("status", "n", 3:4)

ggplot(data=xx, aes(x=Dataset, y=n, fill=status)) +
  geom_bar(stat="identity")+
  geom_col() +
  # geom_text(aes(label=n),vjust=-0.3, size=3.5) +
  theme_bw()+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,27000,5000)) +
  scale_fill_viridis_d(option="B") + theme_minimal() +
  ylab("Number of plots") + theme(axis.title.x = element_blank(),
                                  legend.position = c(0.87, 0.87),
                                  legend.title = element_blank(),
                                  legend.background = element_rect(fill = "white", color = "black",
                                                                   size=0.25),
                                  legend.key.size = unit(0.4, "cm"),
                                  legend.text = element_text(size = 10),
                                  panel.grid = element_blank(),
                                  text=element_text(size=15, color="black"),
                                  legend.spacing.y = unit(0, 'pt'))
ggsave("GraphInvadedPlotsperDataset11082021.png", dpi=300)

# number of plots outside L48 #
DataBase.fill %>%
  ungroup() %>%
  filter(Zone!="L48",
         Zone!="MissingCoords") %>% 
  select(Plot) %>%
  distinct() %>%
  nrow() #530 plots (~0.70% of the total)

DataBase.fill %>%
  ungroup() %>%
  filter(Zone!="L48",
         Zone!="MissingCoords") %>% # 
  select(Dataset,Plot) %>%
  distinct() %>%
  count(Dataset)
# Dataset     n
# 1 FIA       104
# 2 NEON      228
# 3 NPS       198

#number of species##
DataBase.fill %>%
  ungroup() %>%
    select(SpCode) %>%
  distinct() %>%
  nrow()
# 45382 species codes

#number of species per exotic status##
DataBase.fill %>%
  ungroup() %>%
  select(SpCode, NativeStatus) %>%
  distinct() %>%
  count(NativeStatus) 
# NativeStatus     n
# 1 I             1157
# 2 N            10509
# 3 NI             353
# 4 NA           33718
# Large number of NAs, let's check it

#species with NA exotic status per Dataset#
DataBase.fill %>%
  ungroup() %>%
  select(Dataset, SpCode, NativeStatus) %>%
  filter(is.na(NativeStatus)) %>% # no year
  distinct() %>%
  count(Dataset, NativeStatus) %>%
  arrange(n) 
# number of distinct species with NA per dataset (so, the same species can show up in more than one dataset)
# Dataset        NativeStatus     n  
# 1 IL_CTAP      NA               3
# 2 NCVS         NA              27
# 3 NEON         NA             168
# 4 AIM_LMF      NA             178
# 5 VA_NHP       NA             458
# 6 FIA          NA             602
# 7 NWCA         NA             712
# 8 NPS          NA             767
# 9 AIM_TerrADat NA            1726
# 10 VEGBANK     NA           29357

##percent of species exotic status per dataset##
DatasetNA <- DataBase.fill %>%
  ungroup() %>%
  select(Dataset, SpCode, NativeStatus) %>%
  distinct() %>%
  count(Dataset, NativeStatus) %>%
  group_by(Dataset) %>%
  mutate(pct = round((n/sum(n))*100,2),
         NativeStatus = ifelse(is.na(NativeStatus), "NA", NativeStatus))
#for VegBank, species with associated NAs for exotic status are around 88% of all species id for VegBank

ggplot(data=DatasetNA, aes(x=Dataset, y=pct, fill=NativeStatus)) +
       geom_bar(stat="identity")+
       geom_col() +
       scale_fill_viridis_d(option="B") + theme_minimal() +
  ylab("% of species nativeness status per dataset") + theme(axis.title.x = element_blank(),
                                                         legend.position="bottom")

## Cover of invasive species per dataset
rangecover<- DataBase.fill %>%
  group_by(Dataset, NativeStatus) %>%
  summarize(min = min(PctCov, na.rm = TRUE),
            mean = mean(PctCov, na.rm = TRUE),
            max = max(PctCov, na.rm = TRUE))

## Years of data collection ##
table(DataBase.fill$Year)
# min == 1195
# max == 2020

y<- DataBase.fill %>%
  ungroup() %>%
  select(Dataset, Plot, Year) %>%
  distinct() 
dim(y)

x<- DataBase.fill %>%
  ungroup() %>%
  select(Dataset, Plot, Year) %>%
  distinct() %>%
  group_by(Dataset,Plot) %>% 
  count(Plot, sort = T) %>% 
  filter(n>1) 
dim(x) #7,357 plots were resampled at least once


