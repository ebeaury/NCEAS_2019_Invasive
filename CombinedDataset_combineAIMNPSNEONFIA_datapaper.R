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
##  4/21/2021: changes in code to accommodate the files from NCEAS server and the dataset Kristen Pearch worked on 
##  8/21/2021: including new datasets and merging into a full database

library(tidyverse)
theme_set(theme_classic())

setwd('/home/shares/neon-inv/data_paper')


###   Columns to carry forward in combined dataset
DesiredColumns <- c("Dataset", "Site", #AIM data does not have Site
                    "Plot", "Long", "Lat", "Year", 
                    "SpCode", "AcceptedSpeciesName", "OriginalSpeciesName", "ExoticStatus", "PctCov",
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
  mutate(FuzzedCoord = "N") %>%
  rename(Dataset = dataset,
         Site = siteID,
         Plot = plotID,
         Year = year,
         AcceptedSpeciesName = bestname,
         SpCode = Accepted.Symbol,
         Long = decimalLongitude,
         Lat = decimalLatitude,
         ExoticStatus = Native.Status,
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

FIA <- read_csv("data_by_dataset/FIA_DataPaper_07292021.csv") #ALL data including repeated samples (3207 plots)
glimpse(FIA)

# all.equal(FIA$SciName, FIA$bestname)
# FIA %>%
#   select(SciName, bestname) %>%
#   distinct() %>%
#   filter(SciName != bestname) %>%
#   head() # looks like SciName has ssp and var info, so I'll go with bestname

# total of unique plots = 2715
# length(table(FIA$PLT_CN))


FIA <- FIA %>%
  mutate(FuzzedCoord = "Y") %>%
  rename(Site = PLOT,
         Plot = PLT_CN,
         SpCode = VEG_SPCD, 
         PctCov = PlotCover,
         ExoticStatus = inv_L48,
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

# earlier visits of resampled plots:
# AIMdrop <- read_csv("code_by_dataset/extra_csv_AIM/excludeAIM_11Sept2020.csv") 

AIM <- AIM  %>%
  # drop earlier visits for resampled plots:
  # filter(!PLOTKEY %in% AIMdrop$PLOTKEY) %>%
  # convert from proportion to percent cover:
  mutate(PctCov = 100*prop.cover,
         FuzzedCoord = "N",
         Site = NA) %>%
  rename(Dataset = DataSet,
         Plot = PLOTKEY,
         Long = NAD83.X,
         Lat = NAD83.Y,
         Year = VisitYear, 
         SpCode = code,
         ExoticStatus = inv_L48,
         AcceptedSpeciesName = bestname, 
         SampledArea = nHits,
         OriginalSpeciesName = OriginalName) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(AIM)) # Site
setdiff(names(AIM), DesiredColumns)

#code to summarixe the number of distinct plots
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
  select(-Plot) %>%
  rename(ExoticStatus = Exotic,
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

# to fill Growth forms that were not carried out
## there are missing GrowthForm in VEGBANK for lichens and moss, KP filtered the L48 species, but those showed up as NA (nativeness are defined at the level of North America)
## other datasets carry the GrowthForm for them
# usdaKP <- read_csv("/home/shares/neon-inv/raw_VegBank_data/USDA_Plant_List_020821.txt")
# usdaKP <- usdaKP %>% 
#   filter(!is.na(`Native Status`)) %>% 
#   select(`Accepted Symbol`, `Growth Habit`) %>% 
#   distinct() %>% 
#   rename(SpCode = `Accepted Symbol`)

# finds only the last year of data 
# total of unique plots = 9318
# VEGBANK2 <- VEGBANK %>% group_by(VegBankUniqueID) %>% slice(which.max(Year))

VEGBANK <- VEGBANK %>% 
  #extracting the frist four letters from the plotID to use as site info
  mutate(Site = str_sub(UniqueID, 1, 4)) %>%
  rename(Plot = UniqueID,
         Lat_Original = Lat,
         Long_Original = Long,
         Lat = Public.Latitude,
         Long = Public.Longitude,
         AcceptedSpeciesName = bestname,
         ExoticStatus = ExoticStatus,
         SampledArea = Taxon.Observation.Area,
         OriginalSpeciesName = OriginalName) %>%
  mutate(Dataset = ifelse(grepl("VEGBANK", Dataset), "VEGBANK", "NCVS"),
         #Lat = ifelse(grepl("VEGBANK", Site), Public.Latitude, Lat_Original), # for datapaper: only public coordinates
         #Long = ifelse(grepl("VEGBANK", Site), Public.Longitude, Long_Original)  # for datapaper: only public coordinates
         #Laís here: I am asusming the public coordinates are the same as the original ones, but the ones in this column are 
         #the ones that can be public.
         FuzzedCoord = "N") %>%
  dplyr::select(one_of(DesiredColumns))
  # left_join(usdaKP) %>%
  # mutate(GrowthForm = ifelse(Dataset == "VEGBANK" & is.na(GrowthForm), 
  #                            `Growth Habit`, GrowthForm)) %>%
  # select(-`Growth Habit`) %>%
  

setdiff(DesiredColumns, names(VEGBANK))
setdiff(names(VEGBANK), DesiredColumns)

###
####   VANHP    ####
###

VANHP <- read.csv("data_by_dataset/VANHP_datapaper_06_16_21_EB.csv", 
                    na = c('', 'NA')) 
glimpse(VANHP)

VANHP <- VANHP %>%
  mutate(FuzzedCoord = "N", 
         Site = "NA") %>%
  rename(OriginalSpeciesName = VASpeciesName,
         AcceptedSpeciesName = bestname,
         SampledArea = Plot.Size,
         ExoticStatus = ExoticStatus) %>%
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
         Dataset = "NWCA") %>%
  rename(Site = SITE_ID,
         Plot = UniquePlotID,
         OriginalSpeciesName = OriginalName,
         AcceptedSpeciesName = bestname,
         SampledArea = PlotArea_m2,
         ExoticStatus = inv_L48,
         SpCode = Accepted.Symbol,
         PctCov = COVER) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(NWCA))
setdiff(names(NWCA), DesiredColumns)

###
#####    Join    ####
###


DataBase <- bind_rows(AIM, NPS, FIA, NEON, VEGBANK, VANHP, NWCA)
glimpse(DataBase)



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
  mutate(ExoticStatus = ifelse(Zone == "L48" & !is.na(L48status), L48status, ExoticStatus)) %>%
  select(-L48status)

DataBase %>%
  select(Dataset, Site, Plot, Zone) %>%
  distinct() %>%
  count(Zone) #number of plots per zone ##3502, being NPS(3495), VEGBANK(7)

DataBase %>%
  filter(Zone == "MissingCoords",
         !is.na(Long),
         !is.na(Lat)) %>%
  nrow() # yes, all missing coords 
table(DataBase$Zone) #missing coordinates = 50196 records

DataBase %>%
  filter(Zone == "PR") %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset)

###
####checking missing data for Year column####
DataBase %>% 
  filter(is.na(Year)) %>% 
  nrow() # missing Year = 65368 records # LP: should we drop them?

table(DataBase$Year) # checks the range of years

DataBase%>%
  filter(Year == 1195) %>%
  select(Dataset, Plot) %>%
  distinct() # ASIS_2

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
####   Check that sp traits are consistent within zones  ####
###
# 
# checkTraits <- AllSpTrait %>%
#   select(Zone, SpCode, bestname, ExoticStatus, 
#          GrowthForm, Duration, SLA, LDMC, contains("Leaf"),
#          Plant.height, Seed.dry.mass, 
#          StemSpecificDensity, C3.C4, Woodiness) %>%
#   distinct()
# 
# # First make sure code and sciname are consistent:
# checkTraits %>%
#   select(SpCode, bestname) %>%
#   distinct() %>%
#   group_by(SpCode) %>%
#   count() %>%
#   filter(n > 1) 
# 
# checkTraits %>%
#   filter(SpCode %in% c("ANROC", "NODE3", "PYUNU")) %>%
#   select(SpCode, bestname, ExoticStatus, GrowthForm, Duration)

## FIX:
DataBase <- DataBase %>%
  mutate(AcceptedSpeciesName = recode(AcceptedSpeciesName, 
                           "Pyrrocoma uniflora var. uniflora" = "Pyrrocoma uniflora",
                           "Antennaria rosea ssp. confinis" = "Antennaria rosea",
                           "Notholithocarpus densiflorus" = "Lithocarpus densiflorus"))


## rerun checkTraits creation:
# checkTraits <- AllSpTrait %>%
#   select(Zone, SpCode, bestname, ExoticStatus, 
#          GrowthForm, Duration, SLA, LDMC, contains("Leaf"),
#          Plant.height, Seed.dry.mass, 
#          StemSpecificDensity, C3.C4, Woodiness) %>%
#   distinct()

##  Is there ever disagreement on traits w/in a species, excluding NAs:
# traitCounts <- checkTraits %>%
#   group_by(SpCode, Zone) %>%
#   # non-numeric variables:
#   summarise(across(one_of("bestname", "ExoticStatus", "GrowthForm", "Duration", "C3.C4", "Woodiness"), 
#                    ~ n_distinct(.x[!is.na(.x)])),
#             # round the numeric ones:
#             across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4)))) 
# multiples <- traitCounts %>%
#   pivot_longer(!one_of("SpCode", "Zone"), names_to = "variable", values_to = "value") %>%
#   filter(value > 1)
# multiples
# 
# # look at and fix these last few manually:
# checkTraits %>% 
#   filter(SpCode %in% multiples$SpCode) %>% 
#   select(SpCode, Zone, bestname, ExoticStatus, Leaf.area) %>%
#   arrange(SpCode)
# 
# # leaf areas don't look different:
# checkTraits %>%
#   filter(SpCode %in% c("OXAR", "CAPA")) %>%
#   select(SpCode, Leaf.area) %>%
#   mutate(Leaf.area = round(Leaf.area, 2)) %>%
#   distinct() # yeah, same to 2 decimals - just round the full dataset


DataBase <- DataBase %>%
  mutate(ExoticStatus = ifelse(SpCode == "RHYNC3" & Zone == "L48",
                               "NI",
                               ExoticStatus),
         ExoticStatus = ifelse(SpCode == "FIMBR" & Zone == "L48",
                               "NI",
                               ExoticStatus),
         # based on USDA plants:
         ExoticStatus = ifelse(SpCode == "ACMI2" & Zone == "AK",
                               "N",
                               ExoticStatus))

# Above I did n_distinct while removing the NAs, still do have multiples with NAs:
DataBase %>%
  group_by(Zone, SpCode, AcceptedSpeciesName) %>%
  summarize(nStatus = n_distinct(ExoticStatus)) %>%
  filter(nStatus > 1)

###  Deal with NAs   ####

## before filling, check how many species have only missing values:
# redo not by zone:
# traitCounts.noZone <- checkTraits %>%
#   group_by(SpCode) %>%
#   # non-numeric variables:
#   summarise(across(one_of("bestname", "ExoticStatus", "GrowthForm", "Duration", "C3.C4", "Woodiness"),
#                    ~ n_distinct(.x[!is.na(.x)])),
#             # round the numeric ones:
#             across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4))))
# apply(traitCounts.noZone, 2, function(x) sum(x == 0))
# 
# AllSpTrait.fill <- AllSpTrait %>%
#   group_by(SpCode, bestname) %>%
#   fill(GrowthForm:Woodiness, .direction = "downup")

# Now for species codes (by zone, where above was for whole dataset)
DataBase.fill <- DataBase %>%
  group_by(SpCode, AcceptedSpeciesName, Zone) %>%
  fill(ExoticStatus, .direction = "downup") 

# check that have same number with only missing values:
# LP: creates a summarized table in which identifies whether a species code has 
# more than a particular "bestname", "ExoticStatus", "GrowthForm", "Duration" attributed to it.
# I see inconsistencies when adding the VEGBANK data. 
countFilled <- DataBase.fill %>%
  group_by(SpCode) %>%
  # non-numeric variables:
  summarise(across(one_of("AcceptedSpeciesName", "ExoticStatus"), 
                   ~ n_distinct(.x[!is.na(.x)])),
            # round the numeric ones:
            #across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4)))
            ) %>%
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
#have more than one bestname, I am going to manually modify based on the USDA accepted name on 08/20/2021
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
  summarise(across(one_of("AcceptedSpeciesName", "ExoticStatus"), 
                   ~ n_distinct(.x[!is.na(.x)])),
            # round the numeric ones:
            #across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4)))
  ) %>%
  filter(AcceptedSpeciesName>1) # filter species codes with more than one matching bestname
head(countFilled)
 
## species codes with different bestname associated to them
UnmatchData <- DataBase.fill %>% 
  ungroup() %>%
  filter(SpCode %in% countFilled$SpCode) %>%
  select(Dataset, SpCode, AcceptedSpeciesName, ExoticStatus) %>%
  distinct()

#manually modify based on the USDA accepted name on 08/20/2021 
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

# checking if the code worked:
countFilled <- DataBase.fill %>%
  group_by(SpCode) %>%
  # non-numeric variables:
  summarise(across(one_of("AcceptedSpeciesName", "ExoticStatus"), 
                   ~ n_distinct(.x[!is.na(.x)])),
            # round the numeric ones:
            #across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4)))
  ) %>%
  filter(AcceptedSpeciesName>1) # filter species codes with more than one matching bestname
head(countFilled)
nrow(countFilled) # the issues were fixed!

##fixing an error on code FEBR7 that came as 7-Feb before
DataBase.fill <- DataBase.fill %>%
  mutate(ExoticStatus = ifelse(SpCode == "FEBR7" & is.na(ExoticStatus), "I", ExoticStatus))



#all.equal(apply(countFilled, 2, function(x) sum(x == 0)), apply(traitCounts.noZone, 2, function(x) sum(x == 0)))
# good; so same number of species are all NA (confirms filling was done correctly within species)

##  Update few species/genera following suggestion from Jeff Corbin:
DataBase.fill %>%
  filter(SpCode %in% c("CASTI", "POLYG4", "RUMEX", "PLANT", "ELMA7")) %>%
  select(SpCode, AcceptedSpeciesName, ExoticStatus, Zone) %>%
  distinct() %>%
  arrange(SpCode)

DataBase.fill <- DataBase.fill %>%
  mutate(ExoticStatus = ifelse(SpCode == "CASTI",
                               "I",
                               ExoticStatus),
         ExoticStatus = ifelse(SpCode == "ELMA7", "N", ExoticStatus),
         ExoticStatus = ifelse(SpCode == "PLANT", "NI", ExoticStatus),
         ExoticStatus = ifelse(SpCode == "POLYG4" & Zone != "AK",
                               "NI", ExoticStatus),
         ExoticStatus = ifelse(SpCode == "RUMEX" & Zone != "AK", 
                               "NI", ExoticStatus))

#### Checks PctCover column and drop plot with missing info ####
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
#although the species were recorded. I am keeping them (LAIS)

# drop this plot
DataBase.fill <- DataBase.fill %>%
  filter(Plot != "24023611010478")

DataBase.fill %>%
  filter(is.na(PctCov)) %>%
  nrow()

write_rds(DataBase.fill, "final_data/Database_08202021.rds")
write_csv(DataBase.fill, "final_data/Database_08202021.csv")
#write_rds(AllSpTrait.fill, "J:/Projects/PAINLES/DataPrep/AllSpTrait_11March2021.rds")


##### Summaries #####

# number of records #
dim(DataBase.fill) #1926116 records

DataBase.fill %>%
  ungroup() %>%
  filter(Zone!="MissingCoords", # no coordinates
         !is.na(Year)) %>% # no year
  nrow() #1663037 records

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
#75950 plots

# number of invaded plots #
DataBase.fill %>%
  ungroup() %>%
  filter(ExoticStatus=="I") %>%
  select(Plot) %>%
  distinct() %>%
  nrow() #41524 plots (54.67% of the total)

# number of plots  per Dataset#
x<-DataBase.fill %>%
  ungroup() %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset) %>%
  mutate(status = "All",
         pct = round((n/sum(n))*100,2))
# Dataset          n 
# 1 BLM_LMF      12295    
# 2 BLM_TerrADat 14120    
# 3 FIA           2714    
# 4 NCVS          4594    
# 5 NEON          1413    
# 6 NPS          19768
# 7 NWCA          6163    
# 8 VA_NHP        4773    
# 9 VEGBANK      10110  

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
  filter(ExoticStatus=="I") %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset) %>%
  rename(invaded = n)
# Dataset          n
# 1 BLM_LMF         7374
# 2 BLM_TerrADat    9743
# 3 FIA             1904
# 4 NCVS            1723
# 5 NEON             806
# 6 NPS             9143
# 7 NWCA            5132
# 8 VA_NHP          2392
# 9 VEGBANK         3307

xx<-x %>%
  left_join(y) %>%
  mutate(uninvaded = n-invaded) %>%
  select(-n, -status) %>%
  gather("status", "n", 2:3)

ggplot(data=xx, aes(x=Dataset, y=n, fill=status)) +
  geom_bar(stat="identity")+
  geom_col() +
  scale_fill_viridis_d(option="B") + theme_minimal() +
  ylab("number of plots") + theme(axis.title.x = element_blank())

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
# 45247 species codes

#number of species per exotic status##
DataBase.fill %>%
  ungroup() %>%
  select(SpCode, ExoticStatus) %>%
  distinct() %>%
  count(ExoticStatus) 
# ExoticStatus     n
# 1 I                 1140
# 2 N                10453
# 3 NI                 350
# 4 NA               33638 
# Large number of NAs, let's check it

#species with NA exotic status per Dataset#
DataBase.fill %>%
  ungroup() %>%
  select(Dataset, SpCode, ExoticStatus) %>%
  filter(is.na(ExoticStatus)) %>% # no year
  distinct() %>%
  count(Dataset, ExoticStatus) %>%
  arrange(n) 
# number of distinct species with NA per dataset (so, the same species can show up in more than one dataset)
# Dataset        ExoticStatus     n  
# 1 NCVS         NA                  27
# 2 NEON         NA                 168
# 3 BLM_LMF      NA                 178
# 4 VA_NHP       NA                 458
# 5 FIA          NA                 566
# 6 NWCA         NA                 712
# 7 NPS          NA                 767
# 8 BLM_TerrADat NA                1680
# 9 VEGBANK      NA               29357

##percent of species exotic status per dataset##
DatasetNA <- DataBase.fill %>%
  ungroup() %>%
  select(Dataset, SpCode, ExoticStatus) %>%
  distinct() %>%
  count(Dataset, ExoticStatus) %>%
  group_by(Dataset) %>%
  mutate(pct = round((n/sum(n))*100,2),
         ExoticStatus = ifelse(is.na(ExoticStatus), "NA", ExoticStatus))
#for VegBank, species with associated NAs for exotic status are around 88% of all species id for VegBank

ggplot(data=DatasetNA, aes(x=Dataset, y=pct, fill=ExoticStatus)) +
       geom_bar(stat="identity")+
       geom_col() +
       scale_fill_viridis_d(option="B") + theme_minimal() +
  ylab("% of species exotic status per dataset") + theme(axis.title.x = element_blank())

### WORLD MAP ###

## The World Map

# coord objt

sites_coord <- DataBase.fill %>%
  #filter(invasive.response.mean.control > 0) %>% # removes all the presence/absence of invasives
  select(Dataset, Latitude, Longitude, Country) %>% #selects just a few of the columns
  distinct(StudyID, .keep_all = TRUE) %>% #removes duplicates due to several data entries for the same
  filter(!is.na(Latitude)) # removes studies that the coordinates were not entered

sites_coord <- st_as_sf(sites_coord, coords = c("Longitude", "Latitude"), remove = FALSE, 
                        crs = 4326, agr = "constant")

# world map

world <- ne_countries(scale = "medium", returnclass = "sf") #creates the object with the map
class(world) #gives you the category of the object

ggplot(data = world) + #plot the object as a map
  geom_sf() + 
  coord_sf(expand = FALSE) + #this makes the coordinates to show up
  geom_point(data = sites_coord, aes(x = Longitude, y = Latitude), size = 4, #specifies the data
             shape = 21, fill = "darkred") + #specifies how the dots will show
  theme(panel.border = element_blank()) +
  theme(plot.margin = unit(c(0,0.5,0,0.5), "cm")) +
  # theme (axis.ticks.length = unit(2, "cm")) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  ylim(-60,NA) + #defining the limits in the yaxis, NA to indicate that automatic upper limit
  ggtitle("World map of study sites included in the meta-analysis", 
          subtitle = paste0("(", length(unique(sites_coord$StudyID)), " studies from ", length(unique(sites_coord$Country)), " countries)")) #+ # I will probably modify this line
#theme_map()

ggsave("maptitle_final.pdf", dpi = 300, scale = 2) # saves the last generated plot 
ggsave("maptitle_final.png", dpi = 300, scale = 2) # saves the last generated plot 

