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

library(tidyverse)
theme_set(theme_classic())

setwd('/home/shares/neon-inv/data_paper')


###   Columns to carry forward in combined dataset
DesiredColumns <- c("Dataset", "Site", #AIM data does not have Site
                    "Plot", "Long", "Lat", "Year", 
                    "SpCode", "bestname", "ExoticStatus", "PctCov",
                    "GrowthForm", "Duration"#, 
                    #LP: removing traits for the data paper
                    #"TraitNRows", "SLA", "LDMC", "Leaf.area",
                    #"Leaf.N.mass", "Leaf.P.mass", "Plant.height", "Seed.dry.mass", 
                    #"StemSpecificDensity", "C3.C4", "Woodiness"
                    )

###
####   NEON   ####
###

NEON <- read.csv("data_by_dataset/NEONdata_flatted20210225traits.csv")
glimpse(NEON) 
# Accepted.Symbol has NA where code couldn't be matched to an accepted one; neon_sp_code has the original (can be synonym)

# total of unique plots = 1413
# length(table(NEON$plotID))

NEON <- NEON %>%
  mutate(Year = max(c(year_div, year_vegstr), na.rm = TRUE),
         SpCode = ifelse(is.na(Accepted.Symbol), neon_sp_code, Accepted.Symbol)) %>%
  rename(Dataset = dataset,
         Site = siteID,
         Plot = plotID,
         Long = decimalLongitude,
         Lat = decimalLatitude,
         ExoticStatus = Native.Status,
         PctCov = totalcover_sum,
         NEONbasalarea = basalarea,
         NEONcanopycover = canopycover,
         NEONvst_status = vst_status,
         #TraitNRows = N.Rows,
         #StemSpecificDensity = Stem.specific.density..SSD..or.wood.density..stem.dry.mass.per.stem.fresh.volume.
        ) %>%
  select(one_of(DesiredColumns), 
         #next line of code carries whether cover of a particular species in a plot was based on basal areas and/or canopy cover
         # if 0 in both, it came from the 1m2 plots
         # NEONvst_status = if a plot should have veg structure data, but it was not collected yet
         contains("NEON", ignore.case = FALSE))

setdiff(DesiredColumns, names(NEON))
setdiff(names(NEON), DesiredColumns)

###
#####   FIA    ####
###

FIA <- read_csv("data_by_dataset/FIA Veg data Latest Traits 8-4-20.csv") #ALL data including repeated samples (3207 plots)
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
  rename(Site = PLOT,
         Plot = PLT_CN,
         SpCode = VEG_SPCD, 
         PctCov = PlotCover,
         ExoticStatus = inv_L48,
         Long = LON,
         Lat = LAT,
         Year = INVYR,
         #TraitNRows = N.Rows,
         #C3.C4 = `C3/C4`,
         #StemSpecificDensity = SSD
         ) %>%
  mutate(Dataset = "FIA",
         Site = as.character(Site),
         Plot = as.character(Plot)) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(FIA))
setdiff(names(FIA), DesiredColumns)

###
####   AIM data    ####
###

AIM <- read_csv("data_by_dataset/AIM_AllSpTax_24June2020.csv") 
glimpse(AIM)

# earlier visits of resampled plots:
AIMdrop <- read_csv("code_by_dataset/extra_csv_AIM/excludeAIM_11Sept2020.csv") 

AIM <- AIM  %>%
  # drop earlier visits for resampled plots:
  filter(!PLOTKEY %in% AIMdrop$PLOTKEY) %>%
  # convert from proportion to percent cover:
  mutate(PctCov = 100*prop.cover) %>%
  rename(Dataset = DataSet,
         Plot = PLOTKEY,
         Long = NAD83.X,
         Lat = NAD83.Y,
         Year = VisitYear, 
         SpCode = code,
         ExoticStatus = inv_L48,
         #TraitNRows = N.Rows
         ) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(AIM)) # Site
setdiff(names(AIM), DesiredColumns)

AIM %>% select(Plot) %>% distinct() %>% nrow()

###
####   NPS    ####
###

NPS <- read.csv("data_by_dataset/NPS.AllSpTraits_21April2021.csv", 
                na = c('', 'NA'))
glimpse(NPS)

# total of unique plots = 24150
# length(table(NPS$UniqueID))
# multiple years of data for a single plot?
# NPS2 <- NPS %>% group_by(UniqueID) %>% filter(Year == max(Year))

NPS <- NPS %>%
  rename(ExoticStatus = Exotic,
         SpCode = Species,
         PctCov = Pct_Cov,
         Plot1 = Plot,
         Plot = UniqueID,
         # TraitNRows = N_Rows,
         # Leaf.area = Leaf_area,
         # Leaf.N.mass = Leaf_N_mass,
         # Leaf.P.mass = Leaf_P_mass,
         # Plant.height = Plant_height,
         # Seed.dry.mass = Seed_dry_mass,
         # StemSpecificDensity = Stem_specific_density_,
         # C3.C4 = C3C4
         ) %>%
  select(one_of(DesiredColumns))

setdiff(DesiredColumns, names(NPS))
setdiff(names(NPS), DesiredColumns)

###
####   VEGBANK    ####
###

VEGBANK <- read.csv("data_by_dataset/All_VegBank_KPEACH_reduced.csv", 
                    na = c('', 'NA')) # this data includes repeated samples
glimpse(VEGBANK)

# finds only the last year of data 
# total of unique plots = 9318
# VEGBANK2 <- VEGBANK %>% group_by(VegBankUniqueID) %>% slice(which.max(Year))

VEGBANK <- VEGBANK %>% #LP: stopped here!!!!!!
  rename(Site = Dataset,
         Plot = VegBankUniqueID,
         Lat_Original = Lat,
         Long_Original = Long,
         ExoticStatus = NEW_ExoticStatus,
         GrowthForm = NEW_GrowthForm) %>%
  mutate(Dataset = "VEGBANK",
         Lat = Public.Latitude, # for datapaper: only public coordinates
         Long = Public.Longitude # for datapaper: only public coordinates
         ) %>%
  select(one_of(DesiredColumns), contains("NEON", ignore.case = FALSE))

setdiff(DesiredColumns, names(VEGBANK))
setdiff(names(VEGBANK), DesiredColumns)

###
#####    Join    ####
###


AllSpTrait <- bind_rows(AIM, NPS, FIA, NEON, VEGBANK)
glimpse(AllSpTrait)




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

AllSpTrait <- AllSpTrait %>%
  mutate(Zone = case_when(Lat > 50 ~ "AK",
                          Lat < 50 & Lat > 19 & Long > -130 ~ "L48",
                          Long < -150 & Lat < 25 ~ "HI",
                          Lat < 20 & Long < -65 & Long > -70 ~ "PR",
                          TRUE ~ "MissingCoords")) %>%
  left_join(status) %>%
  mutate(ExoticStatus = ifelse(Zone == "L48" & !is.na(L48status), L48status, ExoticStatus)) %>%
  select(-L48status)

AllSpTrait %>%
  select(Dataset, Site, Plot, Zone) %>%
  distinct() %>%
  count(Zone) #number of entries per zone 

AllSpTrait %>%
  filter(Zone == "MissingCoords",
         !is.na(Long),
         !is.na(Lat)) %>%
  nrow() # yes, all missing coords 
table(AllSpTrait$Zone) #missing coordinates = 50196 records #LP: should we drop them?

AllSpTrait %>%
  filter(Zone == "PR") %>%
  select(Dataset, Plot) %>%
  distinct() %>%
  count(Dataset)

###
####checking missing data for Year column####
AllSpTrait %>% 
  filter(is.na(Year)) %>% 
  nrow() # missing Year = 65368 records # LP: should we drop them?

table(AllSpTrait$Year)
AllSpTrait%>%
  filter(Year == 1195) %>%
  select(Dataset, Plot) %>%
  distinct()
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
AllSpTrait <- AllSpTrait %>%
  mutate(bestname = recode(bestname, 
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


AllSpTrait <- AllSpTrait %>%
  mutate(#Leaf.area = round(Leaf.area, 2),
         ExoticStatus = ifelse(SpCode == "RHYNC3" & Zone == "L48",
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
AllSpTrait %>%
  group_by(Zone, SpCode, bestname) %>%
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
AllSpTrait.fill <- AllSpTrait %>%
  group_by(SpCode, bestname, Zone) %>%
  fill(ExoticStatus, .direction = "downup") 

# check that have same number with only missing values:
# LP: creates a summarized table in which identifies whether a species code has 
# more than a particular "bestname", "ExoticStatus", "GrowthForm", "Duration" attributed to it.
# I see inconsistencies when adding the VEGBANK data. 
countFilled <- AllSpTrait.fill %>%
  group_by(SpCode) %>%
  # non-numeric variables:
  summarise(across(one_of("bestname", "ExoticStatus", "GrowthForm", "Duration"#, "C3.C4", "Woodiness"
                          ), 
                   ~ n_distinct(.x[!is.na(.x)])),
            # round the numeric ones:
            #across(SLA:StemSpecificDensity, ~ n_distinct(round(.x[!is.na(.x)], 4)))
            ) %>%
  filter(bestname>1) # filter species codes with more than one matching bestname
head(countFilled)
 
## species codes with different bestname associated to them. Issue is within VegBank
UnmatchData <- AllSpTrait.fill %>% 
  ungroup() %>%
  filter(SpCode %in% countFilled$SpCode) %>%
  select(Dataset, SpCode, bestname, ExoticStatus, GrowthForm, Duration) %>%
  distinct()

# species codes present in VegBank with distinct bestname compared to the other datasets
UnmatchData_VB_CD <- UnmatchData %>% #VB= VegBank; CD= All others
  group_by(SpCode) %>%
  mutate(code_fix = ifelse(Dataset == "VEGBANK", 1, NA)) %>%
  filter(any(is.na(code_fix)))

# species codes present in VegBank with distinct bestname associated to them; 
# these codes did not show up in the other datasets
UnmatchData_VB <- anti_join(UnmatchData, UnmatchData_VB_CD)
# number of species within vegbank only with inconsistencies
length(table(UnmatchData_VB$SpCode))

#all.equal(apply(countFilled, 2, function(x) sum(x == 0)), apply(traitCounts.noZone, 2, function(x) sum(x == 0)))
# good; so same number of species are all NA (confirms filling was done correctly within species)


### LP Rcode run until here
## next steps add data from EVE
## force SpCode to have the same bestname across datasets

##  Update few species/genera following suggestion from Jeff Corbin:
AllSpTrait.fill %>%
  filter(SpCode %in% c("CASTI", "POLYG4", "RUMEX", "PLANT", "ELMA7")) %>%
  select(SpCode, bestname, ExoticStatus, Zone) %>%
  distinct() %>%
  arrange(SpCode)

AllSpTrait.fill <- AllSpTrait.fill %>%
  mutate(ExoticStatus = ifelse(SpCode == "CASTI",
                               "I",
                               ExoticStatus),
         ExoticStatus = ifelse(SpCode == "ELMA7", "N", ExoticStatus),
         ExoticStatus = ifelse(SpCode == "PLANT", "NI", ExoticStatus),
         ExoticStatus = ifelse(SpCode == "POLYG4" & Zone != "AK",
                               "NI", ExoticStatus),
         ExoticStatus = ifelse(SpCode == "RUMEX" & Zone != "AK", 
                               "NI", ExoticStatus))

### Fixing Duration column
table(AllSpTrait.fill$Duration)
## there are equivalent categories in column Duration with different descriptions:
AllSpTrait.fill$Duration <- gsub("Perennial, AN", "Annual, Perennial", AllSpTrait.fill$Duration)
AllSpTrait.fill$Duration <- gsub("Biennial, Perennial, AN", "Annual, Biennial, Perennial", AllSpTrait.fill$Duration)
AllSpTrait.fill$Duration <- gsub("Perennial, Biennial", "Biennial, Perennial", AllSpTrait.fill$Duration)
AllSpTrait.fill$Duration <- gsub("Biennial, AN", "Annual, Biennial", AllSpTrait.fill$Duration)
AllSpTrait.fill$Duration <- gsub("Perennial, Biennial, AN", "Annual, Biennial, Perennial", AllSpTrait.fill$Duration)
AllSpTrait.fill$Duration <- gsub("Annual, Perennial, Biennial", "Annual, Biennial, Perennial", AllSpTrait.fill$Duration)
AllSpTrait.fill$Duration <- gsub("Biennial, Annual, Perennial", "Annual, Biennial, Perennial", AllSpTrait.fill$Duration)
#replace empty cells for NA
AllSpTrait.fill$Duration <- gsub("^$", NA, AllSpTrait.fill$Duration)

##  Check that all datasets are percent cover, not proportion:
AllSpTrait.fill %>%
  group_by(Dataset) %>%
  summarize(min = min(PctCov, na.rm = TRUE),
            mean = mean(PctCov, na.rm = TRUE),
            max = max(PctCov, na.rm = TRUE)) 

# why are there any rows with NA cover
AllSpTrait.fill %>%
  ungroup() %>%
  filter(is.na(PctCov)) %>%
  select(Site, Plot) %>%
  distinct()

AllSpTrait.fill %>%
  filter(Plot == "24023611010478",
         !is.na(PctCov)) # all NA for this plot

# drop this plot
AllSpTrait.fill <- AllSpTrait.fill %>%
  filter(Plot != "24023611010478")

AllSpTrait.fill %>%
  filter(is.na(PctCov)) %>%
  nrow()

write_rds(AllSpTrait.fill, "AllSpTrait.fill.rds")
write_rds(AllSpTrait.fill, "J:/Projects/PAINLES/DataPrep/AllSpTrait_11March2021.rds")


##### Summaries #####

# number of records #
dim(AllSpTrait.fill) #
