###  PAINLES: calc CWMs

##  Helen Sofaer
##  11 May 2020
##  4 Aug 2020: all datasets including traits
##  11 Sept 2020: dropped repeated AIM plots; updated to consistent NI codes, separate Site Plot
##  16 Oct 2020: update codes for few sp Jeff C suggested; add CWM for all other sp
##  21 Oct 2020: SPLIT from data combining file
##  8 March 2021: update after NEON updates
##  11 March 2021: update after fixing some NA/non-NA disagreement in Exotic Status

library(tidyverse)

setwd('J:/Projects/PAINLES/DataPrep/')

# created in: "C:\Users\hsofaer\Documents\HelenProjects\AI_WorkingGroup\PAINLES\Combine\combineAIMNPSNEONFIA.R"
AllSpTrait.fill <- read_rds("AllSpTrait_11March2021.rds")

###
####   Relative cover and CWMs   ####
###


##  Relative cover within plots:
AllSp.Cover <- AllSpTrait.fill %>%
  group_by(Dataset, Site, Plot) %>%
  # relative cover within plots: for for all (incl NA, NI), then for traits
  mutate(TotalCover.allSp = sum(PctCov),
         TotExCover = sum(PctCov[ExoticStatus == "I"], na.rm = TRUE),
         RelExCover = TotExCover / TotalCover.allSp,
         TotalUnkCover = sum(PctCov[is.na(ExoticStatus) | ExoticStatus == "NI"]),
         RelUnkCover = TotalUnkCover / TotalCover.allSp,
         RelCover.ThisSpvsAllSp = PctCov / TotalCover.allSp,
         TotalCover.SLA = sum(PctCov[!is.na(SLA)]),
         TotalCover.LeafN = sum(PctCov[!is.na(Leaf.N.mass)]),
         TotalCover.LDMC = sum(PctCov[!is.na(LDMC)]),
         RelCover.SLA = ifelse(is.na(SLA), NA, PctCov / TotalCover.SLA),
         RelCover.LeafN = ifelse(is.na(Leaf.N.mass), NA, PctCov / TotalCover.LeafN),
         RelCover.LDMC = ifelse(is.na(LDMC), NA, PctCov / TotalCover.LDMC),
         AbundXSLA = RelCover.SLA * SLA,
         AbundXLeafN = RelCover.LeafN * Leaf.N.mass,
         AbundXLDMC = RelCover.LDMC * LDMC)

glimpse(AllSp.Cover)

# # look at two plots
# AllSp.Cover %>%
#   filter(SitePlot == "09010103235631742013-09-01" | SitePlot == "ACAD__1") %>%
#   select(SpCode, ExoticStatus, SLA, LDMC, Leaf.N.mass, PctCov,
#          contains("TotalCov"), contains("RelCover"), contains("AbundX")) %>%
#   as.data.frame()

##  Calculate CWM:
CWM <- AllSp.Cover %>%
  group_by(Dataset, Site, Plot) %>%
  summarise(CWM.SLA = sum(AbundXSLA, na.rm = TRUE),
            CWM.LeafN = sum(AbundXLeafN, na.rm = TRUE),
            CWM.LDMC = sum(AbundXLDMC, na.rm = TRUE))

NativeCover <- AllSpTrait.fill %>%
  group_by(Dataset, Site, Plot) %>%
  filter(ExoticStatus == "N") %>%
  mutate(TotalCover.SLA = sum(PctCov[!is.na(SLA)]),
         TotalCover.LeafN = sum(PctCov[!is.na(Leaf.N.mass)]),
         TotalCover.LDMC = sum(PctCov[!is.na(LDMC)]),
         RelCover.SLA = ifelse(is.na(SLA), NA, PctCov / TotalCover.SLA),
         RelCover.LeafN = ifelse(is.na(Leaf.N.mass), NA, PctCov / TotalCover.LeafN),
         RelCover.LDMC = ifelse(is.na(LDMC), NA, PctCov / TotalCover.LDMC),
         AbundXSLA = RelCover.SLA * SLA,
         AbundXLeafN = RelCover.LeafN * Leaf.N.mass,
         AbundXLDMC = RelCover.LDMC * LDMC) 

CWM.native <- NativeCover %>%
  group_by(Dataset, Site, Plot) %>%
  summarize(CWMnative.SLA = sum(AbundXSLA, na.rm = TRUE),
            CWMnative.LeafN = sum(AbundXLeafN, na.rm = TRUE),
            CWMnative.LDMC = sum(AbundXLDMC, na.rm = TRUE))
glimpse(CWM.native) 


CWM <- CWM %>%
  left_join(CWM.native)
head(CWM)

## Just total columns for native cover by trait:
NativeCover.byPlot <- NativeCover %>%
  select(Dataset, Site, Plot, 
         TotalCoverNative.SLA = TotalCover.SLA,
         TotalCoverNative.LeafN = TotalCover.LeafN,
         TotalCoverNative.LDMC = TotalCover.LDMC) %>%
  distinct()

###
#####   CWM: excluding focal sp   ####
###

otherSpCWM <- AllSpTrait.fill %>%
  ungroup() %>%
  dplyr::select(Dataset, Site, Plot,
                SpCode, PctCov,
                SLA, LDMC, Leaf.N.mass) %>%
  mutate(CWMnonfocal.SLA = -99,
         CWMnonfocal.LeafN = -99,
         CWMnonfocal.LDMC = -99
         )

for (i in 1:nrow(otherSpCWM)) {
  
  # limit data to that plot and other species present (used ifelse b/c of issue with filtering on NA site):
  if (is.na(otherSpCWM$Site[i])) {
    temp.df <- otherSpCWM %>%
      #      that plot:
      filter(is.na(Site),
             Plot == otherSpCWM$Plot[i],
             # drop the focal species:
             SpCode != otherSpCWM$SpCode[i])
  } else {
    temp.df <- otherSpCWM %>%
      #      that plot:
      filter(Site == otherSpCWM$Site[i],
             Plot == otherSpCWM$Plot[i],
             # drop the focal species:
             SpCode != otherSpCWM$SpCode[i])
  }
   
  
  # if there are no other species, set non-focal CWMs to NA:
  if (nrow(temp.df) == 0) {
    
    otherSpCWM$CWMnonfocal.SLA[i] <- NA
    otherSpCWM$CWMnonfocal.LeafN[i] <- NA
    otherSpCWM$CWMnonfocal.LDMC[i] <- NA
    
  } else {
    
    temp.CWMnonfocal <- temp.df %>%
      mutate(TotalCover.SLA = sum(PctCov[!is.na(SLA)]),
             TotalCover.LeafN = sum(PctCov[!is.na(Leaf.N.mass)]),
             TotalCover.LDMC = sum(PctCov[!is.na(LDMC)]),
             RelCover.SLA = ifelse(is.na(SLA), NA, PctCov / TotalCover.SLA),
             RelCover.LeafN = ifelse(is.na(Leaf.N.mass), NA, PctCov / TotalCover.LeafN),
             RelCover.LDMC = ifelse(is.na(LDMC), NA, PctCov / TotalCover.LDMC),
             AbundXSLA = RelCover.SLA * SLA,
             AbundXLeafN = RelCover.LeafN * Leaf.N.mass,
             AbundXLDMC = RelCover.LDMC * LDMC) %>%
      ungroup() %>%
      summarize(CWMnonfocal.SLA = sum(AbundXSLA, na.rm = TRUE),
                CWMnonfocal.LeafN = sum(AbundXLeafN, na.rm = TRUE),
                CWMnonfocal.LDMC = sum(AbundXLDMC, na.rm = TRUE))
    
    otherSpCWM$CWMnonfocal.SLA[i] <- temp.CWMnonfocal$CWMnonfocal.SLA
    otherSpCWM$CWMnonfocal.LeafN[i] <- temp.CWMnonfocal$CWMnonfocal.LeafN
    otherSpCWM$CWMnonfocal.LDMC[i] <- temp.CWMnonfocal$CWMnonfocal.LDMC
    
    if(i %% 1000 == 0) {
      # Print status message
      cat(paste0("iteration: ", i, "\n"))
    }
  }
}

glimpse(otherSpCWM)


# Checks:
otherSpCWM %>%
  filter(CWMnonfocal.SLA == -99 | CWMnonfocal.LeafN == -99 | CWMnonfocal.LDMC == -99) %>%
  nrow()

# look at one plot
otherSpCWM %>%
  filter(Plot == "2011161070401B1")


###
####   Combine the CWM metrics   ####
###


SpLevelData <- AllSp.Cover %>%
  select(-RelCover.SLA, -RelCover.LeafN, -RelCover.LDMC, 
         -contains("AbundX")) %>%
  left_join(NativeCover.byPlot) %>%
  inner_join(CWM) %>%
  inner_join(otherSpCWM)  %>%
  ##  Drop plots with too much unk cover:
  filter(RelUnkCover < .1)

glimpse(SpLevelData)

## Zeros in CWMs arising from 0 total cover:
SpLevelData %>%
  filter(CWM.SLA == 0) %>%
  ungroup() %>%
  summarize(max(TotalCover.SLA))

SpLevelData <- SpLevelData %>%
  mutate(CWM.SLA = ifelse(TotalCover.SLA > 0, CWM.SLA, NA),
         CWM.LeafN = ifelse(TotalCover.LeafN > 0, CWM.LeafN, NA),
         CWM.LDMC = ifelse(TotalCover.LDMC > 0, CWM.LDMC, NA),
         CWMnative.SLA = ifelse(TotalCoverNative.SLA > 0, CWMnative.SLA, NA),
         CWMnative.LeafN = ifelse(TotalCoverNative.LeafN > 0, CWMnative.LeafN, NA),
         CWMnative.LDMC = ifelse(TotalCoverNative.LDMC > 0, CWMnative.LDMC, NA),
         # didn't keep non-focal total cover so calc if >0 discounting this sp
         CWMnonfocal.SLA = ifelse(is.na(SLA), 
                                  ifelse(TotalCover.SLA > 0, CWMnonfocal.SLA, NA),
                                  ifelse(TotalCover.SLA - PctCov > 0, CWMnonfocal.SLA, NA)),
         CWMnonfocal.LeafN = ifelse(is.na(Leaf.N.mass), 
                                    ifelse(TotalCover.LeafN > 0, CWMnonfocal.LeafN, NA),
                                    ifelse(TotalCover.LeafN - PctCov > 0, CWMnonfocal.LeafN, NA)),
         CWMnonfocal.LDMC = ifelse(is.na(LDMC), 
                                   ifelse(TotalCover.LDMC > 0, CWMnonfocal.LDMC, NA),
                                   ifelse(TotalCover.LDMC - PctCov > 0, CWMnonfocal.LDMC, NA)))
glimpse(SpLevelData)
summary(SpLevelData)

# basic checks:
SpLevelData %>%
  ungroup() %>%
  select(Dataset, Plot, SpCode, PctCov) %>%
  as.data.frame() %>%
  filter(!complete.cases(.)) %>%
  nrow()

SpLevelData %>%
  filter(is.na(SLA),
         CWM.SLA != CWMnonfocal.SLA) %>%
  nrow()
SpLevelData %>%
  filter(is.na(Leaf.N.mass),
         CWM.LeafN != CWMnonfocal.LeafN) %>%
  nrow()
SpLevelData %>%
  filter(is.na(LDMC),
         CWM.LDMC != CWMnonfocal.LDMC) %>%
  nrow()

write_rds(SpLevelData, "PAINLES_11March2021.rds")
write_csv(SpLevelData, "PAINLES_11March2021.csv")

