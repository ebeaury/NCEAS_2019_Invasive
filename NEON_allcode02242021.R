
####Getting and setting up the data####
#this clears the global environment and stored objects, lists etc
#rm(list=ls())

# install neonUtilities from GitHub, should be installed at this point
#install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE)

# load neonUtilities and other required packages 
library (neonUtilities)
#library(devtools)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(lubridate)

####################################################################################################################################################################
####specify site or sites or comment out if want to run all sites

#sitesSpecified <- c("SRER", "OSBS")

####################################################################################################################################################################
####Taxonomy table import and processing

###Read morphs table for updating unknown plant species
morphs <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_NEON/neonUnknowns.csv', 
                   header = T, stringsAsFactors = F)

#morphs <- read.csv('H:/NCEAS_invasive/code/neonUnknowns.csv', 
#                    header = T, stringsAsFactors = F)

###Read Ian's taxonomy table
tax <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/taxonomy/taxonomy_temp10_traits_complete.csv', 
                header = T, stringsAsFactors = F)
# names(tax)[names(tax) == "?..Accepted.Symbol"] <- "Accepted.Symbol"

taxModifications <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/taxonomy/multStatusSpL48.csv', 
                 header = T, stringsAsFactors = F)

taxModifications <- taxModifications %>%
  dplyr::rename(#SpCode = 1,
         FinalDecisionL48 = `FINAL.DECISION..L48.`) %>%
  select(SpCode, FinalDecisionL48)


#tax <- read.csv('H:/NCEAS_invasive/IanTaxonomy/taxonomy_temp10_revised.csv', 
#                header = T, stringsAsFactors = F)

##changes to accommodate unmatched species

tax2 <- tax %>%
  left_join(taxModifications, by = c("Accepted.Symbol" = "SpCode")) %>%
  rename(inv_L48Original = inv_L48) %>%
  mutate(inv_L48 = ifelse(is.na(FinalDecisionL48), inv_L48Original, FinalDecisionL48))

# La?s variation
# all sites besides AK and HI
taxSelect_ALL <- select(tax2, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'AccSpeciesName', 
                        'bestname', 'Duration','GrowthForm', 'inv_L48') # inv_L48 reports the native status for all sites besides the AK and HI ones.
# AK sites
taxSelect_AK <- select(tax2, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'AccSpeciesName', 
                       'bestname', 'Duration','GrowthForm', 'inv_AK') # inv_AK reports the native status for Alaska sites
# HI sites
taxSelect_HI <- select(tax2, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'AccSpeciesName', 
                       'bestname', 'Duration','GrowthForm', 'inv_HI') # inv_HI reports the native status for Hawaii site

#La?s: removes the rows with NAs and select unique NEON codes
# all sites besides AK and HI
taxSelect_ALL <- taxSelect_ALL[!is.na(taxSelect_ALL$neon_code),]
taxSelect_ALL <- distinct(taxSelect_ALL, neon_code, .keep_all = TRUE)
# AK sites
taxSelect_AK <- taxSelect_AK[!is.na(taxSelect_AK$neon_code),]
taxSelect_AK <- distinct(taxSelect_AK, neon_code, .keep_all = TRUE)
# HI sites
taxSelect_HI <- taxSelect_HI[!is.na(taxSelect_HI$neon_code),]
taxSelect_HI <- distinct(taxSelect_HI, neon_code, .keep_all = TRUE)

##################################################################################################################################################################################################
####herbaceous/plant presence and percent cover data
#Retrieve data using loadByProduct() function to pipe to R session
#Download data as list for all sites or specify sites, can also feed a list of sites

#specify data product identification
presDataProductID <- as.character('DP1.10058.001')

allDiv <- loadByProduct(dpID = presDataProductID,
                       site = if(exists('sitesSpecified')){
                         sitesSpecified} else {
                           'all'},
                       package = 'basic',
                       check.size = FALSE)

###extract 1m2 data from list of lists and some processing
data_1m2 <- allDiv[["div_1m2Data"]]

data_1m2 <- data_1m2 %>% filter(boutNumber == 1)

#clean some errors that are not yet removed
data_1m2 <- data_1m2 %>% filter(!(siteID == "WOOD" & boutNumber == 2))
data_1m2 <- data_1m2 %>% filter(!(siteID == "TALL" & boutNumber == 3))
data_1m2 <- data_1m2 %>% filter(!(siteID == "DSNY" & boutNumber == 3))

#Clean up taxonomy - update available NEON unknowns
#all taxonomy will carry because already coded and easier to leave, but only work with taxonID moving forward until link with NCEAS Invasive Sp Working Group taxonomy table
#realize for loops are slow but apply was giving me a hard time here
for(w in 1:nrow(morphs)){
  data_1m2$taxonID[which(data_1m2$morphospeciesID==morphs$title[w])]<-morphs$taxonid[w]
}

#keep only the plant data (get rid of the 'other variables such as cover of wood, rock and soil):
data_1m2 <- dplyr::filter(data_1m2, divDataType == "plantSpecies")

#remove targetTaxaPrest No, these are now instances when there were no plant species present in the 1m2 subplots
data_1m2 <- filter(data_1m2, targetTaxaPresent == 'Y')

#remove uniqueID field that provides a unique identifier for each record in the NEON data
data_1m2 <- dplyr::select(data_1m2, -c(uid))

###create year column
data_1m2$year <- substr(data_1m2$endDate, start = 1, stop = 4)

#remove irrelevant fields from 1m2 data
data_1m2 <- dplyr::select(data_1m2, -c(otherVariablesPresent, identificationReferences, remarks, measuredBy, recordedBy, samplingProtocolVersion))

#change data frame name to make match code that previously included the 10 and 100m2 data 
data <- data_1m2

#this crashes my memory, I suggest we identify columns we want and run this one more time
#La?s: in my computer it worked
data <- unique(data)

####handling the taxonomy for the 1m2 herbaceous or plant presence and percent cover data
#La?s: selecting the sites by state, so the Ian's taxonomy can be incorporated separately for AK and HI sites
# all sites besides AK and HI
data_L48 <- filter(data, siteID != "BONA" & siteID != "DEJU" & siteID != "HEAL" & siteID != "TOOL" 
                   & siteID != "BARR" & siteID != "PUUM")
# AK sites
data_AK <- filter(data, siteID == "BONA" | siteID == "DEJU" | siteID == "HEAL" | siteID == "TOOL" | siteID == "BARR")
# HI sites
data_HI <- filter(data, siteID == "PUUM")

#La?s: checking if the slice is right
nrow(data_L48) + nrow(data_HI) + nrow(data_AK) == nrow(data)

#La?s: incorporates Ian's taxonomy
# all sites besides AK and HI # deu ruim
data_L48 <- dplyr::left_join(data_L48, taxSelect_ALL, by = c("taxonID" = "neon_code"))
# AK sites
data_AK <- dplyr::left_join(data_AK, taxSelect_AK, by = c("taxonID" = "neon_code"))
# HI sites
data_HI <- dplyr::left_join(data_HI, taxSelect_HI, by = c("taxonID" = "neon_code"))

#La?s: checking if the join is correct
nrow(data_L48) + nrow(data_HI) + nrow(data_AK) == nrow(data)

#La?s: renames the invasive status column
names(data_L48)[names(data_L48) == "inv_L48"] <- "Native.Status"
names(data_AK)[names(data_AK) == "inv_AK"] <- "Native.Status"
names(data_HI)[names(data_HI) == "inv_HI"] <- "Native.Status"

#La?s: combining all the 3 separate tables
herb_data_ALL <- rbind(data_L48, data_AK, data_HI)

#removes a few columns
herb_data_ALL <- dplyr::select(herb_data_ALL, -c(otherVariables, divDataType, targetTaxaPresent, 
                                                 #taxonID, 
                                                 #scientificName, 
                                                 taxonRank, family, nativeStatusCode))

#La?s: checking if the binding is correct
nrow(herb_data_ALL) == nrow(data)

#add site year
herb_data_ALL$siteYear <- paste(herb_data_ALL$siteID, herb_data_ALL$year, sep = "_")

##################################################################################################################################################################################################
####Incorporating other scales

###extract 10 and 100m2 data from list of lists and some processing
data_10_100m2 <- allDiv[["div_10m2Data100m2Data"]]

##stash a copy for plot filtering 
data_10_100m2Stash <- data_10_100m2

# keeping only the first bout (bout = sequenced numbers of sampling event in a year)
data_10_100m2 <- data_10_100m2 %>% filter(boutNumber == 1)

#clean some errors that are not yet removed
data_10_100m2 <- data_10_100m2 %>% filter(!(siteID == "TALL" & boutNumber == 3))
data_10_100m2 <- data_10_100m2 %>% filter(!(siteID == "DSNY" & boutNumber == 3))

#Clean up taxonomy - update available NEON unknowns
#all taxonomy will carry because already coded and easier to leave, but only work with taxonID moving forward until link with NCEAS Invasive Sp Working Group taxonomy table
for(n in 1:nrow(morphs)){
  data_10_100m2$taxonID[which(data_10_100m2$morphospeciesID==morphs$title[n])]<-morphs$taxonid[n]
}

#remove uniqueID field
data_10_100m2 <- dplyr::select(data_10_100m2, -c(uid))

#make sure subplotID is character (seems like it comes down that way now)
data_10_100m2$subplotID <- as.character(data_10_100m2$subplotID)

#remove targetTaxaPrest N
data_10_100m2 <- filter(data_10_100m2, targetTaxaPresent == 'Y')

#separate the 10m2 data from the 100m2 data:
data_100m2 = data_10_100m2[which(nchar(data_10_100m2$subplotID)<3), ]
data_10m2 = data_10_100m2[which(nchar(data_10_100m2$subplotID)>2), ]

data_10m2Build <- data_1m2

data_10m2Build$subplotID <- as.character(data_10m2Build$subplotID)
# rename 1m2 subplots for easy combine later
data_10m2Build$subplotID[data_10m2Build$subplotID == "31.1.1"] <- "31.1.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "31.4.1"] <- "31.4.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "32.2.1"] <- "32.2.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "32.4.1"] <- "32.4.10"  
data_10m2Build$subplotID[data_10m2Build$subplotID == "40.1.1"] <- "40.1.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "40.3.1"] <- "40.3.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "41.1.1"] <- "41.1.10"
data_10m2Build$subplotID[data_10m2Build$subplotID == "41.4.1"] <- "41.4.10"

#data preparation, add a field that is unique to the 10m and 100m2 data
data_10m2Build$additionalSpecies <- NA
#change the data type
data_10m2Build$additionalSpecies <- as.character(data_10m2Build$additionalSpecies)

#select the fields that are common to the 10m2 data
data_10m2Build <- dplyr::select(data_10m2Build, namedLocation, domainID,	siteID,	decimalLatitude,	decimalLongitude,	geodeticDatum,	coordinateUncertainty,	elevation,	elevationUncertainty,
                                nlcdClass, plotID,	subplotID,	endDate,	boutNumber,	targetTaxaPresent,	taxonID,	scientificName,	taxonRank,	family,	nativeStatusCode,
                                identificationQualifier,	taxonIDRemarks,	morphospeciesID,	morphospeciesIDRemarks,	additionalSpecies)  

data_10m2 <- dplyr::select (data_10m2, -c(samplingProtocolVersion, identificationReferences, remarks, 
                                          plotType, eventID, samplingImpractical, samplingImpracticalRemarks,
                                          biophysicalCriteria, release,
                                          measuredBy,	recordedBy, publicationDate)) ##, publicationDate - this column disappearred  

#combine what was the nested 1m2 subplot data with the 10m2 data to get the complete list of species in each 10m2 subplot 
data_10m2 <- rbind(data_10m2, data_10m2Build)

#now, similarly, aggregate the data at the 100m2 scale subplots

data_100m2Build <- data_10m2

data_100m2Build$subplotID[data_100m2Build$subplotID == "31.1.10"] <- 31
data_100m2Build$subplotID[data_100m2Build$subplotID == "31.4.10"] <- 31
data_100m2Build$subplotID[data_100m2Build$subplotID == "32.2.10"] <- 32
data_100m2Build$subplotID[data_100m2Build$subplotID == "32.4.10"] <- 32
data_100m2Build$subplotID[data_100m2Build$subplotID == "40.1.10"] <- 40
data_100m2Build$subplotID[data_100m2Build$subplotID == "40.3.10"] <- 40
data_100m2Build$subplotID[data_100m2Build$subplotID == "41.1.10"] <- 41
data_100m2Build$subplotID[data_100m2Build$subplotID == "41.4.10"] <- 41

data_100m2 <- dplyr::select (data_100m2, -c(samplingProtocolVersion, identificationReferences, remarks, 
                                            plotType, eventID, samplingImpractical, samplingImpracticalRemarks,
                                            biophysicalCriteria, release,
                                            measuredBy,	recordedBy, publicationDate))##, publicationDate - this column disappearred

data_100m2 <- rbind(data_100m2, data_100m2Build)

#Different people might have measured the 1 and 10m subplots which could result in otherwise duplicate entries, for example.
data_1m2 <- unique(data_1m2)
data_10m2 <- unique(data_10m2)
data_100m2 <- unique(data_100m2)

#create 400m2 plot-level data
#remove subplotID and other fields these might need review, depending on variables of interest
#remove uniqueID field
data_100m2Build2 <- dplyr::select(data_100m2, -c(subplotID, additionalSpecies))
data_100m2Build2[is.na(data_100m2Build2)] <- ""
#make plot list without subplots and other dup causing fields
data_400m2 <- unique(data_100m2Build2)
#create subplotID name for plot-level data
data_400m2$subplotID <- "400"

###create year column
data_10m2$year <- substr(data_10m2$endDate, start = 1, stop = 4)
data_100m2$year <- substr(data_100m2$endDate, start = 1, stop = 4)
data_400m2$year <- substr(data_400m2$endDate, start = 1, stop = 4)

###this chunk combines the data from all scales of measurement into one table, maybe we only care about the 1m2 data for the first analysis, but it might be good to have all the data in one table for others?
#use the *Tax versions of these dataframes if we get it sorted out how to apply Ian's taxonomy

data10_100_400 <- rbind(data_10m2, data_100m2)

data10_100_400_2 <- dplyr::select(data10_100_400, -c(additionalSpecies))

data10_100_400_3 <- rbind(data_400m2, data10_100_400_2)

#add appropriate fields as null to larger subplots

data10_100_400_3$otherVariables <- ""
data10_100_400_3$otherVariables <- as.factor(data10_100_400_3$otherVariables)

data10_100_400_3$percentCover <- ""
data10_100_400_3$percentCover <- as.numeric(data10_100_400_3$percentCover)

data10_100_400_3$heightPlantOver300cm <- ""
data10_100_400_3$heightPlantOver300cm <- as.factor(data10_100_400_3$heightPlantOver300cm)

data10_100_400_3$heightPlantSpecies <- ""
data10_100_400_3$heightPlantSpecies <- as.integer(data10_100_400_3$heightPlantSpecies)

data10_100_400_3$divDataType <- "plantSpecies"
data10_100_400_3$divDataType <- as.factor(data10_100_400_3$divDataType)

#Join all scales in a single dataframe
columnsnames <- colnames(data10_100_400_3) #creates a vector with the desired column names
data_1m2 <- data_1m2[, columnsnames] #selects the columns based on the vector created above

data2 <- rbind(data_1m2, data10_100_400_3)
#removing a couple of columns
data2 <- dplyr::select(data2, -c(divDataType, targetTaxaPresent))

data2 <- unique(data2) 
## ^^ CANNOT CHANGE THIS ONEEE

#La?s: selecting the sites by state, so the Ian's taxonomy can be incorporated separately for AK and HI sites
# all sites besides AK and HI
data_L48 <- filter(data2, siteID != "BONA" & siteID != "DEJU" & siteID != "HEAL" & siteID != "TOOL" 
                   & siteID != "BARR" & siteID != "PUUM")
# AK sites
data_AK <- filter(data2, siteID == "BONA" | siteID == "DEJU" | siteID == "HEAL" | siteID == "TOOL" | siteID == "BARR")
# HI sites
data_HI <- filter(data2, siteID == "PUUM")

#La?s: checking if the slice is right
nrow(data_L48) + nrow(data_HI) + nrow(data_AK) == nrow(data2)

#La?s: incorporates Ian's taxonomy
# all sites besides AK and HI
data_L48X <- data_L48 %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  left_join(taxSelect_ALL, by = c("taxonID2" = "neon_code"))

#filters the unmatches just using neon_code from Ian taxonomy
data_L482 <- filter(data_L48X, is.na(Accepted.Symbol))

#selects the other columns from the taxonomy table to join the tables by common accepted symbols
tax3 <- tax2 %>%
  filter(is.na(neon_code)) %>%
  distinct(Accepted.Symbol, .keep_all = T) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  select(Accepted.Symbol, Accepted.Symbol2, Synonym.Symbol, Symbol, Scientific.Name, AccSpeciesName, 
bestname, Duration,GrowthForm, inv_L48)

# join the taxonomy table to the neon data
data_L482 <- data_L482 %>%
  select(-Accepted.Symbol, -Synonym.Symbol, -Symbol, -Scientific.Name,
  -AccSpeciesName, -bestname, -Duration, -GrowthForm, -inv_L48) %>%
  left_join(tax3, by = c("taxonID2" = "Accepted.Symbol2"))

#filters the matches just using neon_code from Ian taxonomy
data_L483 <- filter(data_L48X, !is.na(Accepted.Symbol))

#check if the 2 pieces of data together have the same length 
nrow(data_L482) + nrow(data_L483) == nrow(data_L48)
length(colnames(data_L482)) == length(colnames(data_L483))

#bind the 2 pieces to return to the final dataset for L48
data_L484 <- rbind(data_L482, data_L483)

#recheck the total dataset
nrow(data_L484) == nrow(data_L48X)


#there are still missing names
data_L482temp <- filter(data_L482, is.na(Accepted.Symbol))
table(data_L482temp$taxonID2)

##select the entries identified to SPECIES level from NEON but NO MATCH to Ian's taxonomy
##when getting the best match years, my hope is that this will be a smaller number
# this codes select rows with a specific pattern. Here I could have used just filter, but 
#I want to keep this line code because it could be helpful later. It searches rows with a 
#partial string match (it does not need to be the entire word)
data_L482tempcountspp <- data_L482temp[grep("species", data_L482temp$taxonRank), ] 
## get distinct one == number of species with no native status
data_L482tempcountspp <- data_L482tempcountspp %>%
  group_by(taxonID2, scientificName, nativeStatusCode) %>%
  tally()

##select the entries identified to genus level
data_L482tempcountgenus <- data_L482temp[grep("genus", data_L482temp$taxonRank), ]
## get distinct one == number of species with no native status
data_L482tempcountgenus <- data_L482tempcountgenus %>%
  group_by(taxonID2, scientificName, nativeStatusCode) %>%
  tally()


# AK sites
data_AKX <- data_AK %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  left_join(taxSelect_AK, by = c("taxonID2" = "neon_code"))

#filters the unmatches just using neon_code from Ian taxonomy
data_AK2 <- filter(data_AKX, is.na(Accepted.Symbol))

#selects the other columns from the taxonomy table to join the tables by common accepted symbols
tax3 <- tax2 %>%
  filter(is.na(neon_code)) %>%
  distinct(Accepted.Symbol, .keep_all = T) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  select(Accepted.Symbol, Accepted.Symbol2, Synonym.Symbol, Symbol, Scientific.Name, AccSpeciesName, 
         bestname, Duration,GrowthForm, inv_AK)

# join the taxonomy table to the neon data
data_AK2 <- data_AK2 %>%
  select(-Accepted.Symbol, -Synonym.Symbol, -Symbol, -Scientific.Name,
         -AccSpeciesName, -bestname, -Duration, -GrowthForm, -inv_AK) %>%
  left_join(tax3, by = c("taxonID2" = "Accepted.Symbol2"))

#filters the matches just using neon_code from Ian taxonomy
data_AK3 <- filter(data_AKX, !is.na(Accepted.Symbol))

#check if the 2 pieces of data together have the same length 
nrow(data_AK2) + nrow(data_AK3) == nrow(data_AK)
length(colnames(data_AK2)) == length(colnames(data_AK3))

#bind the 2 pieces to return to the final dataset for L48
data_AK4 <- rbind(data_AK2, data_AK3)

#recheck the total dataset
nrow(data_AK4) == nrow(data_AKX)


#there are still missing names
data_AK2temp <- filter(data_AK2, is.na(Accepted.Symbol))
table(data_AK2temp$taxonID2)

##select the entries identified to SPECIES level from NEON but NO MATCH to Ian's taxonomy
##when getting the best match years, my hope is that this will be a smaller number
# this codes select rows with a specific pattern. Here I could have used just filter, but 
#I want to keep this line code because it could be helpful later. It searches rows with a 
#partial string match (it does not need to be the entire word)
data_AK2tempcountspp <- data_AK2temp[grep("species", data_AK2temp$taxonRank), ] 
## get distinct one == number of species with no native status
data_AK2tempcountspp <- data_AK2tempcountspp %>%
  group_by(taxonID2, scientificName, nativeStatusCode) %>%
  tally()

##select the entries identified to genus level
data_AK2tempcountgenus <- data_AK2temp[grep("genus", data_AK2temp$taxonRank), ]
## get distinct one == number of species with no native status
data_AK2tempcountgenus <- data_AK2tempcountgenus %>%
  group_by(taxonID2, scientificName, nativeStatusCode) %>%
  tally()
##none


# HI sites
data_HIX <- data_HI %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  left_join(taxSelect_HI, by = c("taxonID2" = "neon_code"))

#filters the unmatches just using neon_code from Ian taxonomy
data_HI2 <- filter(data_HIX, is.na(Accepted.Symbol))

#selects the other columns from the taxonomy table to join the tables by common accepted symbols
tax3 <- tax2 %>%
  filter(is.na(neon_code)) %>%
  distinct(Accepted.Symbol, .keep_all = T) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  select(Accepted.Symbol, Accepted.Symbol2, Synonym.Symbol, Symbol, Scientific.Name, AccSpeciesName, 
         bestname, Duration,GrowthForm, inv_HI)

# join the taxonomy table to the neon data
data_HI2 <- data_HI2 %>%
  select(-Accepted.Symbol, -Synonym.Symbol, -Symbol, -Scientific.Name,
         -AccSpeciesName, -bestname, -Duration, -GrowthForm, -inv_HI) %>%
  left_join(tax3, by = c("taxonID2" = "Accepted.Symbol2"))

#filters the matches just using neon_code from Ian taxonomy
data_HI3 <- filter(data_HIX, !is.na(Accepted.Symbol))

#check if the 2 pieces of data together have the same length 
nrow(data_HI2) + nrow(data_HI3) == nrow(data_HI)
length(colnames(data_HI2)) == length(colnames(data_HI3))

#bind the 2 pieces to return to the final dataset for L48
data_HI4 <- rbind(data_HI2, data_HI3)

#recheck the total dataset
nrow(data_HI4) == nrow(data_HIX)


#there are still missing names
data_HI2temp <- filter(data_HI2, is.na(Accepted.Symbol))
table(data_HI2temp$taxonID2)

##select the entries identified to SPECIES level from NEON but NO MATCH to Ian's taxonomy
##when getting the best match years, my hope is that this will be a smaller number
# this codes select rows with a specific pattern. Here I could have used just filter, but 
#I want to keep this line code because it could be helpful later. It searches rows with a 
#partial string match (it does not need to be the entire word)
data_HI2tempcountspp <- data_HI2temp[grep("species", data_HI2temp$taxonRank), ] 
## get distinct one == number of species with no native status
data_HI2tempcountspp <- data_HI2tempcountspp %>%
  group_by(taxonID2, scientificName, nativeStatusCode) %>%
  tally()

##select the entries identified to genus level
data_HI2tempcountgenus <- data_HI2temp[grep("genus", data_HI2temp$taxonRank), ]
## get distinct one == number of species with no native status
data_HI2tempcountgenus <- data_HI2tempcountgenus %>%
  group_by(taxonID2, scientificName, nativeStatusCode) %>%
  tally()

#La?s: checking if the join is correct
nrow(data_L484) + nrow(data_HI4) + nrow(data_AK4) == nrow(data2)

#La?s: renames the invasive status column
names(data_L484)[names(data_L484) == "inv_L48"] <- "Native.Status"
names(data_AK4)[names(data_AK4) == "inv_AK"] <- "Native.Status"
names(data_HI4)[names(data_HI4) == "inv_HI"] <- "Native.Status"

#La?s: combining all the 3 separate tables
allscales_data_ALL <- rbind(data_L484, data_AK4, data_HI4)

#removes a few columns
allscales_data_ALL <- dplyr::select(allscales_data_ALL, -c(otherVariables, 
                                                           #taxonID, 
                                                           #scientificName, 
                                                           taxonRank, family, nativeStatusCode))


#La?s: checking if the binding is correct
nrow(allscales_data_ALL) == nrow(data2)

#add site year
allscales_data_ALL$siteYear <- paste(allscales_data_ALL$siteID, allscales_data_ALL$year, sep = "_")

####################################################################################################################################################################################
####Woody vegetation structure data

vstDataProductID=as.character('DP1.10098.001')

df <- loadByProduct(dpID=vstDataProductID,
                   site = if(exists('sitesSpecified')){
                     sitesSpecified} else {
                       'all'},
                   package = "basic",
                   check.size = FALSE)

##unlist to create separate dataframes
perplot <- df$vst_perplotperyear
appind <- df$vst_apparentindividual
map <- df$vst_mappingandtagging
shrub <- df$vst_shrubgroup

####Evaluate and process the woody data
plot_df <- select(perplot, domainID, siteID, plotType, plotID, nlcdClass, eventID, treesPresent,
                  treesAbsentList, shrubsPresent, shrubsAbsentList, lianasPresent,
                  lianasAbsentList, nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana,
                  totalSampledAreaTrees, totalSampledAreaShrubSapling, totalSampledAreaLiana,
                  remarks)

print(c("this dataset contains the following growth forms:", unique(appind$growthForm)))

#individual measures of stem and crown
ai_df <- appind %>%
  filter(eventID %in% plot_df$eventID & plotID%in%plot_df$plotID)%>%
  select(-uid, -namedLocation, -date, -domainID)

## add taxonID, the map data, just pulling out the names because taxID not in apparent individual name, rank etc
taxVST <- map%>%
  select(plotID, individualID, taxonID, scientificName, taxonRank)

taxVST$scientificName <- as.character(taxVST$scientificName)

#add scientific name to the apparent individual table
df_out <- left_join(ai_df, taxVST)

##evaluate and process shrub data
sh_df <-  select(shrub, siteID, eventID, plotID, individualID=groupID, taxonID, scientificName, taxonRank, 
                 canopyArea, volumePercent, livePercent)


#sh_df <- filter(shrub, plotID %in% divPlotsFilter) %>%
#  select(siteID, eventID, plotID, individualID=groupID, taxonID, scientificName, taxonRank, 
#         canopyArea, volumePercent, livePercent)


####Pull in the non woody data

#code below is not working, even when I select which Release I want. 
#Downloaded the data through the website and imported into the server, but the files I need are empty
# Emailed Courtney again to try to find a solution
# vst_nonWoodDataProductID=as.character('DP1.10045.001')
# 
# 
# nonWood <- loadByProduct(dpID=vst_nonWoodDataProductID,
#                         site = if(exists('sitesSpecified')){
#                           sitesSpecified} else {
#                             'all'},
#                         package = "basic",
#                         check.size = FALSE, 
#                         release = "RELEASE-2021")

nonWood <- neonUtilities::loadByProduct(
  dpID = "DP1.10045.001",
  site = "all",
  startdate = "2015-01",
  enddate = "2019-12",
  package = "basic",
  release = "RELEASE-2021",
  check.size = FALSE,
  token = Sys.getenv('NEON_PAT')
)

# nonWood <- read.csv("/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_NEON/XXXX.csv")

#Courtney response "Use the older RELEASE-2021 and use DP1.10045.001 to find the `totalSampledAreaOther` data"
#so the old code should work just fine, just need to add an argument "release = 'RELEASE-2021'"

##unlist to create separate dataframes

nonWoodyPerPlot <- nonWood$vst_perplotperyear
nonWoodyPerInd <- nonWood$nst_perindividual

# nonWoodyPerPlot <- df$vst_perplotperyear
# nonWoodyPerInd <- df$`vst_non-woody`

##evaluate and process non woody data in the vst data product
nonWoodyPlot_df <- dplyr::select(nonWoodyPerPlot, domainID, siteID, plotType, plotID, nlcdClass, eventID, cactiPresent,
                          cactiAbsentList, fernsPresent, fernsAbsentList, yuccasPresent,
                          yuccasAbsentList, palmsPresent, palmsAbsentList, ocotillosPresent, ocotillosAbsentList,
                          xerophyllumPresent, xerophyllumAbsentList, nestedSubplotAreaOther, totalSampledAreaOther, 
                          remarks)

print(c("this dataset contains the following growth forms:", unique(nonWoodyPerInd$growthForm)))

#filter individual table by perplot 
nonwind_df <- nonWoodyPerInd %>%
  filter(eventID %in% nonWoodyPlot_df$eventID & plotID%in%nonWoodyPlot_df$plotID)%>%
  select(-uid, -namedLocation, -date, -domainID)

####calculate areas
#get crown areas; calculated as area of an elipse
df_out$coverArea <- (df_out$maxCrownDiameter/2) * (df_out$ninetyCrownDiameter/2) * pi

#get basal area; in an ideal world with have crown cover for all individuals but older data is unlikely to have these and tower plot data will not; create basal area should that be of use
df_out$basalArea <- ifelse(!is.na(df_out$stemDiameter), pi*df_out$stemDiameter, pi*df_out$basalStemDiameter) # how Dave was calculating it
df_out$basalArea2 <- ifelse(!is.na(df_out$stemDiameter), (pi*((df_out$stemDiameter/2)^2))/10000, (pi*((df_out$basalStemDiameter/2)^2))/10000) # LP: thinks this is the right way, from cm² to m²

## remove records that do not contain the necessary data to calculate either of these values, small trees, dead things, things that no longer qualify
#df_out$ck <- ifelse(is.na(df_out$coverArea)&is.na(df_out$basalArea), 0, 1) 
#df_out <- filter(df_out, ck==1)

#what proportion of living material is sp of interest (what is alive etc), only shrub group table
sh_df$coverArea <- sh_df$canopyArea*sh_df$volumePercent/100*sh_df$livePercent/100
sh_df$growthForm <- 'shrubgroup'

#calc area for nonwoody
nonwind_df$coverArea <- (nonwind_df$maxCrownDiameter/2) * (nonwind_df$ninetyCrownDiameter/2) * pi

#assume that if don't have crown diameter than capturing in the diversity data - e.g., pad counts of opuntia
#nonwind_df <- nonwind_df %>% filter(!is.na(coverArea))

#combine woody and shrub group data; aligns matching fields 
df_out <- bind_rows(df_out, sh_df)

#combine woody/shrubgroup and non woody
df_outBoth <- bind_rows(df_out, nonwind_df)

#remove dead etc individuals
target <- c("Live", "Live, insect damaged", "Live, disease damaged", "Live, physically damaged", "Live, other damage", "Live, broken bole")
df_outBoth <- filter(df_outBoth, plantStatus %in% target)

#remove null taxonid; just cleaning odd instances usually in early data
df_outBoth <- filter(df_outBoth, !is.na(scientificName))

####Calculate area and cover
#sum area values by scientific name and growth form, add plotID, add eventID to selects above
countTax <- df_outBoth%>%
  dplyr::group_by(siteID, eventID, plotID, taxonID, scientificName,  taxonRank, growthForm)%>% 
  dplyr::summarize(indCount=n(), totalCrownArea=sum(coverArea), totalBasalArea=sum(basalArea), totalBasalArea2=sum(basalArea2))%>%
  dplyr::ungroup()

# calculate sampled area, the relevant sampling area depends on the growth form being considered
countTax$plotAreaSampled <- NA
countTax$plotAreaSampled <- as.numeric(countTax$plotAreaSampled)

print(c("this dataset contains the following growth forms:", unique(countTax$growthForm)))

for(i in 1:nrow(countTax)){
  countTax$plotAreaSampled[i] <- ifelse(countTax$growthForm[i] %in%c("multi-bole tree", "single bole tree"),
                                        plot_df$totalSampledAreaTrees[countTax$plotID[i]==plot_df$plotID],
                                        ifelse(countTax$growthForm[i] %in%c("sapling", "small tree", "single shrub", "small shrub", "shrubgroup"),
                                               plot_df$totalSampledAreaShrubSapling[countTax$plotID[i]==plot_df$plotID],
                                               ifelse(countTax$growthForm[i] %in%c("palm", "fern", "cactus", "yucca", "xerophyllum", "ocotillo", "tree fern"),
                                                      nonWoodyPlot_df$totalSampledAreaOther[countTax$plotID[i]==nonWoodyPlot_df$plotID],
                                                      (plot_df$totalSampledAreaLiana)
                                               )
                                        )
  )
} 


#calc percent cover 
countTax$percentCoverCanopy <- countTax$totalCrownArea/countTax$plotAreaSampled
countTax$percentCoverBasal <- countTax$totalBasalArea/countTax$plotAreaSampled
countTax$percentCoverBasal100 <- (countTax$totalBasalArea/countTax$plotAreaSampled)*100 # LP: here I transform basal area into percentage, BUT it seems that the prior calcualtion was wrong

countTax$percentCoverBasal2 <- countTax$totalBasalArea2/countTax$plotAreaSampled #LP modified
countTax$percentCoverBasal2100 <- (countTax$totalBasalArea2/countTax$plotAreaSampled)*100 #LP modified

countTax$stemDensity <- countTax$indCount/countTax$plotAreaSampled

#create year col
countTax$year <- substr(countTax$eventID, start = 10, stop = 13)
#countTax
## ^^ CANNOT CHANGE THIS ONEEE [in Lais' computer]

#La?s: selecting the sites by state, so the Ian's taxonomy can be incorporated separately for AK and HI sites
# all sites besides AK and HI
vst_data_L48 <- filter(countTax, siteID != "BONA" & siteID != "DEJU" & siteID != "HEAL" & siteID != "TOOL" 
                       & siteID != "BARR" & siteID != "PUUM")
# AK sites
vst_data_AK <- filter(countTax, siteID == "BONA" | siteID == "DEJU" | siteID == "HEAL" | siteID == "TOOL" | siteID == "BARR")
# HI sites
vst_data_HI <- filter(countTax, siteID == "PUUM")

#La?s: checking if the slice is right
nrow(vst_data_L48) + nrow(vst_data_HI) + nrow(vst_data_AK) == nrow(countTax)

#La?s: incorporates Ian's taxonomy
# all sites besides AK and HI 
vst_data_L48X <- vst_data_L48 %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  left_join(taxSelect_ALL, by = c("taxonID2" = "neon_code"))

#filters the unmatches just using neon_code from Ian taxonomy
vst_data_L482 <- filter(vst_data_L48X, is.na(Accepted.Symbol))

#selects the other columns from the taxonomy table to join the tables by common accepted symbols
tax3 <- tax2 %>%
  filter(is.na(neon_code)) %>%
  distinct(Accepted.Symbol, .keep_all = T) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  select(Accepted.Symbol, Accepted.Symbol2, Synonym.Symbol, Symbol, Scientific.Name, AccSpeciesName, 
         bestname, Duration,GrowthForm, inv_L48)

# join the taxonomy table to the neon data
vst_data_L482 <- vst_data_L482 %>%
  select(-Accepted.Symbol, -Synonym.Symbol, -Symbol, -Scientific.Name,
         -AccSpeciesName, -bestname, -Duration, -GrowthForm, -inv_L48) %>%
  left_join(tax3, by = c("taxonID2" = "Accepted.Symbol2"))

#filters the matches just using neon_code from Ian taxonomy
vst_data_L483 <- filter(vst_data_L48X, !is.na(Accepted.Symbol))

#check if the 2 pieces of data together have the same length 
nrow(vst_data_L482) + nrow(vst_data_L483) == nrow(vst_data_L48)
length(colnames(vst_data_L482)) == length(colnames(vst_data_L483))

#bind the 2 pieces to return to the final dataset for L48
vst_data_L484 <- rbind(vst_data_L482, vst_data_L483)

#recheck the total dataset
nrow(vst_data_L484) == nrow(vst_data_L48X)


#there are still missing names
vst_data_L482temp <- filter(vst_data_L482, is.na(Accepted.Symbol))
table(vst_data_L482temp$taxonID2)

##select the entries identified to SPECIES level from NEON but NO MATCH to Ian's taxonomy
##when getting the best match years, my hope is that this will be a smaller number
# this codes select rows with a specific pattern. Here I could have used just filter, but 
#I want to keep this line code because it could be helpful later. It searches rows with a 
#partial string match (it does not need to be the entire word)
vst_data_L482tempcountspp <- vst_data_L482temp[grep("species", vst_data_L482temp$taxonRank), ] 
## get distinct one == number of species with no native status
vst_data_L482tempcountspp <- vst_data_L482tempcountspp %>%
  group_by(taxonID2, scientificName) %>%
  tally()

##select the entries identified to genus level
vst_data_L482tempcountgenus <- vst_data_L482temp[grep("genus", vst_data_L482temp$taxonRank), ]
## get distinct one == number of species with no native status
vst_data_L482tempcountgenus <- vst_data_L482tempcountgenus %>%
  group_by(taxonID2, scientificName) %>%
  tally()


# AK sites
vst_data_AKX <- vst_data_AK %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  left_join(taxSelect_AK, by = c("taxonID2" = "neon_code"))

#filters the unmatches just using neon_code from Ian taxonomy
vst_data_AK2 <- filter(vst_data_AKX, is.na(Accepted.Symbol))

#selects the other columns from the taxonomy table to join the tables by common accepted symbols
tax3 <- tax2 %>%
  filter(is.na(neon_code)) %>%
  distinct(Accepted.Symbol, .keep_all = T) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  select(Accepted.Symbol, Accepted.Symbol2, Synonym.Symbol, Symbol, Scientific.Name, AccSpeciesName, 
         bestname, Duration,GrowthForm, inv_AK)

# join the taxonomy table to the neon data
vst_data_AK2 <- vst_data_AK2 %>%
  select(-Accepted.Symbol, -Synonym.Symbol, -Symbol, -Scientific.Name,
         -AccSpeciesName, -bestname, -Duration, -GrowthForm, -inv_AK) %>%
  left_join(tax3, by = c("taxonID2" = "Accepted.Symbol2"))

#filters the matches just using neon_code from Ian taxonomy
vst_data_AK3 <- filter(vst_data_AKX, !is.na(Accepted.Symbol))

#check if the 2 pieces of data together have the same length 
nrow(vst_data_AK2) + nrow(vst_data_AK3) == nrow(vst_data_AK)
length(colnames(vst_data_AK2)) == length(colnames(vst_data_AK3))

#bind the 2 pieces to return to the final dataset for L48
vst_data_AK4 <- rbind(vst_data_AK2, vst_data_AK3)

#recheck the total dataset
nrow(vst_data_AK4) == nrow(vst_data_AKX)


#there are still missing names
vst_data_AK2temp <- filter(vst_data_AK2, is.na(Accepted.Symbol))
table(vst_data_AK2temp$taxonID2)

##select the entries identified to SPECIES level from NEON but NO MATCH to Ian's taxonomy
##when getting the best match years, my hope is that this will be a smaller number
# this codes select rows with a specific pattern. Here I could have used just filter, but 
#I want to keep this line code because it could be helpful later. It searches rows with a 
#partial string match (it does not need to be the entire word)
vst_data_AK2tempcountspp <- vst_data_AK2temp[grep("species", vst_data_AK2temp$taxonRank), ] 
## get distinct one == number of species with no native status
vst_data_AK2tempcountspp <- vst_data_AK2tempcountspp %>%
  group_by(taxonID2, scientificName) %>%
  tally()

##select the entries identified to genus level
vst_data_AK2tempcountgenus <- vst_data_AK2temp[grep("genus", vst_data_AK2temp$taxonRank), ]
## get distinct one == number of species with no native status
vst_data_AK2tempcountgenus <- vst_data_AK2tempcountgenus %>%
  group_by(taxonID2, scientificName) %>%
  tally()


# HI sites
vst_data_HIX <- vst_data_HI %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  left_join(taxSelect_HI, by = c("taxonID2" = "neon_code"))

#filters the unmatches just using neon_code from Ian taxonomy
vst_data_HI2 <- filter(vst_data_HIX, is.na(Accepted.Symbol))

#selects the other columns from the taxonomy table to join the tables by common accepted symbols
tax3 <- tax2 %>%
  filter(is.na(neon_code)) %>%
  distinct(Accepted.Symbol, .keep_all = T) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  select(Accepted.Symbol, Accepted.Symbol2, Synonym.Symbol, Symbol, Scientific.Name, AccSpeciesName, 
         bestname, Duration,GrowthForm, inv_HI)

# join the taxonomy table to the neon data
vst_data_HI2 <- vst_data_HI2 %>%
  select(-Accepted.Symbol, -Synonym.Symbol, -Symbol, -Scientific.Name,
         -AccSpeciesName, -bestname, -Duration, -GrowthForm, -inv_HI) %>%
  left_join(tax3, by = c("taxonID2" = "Accepted.Symbol2"))

#filters the matches just using neon_code from Ian taxonomy
vst_data_HI3 <- filter(vst_data_HIX, !is.na(Accepted.Symbol))

#check if the 2 pieces of data together have the same length 
nrow(vst_data_HI2) + nrow(vst_data_HI3) == nrow(vst_data_HI)
length(colnames(vst_data_HI2)) == length(colnames(vst_data_HI3))

#bind the 2 pieces to return to the final dataset for L48
vst_data_HI4 <- rbind(vst_data_HI2, vst_data_HI3)

#recheck the total dataset
nrow(vst_data_HI4) == nrow(vst_data_HIX)


#there are still missing names
vst_data_HI2temp <- filter(vst_data_HI2, is.na(Accepted.Symbol))
table(vst_data_HI2temp$taxonID2)

##select the entries identified to SPECIES level from NEON but NO MATCH to Ian's taxonomy
##when getting the best match years, my hope is that this will be a smaller number
# this codes select rows with a specific pattern. Here I could have used just filter, but 
#I want to keep this line code because it could be helpful later. It searches rows with a 
#partial string match (it does not need to be the entire word)
vst_data_HI2tempcountspp <- vst_data_HI2temp[grep("species", vst_data_HI2temp$taxonRank), ] 
## get distinct one == number of species with no native status
vst_data_HI2tempcountspp <- vst_data_HI2tempcountspp %>%
  group_by(taxonID2, scientificName) %>%
  tally()
#none

##select the entries identified to genus level
vst_data_HI2tempcountgenus <- vst_data_HI2temp[grep("genus", vst_data_HI2temp$taxonRank), ]
## get distinct one == number of species with no native status
vst_data_HI2tempcountgenus <- vst_data_HI2tempcountgenus %>%
  group_by(taxonID2, scientificName) %>%
  tally()
#none


#La?s: checking if the join is correct
nrow(vst_data_L484) + nrow(vst_data_HI4) + nrow(vst_data_AK4) == nrow(countTax)

#La?s: renames the invasive status column
names(vst_data_L484)[names(vst_data_L484) == "inv_L48"] <- "Native.Status"
names(vst_data_AK4)[names(vst_data_AK4) == "inv_AK"] <- "Native.Status"
names(vst_data_HI4)[names(vst_data_HI4) == "inv_HI"] <- "Native.Status"

#La?s: combining all the 3 separate tables
vst_data_ALL <- rbind(vst_data_L484, vst_data_AK4, vst_data_HI4)

#La?s: checking if the binding is correct
nrow(vst_data_ALL) == nrow(countTax)

#add site year
vst_data_ALL$siteYear <- substr(vst_data_ALL$eventID, start = 5, stop = 13)

#########END##########

#allscales_data_ALL is the supposed to be the same as NEONdata_ALL
#vst_data_ALL is the supposed to be the same as vegStructureAllScales

## code for merging datasets ##

filterSites <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_NEON/bestVSTDIV_modified.csv', header = T, stringsAsFactors = F ) 

#codes in vst_status indicate if a site is supposed to have or not vst data, and if this info is available
vstStatus <- filterSites %>%
  select(siteID, vst_status)

#adds the plot type: distributed or tower plot
plotType <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_NEON/All_NEON_TOS_Plot_Centroids_V8.csv', 
                   header = T, stringsAsFactors = F)

plotType <- plotType %>%
  select(plotID, plotType) %>%
  distinct(plotID, .keep_all = TRUE)

###tidying the data
##diversity - all years
NEONdiv_allscalesX <- allscales_data_ALL %>%
  unite(siteYear, siteID, year, sep = "_", remove = FALSE) %>%
  select(siteID, plotID, subplotID, nlcdClass, decimalLongitude, decimalLatitude, year, 
         taxonID2, 
         scientificName, #Original species names from NEON
         Accepted.Symbol, 
         bestname, Scientific.Name, AccSpeciesName, Duration, GrowthForm, percentCover, Native.Status, siteYear) %>%
  left_join(plotType, by = "plotID") 

#selecting only the 1m? subplots
plots1m2 <- c("31.1.1", "40.1.1", "41.1.1", "40.3.1", "31.4.1", "32.2.1", "32.4.1", "41.4.1")
NEONdiv_1m2X <- NEONdiv_allscalesX %>%
  filter(subplotID %in% plots1m2) %>%
  mutate(plotAreaSampled = 1) 

##veg str - all years
#getting coordinates
d_coord <- allscales_data_ALL %>%
  distinct(plotID, .keep_all = TRUE) %>%
  select(plotID, decimalLongitude, decimalLatitude)

NEONvegstrX <- dplyr::left_join(vst_data_ALL, d_coord, by = c("plotID" = "plotID"))

#assign growth forms from IAN's taxonomy to the NEON growtForm, based on taxon ID
#and corrects two categories
NEONvegstrX$growthForm <- ifelse(NEONvegstrX$growthForm == "", NEONvegstrX$GrowthForm, NEONvegstrX$growthForm)
#Substituting the new growth forms by the ones NEON have
NEONvegstrX$growthForm <- ifelse(NEONvegstrX$growthForm == "Tree", "single bole tree", NEONvegstrX$growthForm)
NEONvegstrX$growthForm <- ifelse(NEONvegstrX$growthForm == "Shrub", "small shrub", NEONvegstrX$growthForm)

#Renames columns
names(NEONvegstrX)[names(NEONvegstrX) == "growthForm"] <- "growthForm.NEON"
names(NEONvegstrX)[names(NEONvegstrX) == "GrowthForm"] <- "GrowthForm.IAN"

#selecting the desired columns
NEONvegstrX <- NEONvegstrX %>%
  unite(siteYear, siteID, year, sep = "_", remove = FALSE) %>%
  left_join(plotType, by = "plotID") %>%
  select(siteID, plotID, plotType, decimalLongitude, decimalLatitude, year, 
         taxonID2, 
         scientificName, #Original species names from NEON
         AccSpeciesName, bestname, Accepted.Symbol, growthForm.NEON, GrowthForm.IAN, Native.Status, 
         indCount, totalCrownArea, totalBasalArea, plotAreaSampled, percentCoverCanopy, 
         percentCoverBasal, stemDensity, siteYear) 

##SUMMARY: all data (all years) is in:
#NEONdiv_allscalesX
#NEONdiv_1m2X
#NEONvegstrX

#####FIRST DATA SET: FLATTEN DATA WITH THE BEST MATCH YEARS#####
#create site years column in each of the three main dataframes (1m2 div/herb data, all scales div/herb data, vst data)
filterSites$vstSiteYear <- paste(filterSites$siteID, filterSites$vstYear, sep = "_")
filterSites$div1m2SiteYear <- paste(filterSites$siteID, filterSites$div1m2, sep = "_")
filterSites$divAllScalesSiteYear <- paste(filterSites$siteID, filterSites$divAllScales, sep = "_")


#selecting only the desired year of data collection per site depending on the data
NEONdiv_1m2_best <- NEONdiv_1m2X %>%
  filter(siteYear %in% filterSites$div1m2SiteYear)#selecting only the common plotIDs with the diversity data

NEONvegstr_best <- NEONvegstrX %>%
  filter(siteYear %in% filterSites$vstSiteYear,
         plotID %in% NEONdiv_1m2_best$plotID) # the problem must here

##Merging datasets

#arranging veg structure data to match diversity data
NEON_vegstr_merg <- NEONvegstr_best %>%
  #removes the growth forms and NAs (I don't know why)
  filter(!(growthForm.NEON == "cactus" | growthForm.NEON == "fern" | growthForm.NEON == "ocotillo")) %>%
  mutate(percentCover = if_else(!is.na(percentCoverCanopy), percentCoverCanopy, percentCoverBasal),
         metric = if_else(!is.na(percentCoverCanopy), "percentCoverCanopy", "percentCoverBasal"),
         dataset = "NEON",
         subplotID = NA,
         #creates a code to further select spp different datasets
         code = 1) %>%
  dplyr::rename(GrowthForm = GrowthForm.IAN,
         layer = growthForm.NEON) %>%
  #unite(layerAreaSampled, layer, plotAreaSampled, sep = "_") %>%
  filter(!is.na(percentCover)) %>%
  select(dataset, siteID, plotID, plotType, subplotID, decimalLongitude, decimalLatitude, year, 
         taxonID2, 
         scientificName, #Original species names from NEON
         AccSpeciesName, Accepted.Symbol, 
         bestname, Native.Status, GrowthForm, percentCover, metric, layer, plotAreaSampled, code)

#arranging diversity data to match vegetation structure data
NEON_div_merg <- NEONdiv_1m2_best %>%
  mutate(dataset = "NEON",
         metric = "percentCover",
         layer = "herbaceous",
         #creates a code to further select spp different datasets
         code = 0) %>% 
  filter(!is.na(percentCover)) %>%
  #left_join(NEON_temp2, by = "plotID") %>%
  select(dataset, siteID, plotID, plotType, subplotID, decimalLongitude, decimalLatitude, year, 
         taxonID2,
         scientificName, #Original species names from NEON
         AccSpeciesName, Accepted.Symbol, 
         bestname, Native.Status, GrowthForm, percentCover, metric, layer, plotAreaSampled, code)

#creates a column to identify the years in which diversity data was collected
NEON_div_merg2 <- NEON_div_merg %>%
  mutate(year_data = "year_div")

#creates a column to identify the years in which veg structure data was collected
NEON_vegstr_merg2 <- NEON_vegstr_merg %>%
  mutate(year_data = "year_vegstr")

#bind both data
NEON_temp <- bind_rows(NEON_div_merg2, NEON_vegstr_merg2)

#selects entries based on the following rationale:
#1.If a species is found in both the veg structure data and the plant presence and percent cover data within a particular plot, 
#use the vegetation structure data.  Otherwise I think we run the risk of double counting cover. 
#2.If a species is found in only the veg structure data and not the plant presence and percent cover data within a particular plot, 
#use the veg structure data.
#3.If a species is found in the plant presence and percent cover data and not the veg structure data within a particular plot, 
#use the plant presence and percent cover data. 
NEON_temp <- NEON_temp %>% 
  group_by(dataset, siteID, 
           decimalLongitude, decimalLatitude, 
           taxonID2,
           Accepted.Symbol, 
           bestname, Native.Status, GrowthForm, AccSpeciesName) %>% 
  filter(if (sum(code)>=1) layer != "herbaceous" else layer == "herbaceous")

NEON_temp$year <- as.numeric(NEON_temp$year)

#flatten the diversity and vegetation structure data but keep information of area sampled 
NEONdata_flatted <- NEON_temp %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = year_data, 
              values_from = year,
              values_fill = list(year = 0)) %>%
  dplyr::select(-row) %>%
  dplyr::mutate(metric1 = if_else(metric == "percentCoverBasal", 1, 0),
         metric2 = if_else(metric == "percentCoverCanopy", 1, 0),
         row = row_number()) %>%
  tidyr::pivot_wider(names_from = layer, 
              values_from = plotAreaSampled,
              values_fill = list(plotAreaSampled = 0)) %>%
  dplyr::select(-row, -metric) %>% 
  dplyr::rename(small_tree = `small tree`,
         single_bole_tree = `single bole tree`,
         single_shrub = `single shrub`,
         small_shrub = `small shrub`, 
         multi_bole_tree = `multi-bole tree`) %>%
  #creates a dummy variable for vegetation structure to count in how many plots that data was collected from. Although for the veg str they differ in size
  dplyr::mutate(total.vegstr = ifelse(year_vegstr == 0, 0, 1)) %>%
  #mutate(subplotID = if_else(is.na(subplotID), 0, 1))
  dplyr::group_by(dataset, siteID, plotID, decimalLongitude, decimalLatitude, 
                  taxonID2, scientificName, plotType,
                  Accepted.Symbol, 
           bestname, Native.Status, GrowthForm, AccSpeciesName) %>%
  dplyr::summarize(#if basal area is used to estimate this particular species cover
    basalarea = if_else(sum(metric1)>0, 1, 0),
    canopycover = if_else(sum(metric2)>0, 1, 0),
    herbaceoustemp_area = sum(unique(!is.na(subplotID))),
    herbaceous_area_temp = ifelse(herbaceoustemp_area == 0, NA, length(unique(subplotID))),
    small_tree_area = sum(small_tree),
    sapling_area = sum(sapling),
    single_bole_tree_area = sum(single_bole_tree),
    liana_area = sum(liana),
    single_shrub_area = sum(single_shrub),
    small_shrub_area = sum(small_shrub),
    multi_bole_tree_area = sum(multi_bole_tree),
    palm_area = sum(palm),
    yucca_area = sum(yucca), 
    year_div = max(year_div),
    year_vegstr = max(year_vegstr),
    herbaceous_area = ifelse(year_div>0 & year_vegstr>0, herbaceous_area_temp-1, herbaceous_area_temp),
    herbaceous_total_area = ifelse(year_div == 2015 |
                                     year_div == 2016|
                                     year_div == 2017|
                                     year_div == 2018, 8, ifelse(year_div == 0, 0, 6)),
    #totalcover_mean = ifelse(herbaceous_total_area>0, sum(percentCover)/herbaceous_total_area,
    #                    mean(percentCover)),
    plotCover = ifelse(herbaceous_total_area>0, sum(percentCover)/herbaceous_total_area,
                             sum(percentCover))) %>%
  #I checked the number of subplots per plot for the herbaceous layer
  dplyr::mutate(herbaceous_area = replace(herbaceous_area, is.na(herbaceous_area), 0),
         small_tree_area = replace(small_tree_area, small_tree_area == 0, NA),
         sapling_area = replace(sapling_area, sapling_area == 0, NA),
         single_bole_tree_area = replace(single_bole_tree_area, single_bole_tree_area == 0, NA),
         liana_area = replace(liana_area, liana_area == 0, NA),
         single_shrub_area = replace(single_shrub_area, single_shrub_area == 0, NA),
         small_shrub_area = replace(small_shrub_area, small_shrub_area == 0, NA),
         multi_bole_tree_area = replace(multi_bole_tree_area, multi_bole_tree_area == 0, NA),
         palm_area = replace(palm_area, palm_area == 0, NA),
         yucca_area = replace(yucca_area, yucca_area == 0, NA),
         year_div = replace(year_div, year_div == 0, NA),
         year_vegstr = replace(year_vegstr, year_vegstr == 0, NA),
         Accepted.Symbol = ifelse(is.na(Accepted.Symbol), taxonID2, Accepted.Symbol),
         PlotArea.m2 = ifelse(plotType == "distributed", 400, 800),
         # gets the maximum area sampled for that particular observation (because to get the final cover value, 
         # the %/m² was summed across layers)
         SampledAreaVST = pmax(small_tree_area, sapling_area, single_bole_tree_area, liana_area,
                                     single_shrub_area, small_shrub_area, multi_bole_tree_area, na.rm = TRUE),
         Original.SampledArea = ifelse(herbaceous_total_area == 0, SampledAreaVST, herbaceous_total_area),
         # Y = layers were sampled for this obs; "N" = only herbaceous cover was sampled
         RecordedStrata = ifelse(herbaceous_total_area == 0, "Y", "N"),
         year = ifelse(is.na(year_vegstr), year_div, year_vegstr)) %>%
  ungroup() %>%
  dplyr::select(-herbaceoustemp_area, -herbaceous_area_temp, -SampledAreaVST) %>%
  #dplyr::rename(Accepted.Symbol = Accepted.Symbol2) %>%
  #dplyr::rename(Accepted.Symbol = taxonID) %>%
  ###IMPORTANT# dplyr::select(dataset, siteID, plotID, decimalLongitude, decimalLatitude,
  #        year,
  #        Accepted.Symbol, Native.Status, GrowthForm, AccSpeciesName, bestname,
  #        scientificName, #Original species names from NEON
  #        taxonID2,
  #        plotCover,
  #        SampledArea,
  #        vst_status,
  #        Strata) %>%
  #dplyr::select(-taxonID2) %>%
  ungroup() %>%
  left_join(vstStatus, by = "siteID")

glimpse(NEONdata_flatted)

##NEONdata_flatted missing Native.status
taxneoncode <- tax2 %>%
  filter(!is.na(neon_code)) %>%
  select(neon_code, inv_L48, inv_AK, inv_HI) %>%
  distinct()

uniquespp <- data2 %>%
  select(taxonID, scientificName,taxonRank) %>%
  distinct() %>%
  mutate(taxonID2 = str_remove(taxonID, "SPP$")) %>% #removes the SPP when by the end of a string
  select(-taxonID)
  
NEONdata_flattedmissing <- NEONdata_flatted %>%
  filter(is.na(Native.Status)) %>%
  group_by(taxonID2) %>%
  summarise(plotCover = round(sum(plotCover), 2),
            number = n()) %>%
  #excludes codes that return NA as native status from Ian's taxonomy table
  anti_join(taxneoncode, by = c("taxonID2" = "neon_code")) %>%
  left_join(uniquespp, by = c("taxonID2" = "taxonID2"))
##from those, the ones id to spp:
## TRDA = do not worry, the entry is accurate
## LESE17, HOTR = have equivalents. LESE17 is in accepted.symbol, but it has
#a different neon_code, so it wasn't match properly. HOTR should be HOTRT
## BODI, TRIN3, PPRC, CAAQS, SEDED = those species are not at all in Ian's taxonomy.
## all the other codes are species with no id. They should be NA.

NEONdata_flatted <- NEONdata_flatted %>%
  #fixes the year column 
  select(-taxonID2)

#exporting final file
# write.csv(NEONdata_flatted,
#           '/home/shares/neon-inv/data_paper/data_by_dataset/NEONDataPaperALLCols20220408.csv',
#           row.names = FALSE)


###### END ######

######Attaching TRAIT data######

# taxIan <- tax %>%
#   select(Accepted.Symbol, 
#          #AccSpeciesID, AccSpeciesName, GrowthForm, inv_L48, inv_AK, inv_HI, Duration, bestname,
#          N.Rows, SLA, LDMC, Leaf.area, 
#          Leaf.N.mass, Leaf.P.mass, Plant.height, Seed.dry.mass, 
#          Stem.specific.density..SSD..or.wood.density..stem.dry.mass.per.stem.fresh.volume., C3.C4, Woodiness) %>%
#   distinct(Accepted.Symbol, .keep_all = T)
# 
# NEONdata_flatted2 <- NEONdata_flatted %>%
#   left_join(taxIan, by = c("Accepted.Symbol" = "Accepted.Symbol")) 

#exporting final file
#write.csv(NEONdata_flatted_ALL2, file.path('nceas_data_entiregroup/FINAL_CorrectSppMatch/NEONdata_flatted20210225traits.csv'))

#####SECOND DATA SET: ONLY DIVERSITY DATA 1m? WITH THE BEST MATCH YEARS#####
# 
# NEONdata_div1m2 <- NEON_div_merg %>%
#     dplyr::group_by(dataset, siteID, plotID, decimalLongitude, decimalLatitude, year, 
#                   taxonID2,
#                   Accepted.Symbol, 
#            bestname, Native.Status, GrowthForm, AccSpeciesName) %>%
#   dplyr::summarize(sumcover = sum(percentCover),
#             #here it counts the area per layer a species was sampled AND present
#             herbaceous_area = length(unique(subplotID))) %>%
#   dplyr::mutate(herbaceous_area = replace(herbaceous_area, herbaceous_area == 0, NA),
#          herbaceous_total_area = ifelse(year == 2019, 6, 8),
#          totalcover = sumcover/herbaceous_total_area,
#          Accepted.Symbol = ifelse(is.na(Accepted.Symbol), taxonID2, Accepted.Symbol)) %>%
#   ungroup() %>%
#   #dplyr::select(-Accepted.Symbol, -taxonID) %>%
#   #dplyr::rename(Accepted.Symbol = Accepted.Symbol2) %>%
#   dplyr::select(-sumcover, -taxonID2) %>%
#   left_join(vstStatus, by = "siteID")
# 
# 
# NEONdata_flattedmissing2 <- NEONdata_div1m2 %>%
#   filter(is.na(Native.Status)) %>%
#   group_by(taxonID2) %>%
#   summarise(totalcover_sum = round(sum(totalcover), 2),
#             number = n()) %>%
#   #excludes codes that return NA as native status from Ian's taxonomy table
#   anti_join(taxneoncode, by = c("taxonID2" = "neon_code")) %>%
#   left_join(uniquespp, by = c("taxonID2" = "taxonID2"))
##from those, the ones id to spp:
## TRDA = do not worry, the entry is accurate
## LESE17, HOTR = have equivalents. LESE17 is in accepted.symbol, but it has
#a different neon_code, so it wasn't match properly. HOTR should be HOTRT; SEDED should be SEDE2
## BODI, TRIN3, PPRC, CAAQS, SEDED = those species are not at all in Ian's taxonomy.
## all the other codes are species with no id. They should be NA.

#exporting final file
#write.csv(NEONdata_div1m2, file.path('nceas_data_entiregroup/FINAL_CorrectSppMatch/NEONdata_div1m20210225.csv'))

##########Number of plots of diversity and vegetation structure per site in the flatted version##########

PlotsSites_flatted <- NEONdata_flatted %>%
  #filter(siteID == "DELA" | siteID == "JERC" | siteID == "SRER" | siteID == "UNDE" |
  #         siteID == "WREF") %>%
  select(siteID, plotID) %>%
  group_by(siteID) %>%
  distinct(plotID, .keep_all = TRUE) %>%
  summarize(N_plotIDDiv=n())

PlotsSites_vst <- NEON_vegstr_merg2 %>%
  #filter(siteID == "DELA" | siteID == "JERC" | siteID == "SRER" | siteID == "UNDE" |
  #         siteID == "WREF") %>%
  select(siteID, plotID) %>%
  group_by(siteID) %>%
  distinct(plotID, .keep_all = TRUE) %>%
  summarize(N_plotIDVst=n())

PlotsSites <- left_join(PlotsSites_flatted, PlotsSites_vst, by = "siteID")

#exporting final file
#write.csv(PlotsSites, file.path('PlotsSites_20200901.csv'))

##########NI species in the flatted dataset##########

NEONdata_flattedAbund <- NEONdata_flatted %>%
  filter(!is.na(totalcover_sum),
         !is.na(Native.Status))

NEON_NIspp <- NEONdata_flattedAbund %>%
  # Estimate total plot cover and relative cover
  group_by(siteID, plotID) %>% 
  mutate(TotPlotCov=sum(totalcover_sum),
         RelCov = totalcover_sum/TotPlotCov) %>%
  group_by(siteID, 
           #plotID, 
           Accepted.Symbol, 
           Native.Status) %>%
  summarise(RelCov_WithVegStr = sum(RelCov),
            TotPlotCov_WithVegStr=mean(TotPlotCov),
            #code = "WithVegStr"
            ) %>%
  filter(Native.Status == "NI")

#exporting final file
#write.csv(NEON_NIspp, file.path('NEON_NIspp_20200901.csv'))

##### 02-24-2021 checking missing species codes from taxonomy table  ####

## *diversity data* before taxonomy

uniqueSppDiv <- data2 %>%
  select(taxonID, scientificName) %>%
  distinct()

## *veg structure data* before taxonomy

uniqueSppVeg <- countTax %>%
  select(taxonID, scientificName) %>%
  distinct()

##merging both

uniqueNEONspp <- rbind(uniqueSppDiv, uniqueSppVeg)

uniqueNEONspp <- uniqueNEONspp %>%
  distinct()

#selects only neon_code from Ian's taxonomy
taxneoncode <- tax %>%
  select(neon_code) %>%
  filter(!is.na(neon_code)) %>%
  mutate(neon_code2 = neon_code) %>%
  distinct()

#merge unique occurrences of NEON species and Ian's neon_code, to find the missing codes
uniqueNEONsppIan <- left_join(uniqueNEONspp, taxneoncode, by = c("taxonID" = "neon_code2"))

#unmatched codes
uniqueNEONsppIanUnmatch <- uniqueNEONsppIan %>%
  filter(is.na(neon_code))
dim(uniqueNEONsppIanUnmatch)
# 437 unmatches

#selects only Accepted.Symbol from Ian's taxonomy
taxAccSymbol <- tax %>%
  select(Accepted.Symbol) %>%
  filter(!is.na(Accepted.Symbol)) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  distinct()

#merges the unmatched to accepted symbol from Ian's taxonomy
uniqueNEONsppIanUnmatchAccSymbol <- left_join(uniqueNEONsppIanUnmatch, taxAccSymbol, by = c("taxonID" = "Accepted.Symbol2"))

#finds the potential matches
uniqueNEONsppIanNEWMATCH <- uniqueNEONsppIanUnmatchAccSymbol %>%
  filter(!is.na(Accepted.Symbol))
dim(uniqueNEONsppIanNEWMATCH)
#potential new matches -> 242

#finds the species codes with NO matches
uniqueNEONsppIanNOMATCH <- uniqueNEONsppIanUnmatchAccSymbol %>%
  filter(is.na(Accepted.Symbol))
dim(uniqueNEONsppIanNOMATCH)
#codes with NO MATCHES with Ian Taxonomy -> 195

###IMPORTANT: THIS IS FOR ALL YEARS, NOT ONLY THE BEST MATCHES

## Finding the same numbers to best match years

## *diversity data* before taxonomy

uniqueSppDiv <- data2 %>%
  mutate(year = year(endDate)) %>%
  unite(siteYear, siteID, year, sep = "_", remove = FALSE) %>%
  filter(siteYear %in% filterSites$div1m2SiteYear) %>%
  select(taxonID, scientificName) %>%
  distinct()

## *veg structure data* before taxonomy

uniqueSppVeg <- countTax %>%
  unite(siteYear, siteID, year, sep = "_", remove = FALSE) %>%
  filter(siteYear %in% filterSites$vstSiteYear) %>%
  select(taxonID, scientificName) %>%
  distinct()

##merging both

uniqueNEONspp <- rbind(uniqueSppDiv, uniqueSppVeg)

uniqueNEONspp <- uniqueNEONspp %>%
  distinct()

#selects only neon_code from Ian's taxonomy
taxneoncode <- tax %>%
  select(neon_code) %>%
  filter(!is.na(neon_code)) %>%
  mutate(neon_code2 = neon_code) %>%
  distinct()

#merge unique occurrences of NEON species and Ian's neon_code, to find the missing codes
uniqueNEONsppIan <- left_join(uniqueNEONspp, taxneoncode, by = c("taxonID" = "neon_code2"))

#unmatched codes
uniqueNEONsppIanUnmatch <- uniqueNEONsppIan %>%
  filter(is.na(neon_code))
dim(uniqueNEONsppIanUnmatch)
# 213 unmatches

#selects only Accepted.Symbol from Ian's taxonomy
taxAccSymbol <- tax %>%
  select(Accepted.Symbol, bestname) %>%
  filter(!is.na(Accepted.Symbol)) %>%
  mutate(Accepted.Symbol2 = Accepted.Symbol) %>%
  distinct()

#merges the unmatched to accepted symbol from Ian's taxonomy
uniqueNEONsppIanUnmatchAccSymbol <- left_join(uniqueNEONsppIanUnmatch, taxAccSymbol, by = c("taxonID" = "Accepted.Symbol2"))

#finds the potential matches
uniqueNEONsppIanNEWMATCH <- uniqueNEONsppIanUnmatchAccSymbol %>%
  filter(!is.na(Accepted.Symbol))
dim(uniqueNEONsppIanNEWMATCH)
#potential new matches -> 151

#finds the species codes with NO matches
uniqueNEONsppIanNOMATCH <- uniqueNEONsppIanUnmatchAccSymbol %>%
  filter(is.na(Accepted.Symbol))
dim(uniqueNEONsppIanNOMATCH)
#codes with NO MATCHES with Ian Taxonomy -> 62

### Check missing species code matches after the final dataset
