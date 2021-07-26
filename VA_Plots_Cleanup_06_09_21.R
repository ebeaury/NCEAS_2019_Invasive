#Organize VA NHP 03-22-21 data for merging with NCEAS data
#install.packages ("stringr")

library ("dplyr")
library ("stringr")
setwd ("~/Documents/Darwin Relatedness Project/VA NHP data 3-22-21")

SppData <- read.csv ("VAPLOTS_03222021_CompositionalData.csv")
colnames(SppData)[colnames(SppData) == "Acronym"] <- "SpCode"
SppDetails <- read.csv ("VANHP_MasterSppList_03_22_21_ForMatching.csv")
colnames(SppDetails)[colnames(SppDetails) == "ACRONYM"] <- "SpCode"
PlotData <-read.csv ("VANHP_PlotMetadata_03_22_21.csv")

#head (SppData)
#head (SppDetails)
#head (PlotData)

unique (SppData$Outside.Plot)
SppData [is.na (SppData)] <- 0
#Remove species that are outside plot - SppData$Outside.Plot = 1
SppData <- subset (SppData, Outside.Plot != 1)
SppData <- subset (SppData, select = -c(Outside.Plot))
#Remove ID.flag = 5 (nonvascular). The other categories are all "probable" species ID, so leave and delete column
SppData <- subset (SppData, ID.Flag != 5)
SppData <- subset (SppData, select = -c(ID.Flag))

#Merge species spp details
SppPlus <- left_join(SppData, SppDetails)
#Merge species and plot details
VADataFull <- left_join (SppPlus, PlotData, by = "Plot")
VADataSmall <- VADataFull [, c(1, 2, 3, 4, 5, 6, 7, 20, 25, 39, 41, 42)]  
VADataSmall [is.na (VADataSmall)] <- 0
#Remove plots that have Plot.Size = 0
VADataSmall <- subset (VADataSmall, Plot.Size != 0)

#Convert Exotic/Native
unique (VADataSmall$exotic)
VADataSmall <- VADataSmall %>%
  mutate (VAExCode = ifelse(exotic == 1, "I", "N"), .keep = "unused")

#Convert Date
VADataSmall$Date2 <- as.Date(VADataSmall$Date, format = "%m/%d/%y")
VADataSmall$Date3 <- format(VADataSmall$Date2, format="%Y")
VADataSmall <- subset (VADataSmall, select = -c(Date, Date2))
colnames (VADataSmall)[colnames(VADataSmall) == "Date3"] <- "Year"

#Convert Lat Long
#Latitude is DDMMSS.ssss, Longitude is DDMMSS.ssss but longitude is stored without negative sign.
VADataSmall <- transform (VADataSmall, LatDeg = substr(as.character(Latitude), 1, 2), LatMin = substr(as.character(Latitude), 3, 4), LatSec = substr(as.character(Latitude), 5, 10))
VADataSmall <- transform (VADataSmall, LonDeg = substr(as.character(Longitude), 1, 2), LonMin = substr(as.character(Longitude), 3, 4), LonSec = substr(as.character(Longitude), 5, 10))
#Formula = DecimalDegrees (DD) = deg + (min/60) + sec/3600). Longitude (W) is negative
VADataSmall$NewLat <- as.numeric(VADataSmall$LatDeg) + (as.numeric(VADataSmall$LatMin)/60) + (as.numeric(VADataSmall$LatSec)/3600)
VADataSmall$NewLon <- -(as.numeric(VADataSmall$LonDeg) + (as.numeric(VADataSmall$LonMin)/60) + (as.numeric(VADataSmall$LonSec)/3600))

#Cover Classes
#1 - 0.05%
#2 - 0.55%
#3 - 1.5%
#4 - 3.5%
#5 - 7.5%
#6 - 17.5%
#7 - 37.5%
#8 - 62.5%
#9 - 87.5%

NewCover <- c(0.05, 0.55, 1.5, 3.5, 7.5, 17.5, 37.5, 62.5, 87.5)
VADataSmall$NewCov <- NewCover [VADataSmall$Cover]

#Clean up
#VADataSmall <- subset (VADataSmall, select = -c(Date, Date2))
VADataFinal <- subset (VADataSmall, select = -c(Latitude, Longitude, LatDeg, LatMin, LatSec, LonDeg, LonMin, LonSec, Cover, speciesID))
colnames (VADataFinal)[colnames(VADataFinal) == "NewLat"] <- "Lat"
colnames (VADataFinal)[colnames(VADataFinal) == "NewLon"] <- "Long"
colnames (VADataFinal)[colnames(VADataFinal) == "NewCov"] <- "PctCov"
colnames (VADataFinal)[colnames(VADataFinal) == "SPECIES"] <- "bestname"

VADataFinal$Dataset <- "VA_NHP"
VADataFinal$VASpeciesName <- VADataFinal$bestname
VADataFinal <- VADataFinal[, c(12, 1, 9, 10, 6, 8, 2, 3, 4, 5, 11, 10, 7)]

#Look for duplicates. None found by plot+SpCode.
duplicates1 <- VADataFinal %>% 
  group_by(Plot, SpCode) %>% 
  summarise(n_obs = n()) %>% 
  filter(n_obs > 1) 

#Clean up some specific mismatches


#Run through PLANTS Database
#Using KPEACH_ExoticStatus file that incorporates changes to taxonomy_temp_10
Taxo <- read.csv ("KPEACH_ExoticStatus_authority_031621.csv")
#Taxo <- read.csv ("taxonomy_temp10_revised.csv")

#VADataFinal$bestname  <- str_replace_all(VADataFinal$bestname , " complex", "")
#If a row has a bestname that is just the genus followed by "sp." it won't match with the exotic status authority document.  Ex. "Viola sp." has to be re-coded to "Viola" for it to line up with the SpCode list and get a match. So I am removing "sp." and "SP." anywhere they appear with a space before and after them. The space requirement prevents gsub from recoding "ssp." to "s", which we don't want.
VADataFinal$bestname <- gsub("\\bsp\\.\\b", " ", VADataFinal$bestname, ignore.case = FALSE)

VADataFinal$bestname <- gsub("\\bSP\\.\\b", " ", VADataFinal$bestname, ignore.case = FALSE)
#Stripping any extra white space from the names
VADataFinal$bestname  <- trimws(VADataFinal$bestname, which = c("right"))

#I checked all species whose PctCov in an obseration was >3.5%.

VADataFinal$bestname <- gsub("Phragmites australis complex", "Phragmites australis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Pontederia cordata var. cordata", "Pontederia cordata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coleataenia rigidula ssp. rigidula", "Coleataenia rigidula", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Kellochloa verrucosa", "Panicum verrucosum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hamamelis virginiana var. virginiana", "Hamamelis virginiana", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Magnolia virginiana var. virginiana", "Magnolia virginiana", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Juncus effusus complex", "Juncus effusus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Betula lenta var. lenta", "Betula lenta", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Cartrema americanum", "Cartrema americana", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Calamagrostis breviligulata ssp. breviligulata", "Ammophila breviligulata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex austrolucorum", "Carex lucorum var. austrolucorum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Betula lenta var. lenta", "Betula lenta", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Onoclea sensibilis var. sensibilis", "Onoclea sensibilis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex bullata var. greenei", "Carex bullata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Rhododendron viscosum var. viscosum", "Rhododendron viscosum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Opuntia humifusa complex", "Opuntia humifusa", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex atlantica / howei", "Carex howei", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Solidago rugosa var. aspera", "Solidago rugosa ssp. aspera", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Onoclea sensibilis var. sensibilis", "Onoclea sensibilis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Clitoria mariana var. mariana", "Clitoria mariana", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Panicum philadelphicum var. philadelphicum", "Panicum philadelphicum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Panicum philadelphicum ssp. philadelphicum", "Panicum philadelphicum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coleataenia rigidula ssp. condensa", "Coleataenia rigidula", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Sagittaria latifolia var. latifolia", "Sagittaria latifolia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Orbexilum psoralioides", "Orbexilum pedunculatum var. psoralioides", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex intumescens var. intumescens", "Carex intumescens", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Stachys hyssopifolia var. hyssopifolia", "Stachys hyssopifolia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hydrocotyle verticillata / prolifera", "Hydrocotyle prolifera", VADataFinal$bestname)
#VADataFinal$bestname <- gsub("Persicaria", "Polygonum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Persicaria arifolia", "Polygonum arifolium", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Smallanthus uvedalia", "Smallanthus uvedalius", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Persicaria hydropiperoides var. hydropiperoides", "Polygonum hydropiperoides", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Endotropis alnifolia", "Rhamnus alnifolia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Cyperus odoratus var. odoratus", "Cyperus odoratus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola edulis", "Viola esculenta", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hydrocotyle sp. (peltate)", "Hydrocotyle", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Toxicodendron radicans var. radicans", "Toxicodendron radicans", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hexasepalum teres", "Diodella teres", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Triplasis purpurea var. purpurea", "Triplasis purpurea", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Pityopsis nervosa", "Pityopsis graminifolia var. latifolia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Glyceria striata var. striata", "Glyceria striata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hydrangea barbara", "Decumaria barbara", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Triplasis purpurea var. purpurea", "Triplasis purpurea", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Symphyotrichum sp. nov.", "Symphyotrichum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Arthraxon hispidus var. hispidus", "Arthraxon hispidus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coleataenia longifolia ssp. longifolia", "Coleataenia longifolia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Cerastium velutinum var. velutinum", "Cerastium velutinum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Convallaria pseudomajalis", "Convallaria majuscula", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola pubescens/eriocarpa complex", "Viola pubescens", VADataFinal$bestname)
VADataFinal$bestname <- gsub("graminoid", "Poaceae", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carya glabra / ovalis", "Carya glabra", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola pubescens/eriocarpa complex", "Viola pubescens var. eriocarpon", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Packera paupercula var. paupercula", "Packera paupercula", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Ptelea trifoliata var. trifoliata", "Ptelea trifoliata ssp. trifoliata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Monarda punctata var. punctata", "Monarda punctata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Andropogon glomeratus complex", "Andropogon glomeratus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Solidago sempervirens complex", "Solidago sempervirens", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola lanceolata var. lanceolata", "Viola lanceolata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hedera helix var. helix", "Hedera helix", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola eriocarpa", "Viola eriocarpon", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Primula meadia", "Dodecatheon meadia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Silphium asteriscus var. trifoliatum", "Silphium trifoliatum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("grass", "Poaceae", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Spirodela / Landoltia", "Spirodela", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Anticlea glauca", "Zigadenus glaucus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Crataegus crus-galli var. crus-galli", "Crataegus crus-galli", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Ampelopsis glandulosa", "Ampelopsis brevipedunculata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Asclepias tuberosa var. tuberosa", "Asclepias tuberosa", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex copulata", "Carex laxiculmis var. copulata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex leptalea var. leptalea", "Carex leptalea", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (montanae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (laxiflorae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Dichanthelium commutatum var. commutatum", "Dichanthelium commutatum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (laxiflorae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Erigeron canadensis / pusillus complex", "Erigeron canadensis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Galium tinctorium var. tinctorium", "Galium tinctorium", VADataFinal$bestname)
#VADataFinal$bestname <- gsub("Persicaria", "Polygonum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Trillidium undulatum", "Trillium undulatum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola pedata var. pedata", "Viola pedata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Arabidopsis lyrata ssp. lyrata", "Arabidopsis lyrata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Asclepias incarnata var. incarnata", "Asclepias incarnata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Campanula aparinoides var. aparinoides", "Campanula aparinoides", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex albicans complex", "Carex albicans var. albicans", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (laxiflorae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (ovales group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (montanae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Chamaecrista nictitans var. nictitans", "Chamaecrista nictitans", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coleataenia anceps ssp. anceps", "Panicum anceps", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Comandra umbellata var. umbellata", "Comandra umbellata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Gonolobus suberosus var. suberosus", "Gonolobus suberosus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hypericum (Hypericum subg.)", "Hypericum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Ionactis linariifolia", "Aster linariifolius", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Lespedeza hirta var. hirta", "Lespedeza hirta", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Ligustrum obtusifolium var. obtusifolium", "Ligustrum obtusifolium", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Luzula multiflora var. multiflora", "Luzula multiflora", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Nabalus altissimus", "Prenanthes altissima", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Nabalus altissimus", "Prenanthes alba ssp. pallida", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Nabalus album", "Prenanthes alba", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Nabalus trifoliolatus", "Prenanthes trifoliolata", VADataFinal$bestname)
#VADataFinal$bestname <- gsub("Nabalus", "Prenanthes", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Oenothera biennis complex", "Oenothera biennis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Oenothera fruticosa complex", "Oenothera fruticosa", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Phoradendron leucarpum ssp. leucarpum", "Phoradendron leucarpum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Potentilla canadensis / simplex", "Potentilla canadensis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Symphyotrichum racemosum var. racemosum", "Symphyotrichum racemosum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Digitaria filiformis var. filiformis", "Digitaria filiformis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Salix xfragilis", "Salix fragilis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (laxiflorae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (montanae group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Pilosella officinarum", "Hieracium pilosella", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Boechera serotina", "Arabis serotina", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Myosotis laxa ssp. laxa", "Myosotis laxa", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Veronica serpyllifolia var. serpyllifolia", "Veronica serpyllifolia", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. 2 sp. nov.", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Morella pumila", "Morella cerifera", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Boechera", "Arabis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Dichanthelium sp. (dichotomum complex) ", "Dichanthelium", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Aronia prunifolia", "Aronia arbutifolia var. atropurpurea", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola sp. (caulescent)", "Viola", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Echinochloa crusgalli var. crusgalli", "Echinochloa crus-galli", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Lithospermum virginianum", "Onosmodium virginianum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hylodesmum pauciflorum", "Desmodium pauciflorum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Dasistoma macrophyllum", "Dasistoma macrophylla", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Dichanthelium sp. (acuminatum complex) ", "Dichanthelium", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Endotropis lanceolata", "Rhamnus lanceolata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Rosa carolina ssp. carolina", "Rosa carolina", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Symphyotrichum concinnum", "Symphyotrichum laeve var. concinnum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Arabadopsis lyrata", "Arabis lyrata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Asclepias tuberosa var. rolfsii", "Asclepias rolfsii", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Amaranthus hybridus ssp. hybridus", "Amaranthus hybridus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Boechera burkii", "Arabis laevigata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Cyperus subsquarrosus", "Lipocarpha micrantha", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hypericum (Hypericum subg.)", "Hypericum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Pycnanthemum torreyi", "Pycnanthemum verticillatum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coleataenia anceps ssp. rhizomata", "Coleataenia anceps var. rhizomata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Dichanthelium portoricense ssp. patulum", "Dichanthelium portoricense", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Andropogon cretaceus", "Andropogon glomeratus var. glaucopsis", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coleataenia rigidula complex", "Coleataenia rigidula", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Cyperus neotropicalis", "Lipocarpha maculata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Symphyotrichum concolor var. concolor", "Symphyotrichum concolor", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Solidago rugosa var. rugosa", "Solidago rugosa", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carex sp. (ovales group)", "Carex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Smilax ecirrata", "Smilax ecirrhata var. biltmoreana", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Myriopteris tomentosa", "Cheilanthes tomentosa", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Nanopanax  trifolius", "Panax trifolius", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Chenopodiastrum  simplex", "Chenopodium simplex", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Tripsacum dactyloides var. dactyloides", "Tripsacum dactyloides", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Crataegus margarettae", "Crataegus margarettiae", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Noccaea perfoliata", "Microthlaspi perfoliatum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hymenachne hemitomon", "Panicum hemitomon", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Viola sp. (acaulescent)", "Viola", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Coryphopteris simulata", "Dryopteris simulata", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Cleistesiopsis bifaria", "Cleistes bifaria", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Lespedeza hirta xrepens", "Lespedeza hirta", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Triphora trianthophora var. trianthophora", "Triphora trianthophora", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Paronychia virginica var. virginica", "Paronychia virginica", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Zephyranthes atamasco", "Zephyranthes atamasca", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hypericum (Triadenum subg.)", "Hypericum", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Silphium asteriscus var. latifolium", "Silphium confertifolium", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Oreojuncus trifidus", "Juncus trifidus var. monanthos", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Carduus acanthoides ssp. acanthoides", "Carduus acanthoides", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Styrax americanus var. americanus", "Styrax americanus", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Juncus scirpoides var. scirpoides", "Juncus scirpoides", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Rhynchospora cephalantha var. cephalantha", "Rhynchospora cephalantha", VADataFinal$bestname)
VADataFinal$bestname <- gsub("Hypericum interior", "Hypericum densiflorum", VADataFinal$bestname)


#Get rid of nonvascular plants (ferns, mosses, clubmosses)
VADataFinal <- VADataFinal[!(VADataFinal$bestname == "Claytosmunda claytoniana" |
                               VADataFinal$bestname == "Pteridium aquilinum ssp. pseudocaudatum" |
                               VADataFinal$bestname == "Bryodesma rupestre" |
                               VADataFinal$bestname == "Pteridium aquilinum ssp. latiusculum" |
                               VADataFinal$bestname == "Homalosorus pycnocarpos" |
                               VADataFinal$bestname == "Myriopteris lanosa" |
                               VADataFinal$bestname == "Lycopodioides apodum" |
                               VADataFinal$bestname == "fern" |
                               VADataFinal$bestname == "Pleopeltis michauxiana" |
                               VADataFinal$bestname == "Huperzia appressa" |
                               VADataFinal$bestname == "Myriopteris rufa" |
                               VADataFinal$bestname == "Dryopteris xclintoniana"), ]

#Update loaded KPEACH_ExoticStatus_authority_031621.csv file ("Taxo") to fix a few glitches in matching.
Persicaria <- c("", "Persicaria", "NI", "NA", "POLYG4", "USDA_PLANTLIST", "Annual, Perennial", "Forb, Shrub, Vine", "NI")

Taxo <- rbind (Taxo, Persicaria)


#Species to add to list


#Table of species with no match in KPEACH_ExoticStatus_authority table

no_match <- dplyr::anti_join(VADataFinal, Taxo, by="bestname")
NoMatchNames <- (unique (no_match$bestname))
no_match <- no_match[order(-no_match$PctCov), ]

#Table of species with no match in KPEACH_ExoticStatus_authority table

#44 rows with no match after filtering out <.10 cover. These are all hybrids or observations with low confidence in species or genus identification (because multiple genera or species are listed)
to_be_matched <- no_match %>% 
  filter(!str_detect(bestname,  ".\\+|\\+."))
to_be_matched <- to_be_matched %>% 
  filter(bestname != "orchid") %>%
  filter(bestname != "woody") %>%
  filter(bestname != "Poaceae") %>%
  filter(bestname != "forb") %>%
  filter(bestname != "Nabalus") %>%
  filter(bestname != "mint") %>%
  filter(bestname != "Asteracea") %>%
  filter(bestname != "Liliaceae") %>%
  filter(bestname != "0") %>%
  filter(bestname != "Fabaceae") %>%
  filter(!str_detect(bestname, "\\d")) 
#following Kristen's treatment of Nabalus: only removing Nabalus here because I cannot really figure out what it should be. The internet says that it used to be considered part of the genus Prenanthes but there are also observations of Prenanthes at this site so if I renamed it Prenanthes it would just look like duplicated in the data table 
no_match <- unique(no_match)

#Adding made up SpCodes for the hybrid rows

#Adding new 'fake' species codes for the rows with no species codes (because they are hybrids or observer was uncertain of ID)
#Making a column of row numbers
no_match <- cbind(rownames(no_match), data.frame(no_match, row.names=NULL))
#Adding a - so each new species code will just have a 'NOMATCH' then a dash followed by sequential numbers
numbers <- paste0('-', no_match$`rownames(no_match)`)
#Final step
no_match <- no_match %>% 
  mutate('SpCode' = paste0("NOMATCH_VANHP", numbers)) %>% 
  select(-'rownames(no_match)') %>% 
  rename("VANHP_Name" = "OLDSPECIES")
no_match <- no_match %>%
  select(-'COMMON.NAME', 'Long.1')

VADataFinal <- unique(VADataFinal)
VA_with_SpCode <- dplyr::inner_join(VADataFinal, Taxo, by="bestname")
VA_with_SpCode <- VA_with_SpCode %>%
  select(-'COMMON.NAME', -'Long.1')
VA_with_SpCode <- VA_with_SpCode %>% rename("VANHP_Name" = "OLDSPECIES")

VA_with_SpCode <- VA_with_SpCode %>%
  select(-SpCode.x) %>%
  rename("SpCode" = "SpCode.y")
#Merging the df that DOES has correct species codes with the no_match table I made with the newly invented species codes
VA_with_SpCode <- merge(VA_with_SpCode, no_match, all = TRUE)
#VA_with_SpCode <- VA_with_SpCode %>% rename("VANHP_Name" = "OLDSPECIES")
VA_with_SpCode <- VA_with_SpCode %>%
  select(-'Long.1')

#Renaming USDA_Duration column to just 'Duration' 
VA_with_SpCode <- VA_with_SpCode %>% rename('Duration' = 'USDA_Duration') %>% select(-Synonym.Symbol)

#Looking again for duplicates
duplicates2 <- VA_with_SpCode %>% 
  group_by(Plot, SpCode) %>% 
  summarise(n_obs = n()) %>% 
  filter(n_obs > 1) 

#There are 86 duplicates, where >1 e.g. Carex in a plot appear as duplicates. We decided to sum cover and have one value per plot.

#Sum PctCov for taxa with same bestname
VA_SummedDuplicates <- VA_with_SpCode %>% 
  group_by(Dataset, Plot, Lat, Long, Year, Plot.Size, SpCode, bestname,
           VAExCode, USDA_Exotic_Status, ExoticStatus_Origin, NEW_ExoticStatus, NEON_GrowthForm,
           USDA_Growth_Form, Duration) %>%
  summarise (PctCov = sum(PctCov))
#Merge VANHP_Names for rows with same bestname and delete duplicates

VA_with_SpCode2 <- VA_with_SpCode %>% 
  group_by(Dataset, Plot, Lat, Long, Year, Plot.Size, SpCode, bestname,
           VAExCode,  USDA_Exotic_Status, ExoticStatus_Origin, NEW_ExoticStatus, NEON_GrowthForm,
           USDA_Growth_Form, Duration) %>%
  summarise (VANHP_Name = toString (VANHP_Name)) %>%
  ungroup()

#Merge to bring together new VANHP_Name and PctCov
VA_Output <- left_join(VA_SummedDuplicates, VA_with_SpCode2, by = c("Dataset", "Plot", "Lat", "Long", "Year", "Plot.Size", "SpCode", "bestname", 
                                                                    "VAExCode", "USDA_Exotic_Status", "ExoticStatus_Origin", 'NEW_ExoticStatus', 
                                                                     "NEON_GrowthForm",'USDA_Growth_Form', "Duration"))
VA_Output <- VA_Output %>%
  select ("Dataset", "Plot", "Lat", "Long", "Year", "Plot.Size", "SpCode", "bestname", "VANHP_Name",
          "PctCov", "VAExCode", "USDA_Exotic_Status", "ExoticStatus_Origin", 'NEW_ExoticStatus', "NEON_GrowthForm",
          'USDA_Growth_Form', "Duration")

#As csv
write.csv(VA_Output, "VANHP_ALL_COLS_06_15_21.csv", row.names = FALSE)


#Exporting reduced data table
VA_OutputReduced <- VA_Output %>%
  select(-'VAExCode', -'NEON_GrowthForm', -'USDA_Growth_Form', -'USDA_Exotic_Status')
#Just using this to see if any rows are dropped because there is no latitute and longitude. There really ought to be lat and long so just trying to catch that here
NoGPS <-dplyr::filter(VA_Output,is.na(Lat)) #There are none.
#As csv
write.csv(VA_OutputReduced,"VANHP_reduced_06_15_21.csv", row.names = FALSE)


                   