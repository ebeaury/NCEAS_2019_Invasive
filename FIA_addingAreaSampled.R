### Code created by Laís on 07/29/2021 ###
## Goal1: add the total area sampled ##
## Goal2: remove undesired columns (DESIGNCD and traits columns) ##

FIAdata<- read.csv('/home/shares/neon-inv/data_paper/data_by_dataset/archived/FIA Veg data Latest Traits 8-4-20.csv', 
                    header = T, stringsAsFactors = F)
FIAdata <- select(FIAdata, -X)

FIAtotalArea <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_FIA/FIA_PlotArea_8-4-20.csv',
                         header  =T, stringsAsFactors = F)
FIAtotalArea <- select(FIAtotalArea, -X)

FIAdatafinal <- FIAdata %>%
  left_join(FIAtotalArea) %>%
  select(-DESIGNCD, -STATECD, -DESIGNCD, -c(18:28))

##checking if all plots have associated SampledArea info##
table(FIAdatafinal$SampledArea)

FIAdatafinal %>%
  filter(is.na(SampledArea)) %>%
  distinct(PLT_CN, .keep_all = TRUE) %>%
  summarize(NAs=n())

#plot 85 for a few entries has no cover information associated. Although the year of data collection is 
# the same as the other rows for this plot, they have different PLT_CN


##exporting file##
write.csv(FIAdatafinal, "/home/shares/neon-inv/data_paper/data_by_dataset/FIA_DataPaper_07292021.csv",
          row.names = FALSE)
