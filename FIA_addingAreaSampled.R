### Code created by Laís on 07/29/2021 ###
## Goal1: add the total area sampled ##
## Goal2: remove undesired columns (DESIGNCD and traits columns) ##

FIAdata <- read.csv('/home/shares/neon-inv/data_paper/data_by_dataset/archived/FIA Veg data Latest Traits 8-4-20.csv', 
                    header = T, stringsAsFactors = F)
FIAtotalArea <- read.csv('/home/shares/neon-inv/data_paper/code_by_dataset/extra_csv_FIA/FIA_PlotArea_8-4-20.csv',
                         header  =T, stringsAsFactors = F)

FIAdatafinal <- FIAdata %>%
  leftjoin(FIAtotalArea, by = "PLT_CN") %>%
  select(-DESIGNCD, -X1, -STATECD, -DESIGNCD, -c(19:29))

write.csv(FIAdatafinal, "/home/shares/neon-inv/data_paper/data_by_dataset/FIA_DataPaper_07292021.csv",
          row.names = FALSE)