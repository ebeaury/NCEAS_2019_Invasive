###  Abundance-Impacts plot summary
##  Generate plot-level diversity indices from species cover data

##  Helen Sofaer
##  25 Oct 2019
##  14 Nov 2019: dropped Census info; updated taxonomy table (v9)
##  2 Jan 2019: switched to codyn-based code for most metrics
##  6 march 2020: updated after asigning some of the NI sp to either N or I
##  25 June 2020: updated to spring 2020 data version

library(tidyverse)
library(codyn)

setwd("C:/Users/hsofaer/Documents/HelenProjects/AI_AIM")

###
#####   Import data     #####
###

# All species cover data from BLM AIM&LMF protocols, spring 2020 version:
AllSp <- read_rds("AIM_AllSpTax_24June2020.rds")
glimpse(AllSp)

##  Make format match desired for NCEAS AI: (drops trait data etc)
AllSp <- AllSp %>%
  rename(Plot = PLOTKEY, Long = NAD83.X, Lat = NAD83.Y, Year = VisitYear,
         Species = bestname, Exotic = inv_L48) %>%
  mutate(Pct_Cov = prop.cover * 100) %>%
  dplyr::select(Plot, Long, Lat, Year, Species, Exotic, Pct_Cov)
head(AllSp)

AllSp %>%
  filter(is.na(Pct_Cov)) %>%
  nrow()

###
#####   Community-level metrics from codyn package    ####
###
##  codyn functions don't work with tidyverse group_by (or with map on nested df)

##  unite plot info and exotic status to get single grouping variable for codyn functions
sum(grepl("_", AllSp$Plot)) # make sure no underscores in plot name
## 2020 version does have _ and - so use double __
sum(grepl("__", AllSp$Plot))

AIM.Sp <- AllSp %>%
  # Exclude unknown species
  filter(!is.na(Species),
         !is.na(Exotic)) %>%
  # unite all columns for which separate estimate is desired (site, growth habit, etc)
  unite(PlotByStatus, Plot, Year, Exotic, sep = '__') 

##  Diversity metrics:
H.df <- community_diversity(AIM.Sp, metric = "Shannon",
                            abundance.var = "Pct_Cov", replicate.var = "PlotByStatus")
InvSimp.df <- community_diversity(AIM.Sp, metric = "InverseSimpson",
                                  abundance.var = "Pct_Cov", replicate.var = "PlotByStatus")

##  Richness and evenness: using Evar following Avolio etal 2019 Ecosphere
Even.df <- community_structure(AIM.Sp, metric = "Evar", 
                               abundance.var = "Pct_Cov", replicate.var = "PlotByStatus")
head(Even.df)

##  Combine:
codynMetrics <- H.df %>%
  inner_join(InvSimp.df) %>%
  inner_join(Even.df) %>%
  separate(PlotByStatus, c("Plot", "Year", "Exotic"), sep = "__") %>%
  mutate(Year = as.numeric(Year))
head(codynMetrics)


###
#####   Summarize unknown sp cover & join   ####
###

AIM.PlotDiv <- AllSp %>%
  group_by(Plot, Long, Lat, Year, Exotic) %>%
  summarise(TotalPctCover = sum(Pct_Cov),
            # get 'richness' of unk sp codes too:
            Richness = n()) %>%
  # reshape to calculate and separate info on unknown species:
  pivot_longer(c(TotalPctCover, Richness), names_to = "variable", values_to = "value") %>%
  unite(Var_SpType, variable, Exotic, sep = "_") %>%
  pivot_wider(names_from = Var_SpType, names_sep = "_",
              values_from = value, values_fill = list(value = 0)) %>%
  mutate(AbsUnkCover = TotalPctCover_NA + TotalPctCover_NI,
         RelUnkCover = AbsUnkCover / (AbsUnkCover + TotalPctCover_I + TotalPctCover_N),
         AbsUnkRich = Richness_NA + Richness_NI,
         RelUnkRich = AbsUnkRich/(AbsUnkRich + Richness_I + Richness_N)) %>%
  # leave NA and NI info wide, gather I and N for joining:
  pivot_longer(c(TotalPctCover_I, Richness_I, TotalPctCover_N, Richness_N),
               names_to = c("variable", "Exotic"), names_sep = "_", values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value, values_fill = list(value = 0)) %>%
  # the above creates a dataset with a row for I or N cover of 0
  left_join(codynMetrics) %>%
  mutate(EvennessPielou = Shannon / log(Richness)) %>%
  select(-richness) %>%
  # rename:
  rename(EvennessEvar = Evar, DiversityShannon = Shannon, DiversityInvSimpson = InverseSimpson)
glimpse(AIM.PlotDiv)

write_csv(AIM.PlotDiv, "AIM_PlotDiv_25June2020.csv")
write_rds(AIM.PlotDiv, "AIM_PlotDov_25June2020.rds")

###
#####   Unknown status   ####
###

##   Amt of cover from unk/NI sp   
hist(AIM.PlotDiv$RelUnkCover)
hist(AIM.PlotDiv$RelUnkRich)

##  Cutoff point (relative abundance unk sp) vs plots included:
p.ecdfUnkCover <- AIM.PlotDiv %>%
  select(Plot, RelUnkCover) %>%
  distinct() %>%
  ggplot(aes(RelUnkCover)) +
  stat_ecdf(pad = FALSE) +
  ylab("Proportion of plots included at this cutoff") +
  xlab("Relative cover of unknown and NI species") +
  theme_bw()
ggsave('AIM_ecdfUnkRelCover.png')
