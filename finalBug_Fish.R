# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)


# Filter "final" bug and fish datasets to just those sites with associated environmental data for final analyses

# Bring in final environmental dataset
env <- read.csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv') 

# Fix VCU UID problems
envFine <- filter(env, DataSource != 'INSTAR')
envFix <- filter(env, DataSource == 'INSTAR') %>%
  mutate(UID = paste(StationID, Year, sep='_')) %>%
  bind_rows(envFine)
env <- envFix
rm(envFix);rm(envFine)


### Bugs

# Bring in final bug dataset
bugs <- read_csv('data/processedData/VA_FF_WVA_MD_Other_bugData.csv') %>%
  mutate(Year = year(CollDate))

bugFinal <- filter(bugs, UID %in% env$UID) %>%
  select(UID,DataSource,StationID, CollDate, Year, everything())


# Any taxa to drop?
bugFinal[,6:541] <- apply(bugFinal[,6:541], 2, function(x){replace(x, is.na(x), 0)})
bugFinal[,6:541] <- apply(bugFinal[,6:541], 2, function(x){as.numeric(as.character(x))})

haveRecords <- colSums(bugFinal[,6:length(bugFinal)], na.rm=T) != 0
subsetList <- haveRecords[haveRecords == TRUE]
bugFinal1 <- select(bugFinal,UID, DataSource, StationID, CollDate, Year, names(subsetList))



# How many records per partner?
nrow(filter(bugFinal1,DataSource %in% c('DEQ'))) # DEQ = 665, so low bc only started doing genus level taxa work in 2009
nrow(filter(bugFinal1,DataSource %in% c("Stressed Site"))) # VA targeted Stress= 84
nrow(filter(bugFinal1,DataSource %in% c("RMN"))) # VA RMN= 7

nrow(filter(bugFinal,DataSource %in% c("NRSA"    ))) # NRSA = 0
nrow(filter(bugFinal,DataSource %in% c("MAIA"    ))) # MAIA = 0
nrow(filter(bugFinal,DataSource %in% c("MAHA"    ))) # MAHA = 0

nrow(filter(bugFinal1,DataSource %in% c("INSTAR"))) # VCU= 14
nrow(filter(bugFinal1,DataSource %in% c("FFX_USGS","FFX_Probabilistic","FFX_Reference" ))) # Fairfax = 679
nrow(filter(bugFinal1,DataSource %in% c("WVA_BUG"))) # WVA = 3754
nrow(filter(bugFinal1,DataSource %in% c("MD_BUG"))) # MD = 1958

# how many taxa have been collected?
ncol(bugFinal1)-5# 490 taxa




write.csv(bugFinal1,'data/processedData/finalBugData.csv',row.names = F)


### Fish

# Bring in final fish dataset
fish <- read_csv('data/processedData/VA_FF_WVA_MD_Other_fishData.csv') %>%
  mutate(Year = year(CollDate))

fishFinal <- filter(fish, UID %in% env$UID)

# Any taxa to drop?
fishFinal[,17:222] <- apply(fishFinal[,17:222], 2, function(x){replace(x, is.na(x), 0)})
fishFinal[,17:222] <- apply(fishFinal[,17:222], 2, function(x){as.numeric(as.character(x))})

haveRecords <- colSums(fishFinal[,17:length(fishFinal)], na.rm=T) != 0
subsetList <- haveRecords[haveRecords == TRUE]
fishFinal1 <- select(fishFinal,UID, DataSource, StationID, StreamName, Order, Lat, Long, basin, 
                     Duration, StWidMax, FSampID, FRepNum, Year, CollDate, CollMeth, Northing, names(subsetList))


# How many records per partner?
nrow(filter(fishFinal,DataSource %in% c('DEQ'))) # DEQ = 411, so low bc only started fish work in 2005
nrow(filter(fishFinal1,DataSource %in% c("Stressed Site"))) # VA targeted Stress= 50
nrow(filter(fishFinal1,DataSource %in% c("RMN"))) # VA RMN= 5
nrow(filter(fishFinal,DataSource %in% c("NRSA"    ))) # NRSA = 29
nrow(filter(fishFinal,DataSource %in% c("MAIA"    ))) # MAIA = 22
nrow(filter(fishFinal,DataSource %in% c("MAHA"    ))) # MAHA = 86

nrow(filter(fishFinal1,DataSource %in% c("INSTAR"))) # VCU= 0
nrow(filter(fishFinal,DataSource %in% c("FFX_USGS","FFX_Probabilistic","FFX_Reference" ))) # Fairfax = 289
nrow(filter(fishFinal,DataSource %in% c("WVA_FISH"))) # WVA = 172
nrow(filter(fishFinal,DataSource %in% c("MD_FISH"    ))) # MD = 1805


# how many taxa have been collected?
ncol(fishFinal1)-16# 192 taxa


write.csv(fishFinal1,'data/processedData/finalFishData.csv',row.names = F)
