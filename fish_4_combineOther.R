# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)


# Combine other data sources and the previously combined EDAS, Fairfax, WVA, MD fish data


# Bring in combined environmental dataset
envSmash <- read.csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv')
envSmash$DataSource <- as.character(envSmash$DataSource)
envSmash$StationID <- as.character(envSmash$StationID)
envSmashLessOriginal <- filter(envSmash,!DataSource %in% c("MAHA","MAIA","NRSA","RMN","Stressed Site",'INSTAR'))

# need to fix datasource on the bug side of things
# bring in fish
fish <- read_csv('data/processedData/VA_FF_WVA_MD_fishData.csv') %>%
  mutate(Year = year(CollDate))


# Get original DataSource from envSmash
envOrig <- filter(envSmash,DataSource %in% c("MAHA","MAIA","NRSA","RMN","Stressed Site","INSTAR"))
# Fix VCU names
#envOrigVCU <- filter(envSmash,DataSource =='INSTAR') %>%
#  mutate(StationID = substr(StationID,1,nchar(StationID)-7))
#envOrig <- rbind(envOrig,envOrigVCU)
#rm(envOrigVCU)

# filter fish to get records from envOrig
fish1 <- filter(fish,!StationID %in% envOrig$StationID)
fishOrig <- filter(fish,StationID %in% envOrig$StationID)

fishOrig1 <- left_join(fishOrig,select(envOrig,DataSource,StationID,Year),by=c("StationID", "Year")) %>%
  select(UID,DataSource.x,DataSource.y,StationID,Year,everything()) %>%
  mutate(DataSource=ifelse(!is.na(DataSource.y),DataSource.y,DataSource.x)) %>% # overwrite Datasource with better information
  select(UID,DataSource,StationID, CollDate, Year,everything())%>%
  select(-c(DataSource.x,DataSource.y))


# Smash those back together
fishSmash <- rbind(fish1,fishOrig1)
fishSmash1 <- fishSmash[!duplicated(fishSmash[,c(3,14)]),] # Get rid of duplicates, if they exist


write.csv(fishSmash1,'data/processedData/VA_FF_WVA_MD_Other_fishData.csv',row.names=F)

