# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)


# Combine other data sources and the previously combined EDAS, Fairfax, WVA, MD bug data


# Bring in combined environmental dataset
envSmash <- read.csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv')
envSmash$DataSource <- as.character(envSmash$DataSource)
envSmash$StationID <- as.character(envSmash$StationID)
envSmashLessOriginal <- filter(envSmash,!DataSource %in% c("MAHA","MAIA","NRSA","RMN","Stressed Site",'INSTAR'))

# need to fix datasource on the bug side of things
# bring in bugs
bugs <- read_csv('data/processedData/VA_FF_WVA_MD_bugData.csv') %>%
  mutate(Year = year(CollDate))%>%
  select(-c(BenSampID))

# Get original DataSource from envSmash
envOrig <- filter(envSmash,DataSource %in% c("MAHA","MAIA","NRSA","RMN","Stressed Site","INSTAR"))
# Fix VCU names
#envOrigVCU <- filter(envSmash,DataSource =='INSTAR') %>%
#  mutate(StationID = substr(StationID,1,nchar(StationID)-7))
#envOrig <- rbind(envOrig,envOrigVCU)
#rm(envOrigVCU)

# filter bugs to get records from envOrig
bugs1 <- filter(bugs,!StationID %in% envOrig$StationID)
bugsOrig <- filter(bugs,StationID %in% envOrig$StationID)
bugsOrig_ <- bugsOrig[!duplicated(bugsOrig[,c(3,4)]),] # remove duplicated samples/sampledates

bugsOrig1 <- left_join(bugsOrig_,select(envOrig,DataSource,StationID,Year),by=c("StationID", "Year")) %>%
  select(UID,DataSource.x,DataSource.y,StationID,Year,everything()) %>%
  mutate(DataSource=ifelse(!is.na(DataSource.y),DataSource.y,DataSource.x)) %>% # overwrite Datasource with better information
  select(UID,DataSource,StationID, CollDate, Year,everything())%>%
  select(-c(DataSource.x,DataSource.y))


# Smash those back together
bugSmash <- rbind(bugs1,bugsOrig1)
bugSmash1 <- bugSmash[!duplicated(bugSmash[,c(3,4)]),] # Get rid of duplicates, again



write.csv(bugSmash1,'data/processedData/VA_FF_WVA_MD_Other_bugData.csv',row.names=F)
