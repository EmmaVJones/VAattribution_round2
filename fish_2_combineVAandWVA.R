# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)

# Combine West Virginia fish data with the previously combined EDAS and Fairfax fish data

WVAF <- read_csv("data/WVA/VA BCG Fish Data Table.csv") %>%
  mutate(DataSource="WVA_FISH",basin=Basin,
         CollDate=as.Date(CollDate,format="%m/%d/%Y")) %>%
  select(-c(SAMPLE_ID,Basin,`No Fish`)) %>%
  select(DataSource,UID,StationID,StreamName,Order,Lat,Long,basin,everything())



#Bring in DEQ EDAS template
EDASF <- read_csv('data/processedData/EDASandFairfax_fishData.csv') %>%
  mutate(UID = paste(StationID, year(CollDate), sep='_')) %>%
  select(DataSource, UID, StationID, CollDate, everything())
# EDAS has way more columns, need to map things in very carefully


# See what already matches and what needs to be added to either dataset
n1 <- unique(names(EDASF)[17:208])
n2 <- unique(names(WVAF)[17:150])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 120
needToMap <- n2[!(n2 %in% n1)]
length(needToMap) # 14 species to map


# Before doing taxa math, change NA's to 0's to avoid problems adding NAs
WVAF[,17:length(WVAF)][is.na(WVAF[,17:length(WVAF)])] <- 0


# Change any WVA taxa names that already have a different name in EDAS
WVAF_smash <- mutate(WVAF,
                   # These need to be mapped
                   # Campostoma spp and Cyprinella spp already collapsed in WVA data so cant use individual info for dataset
                   `Moxostoma duquesnei` = `Moxostoma duquesnii`,
                   `Etheostoma tenneseense` = `Etheostoma tennesseense`,
                   `Moxostoma rhothoeca` = `Thoburnia rhothoeca`) %>%
  select(-c(`Moxostoma duquesnii`, `Etheostoma tennesseense`,`Thoburnia rhothoeca`)) 

# Add taxa to EDASF to match WVA options that we don't have in database yet
EDASF <- mutate(EDASF,
                `Ameiurus melas`=NA,`Lythrurus umbratilis`=NA,`Minytrema melanops`=NA,
                `Moxostoma anisurum`=NA,`Notropis atherinoides`=NA,`Notropis stramineus`=NA,
                `Noturus miurus`=NA,`Pararhinichthys bowersi` = NA,`Percina maculata`=NA,
                `Percopsis omiscomaycus`=NA,`Phenacobius mirabilis`=NA)     


# Now add columns to WVA_smash if they arent in there 
n1 <- unique(names(EDASF)[17:219])
n2 <- unique(names(WVAF_smash)[17:150])
needToAdd <- n1[!(n1 %in% n2)]
WVAF_smash1 <- WVAF_smash
WVAF_smash1[needToAdd] <- NA
names(WVAF_smash1) %in% names(EDASF)
WVAF_smash2 <- WVAF_smash1[,names(EDASF)]

names(WVAF_smash2) == names(EDASF)


# Actually combine the two datasets, finally
fishSmash <- rbind(EDASF,WVAF_smash2)

fishSmash2 <- fishSmash[ ,order(names(fishSmash))] %>%
  select(DataSource,UID, StationID,StreamName,Order,Lat,Long,basin,Duration,
         StWidMax,FSampID,FRepNum,Year,CollDate,CollMeth,Northing,everything())

# Make NA's 0's
fishSmash3 <- fishSmash2
fishSmash3[,17:219] <- apply(fishSmash3[,17:219], 2, function(x){replace(x, is.na(x), 0)})

# Make sure numbers are really numeric
fishSmash3[,17:219] <- apply(fishSmash3[,17:219], 2, function(x){as.numeric(as.character(x))})


#Fix Year where NA
fishSmash4 <- mutate(fishSmash3,Year=ifelse(is.na(Year),
                                            format(CollDate,format="%Y"),Year))


# Delete accidental rep2's
fishSmash5 <- fishSmash4[ -(which( fishSmash4$StationID=='5AOTR008.07' & fishSmash4$CollDate == '2014-10-08') ), ]
fishSmash5 <- fishSmash5[ -(which( fishSmash5$StationID=='4BGAR001.19' & fishSmash5$CollDate == '2014-08-28') ), ]
fishSmash5 <- fishSmash5[ -(which( fishSmash5$StationID=='2-FAS002.67')), ] # Falling springs stations dont have associated stressor data to run analyses
fishSmash5 <- fishSmash5[ -(which( fishSmash5$StationID=='2-FAS001.08')), ] # Falling springs stations dont have associated stressor data to run analyses



write.csv(fishSmash5,'data/processedData/VA_FF_WVA_fishData.csv',row.names=F)

