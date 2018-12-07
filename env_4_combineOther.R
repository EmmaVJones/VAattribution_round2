# in R 3.4.4
library(tidyverse)
library(readxl)


# Combine other data sources and the previously combined EDAS, Fairfax, WVA, MD environmental data


# Bring in combined dataset
envSmash <- read_csv('data/processedData/VA_FF_WVA_MD_envData.csv')

# Bring in additional data from original data smash (round1)
envOrig <- readxl::read_excel('data/VA/EnviroDataSmashV10EVJJRH.xlsx', sheet = 'stressorData_EVJ') %>%
  filter(DataSource %in% c("INSTAR","MAHA","MAIA","NRSA","RMN","Stressed Site")) 
# just get previously organized data from VA, dont take the 'State ProbMon' bc the 2018IR has much better data than this version (more QAed)


# make envOrig match envSmash
envOrig1 <- envOrig[,names(envOrig) %in% names(envSmash)] %>%
  mutate(TotHab_wMD= TotHab, IR2018=NA, UID = paste(StationID,Year, sep='_'))
n1 <- unique(names(envSmash))
n2 <- unique(names(envOrig1))
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 211, nice!
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#perfect!
needToAdd <- n1[!(n1 %in% n2)]

envOrig2 <- envOrig1[,names(envSmash)] 

names(envOrig2)== names(envSmash)


# Make character NA's to real NA's
envOrig3 <- envOrig2
envOrig3[28:211] <- sapply(envOrig3[28:211],as.numeric)

# finally Combine
envSmash2 <- rbind(envSmash,envOrig3)

# Fix VCU names

# Get original DataSource from envSmash
envSmash3 <- filter(envSmash2,DataSource !='INSTAR')
# Fix VCU names
envOrigVCU <- filter(envSmash2,DataSource =='INSTAR') %>%
  mutate(StationID = substr(StationID,1,nchar(StationID)-7))
envSmash4 <- rbind(envSmash3,envOrigVCU)

rm(envOrig);rm(envOrig1);rm(envOrig2);rm(envOrig3);rm(envOrigVCU);rm(envSmash);rm(envSmash2);rm(envSmash3)

### Fix Ecoregion, and BioRegion issues for nonDEQ data
library(rgdal)
# I did all GIS processing in spatialDataProcessing.mxd
# For ecoregions, I downloaded the EPA level 3 ecoregion layer (without state boundaries) from
# https://www.epa.gov/eco-research/level-iii-and-iv-ecoregions-continental-united-states
# I clipped the ecoregion file to state boundaries of VA, WVA, and MD

# Bioregion file- select the different bioregions from ecoregion layer clipped to state boundaries
# and merge polygons

eco <- readOGR('data/spatialData','VA_MD_WVA_DC_ecoregion_L3_wgs84')
bioregion <- readOGR('data/spatialData','bioregion')

sites_shp <- select(envSmash4,StationID,LatitudeDD,LongitudeDD) %>%
  filter(!is.na(LongitudeDD) | !is.na(LatitudeDD)) # get rid of sites missing location information for analysis
coordinates(sites_shp) <- ~LongitudeDD+LatitudeDD
proj4string(sites_shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")                                 


envSmash5 <- envSmash4 %>%
  filter(!is.na(LongitudeDD) | !is.na(LatitudeDD)) # get rid of sites missing location information for analysis

for(i in 1:nrow(envSmash5)){
  envSmash5$EcoRegion[i] <- as.character(over(sites_shp[i,],eco)$US_L3NAME)
  envSmash5$BioRegion[i] <- as.character(over(sites_shp[i,],bioregion)$Bioregion)
}

# Fix what didn't work
envSmash5[envSmash5$StationID=='5ABVC003.15',]$BioRegion <- 'Coast'


write.csv(envSmash5,"data/processedData/VA_FF_WVA_MD_Other_envData.csv", row.names=F)
write.csv(envSmash5,"data/processedData/finalEnvData.csv", row.names=F)
