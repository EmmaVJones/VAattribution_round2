# in R 3.4.4
library(tidyverse)
library(readxl)

# Combine Maryland fish data with the previously combined EDAS, Fairfax, and WVA fish data

# bring in combined Virginia, Fairfax, WVA fish data
fishSmash <- read_csv('data/processedData/VA_FF_WVA_fishData.csv')

# bring in Maryland Data
MDlong <- read_excel("data/Maryland/SiteInfoChemLandCoverHabTaxa_JRH.xlsx", sheet = 'Fish') %>% 
  mutate(DataSource = "MD_FISH", UID= SITEYR, StationID = SITEYR, CollDate = as.Date(DATE_SUM,format='%Y-%m-%d')) %>%
  select(DataSource, UID, StationID , Year, CollDate, FishTaxa, TOTAL) %>%
  mutate(FFinalID=tolower(FishTaxa))

# Bring in EDAS master taxa list to convert common name to scientific
master <- read_excel('data/EDAS_Fishes_Master_Taxa.xlsx') %>%
  select(FFinalID,`Genus-Species`)

MDlong1 <- left_join(MDlong,master,by='FFinalID') # join the genus-species name from EDAS
MDlong_good <- filter(MDlong1,!is.na(`Genus-Species`))
MDlong_NA <- filter(MDlong1,is.na(`Genus-Species`)) %>%
  mutate(`Genus-Species`= dplyr::recode(`FFinalID`, # Fix the taxa that didnt match 
                                        'lepomis hybrid' = 'Hybrid  Z', 
                                        'potomac sculpin' = 'Cottus  girardi',
                                        'northern hogsucker' = 'Hypentelium nigricans',
                                        'blue ridge sculpin' = 'Cottus  caeruleomentum',
                                        "blacknose dace" = 'Rhinichthys  atratulus', # we chose eastern blacknose dace bc most drainages flow east in MD
                                        "cutlip minnow" = 'Exoglossum  maxillingua',
                                        'american eel' = 'Anguilla  rostrata',
                                        "cyprinid (unknown)" = 'Hybrid Minnow',
                                        "sunfish (hybrid)" = 'Hybrid  Z',    
                                        "cyprinid hybrid" = 'Hybrid Minnow',      
                                        "notropis sp." = 'Notropis  species',     
                                        "cutthroat trout" = 'Oncorhynchus clarkii',      # not in EDAS
                                        "american shad" = 'Alosa  sapidissima',
                                        "cyprinella sp." = 'Cyprinella spp',       #split out FF and 
                                        "oriental weatherfish" = 'Misgurnus anguillicaudatus',  # not in EDAS
                                        "lamprey sp." = 'Lampetra  species')) %>%
  filter(!is.na(`Genus-Species`) & `Genus-Species` != "no fish observed" &
         `Genus-Species` != 'sculpin (unknown)' & `Genus-Species` !='sunfish (unknown)',
         `Genus-Species` != 'enneacanthus sp.') # get rid of lacking data

MDlong2 <- rbind(MDlong_good, MDlong_NA) %>%
  select(-c(FishTaxa, FFinalID)) %>%
  arrange(StationID) 
# Sum taxa by StationID so no duplicate columns when going from long to wide
MDlong3 <- MDlong2 %>%
  group_by(DataSource, UID, StationID, Year, CollDate, `Genus-Species`) %>% 
  summarise(Count = sum(TOTAL)) %>%
  mutate(StreamName= NA,  Lat = NA, Long = NA, basin = NA, Duration = NA, 
         StWidMax = NA, FSampID = NA, FRepNum = NA, CollMeth = NA, Northing = NA) %>%
  select(DataSource, UID, StationID ,StreamName,  Lat, Long, basin, Duration, StWidMax, 
         FSampID, FRepNum, Year, CollDate,CollMeth, Northing, `Genus-Species`, Count) %>%
  ungroup()



# Fix order
MDE <- read_excel("data/Maryland/SiteInfoChemLandCoverHabTaxa_JRH.xlsx", sheet = 'SiteInfoChemHabitatLandCover') %>%
  select(SITEYR,ORDER) %>%
  dplyr::rename(StationID = SITEYR)

MDlong4 <- left_join(MDlong3, MDE, by=c("StationID")) %>%
  mutate(Order = ORDER) %>%
  select(DataSource, UID, StationID ,StreamName, Order, Lat, Long, basin, Duration, StWidMax, 
         FSampID, FRepNum, Year, CollDate,CollMeth, Northing, `Genus-Species`, Count)


# Change from long to wide format
MDF <- spread(MDlong4,key= `Genus-Species`, value= Count)

# Before doing taxa math, change NA's to 0's to avoid problems adding NAs
MDF[,17:length(MDF)][is.na(MDF[,17:length(MDF)])] <- 0


# Fix shiners and stonerollers, make into a spp but keep original to run in case the taxonomists have data on individuals
MDF <- mutate(MDF,`Cyprinella spp`= `Cyprinella spp` + `Cyprinella  analostana` + `Cyprinella  spiloptera`,
         `Campostoma spp` = `Campostoma  anomalum`)# + `Campostoma  oligolepis`) # no oligolepis in MD data
rm(MDlong);rm(MDlong_good);rm(MDlong_NA);rm(MDlong1);rm(MDlong2);rm(MDlong3);rm(MDlong4);rm(master)



# Taxa Mapping process

# spaces between names could be a matching problem
library(stringr)
names(fishSmash) <- str_replace(gsub("\\s+", " ", str_trim(names(fishSmash))), "B", "b")
names(MDF) <- str_replace(gsub("\\s+", " ", str_trim(names(MDF))), "B", "b")
# See what already matches and what needs to be added to either dataset
n1 <- unique(names(fishSmash)[16:219])
n2 <- unique(names(MDF)[16:94])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 76
needToMap <- n2[!(n2 %in% n1)]
length(needToMap) # 3 species to map


# Add taxa to fishSmash to match Maryland options that we don't have in database yet
fishSmash1 <- mutate(fishSmash,'Oncorhynchus clarkii' = NA, "Misgurnus anguillicaudatus" = NA,'Notropis species' = NA) 
fishSmash2 <- fishSmash1[ ,order(names(fishSmash1))] %>%
  select(DataSource,UID,StationID,StreamName,Order,Lat,Long,basin,Duration,
         StWidMax,FSampID,FRepNum,Year,CollDate,CollMeth,Northing,everything())


# Now add columns to MDF if they arent in there 
n1 <- unique(names(fishSmash2)[16:222])
n2 <- unique(names(MDF)[16:94])
needToAdd <- n1[!(n1 %in% n2)]
MDF1 <- MDF
MDF1[needToAdd] <- NA
names(MDF1) %in% names(fishSmash2)
MDF2 <- MDF1[,names(fishSmash2)]

names(MDF2) == names(fishSmash2)


# Actually combine the two datasets, finally
fishSmash3 <- rbind(fishSmash2,MDF2)

# Make NA's 0's
fishSmash4 <- fishSmash3
fishSmash4[,16:222] <- apply(fishSmash4[,16:222], 2, function(x){replace(x, is.na(x), 0)})

# Make sure numbers are really numeric
fishSmash4[,16:222] <- apply(fishSmash4[,16:222], 2, function(x){as.numeric(as.character(x))})


#Fix Year where NA
fishSmash4 <- mutate(fishSmash4,Year=ifelse(is.na(Year),
                                            format(CollDate,format="%Y"),Year))



write.csv(fishSmash4,'data/processedData/VA_FF_WVA_MD_fishData.csv',row.names=F)
