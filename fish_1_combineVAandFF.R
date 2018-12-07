# in R 3.4.4
library(tidyverse)
library(readxl)

# Combine Fairfax fish data with EDAS data


# Bring in Fairfax dataset
FFF <- readxl::read_excel('data/Fairfax/DEQ BCG Taxa Data v3.xlsx',sheet='Fish') %>%
  select(-c(`Year`))

#Bring in DEQ genus EDAS template (originally from C:/HardDriveBackup/R/BCG/VAattribution/Jason/)
EDASF <- readxl::read_excel('data/VA/FishSpecies.xlsx',sheet='AllTaxa')%>%
  mutate(DataSource='DEQ')%>%select(DataSource,everything())
# EDAS has way more columns, need to map things in very carefully

# See what we are working with here
n1 <- unique(names(EDASF)[16:206])
n2 <- unique(names(FFF)[5:59])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 6
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#49 taxa in Fairfax dataset that I need to map to DEQ taxa, yikes!

# spaces between names could be biggest problem
library(stringr)
names(EDASF) <- str_replace(gsub("\\s+", " ", str_trim(names(EDASF))), "B", "b")
n1 <- unique(names(EDASF)[16:206])
n2 <- unique(names(FFF)[5:59])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 52
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#4 taxa in Fairfax dataset that I need to map to DEQ taxa, nice!

# Any rep2's in FF data?
z <- data.frame(StationID=NA,SampDate=NA,rep=NA)
rep2 <- FFF[duplicated(FFF$CollDate),1:3]
for(i in 1:length(unique(rep2$CollDate))){
  x <- filter(rep2,CollDate %in% unique(rep2$CollDate)[i])
  date1 <- as.Date(unique(x$CollDate),format='%m/%d/%Y')
  if(length(unique(x$StationID)) <  nrow(x)){
    print(paste(i,'yes'))}
    #y <- cbind(StationID=as.character(x[duplicated(x$StationID),2]),SampDate=as.character(date1),rep="rep2?")}
  #z[i,] <- y
}


# Manually adjust Fairfax taxa names to match EDAS format

FFF_smash <- mutate(FFF,
                    StreamName= NA, Order = NA, Lat = NA, Long= NA, basin=NA,
                    Duration= NA, StWidMax=NA,FSampID= NA, FRepNum=1, 
                    CollMeth=NA, Northing=NA, Year = NA,
                    # These need to be mapped
                    `Catostomus commersonii` = `Catostomus commersoni`,
                    #`Channa argus`,# dont change name, just not in our DB
                    #`Cyprinella spp`,#`Cyprinella analostana` and `Cyprinella spiloptera` need to be combined on DEQ EDAS end
                    `Hybrid Z` = `Lepomis hybrid`) %>% # no sunfish hybrid in DB. what should I call it?
  select(-c(`Catostomus commersoni`,`Lepomis hybrid`)) %>%
  select(DataSource,StationID,StreamName,Order,Lat,
         Long,basin,Duration,StWidMax,FSampID,FRepNum,
         Year,CollDate,CollMeth,Northing,everything())

# Add taxa to EDASF to match properly
EDASF <- mutate(EDASF,`Channa argus`=NA, 
                # add Cyprinella spp to EDAS and combine analostana and spiloptera for comparison
                # to Fairfax, will run stats both ways for all datasources
                `Cyprinella spp`=`Cyprinella analostana` + `Cyprinella spiloptera`)

# Add columns to FFF_smash if they arent in there 
n1 <- unique(names(EDASF)[16:208])
n2 <- unique(names(FFF_smash)[16:71])
needToAdd <- n1[!(n1 %in% n2)]
FFF_smash1 <- FFF_smash
FFF_smash1[needToAdd] <- NA
names(FFF_smash1) %in% names(EDASF)
FFF_smash2 <- FFF_smash1[,names(EDASF)] # reorder columns to match EDASF

names(FFF_smash2) == names(EDASF)

# Actually combine the two datasets, finally
fishSmash <- rbind(EDASF,FFF_smash2) %>%
  mutate(`Campostoma spp` = `Campostoma anomalum` + `Campostoma oligolepis`,
         `Cottus_Broadband` = `Cottus sp_ 4` + `Cottus sp_ 5` + `Cottus sp_ 1`) %>%
  #remove those combined species for BCG analyses
  select(-c(`Cottus sp_ 4`,`Cottus sp_ 5`,`Cottus sp_ 1`))

fishSmash2 <- fishSmash[ ,order(names(fishSmash))] %>%
  select(DataSource,StationID,StreamName,Order,Lat,Long,basin,Duration,
         StWidMax,FSampID,FRepNum,Year,CollDate,CollMeth,Northing,everything())
# Make NA's 0's
fishSmash3 <- fishSmash2
fishSmash3[,16:207] <- apply(fishSmash3[,16:207], 2, function(x){replace(x, is.na(x), 0)})

# fix naked cr stationID
fishSmash4 <- fishSmash3
fishSmash4$StationID <- dplyr::recode(fishSmash3$StationID,`1BNKD003.81` ="1BNAK003.81")
# make sure it worked...
filter(fishSmash4,StationID == '1BNAK003.81')
filter(fishSmash4,StationID == '1BNKD003.81')


write.csv(fishSmash4,'data/processedData/EDASandFairfax_fishData.csv',row.names=F)

