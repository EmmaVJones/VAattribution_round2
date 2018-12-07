# how to get taxa counts from Final datasets

library(tidyverse)

# Start with fall bugs
############################################################################################
# taken from FallBugAnalyses.Rmd

bugData <- read_csv('data/processedData/allFallBugs.csv')# %>% 
#mutate(UID=paste(StationID,"_",Year,"/",BenSampID,sep="")) %>%
#mutate(UID=paste(StationID,"_",Year,"/",as.Date(CollDate),sep="")) %>%
#select(UID,everything()) %>% select(-c(Year)) 
bugData <- as.data.frame(bugData)
y <- filter(bugData,UID %in% bugData[duplicated(bugData$UID),]$UID)
# get rid of Fairfax QA data bc not unique UID
bugData <- filter(bugData,!(UID %in% y$UID));rm(y)
# Then change UID back to original format so it can match to the envData (which has no way of getting a BenSampID)
#bugData$UID <- gsub("\\/.*","",bugData$UID)
#y <- filter(bugData,UID %in% bugData[duplicated(bugData$UID),]$UID)%>%select(UID,StationID,CollDate,BenSampID)
# drop weird reps by BenSampID, these were chosen by first date (where available) or lower bensampid
#bugData <- filter(bugData,!(BenSampID %in% 
#                     c('POH1727','XMJ9228','cks7171','stm9745','bmt7502','DCK2262',
#                       'ram7608','SCM4727','XRA7458','XRD7463','YST7460','YST7461',
#                       'YST7462','rap7607','RNF3593','ROA8012','TKR9147','wrn4827',
#                       'XOK7455','XOK7456','XOK7457','MHN6030','PBXATTRO1b',
#                       'KOX8266','CPN8128','RMNRAM6287','BLD1827')))
bugTaxa <- select(bugData,UID,Acentrella:Zygoptera)#%>%select(-BenSampID)
bugTaxa <- as.data.frame(bugTaxa)
bugTaxa[2:491] <- apply(bugTaxa[2:491], 2, function(x) as.numeric(as.character(x)))
bugTaxa[is.na(bugTaxa)]=0


envData <- read_csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv') %>%
  select(UID,DataSource,StationID,Year,LongitudeDD,LatitudeDD,totalArea_sqMile,
         Basin,SubBasin,EcoRegion,BioRegion,DO,pH,SpCond,TN,TP,TotHab,
         TotHab_wMD,Cl,Sf, ## PARAMETERS ADDED FOR ROUND TWO OF ANALYSES
         LRBS,MetalCCU,wshdImpPCT)
envData[envData=="NA"]=NA
envData[12:23] <- apply(envData[12:23], 2, function(x) as.numeric(as.character(x)))
envData <- as.data.frame(envData)

# change all / in bug taxa names to _ so doesnt mess up file directory structure with paste()
names(bugTaxa) <- gsub(x = names(bugTaxa), pattern = "/", replacement = "_")

# Get rid of odd measures
weirdDO <- filter(envData, DO > 14) 
weirdpH <- filter(envData, pH > 14 & pH < 0) 
weirdSpCond <- filter(envData, SpCond > 5000)
weirdTN <- filter(envData, TN > 5)
weirdTP <- filter(envData, TP > 1)
weirdTotHab <- filter(envData, TotHab < 0 | TotHab > 200)
weirdTotHab_wMD <- filter(envData, TotHab_wMD < 0 | TotHab_wMD > 200)
weirdLRBS <- filter(envData, LRBS < -4 | LRBS > 2)
weirdwshdImpPCT <- filter(envData, wshdImpPCT < 0 | wshdImpPCT > 100)
rm(weirdpH);rm(weirdSpCond);rm(weirdTN);rm(weirdTP);rm(weirdTotHab);rm(weirdTotHab_wMD);rm(weirdLRBS);rm(weirdwshdImpPCT)

# if these are worth dropping, then do next step 
sitesToDrop <- c(weirdDO$UID)#,weirdpH$UID,#weirdSpCond$UID,weirdTN$UID,weirdTP$UID,weirdTotHab$UID,weirdLRBS$UID,weirdwshdImpPCT$UID)

envData <- filter(envData, !(UID %in% sitesToDrop))
####################################################################################################################

fallBugs <- merge(bugTaxa,select(envData,UID,DO),by='UID')

fallBugOccurrence <- data.frame(colSums(fallBugs[,2:492])) %>%
  tibble::rownames_to_column() %>%
  rename_(taxaName = names(.)[1], fallBugOccurrence = names(.)[2])



rm(bugData);rm(bugTaxa);rm(envData);rm(fallBugs);rm(weirdDO)












# Spring Bug Occurrence
################################################################################################################
# taken from SpringBugAnalyses.Rmd

bugData <- read_csv('data/processedData/allSpringBugs.csv')# %>% 
#mutate(UID=paste(StationID,"_",Year,"/",BenSampID,sep="")) %>%
#mutate(UID=paste(StationID,"_",Year,"/",as.Date(CollDate),sep="")) %>%
#select(UID,everything()) %>% select(-c(Year)) 
bugData <- as.data.frame(bugData)
y <- filter(bugData,UID %in% bugData[duplicated(bugData$UID),]$UID)
# get rid of Fairfax QA data bc not unique UID
bugData <- filter(bugData,!(UID %in% y$UID));rm(y)
# Then change UID back to original format so it can match to the envData (which has no way of getting a BenSampID)
#bugData$UID <- gsub("\\/.*","",bugData$UID)
#y <- filter(bugData,UID %in% bugData[duplicated(bugData$UID),]$UID)%>%select(UID,StationID,CollDate,BenSampID)
# drop weird reps by BenSampID, these were chosen by first date (where available) or lower bensampid
#bugData <- filter(bugData,!(BenSampID %in% 
#                     c('POH1727','XMJ9228','cks7171','stm9745','bmt7502','DCK2262',
#                       'ram7608','SCM4727','XRA7458','XRD7463','YST7460','YST7461',
#                       'YST7462','rap7607','RNF3593','ROA8012','TKR9147','wrn4827',
#                       'XOK7455','XOK7456','XOK7457','MHN6030','PBXATTRO1b',
#                       'KOX8266','CPN8128','RMNRAM6287','BLD1827')))
bugTaxa <- select(bugData,UID,Acentrella:Zygoptera)#%>%select(-BenSampID)
bugTaxa <- as.data.frame(bugTaxa)
bugTaxa[2:491] <- apply(bugTaxa[2:491], 2, function(x) as.numeric(as.character(x)))
bugTaxa[is.na(bugTaxa)]=0


envData <- read_csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv') %>%
  select(UID,DataSource,StationID,Year,LongitudeDD,LatitudeDD,totalArea_sqMile,
         Basin,SubBasin,EcoRegion,BioRegion,DO,pH,SpCond,TN,TP,TotHab,
         TotHab_wMD,Cl,Sf, ## PARAMETERS ADDED FOR ROUND TWO OF ANALYSES
         LRBS,MetalCCU,wshdImpPCT)
envData[envData=="NA"]=NA
envData[12:23] <- apply(envData[12:23], 2, function(x) as.numeric(as.character(x)))
envData <- as.data.frame(envData)



# change all / in bug taxa names to _ so doesnt mess up file directory structure with paste()
names(bugTaxa) <- gsub(x = names(bugTaxa), pattern = "/", replacement = "_")

# Get rid of odd measures
weirdDO <- filter(envData, DO > 14) 
weirdpH <- filter(envData, pH > 14 & pH < 0) 
weirdSpCond <- filter(envData, SpCond > 5000)
weirdTN <- filter(envData, TN > 5)
weirdTP <- filter(envData, TP > 1)
weirdTotHab <- filter(envData, TotHab < 0 | TotHab > 200)
weirdTotHab_wMD <- filter(envData, TotHab_wMD < 0 | TotHab_wMD > 200)
weirdLRBS <- filter(envData, LRBS < -4 | LRBS > 2)
weirdwshdImpPCT <- filter(envData, wshdImpPCT < 0 | wshdImpPCT > 100)
rm(weirdpH);rm(weirdSpCond);rm(weirdTN);rm(weirdTP);rm(weirdTotHab);rm(weirdTotHab_wMD);rm(weirdLRBS);rm(weirdwshdImpPCT)

# if these are worth dropping, then do next step 
sitesToDrop <- c(weirdDO$UID)#,weirdpH$UID,#weirdSpCond$UID,weirdTN$UID,weirdTP$UID,weirdTotHab$UID,weirdLRBS$UID,weirdwshdImpPCT$UID)

envData <- filter(envData, !(UID %in% sitesToDrop))
#########################################################################################################################



springBugs <- merge(bugTaxa,select(envData,UID,DO),by='UID')

springBugOccurrence <- data.frame(colSums(springBugs[,2:492])) %>%
  tibble::rownames_to_column() %>%
  rename_(taxaName = names(.)[1], springBugOccurrence = names(.)[2])

rm(bugData);rm(bugTaxa);rm(envData);rm(springBugs);rm(weirdDO)



















# Fish Occurrence
################################################################################################################
# taken from FishAnalyses.Rmd

fishData <- read_csv('data/processedData/finalFishData.csv') 
fishData <- as.data.frame(fishData)
envData <- read_csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv') %>%
  select(UID,DataSource,StationID,Year,LongitudeDD,LatitudeDD,totalArea_sqMile,
         Basin,SubBasin,EcoRegion,BioRegion,DO,pH,SpCond,TN,TP,TotHab,
         TotHab_wMD,Cl,Sf, ## PARAMETERS ADDED FOR ROUND TWO OF ANALYSES
         LRBS,MetalCCU,wshdImpPCT)

envData[envData=="NA"]=NA
envData[12:23] <- apply(envData[12:23], 2, function(x) as.numeric(as.character(x)))
envData <- as.data.frame(envData)

fishTaxa <- select(fishData,UID,`Acantharchus pomotis`:`Umbra pygmaea`)
fishTaxa <- as.data.frame(fishTaxa)
fishTaxa[2:193] <- apply(fishTaxa[2:193], 2, function(x) as.numeric(as.character(x)))
# Get rid of odd measures
weirdDO <- filter(envData, DO > 14) 
weirdpH <- filter(envData, pH > 14 & pH < 0) 
weirdSpCond <- filter(envData, SpCond > 5000)
weirdTN <- filter(envData, TN > 5)
weirdTP <- filter(envData, TP > 1)
weirdTotHab <- filter(envData, TotHab < 0 | TotHab > 200)
weirdTotHab_wMD <- filter(envData, TotHab_wMD < 0 | TotHab_wMD > 200)
weirdLRBS <- filter(envData, LRBS < -4 | LRBS > 2)
weirdwshdImpPCT <- filter(envData, wshdImpPCT < 0 | wshdImpPCT > 100)
rm(weirdpH);rm(weirdSpCond);rm(weirdTN);rm(weirdTP);rm(weirdTotHab);rm(weirdTotHab_wMD);rm(weirdLRBS);rm(weirdwshdImpPCT)

# if these are worth dropping, then do next step 
sitesToDrop <- c(weirdDO$UID)#,weirdpH$UID,#weirdSpCond$UID,weirdTN$UID,weirdTP$UID,weirdTotHab$UID,weirdLRBS$UID,weirdwshdImpPCT$UID)

envData <- filter(envData, !(UID %in% sitesToDrop))

##############################################################################################################



fish <- merge(fishTaxa,select(envData,UID,DO),by='UID')

fishOccurrence <- data.frame(colSums(fish[,2:194])) %>%
  tibble::rownames_to_column() %>%
  rename_(taxaName = names(.)[1], fishOccurrence = names(.)[2])

rm(fishData);rm(fishTaxa);rm(envData);rm(fish);rm(weirdDO)




















#### Save everything

write.csv(fallBugOccurrence,'FinalDataAndPlots/fallBugOccurrence.csv',row.names = F)
write.csv(springBugOccurrence,'FinalDataAndPlots/springBugOccurrence.csv',row.names = F)
write.csv(fishOccurrence,'FinalDataAndPlots/fishOccurrence.csv',row.names = F)
