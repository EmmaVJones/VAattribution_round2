# in R 3.4.4 
library(readxl)

# NOTE:
# For the plyr function to work, must NOT load tidyverse bc lost the . functionality in ddply statement
# This is why cannot do this calculation on the fly in the app
library(plyr)
library(dplyr)
library(sf)

# This script takes the environmental data from DEQ, Fairfax, WVA to make a spatial dataset to show 
#  species distribution in leaflet map

# Bring in station data
sites <- readr::read_csv('data/processedData/VA_FF_WVA_MD_Other_envData.csv') %>% # must use readr::read_csv() because it can handle / in column names
  dplyr::select(DataSource,UID,StationID:LatitudeDD,Order) %>%
  mutate(DataSource = dplyr::recode(DataSource, 'State ProbMon' = 'DEQ')) 
sites$Order <- factor(sites$Order, levels=sort(unique(sites$Order)))
sites$StationID <- as.character(sites$StationID)
sites$DataSource <- as.character(sites$DataSource)

#### SPRING BUGS ########

# Bring in spring bug data
bugSp <- readr::read_csv('data/processedData/allSpringBugs.csv')
names(bugSp)[6:495] <- gsub('\\.',' ',names(bugSp)[6:495]) # really a step from fish data, but make sure no taxa names use . instead of space
bugSp <- mutate(bugSp,Year=lubridate::year(CollDate)) %>% 
  select(DataSource,StationID,Year,UID,Acentrella:Zygoptera) 
bugSp$Year <- as.numeric(as.character(bugSp$Year))
bugSp$DataSource <- as.character(bugSp$DataSource)
bugSp$StationID <- as.character(bugSp$StationID)

# filter bugSp by only sites we have env data for
bugSp <- filter(bugSp, UID %in% sites$UID) %>%
  mutate(DataSource = dplyr::recode(DataSource, 'State ProbMon' = 'DEQ'))

# Make a sf object of all sites
sites_shp <- st_as_sf(dplyr::select(sites,StationID:LatitudeDD,Order),coords=c('LongitudeDD','LatitudeDD'),
                      crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",agr = 'constant')%>%
  mutate(UID= paste(StationID,Year,sep='_'))


# Make a dataset of how many taxa collected at station, how many years sampled

#i=names(bugSp)[4]
stationBySpeciesUniqueList <- list()
notFoundInDatabase <- data.frame(Species=NA)

for(i in names(bugSp)[5:length(names(bugSp))]){
  print(i)
  dat <- dplyr::select(bugSp,DataSource, StationID,Year,UID,i) %>%#names(bugSp)[i]) %>%
    filter(.[[5]] != 0) %>% rename(Count = !!names(.[5]))
  if(nrow(dat)>1){
    sitesToPlot <- filter(sites,StationID %in% dat$StationID & Year %in% dat$Year) %>%
      full_join(dat,by=c('DataSource','StationID','Year','UID'))
    sitesToPlot$Count[is.na(sitesToPlot$Count)] <- 0
    
    sitesToPlot2 <- select(sitesToPlot,DataSource,StationID,Year,Count) %>%
      plyr::ddply(.(StationID), summarize,YearsSampled=paste(Year,collapse=", "),
                  TotalCount=sum(Count)) %>%
      left_join(sitesToPlot,by='StationID')
    sitesToPlot2 <- sitesToPlot2[!duplicated(sitesToPlot2$StationID),] %>%
      select(DataSource, StationID,YearsSampled,TotalCount,Order,UID,LongitudeDD,LatitudeDD)
    
    stationBySpeciesUniqueList[[i]] <- sitesToPlot2
  }else{
    notFoundInDatabase <- rbind(notFoundInDatabase,i) 
  }
}
notFoundInDatabase <- na.omit(notFoundInDatabase)

# make sure we got everyone
nrow(notFoundInDatabase) + length(stationBySpeciesUniqueList) == length(names(bugSp)[5:length(bugSp)])


saveRDS(stationBySpeciesUniqueList,'data/processedData/springbugStationsUnique.RDS')

# Check it will work in a map
spe <- names(bugSp)[5]
#sitesToPlot <- stationBySpeciesUniqueList[spe][[1]]

sitesToPlot <- stationBySpeciesUniqueList$`Acentrella`
sitesToPlot <- st_as_sf(sitesToPlot, coords = c("LongitudeDD","LatitudeDD"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", agr = "constant")

library(leaflet)
library(mapview)
pal <- colorFactor(c('red','orange','yellow','limegreen','blue','purple'),domain = NULL, levels(sites$Order), ordered=T)

leaflet(sites_shp) %>%
  addProviderTiles(providers$Stamen.TopOSMRelief,group='Stamen Topo') %>%
  addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
  addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
  #addPolygons(data=basins,color='navy',fill=0.9,stroke=0.1,group="Basins",
  #            popup=paste("Basin: ",basins@data$BASIN,sep="")) %>% hideGroup('Basins') %>%
  #addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
  #            popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep="")) %>% hideGroup('Ecoregions') %>%
  addCircleMarkers(data=sites_shp,radius=6,
                   color=~'black',stroke=F,fillOpacity=1,
                   group='All Sites',layerId=~StationID_Trend,
                   popup=popupTable(sites_shp, zcol = c("StationID","Year","StationID_Trend","Order"))) %>% hideGroup('All Sites') %>%
  addFeatures(data= sitesToPlot,#~LongitudeDD,~LatitudeDD,
              radius=6,color='black',fillColor =~pal(Order),fillOpacity = 1,
              opacity=1,weight = 2,stroke=T,group='Capture Location',layerId=~UID,
              popup=popupTable(sitesToPlot, zcol = c("DataSource","StationID","YearsSampled","Order","TotalCount"))) %>% 
  addLegend("bottomleft",colors=c('red','orange','yellow','limegreen','blue','purple'),
            labels=c('First','Second','Third','Fourth','Fifth','6+'),
            title='Stream Order')%>% #,group='Legend')%>%
  addLayersControl(baseGroups=c('Stamen Topo','Esri World Imagery','Open Street Map'),
                   overlayGroups = c('Basins','Ecoregions','All Sites','Capture Location'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')%>%
  addMouseCoordinates()

rm(notFoundInDatabase);rm(sitesToPlot);rm(sitesToPlot2);rm(stationBySpeciesUniqueList);rm(bugSp);rm(sites_shp);rm(dat)











#### FALL BUGS ########

# Bring in fall bug data
bugF <- readr::read_csv('data/processedData/allFallBugs.csv')
names(bugF)[6:495] <- gsub('\\.',' ',names(bugF)[6:495]) # really a step from fish data, but make sure no taxa names use . instead of space
bugF <- mutate(bugF,Year=lubridate::year(CollDate)) %>% 
  select(DataSource,StationID,Year,UID,Acentrella:Zygoptera)
bugF$Year <- as.numeric(as.character(bugF$Year))

# filter bugF by only sites we have env data for
bugF <- filter(bugF, UID %in% sites$UID) %>%
  mutate(DataSource = dplyr::recode(DataSource, 'State ProbMon' = 'DEQ'))



# Make a sf object of all sites
sites_shp <- st_as_sf(dplyr::select(sites,StationID:LatitudeDD,Order),coords=c('LongitudeDD','LatitudeDD'),
                      crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",agr = 'constant')%>%
  mutate(UID= paste(StationID,Year,sep='_'))


# Make a dataset of how many taxa collected at station, how many years sampled

#i=names(bugF)[4]
stationBySpeciesUniqueList <- list()
notFoundInDatabase <- data.frame(Species=NA)

for(i in names(bugF)[5:length(names(bugF))]){
  print(i)
  dat <- dplyr::select(bugF,DataSource, StationID,Year,UID,i) %>%#names(bugF)[i]) %>%
    filter(.[[5]] != 0) %>% rename(Count = !!names(.[5]))
  if(nrow(dat)>1){
    sitesToPlot <- filter(sites,StationID %in% dat$StationID & Year %in% dat$Year) %>%
      full_join(dat,by=c('DataSource','StationID','Year','UID'))
    sitesToPlot$Count[is.na(sitesToPlot$Count)] <- 0
    
    sitesToPlot2 <- select(sitesToPlot,DataSource,StationID,Year,Count) %>%
      plyr::ddply(.(StationID), summarize,YearsSampled=paste(Year,collapse=", "),
                  TotalCount=sum(Count)) %>%
      left_join(sitesToPlot,by='StationID')
    sitesToPlot2 <- sitesToPlot2[!duplicated(sitesToPlot2$StationID),] %>%
      select(DataSource, StationID,YearsSampled,TotalCount,Order,UID,LongitudeDD,LatitudeDD)
    
    stationBySpeciesUniqueList[[i]] <- sitesToPlot2
  }else{
    notFoundInDatabase <- rbind(notFoundInDatabase,i) 
  }
}
notFoundInDatabase <- na.omit(notFoundInDatabase)

# make sure we got everyone
nrow(notFoundInDatabase) + length(stationBySpeciesUniqueList) == length(names(bugF)[5:length(bugF)])


saveRDS(stationBySpeciesUniqueList,'data/processedData/fallbugStationsUnique.RDS')

# Check it will work in a map
spe <- names(bugF)[4]
#sitesToPlot <- stationBySpeciesUniqueList[spe][[1]]

sitesToPlot <- stationBySpeciesUniqueList$`Acentrella`
sitesToPlot <- st_as_sf(sitesToPlot, coords = c("LongitudeDD","LatitudeDD"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", agr = "constant")

library(leaflet)
library(mapview)
pal <- colorFactor(c('red','orange','yellow','limegreen','blue','purple'),domain = NULL, levels(sites$Order), ordered=T)

leaflet(sites_shp) %>%
  addProviderTiles(providers$Stamen.TopOSMRelief,group='Stamen Topo') %>%
  addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
  addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
  #addPolygons(data=basins,color='navy',fill=0.9,stroke=0.1,group="Basins",
  #            popup=paste("Basin: ",basins@data$BASIN,sep="")) %>% hideGroup('Basins') %>%
  #addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
  #            popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep="")) %>% hideGroup('Ecoregions') %>%
  addCircleMarkers(data=sites_shp,radius=6,
                   color=~'black',stroke=F,fillOpacity=1,
                   group='All Sites',layerId=~StationID_Trend,
                   popup=popupTable(sites_shp, zcol = c("StationID","Year","StationID_Trend","Order"))) %>% hideGroup('All Sites') %>%
  addFeatures(data= sitesToPlot,#~LongitudeDD,~LatitudeDD,
              radius=6,color='black',fillColor =~pal(Order),fillOpacity = 1,
              opacity=1,weight = 2,stroke=T,group='Capture Location',layerId=~UID,
              popup=popupTable(sitesToPlot, zcol = c("DataSource","StationID","YearsSampled","Order","TotalCount"))) %>% 
  addLegend("bottomleft",colors=c('red','orange','yellow','limegreen','blue','purple'),
            labels=c('First','Second','Third','Fourth','Fifth','6+'),
            title='Stream Order')%>%#,group='Legend')%>%
  addLayersControl(baseGroups=c('Stamen Topo','Esri World Imagery','Open Street Map'),
                   overlayGroups = c('Basins','Ecoregions','All Sites','Capture Location'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')%>%
  addMouseCoordinates()



rm(notFoundInDatabase);rm(sitesToPlot);rm(sitesToPlot2);rm(stationBySpeciesUniqueList);rm(bugF);rm(sites_shp);rm(dat)











#### FISH ########

fishSp <- readr::read_csv('data/processedData/finalFishData.csv') 
names(fishSp)[17:208] <- gsub('\\.',' ',names(fishSp)[17:208]) # get rid of . as space in taxa names
fishSp <- select(fishSp, DataSource,StationID,Year,UID,`Acantharchus pomotis`:`Umbra pygmaea`)
fishSp$Year <- as.numeric(as.character(fishSp$Year))


# filter fishSp by only sites we have env data for
fishSp <- filter(fishSp, UID %in% sites$UID)

sites_shp <- st_as_sf(dplyr::select(sites,StationID:LatitudeDD,Order),coords=c('LongitudeDD','LatitudeDD'),
                      crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",agr = 'constant')%>%
  mutate(UID= paste(StationID,Year,sep='_'))


#i=names(fishSp)[4]
stationBySpeciesUniqueList <- list()
notFoundInDatabase <- data.frame(Species=NA)

for(i in names(fishSp)[5:length(names(fishSp))]){
  print(i)
  dat <- dplyr::select(fishSp,DataSource,StationID,Year,UID,i) %>%#names(fishSp)[i]) %>%
    filter(.[[5]] != 0) %>% rename(Count = !!names(.[5]))
  if(nrow(dat)>1){
    sitesToPlot <- filter(sites,StationID %in% dat$StationID & Year %in% dat$Year) %>%
      full_join(dat,by=c('DataSource','StationID','Year','UID')) %>%
      filter(!is.na(LongitudeDD) & !is.na(LatitudeDD))
    sitesToPlot$Count[is.na(sitesToPlot$Count)] <- 0
    
    sitesToPlot2 <- select(sitesToPlot,DataSource,StationID,Year,Count) %>%
      plyr::ddply(.(StationID), summarize,YearsSampled=paste(Year,collapse=", "),
                  TotalCount=sum(Count)) %>%
      left_join(sitesToPlot,by='StationID')
    sitesToPlot2 <- sitesToPlot2[!duplicated(sitesToPlot2$StationID),] %>%
      select(DataSource,StationID,YearsSampled,TotalCount,Order,UID,LongitudeDD,LatitudeDD)
    
    stationBySpeciesUniqueList[[i]] <- sitesToPlot2
  }else{
    notFoundInDatabase <- rbind(notFoundInDatabase,i) 
  }
}
notFoundInDatabase <- na.omit(notFoundInDatabase)

# make sure we got everyone
nrow(notFoundInDatabase) + length(stationBySpeciesUniqueList) == length(names(fishSp)[5:length(fishSp)])


saveRDS(stationBySpeciesUniqueList,'data/processedData/fishStationsUnique.RDS')


spe <- names(fishSp)[4]
#sitesToPlot <- stationBySpeciesUniqueList[spe][[1]]

sitesToPlot <- stationBySpeciesUniqueList$`Acantharchus pomotis`
sitesToPlot <- st_as_sf(sitesToPlot, coords = c("LongitudeDD","LatitudeDD"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", agr = "constant")

library(leaflet)
library(mapview)
pal <- colorFactor(c('red','orange','yellow','limegreen','blue','purple'),domain = NULL, levels(sites$Order), ordered=T)

leaflet(sites_shp) %>%
  addProviderTiles(providers$Stamen.TopOSMRelief,group='Stamen Topo') %>%
  addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
  addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
  #addPolygons(data=basins,color='navy',fill=0.9,stroke=0.1,group="Basins",
  #            popup=paste("Basin: ",basins@data$BASIN,sep="")) %>% hideGroup('Basins') %>%
  #addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
  #            popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep="")) %>% hideGroup('Ecoregions') %>%
  addCircleMarkers(data=sites_shp,radius=6,
                   color=~'black',stroke=F,fillOpacity=1,
                   group='All Sites',layerId=~StationID_Trend,
                   popup=popupTable(sites_shp, zcol = c("StationID","Year","StationID_Trend","Order"))) %>% hideGroup('All Sites') %>%
  addFeatures(data= sitesToPlot,#~LongitudeDD,~LatitudeDD,
              radius=6,color='black',fillColor =~pal(Order),fillOpacity = 1,
              opacity=1,weight = 2,stroke=T,group='Capture Location',layerId=~UID,
              popup=popupTable(sitesToPlot, zcol = c("DataSource","StationID","YearsSampled","Order","TotalCount"))) %>% 
  addLegend("bottomleft",colors=c('red','orange','yellow','limegreen','blue','purple'),
            labels=c('First','Second','Third','Fourth','Fifth','6+'),
            title='Stream Order')%>%#,group='Legend')%>%
  addLayersControl(baseGroups=c('Stamen Topo','Esri World Imagery','Open Street Map'),
                   overlayGroups = c('Basins','Ecoregions','All Sites','Capture Location'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')%>%
  addMouseCoordinates()


