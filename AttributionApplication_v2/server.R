# Built in R version 3.4.4

source('global.R')

Ecoregions <- readOGR('data','VA_MD_WVA_DC_ecoregion_L3_wgs84')
Ecoregions@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
Bioregions <- readOGR('data','bioregion')
Bioregions@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


shinyServer(function(input, output, session) {
  
  ##-------------------------------------------- FISH -----------------------------------------###
  # Distribution Map
  
  
  speciesUniqueSites <- reactive({
    req(input$fishSpecies)
    st_as_sf(stationBySpeciesUniqueList[input$fishSpecies][[1]], 
             coords = c("LongitudeDD","LatitudeDD"), 
             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
             agr = "constant")
  })
  
  pal <- colorFactor(c('red','orange','yellow','limegreen','blue','purple','gray'),domain = NULL, levels(sites$Order), ordered=T)
  
  output$FISH_distMap <- renderLeaflet({
    leaflet(sites_shp) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,group='Nat Geo World Map') %>%
      #addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape') %>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addPolygons(data=Bioregions,color='navy',fill=0.9,stroke=0.1,group="Bioregions",
                  popup=paste("Basin: ",Bioregions@data$BASIN,sep="")) %>% hideGroup('Bioregions') %>%
      addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
                  popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep="")) %>% hideGroup('Ecoregions') %>%
      addLegend("bottomright",pal=pal, values=levels(sites$Order),title="Stream Order") %>%
      #addLegend("bottomleft",colors=c('red','orange','yellow','limegreen','blue','purple'),
      #          labels=c('First','Second','Third','Fourth','Fifth','6+'),
      #          title='Stream Order')%>%
      addLayersControl(baseGroups=c('Nat Geo World Map','Esri World Imagery','Open Street Map'),
                       overlayGroups = c('Bioregions','Ecoregions','All Sites','Capture Location'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()
  })
  
  observe({
    leafletProxy('FISH_distMap', data= speciesUniqueSites()) %>% clearMarkers() %>%
      addCircleMarkers(radius=6,color='black', fillColor =~pal(Order),fillOpacity = 1, 
                       opacity=1,weight = 2,stroke=T,group='Capture Location',layerId=~UID,
                       popup=popupTable(speciesUniqueSites(), zcol = c("DataSource","StationID","YearsSampled","Order","TotalCount")))%>%
      addCircleMarkers(data=sites_shp,radius=6,
                       color=~'black',stroke=F,fillOpacity=1,
                       group='All Sites',layerId=~StationID_Trend,
                       popup=popupTable(sites_shp, zcol = c("StationID","Year","StationID_Trend","Order"))) %>% hideGroup('All Sites')
    
  })
  
  
  # Tabs for each parameter
  nameOfSpecies <- reactive(input$fishSpecies)
  subpopDO <- reactive(input$DOsubpopInput)
  subpopDOcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopDO(),'/CDF/cdfdataDO.RDS',sep=""))})
  #subpopDOcdf <- reactive({switch(input$DOsubpopInput, 'Mountain'=fishcdfdataDOMountain, 'Piedmont'=fishcdfdataDOPiedmont, 'Coast'=fishcdfdataDOCoast)})
  callModule(plotsOnTab,'DO',fishcdfdataDO,"Fish","DO",nameOfSpecies)
  callModule(plotsOnTabSubpop,'DOsubpop',subpopDOcdf,"Fish","DO",nameOfSpecies,subpopDO)  
  
  subpoppH <- reactive(input$pHsubpopInput)
  subpoppHcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpoppH(),'/CDF/cdfdatapH.RDS',sep=""))})
  #subpoppHcdf <- reactive({switch(input$pHsubpopInput, 'Mountain'=fishcdfdatapHMountain, 'Piedmont'=fishcdfdatapHPiedmont, 'Coast'=fishcdfdatapHCoast)})
  callModule(plotsOnTab,'pH',fishcdfdatapH,"Fish","pH",nameOfSpecies)
  callModule(plotsOnTabSubpop,'pHsubpop',subpoppHcdf,"Fish","pH",nameOfSpecies,subpoppH)  
  
  subpopSpCond <- reactive(input$SpCondsubpopInput)
  subpopSpCondcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopSpCond(),'/CDF/cdfdataSpCond.RDS',sep=""))})
  #subpopSpCondcdf <- reactive({switch(input$SpCondsubpopInput, 'Mountain'=fishcdfdataSpCondMountain, 'Piedmont'=fishcdfdataSpCondPiedmont, 'Coast'=fishcdfdataSpCondCoast)})
  callModule(plotsOnTab,'SpCond',fishcdfdataSpCond,"Fish","SpCond",nameOfSpecies)
  callModule(plotsOnTabSubpop,'SpCondsubpop',subpopSpCondcdf,"Fish","SpCond",nameOfSpecies,subpopSpCond)  
  
  subpopTP <- reactive(input$TPsubpopInput)
  subpopTPcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopTP(),'/CDF/cdfdataTP.RDS',sep=""))})
  #subpopTPcdf <- reactive({switch(input$TPsubpopInput, 'Mountain'=fishcdfdataTPMountain, 'Piedmont'=fishcdfdataTPPiedmont, 'Coast'=fishcdfdataTPCoast)})
  callModule(plotsOnTab,'TP',fishcdfdataTP,"Fish","TP",nameOfSpecies)
  callModule(plotsOnTabSubpop,'TPsubpop',subpopTPcdf,"Fish","TP",nameOfSpecies,subpopTP)  
  
  subpopTN <- reactive(input$TNsubpopInput)
  subpopTNcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopTN(),'/CDF/cdfdataTN.RDS',sep=""))})
  #subpopTNcdf <- reactive({switch(input$TNsubpopInput, 'Mountain'=fishcdfdataTNMountain, 'Piedmont'=fishcdfdataTNPiedmont, 'Coast'=fishcdfdataTNCoast)})
  callModule(plotsOnTab,'TN',fishcdfdataTN,"Fish","TN",nameOfSpecies)
  callModule(plotsOnTabSubpop,'TNsubpop',subpopTNcdf,"Fish","TN",nameOfSpecies,subpopTN)  
  
  subpopTotHab <- reactive(input$TotHabsubpopInput)
  subpopTotHabcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopTotHab(),'/CDF/cdfdataTotHab.RDS',sep=""))})
  #subpopTotHabcdf <- reactive({switch(input$TotHabsubpopInput, 'Mountain'=fishcdfdataTotHabMountain, 'Piedmont'=fishcdfdataTotHabPiedmont, 'Coast'=fishcdfdataTotHabCoast)})
  callModule(plotsOnTab,'TotHab',fishcdfdataTotHab,"Fish","TotHab",nameOfSpecies)
  callModule(plotsOnTabSubpop,'TotHabsubpop',subpopTotHabcdf,"Fish","TotHab",nameOfSpecies,subpopTotHab)  
  
  subpopTotHab_wMD <- reactive(input$TotHab_wMDsubpopInput)
  subpopTotHab_wMDcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopTotHab_wMD(),'/CDF/cdfdataTotHab_wMD.RDS',sep=""))})
  #subpopTotHabcdf <- reactive({switch(input$TotHabsubpopInput, 'Mountain'=fishcdfdataTotHabMountain, 'Piedmont'=fishcdfdataTotHabPiedmont, 'Coast'=fishcdfdataTotHabCoast)})
  callModule(plotsOnTab,'TotHab_wMD',fishcdfdataTotHab_wMD,"Fish","TotHab_wMD",nameOfSpecies)
  callModule(plotsOnTabSubpop,'TotHab_wMDsubpop',subpopTotHabcdf,"Fish","TotHab_wMD",nameOfSpecies,subpopTotHab_wMD)  
  
  subpopLRBS <- reactive(input$LRBSsubpopInput)
  subpopLRBScdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopLRBS(),'/CDF/cdfdataLRBS.RDS',sep=""))})
  #subpopLRBScdf <- reactive({switch(input$LRBSsubpopInput, 'Mountain'=fishcdfdataLRBSMountain, 'Piedmont'=fishcdfdataLRBSPiedmont, 'Coast'=fishcdfdataLRBSCoast)})
  callModule(plotsOnTab,'LRBS',fishcdfdataLRBS,"Fish","LRBS",nameOfSpecies)
  callModule(plotsOnTabSubpop,'LRBSsubpop',subpopLRBScdf,"Fish","LRBS",nameOfSpecies,subpopLRBS)  
  
  subpopwshdImpPCT <- reactive(input$wshdImpPCTsubpopInput)
  subpopwshdImpPCTcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopwshdImpPCT(),'/CDF/cdfdatawshdImpPCT.RDS',sep=""))})
  #subpopwshdImpPCTcdf <- reactive({switch(input$wshdImpPCTsubpopInput, 'Mountain'=fishcdfdatawshdImpPCTMountain, 'Piedmont'=fishcdfdatawshdImpPCTPiedmont, 'Coast'=fishcdfdatawshdImpPCTCoast)})
  callModule(plotsOnTab,'wshdImpPCT',fishcdfdatawshdImpPCT,"Fish","wshdImpPCT",nameOfSpecies)
  callModule(plotsOnTabSubpop,'wshdImpPCTsubpop',subpopwshdImpPCTcdf,"Fish","wshdImpPCT",nameOfSpecies,subpopwshdImpPCT)  
  
  subpopSf <- reactive(input$SfsubpopInput)
  subpopSfcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopSf(),'/CDF/cdfdataSf.RDS',sep=""))})
  #subpopSfcdf <- reactive({switch(input$SfsubpopInput, 'Mountain'=fishcdfdataSfMountain, 'Piedmont'=fishcdfdataSfPiedmont, 'Coast'=fishcdfdataSfCoast)})
  callModule(plotsOnTab,'Sf',fishcdfdataSf,"Fish","Sf",nameOfSpecies)
  callModule(plotsOnTabSubpop,'Sfsubpop',subpopSfcdf,"Fish","Sf",nameOfSpecies,subpopSf)  
  
  subpopCl <- reactive(input$ClsubpopInput)
  subpopClcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Fish/',subpopCl(),'/CDF/cdfdataCl.RDS',sep=""))})
  #subpopClcdf <- reactive({switch(input$ClsubpopInput, 'Mountain'=fishcdfdataClMountain, 'Piedmont'=fishcdfdataClPiedmont, 'Coast'=fishcdfdataClCoast)})
  callModule(plotsOnTab,'Cl',fishcdfdataCl,"Fish","Cl",nameOfSpecies)
  callModule(plotsOnTabSubpop,'Clsubpop',subpopClcdf,"Fish","Cl",nameOfSpecies,subpopCl)  
  
  
  #fishParameterTITAN <- reactive(input$fishTitanOverview)
  #fishParameterTITAN <- reactive({switch(input$fishTitanOverview, 'Dissolved Oxygen'='DO','pH'='pH','Specific Conductivity'='SpCond',
  #                                       'Total Phosphorus'='TP', 'Total Nitrogen'='TN', 'Total Habitat'='TotHab','LRBS'='LRBS',
  #                                       'Percent Impervious'='wshdImpPCT')})
  #callModule(titanOverviewPlotting,'fishTitanOverviewPlot','TITANoverview','Fish',fishParameterTITAN)
  
  
  
  
  
  
  
  ##-------------------------------------------- BUGS -----------------------------------------###
  # Distribution Map
  
  # need to reorganize so get total count per site, regardless of sample date
  
  
  bugspeciesUniqueSites <- reactive({
    req(input$bugSpecies)
    st_as_sf(bugstationBySpeciesUniqueList[input$bugSpecies][[1]], 
             coords = c("LongitudeDD","LatitudeDD"), 
             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
             agr = "constant")
  })
  
  #pal <- colorFactor(c('red','orange','yellow','limegreen','blue','purple','gray'),domain = NULL, levels(sites$Order), ordered=T)
  
  output$BUG_distMap <- renderLeaflet({
    leaflet(sites_shp) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,group='Nat Geo World Map') %>%
      #addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape') %>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addPolygons(data=Bioregions,color='navy',fill=0.9,stroke=0.1,group="Bioregions",
                  popup=paste("Basin: ",Bioregions@data$BASIN,sep="")) %>% hideGroup('Bioregions') %>%
      addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
                  popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep="")) %>% hideGroup('Ecoregions') %>%
      addLegend("bottomright",pal=pal, values=levels(sites$Order),title="Stream Order") %>%
      addLayersControl(baseGroups=c('Nat Geo World Map','Esri World Imagery','Open Street Map'),
                       overlayGroups = c('Bioregions','Ecoregions','All Sites','Caputre Location'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()
  })
  
  observe({
    leafletProxy('BUG_distMap', data= bugspeciesUniqueSites()) %>% clearMarkers() %>%
      addCircleMarkers(radius=6,color='black', fillColor =~pal(Order),fillOpacity = 1, 
                       opacity=1,weight = 2,stroke=T,group='Capture Location',layerId=~UID,
                       popup=popupTable(bugspeciesUniqueSites(), zcol = c("DataSource","StationID","YearsSampled","Order","TotalCount")))%>%
      addCircleMarkers(data=sites_shp,radius=6,
                       color=~'black',stroke=F,fillOpacity=1,
                       group='All Sites',layerId=~StationID_Trend,
                       popup=popupTable(sites_shp, zcol = c("StationID","Year","StationID_Trend","Order"))) %>% hideGroup('All Sites')
  })
  
  # Tabs for each parameter
  bugnameOfSpecies <- reactive(ifelse(grepl('/',input$bugSpecies), 
                                      gsub(input$bugSpecies,pattern = "/", replacement = "_"),
                                      input$bugSpecies))
  bugsubpopDO <- reactive(input$bugDOsubpopInput)
  bugsubpopDOcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopDO(),'/CDF/cdfdataDO.RDS',sep=""))})
  callModule(plotsOnTab,'bugDO',bugcdfdataDO,"Bug","DO",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugDOsubpop',bugsubpopDOcdf,"Bug","DO",bugnameOfSpecies,bugsubpopDO)  
  
  bugsubpoppH <- reactive(input$bugpHsubpopInput)
  bugsubpoppHcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpoppH(),'/CDF/cdfdatapH.RDS',sep=""))})
  callModule(plotsOnTab,'bugpH',bugcdfdatapH,"Bug","pH",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugpHsubpop',bugsubpoppHcdf,"Bug","pH",bugnameOfSpecies,bugsubpoppH)  
  
  bugsubpopSpCond <- reactive(input$bugSpCondsubpopInput)
  bugsubpopSpCondcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopSpCond(),'/CDF/cdfdataSpCond.RDS',sep=""))})
  callModule(plotsOnTab,'bugSpCond',bugcdfdataSpCond,"Bug","SpCond",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugSpCondsubpop',bugsubpopSpCondcdf,"Bug","SpCond",bugnameOfSpecies,bugsubpopSpCond)  
  
  bugsubpopTP <- reactive(input$bugTPsubpopInput)
  bugsubpopTPcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopTP(),'/CDF/cdfdataTP.RDS',sep=""))})
  callModule(plotsOnTab,'bugTP',bugcdfdataTP,"Bug","TP",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugTPsubpop',bugsubpopTPcdf,"Bug","TP",bugnameOfSpecies,bugsubpopTP)  
  
  bugsubpopTN <- reactive(input$bugTNsubpopInput)
  bugsubpopTNcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopTN(),'/CDF/cdfdataTN.RDS',sep=""))})
  callModule(plotsOnTab,'bugTN',bugcdfdataTN,"Bug","TN",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugTNsubpop',bugsubpopTNcdf,"Bug","TN",bugnameOfSpecies,bugsubpopTN)  
  
  bugsubpopTotHab <- reactive(input$bugTotHabsubpopInput)
  bugsubpopTotHabcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopTotHab(),'/CDF/cdfdataTotHab.RDS',sep=""))})
  callModule(plotsOnTab,'bugTotHab',bugcdfdataTotHab,"Bug","TotHab",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugTotHabsubpop',bugsubpopTotHabcdf,"Bug","TotHab",bugnameOfSpecies,bugsubpopTotHab)  
  
  bugsubpopTotHab_wMD <- reactive(input$bugTotHab_wMDsubpopInput)
  bugsubpopTotHab_wMDcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopTotHab(),'/CDF/cdfdataTotHab_wMD.RDS',sep=""))})
  callModule(plotsOnTab,'bugTotHab_wMD',bugcdfdataTotHab_wMD,"Bug","TotHab_wMD",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugTotHab_wMDsubpop',bugsubpopTotHab_wMDcdf,"Bug","TotHab_wMD",bugnameOfSpecies,bugsubpopTotHab_wMD)  
  
  bugsubpopLRBS <- reactive(input$bugLRBSsubpopInput)
  bugsubpopLRBScdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopLRBS(),'/CDF/cdfdataLRBS.RDS',sep=""))})
  callModule(plotsOnTab,'bugLRBS',bugcdfdataLRBS,"Bug","LRBS",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugLRBSsubpop',bugsubpopLRBScdf,"Bug","LRBS",bugnameOfSpecies,bugsubpopLRBS)  
  
  bugsubpopwshdImpPCT <- reactive(input$bugwshdImpPCTsubpopInput)
  bugsubpopwshdImpPCTcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopwshdImpPCT(),'/CDF/cdfdatawshdImpPCT.RDS',sep=""))})
  callModule(plotsOnTab,'bugwshdImpPCT',bugcdfdatawshdImpPCT,"Bug","wshdImpPCT",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugwshdImpPCTsubpop',bugsubpopwshdImpPCTcdf,"Bug","wshdImpPCT",bugnameOfSpecies,bugsubpopwshdImpPCT)  
  
  bugsubpopSf <- reactive(input$bugSfsubpopInput)
  bugsubpopSfcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopSf(),'/CDF/cdfdataSf.RDS',sep=""))})
  callModule(plotsOnTab,'bugSf',bugcdfdataSf,"Bug","Sf",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugSfsubpop',bugsubpopSfcdf,"Bug","Sf",bugnameOfSpecies,bugsubpopSf)  
  
  bugsubpopCl <- reactive(input$bugClsubpopInput)
  bugsubpopClcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/Bug/',bugsubpopCl(),'/CDF/cdfdataCl.RDS',sep=""))})
  callModule(plotsOnTab,'bugCl',bugcdfdataCl,"Bug","Cl",bugnameOfSpecies)
  callModule(plotsOnTabSubpop,'bugClsubpop',bugsubpopClcdf,"Bug","Cl",bugnameOfSpecies,bugsubpopCl)  
  
  #bugParameterTITAN <- reactive({switch(input$bugTitanOverview, 'Dissolved Oxygen'='DO','pH'='pH','Specific Conductivity'='SpCond',
  #                                      'Total Phosphorus'='TP', 'Total Nitrogen'='TN', 'Total Habitat'='TotHab','LRBS'='LRBS',
  #                                      'Percent Impervious'='wshdImpPCT')})
  #callModule(titanOverviewPlotting,'bugTitanOverviewPlot','TITANoverview','Bug',bugParameterTITAN)
  
  
  
  ##-------------------------------------------- FALL BUGS -----------------------------------------###
  # Distribution Map
  
  # need to reorganize so get total count per site, regardless of sample date
  
  
  fallbugspeciesUniqueSites <- reactive({
    req(input$fallbugSpecies)
    st_as_sf(fallbugstationBySpeciesUniqueList[input$fallbugSpecies][[1]], 
             coords = c("LongitudeDD","LatitudeDD"), 
             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
             agr = "constant")
  })
  
  #pal <- colorFactor(c('red','orange','yellow','limegreen','blue','purple','gray'),domain = NULL, levels(sites$Order), ordered=T)
  
  output$FALLBUG_distMap <- renderLeaflet({
    leaflet(sites_shp) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,group='Nat Geo World Map') %>%
      #addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape') %>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addPolygons(data=Bioregions,color='navy',fill=0.9,stroke=0.1,group="Bioregions",
                  popup=paste("Basin: ",Bioregions@data$BASIN,sep="")) %>% hideGroup('Bioregions') %>%
      addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
                  popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep="")) %>% hideGroup('Ecoregions') %>%
      addLegend("bottomright",pal=pal, values=levels(sites$Order),title="Stream Order") %>%
      addLayersControl(baseGroups=c('Nat Geo World Map','Esri World Imagery','Open Street Map'),
                       overlayGroups = c('Bioregions','Ecoregions','All Sites','Caputre Location'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()
  })
  
  observe({
    leafletProxy('FALLBUG_distMap', data= fallbugspeciesUniqueSites()) %>% clearMarkers() %>%
      addCircleMarkers(radius=6,color='black', fillColor =~pal(Order),fillOpacity = 1, 
                       opacity=1,weight = 2,stroke=T,group='Capture Location',layerId=~UID,
                       popup=popupTable(fallbugspeciesUniqueSites(), zcol = c("DataSource","StationID","YearsSampled","Order","TotalCount")))%>%
      addCircleMarkers(data=sites_shp,radius=6,
                       color=~'black',stroke=F,fillOpacity=1,
                       group='All Sites',layerId=~StationID_Trend,
                       popup=popupTable(sites_shp, zcol = c("StationID","Year","StationID_Trend","Order"))) %>% hideGroup('All Sites')
  })
  
  # Tabs for each parameter
  fallbugnameOfSpecies <- reactive(ifelse(grepl('/',input$fallbugSpecies), 
                                          gsub(input$fallbugSpecies,pattern = "/", replacement = "_"),
                                          input$fallbugSpecies))
  fallbugsubpopDO <- reactive(input$fallbugDOsubpopInput)
  fallbugsubpopDOcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopDO(),'/CDF/cdfdataDO.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugDO',fallbugcdfdataDO,"fallBug","DO",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugDOsubpop',fallbugsubpopDOcdf,"fallBug","DO",fallbugnameOfSpecies,fallbugsubpopDO)  
  
  fallbugsubpoppH <- reactive(input$fallbugpHsubpopInput)
  fallbugsubpoppHcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpoppH(),'/CDF/cdfdatapH.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugpH',fallbugcdfdatapH,"fallBug","pH",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugpHsubpop',fallbugsubpoppHcdf,"fallBug","pH",fallbugnameOfSpecies,fallbugsubpoppH)  
  
  fallbugsubpopSpCond <- reactive(input$fallbugSpCondsubpopInput)
  fallbugsubpopSpCondcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopSpCond(),'/CDF/cdfdataSpCond.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugSpCond',fallbugcdfdataSpCond,"fallBug","SpCond",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugSpCondsubpop',fallbugsubpopSpCondcdf,"fallBug","SpCond",fallbugnameOfSpecies,fallbugsubpopSpCond)  
  
  fallbugsubpopTP <- reactive(input$fallbugTPsubpopInput)
  fallbugsubpopTPcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopTP(),'/CDF/cdfdataTP.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugTP',fallbugcdfdataTP,"fallBug","TP",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugTPsubpop',fallbugsubpopTPcdf,"fallBug","TP",fallbugnameOfSpecies,fallbugsubpopTP)  
  
  fallbugsubpopTN <- reactive(input$fallbugTNsubpopInput)
  fallbugsubpopTNcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopTN(),'/CDF/cdfdataTN.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugTN',fallbugcdfdataTN,"fallBug","TN",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugTNsubpop',fallbugsubpopTNcdf,"fallBug","TN",fallbugnameOfSpecies,fallbugsubpopTN)  
  
  fallbugsubpopTotHab <- reactive(input$fallbugTotHabsubpopInput)
  fallbugsubpopTotHabcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopTotHab(),'/CDF/cdfdataTotHab.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugTotHab',fallbugcdfdataTotHab,"fallBug","TotHab",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugTotHabsubpop',fallbugsubpopTotHabcdf,"fallBug","TotHab",fallbugnameOfSpecies,fallbugsubpopTotHab)  
  
  fallbugsubpopTotHab_wMD <- reactive(input$fallbugTotHab_wMDsubpopInput)
  fallbugsubpopTotHab_wMDcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopTotHab(),'/CDF/cdfdataTotHab_wMD.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugTotHab_wMD',fallbugcdfdataTotHab_wMD,"fallBug","TotHab_wMD",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugTotHab_wMDsubpop',fallbugsubpopTotHab_wMDcdf,"fallBug","TotHab_wMD",fallbugnameOfSpecies,fallbugsubpopTotHab_wMD)  
  
  fallbugsubpopLRBS <- reactive(input$fallbugLRBSsubpopInput)
  fallbugsubpopLRBScdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopLRBS(),'/CDF/cdfdataLRBS.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugLRBS',fallbugcdfdataLRBS,"fallBug","LRBS",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugLRBSsubpop',fallbugsubpopLRBScdf,"fallBug","LRBS",fallbugnameOfSpecies,fallbugsubpopLRBS)  
  
  fallbugsubpopwshdImpPCT <- reactive(input$fallbugwshdImpPCTsubpopInput)
  fallbugsubpopwshdImpPCTcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopwshdImpPCT(),'/CDF/cdfdatawshdImpPCT.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugwshdImpPCT',fallbugcdfdatawshdImpPCT,"fallBug","wshdImpPCT",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugwshdImpPCTsubpop',fallbugsubpopwshdImpPCTcdf,"fallBug","wshdImpPCT",fallbugnameOfSpecies,fallbugsubpopwshdImpPCT)  
  
  fallbugsubpopSf <- reactive(input$fallbugSfsubpopInput)
  fallbugsubpopSfcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopSf(),'/CDF/cdfdataSf.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugSf',fallbugcdfdataSf,"fallBug","Sf",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugSfsubpop',fallbugsubpopSfcdf,"fallBug","Sf",fallbugnameOfSpecies,fallbugsubpopSf)  
  
  fallbugsubpopCl <- reactive(input$fallbugClsubpopInput)
  fallbugsubpopClcdf <- reactive({readRDS(paste('www/FinalDataAndPlots/Bioregion/fallBug/',fallbugsubpopCl(),'/CDF/cdfdataCl.RDS',sep=""))})
  callModule(plotsOnTab,'fallbugCl',fallbugcdfdataCl,"fallBug","Cl",fallbugnameOfSpecies)
  callModule(plotsOnTabSubpop,'fallbugClsubpop',fallbugsubpopClcdf,"fallBug","Cl",fallbugnameOfSpecies,fallbugsubpopCl)  
  
  #fallbugParameterTITAN <- reactive({switch(input$fallbugTitanOverview, 'Dissolved Oxygen'='DO','pH'='pH','Specific Conductivity'='SpCond',
  #                                          'Total Phosphorus'='TP', 'Total Nitrogen'='TN', 'Total Habitat'='TotHab','LRBS'='LRBS',
  #                                          'Percent Impervious'='wshdImpPCT')})
 # callModule(titanOverviewPlotting,'fallbugTitanOverviewPlot','TITANoverview','fallBug',bugParameterTITAN)
})
