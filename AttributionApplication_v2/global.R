# Built in R version 3.4.4

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(mapview)
library(rgdal)
library(plotly)
library(sf)


fishSp <- read_csv('data/finalFishData.csv') %>%
  select(StationID,Year,UID,`Acantharchus pomotis`:`Umbra pygmaea`)
fishSp$Year <- as.numeric(as.character(fishSp$Year))


sites <- read.csv('data/finalEnvData.csv')
sites$Order <- factor(sites$Order, ordered = TRUE, levels = c('1','2','3','4','5','6','NA'))


sites_shp <- st_as_sf(dplyr::select(sites,StationID:LatitudeDD,Order),coords=c('LongitudeDD','LatitudeDD'),
                      crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",agr = 'constant')%>%
  mutate(UID= paste(StationID,Year,sep='_'))

stationBySpeciesUniqueList  <- readRDS('data/fishStationsUnique.RDS')
bugstationBySpeciesUniqueList  <- readRDS('data/springbugStationsUnique.RDS')
fallbugstationBySpeciesUniqueList  <- readRDS('data/fallbugStationsUnique.RDS')

fishcdfdataDO <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataDO.RDS')
fishcdfdatapH <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdatapH.RDS')
fishcdfdataSpCond <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataSpCond.RDS')
fishcdfdataTP <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataTP.RDS')
fishcdfdataTN <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataTN.RDS')
fishcdfdataTotHab <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataTotHab.RDS')
fishcdfdataTotHab_wMD <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataTotHab_wMD.RDS')
fishcdfdataLRBS <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataLRBS.RDS')
fishcdfdatawshdImpPCT <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdatawshdImpPCT.RDS')
fishcdfdataSf <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataSf.RDS')
fishcdfdataCl <- readRDS('www/FinalDataAndPlots/Fish/CDF/cdfdataCl.RDS')
bugcdfdataDO <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataDO.RDS')
bugcdfdatapH <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdatapH.RDS')
bugcdfdataSpCond <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataSpCond.RDS')
bugcdfdataTP <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataTP.RDS')
bugcdfdataTN <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataTN.RDS')
bugcdfdataTotHab <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataTotHab.RDS')
bugcdfdataTotHab_wMD <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataTotHab_wMD.RDS')
bugcdfdataLRBS <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataLRBS.RDS')
bugcdfdatawshdImpPCT <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdatawshdImpPCT.RDS')
bugcdfdataSf <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataSf.RDS')
bugcdfdataCl <- readRDS('www/FinalDataAndPlots/Bug/CDF/cdfdataCl.RDS')
fallbugcdfdataDO <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataDO.RDS')
fallbugcdfdatapH <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdatapH.RDS')
fallbugcdfdataSpCond <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataSpCond.RDS')
fallbugcdfdataTP <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataTP.RDS')
fallbugcdfdataTN <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataTN.RDS')
fallbugcdfdataTotHab <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataTotHab.RDS')
fallbugcdfdataTotHab_wMD <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataTotHab_wMD.RDS')
fallbugcdfdataLRBS <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataLRBS.RDS')
fallbugcdfdatawshdImpPCT <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdatawshdImpPCT.RDS')
fallbugcdfdataSf <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataSf.RDS')
fallbugcdfdataCl <- readRDS('www/FinalDataAndPlots/fallBug/CDF/cdfdataCl.RDS')



plottingUI <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns('image'),height = '500px',width = '25%')
  )
}
plotting <- function(input,output,session,model,fishbug,parameter,sp){
  output$image <- renderImage({
    #filename <- normalizePath(file.path('./www/TestDataAndPlots/',model,parameter, paste(sp(),'.jpg', sep='')))
    filename <- normalizePath(file.path('./www/FinalDataAndPlots/',fishbug,model,parameter, paste(sp(),'.jpg', sep='')))
#    print(filename)
    list(src = filename,alt = paste('No ',model,' plot available for species.',sep=''))
  }, deleteFile = FALSE)
}






plotsOnTabUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('CDF'),width = '500px'),
    DT::dataTableOutput(ns('cdfTable'), width = "75%"),
    plottingUI(ns('gam')),
    DT::dataTableOutput(ns('gamTable'), width = "75%")#,
    #plottingUI(ns('titan')),
    #DT::dataTableOutput(ns('titanTable'), width = "75%")
  )
}

plotsOnTab <- function(input,output,session,cdfdata,fishbug,parameter,sp){
  output$CDF <- renderPlotly({CDFplot(cdfdata,sp())})
  #callModule(plotting,"glm",'GLM',parameter,sp)
  
  cdfStats <- reactive({
    cdfstats <- select(cdfdata, value, starts_with('csum')) %>%
      select(value, contains(sp()))
    names(cdfstats)[2] <- 'taxa'
    
    return(data.frame(Taxa = sp(), 
                      x5 = format(cdfstats[findInterval(0.05, cdfstats$taxa),1],digits=4),
                      x25 = format(cdfstats[findInterval(0.25, cdfstats$taxa),1],digits=4),
                      x50 = format(cdfstats[findInterval(0.5, cdfstats$taxa),1],digits=4),
                      x75 = format(cdfstats[findInterval(0.75, cdfstats$taxa),1],digits=4),
                      x95 = format(cdfstats[findInterval(0.95, cdfstats$taxa),1],digits=4)))
  })
  
  output$cdfTable <- DT::renderDataTable({
    req(cdfStats())
    DT::datatable(cdfStats(), rownames = F,options=list(dom='t'), colnames = c('Taxa',"5th", "25th", '50th', '75th','95th'))
  })
  
  
  gamStats <- reactive({ 
    #print(sp())
    #print(parameter)
    #print(paste('www/FinalDataAndPlots/',fishbug,'/GAM/',parameter,'/',parameter,'.RDS',sep=""))
    
    readRDS(paste('www/FinalDataAndPlots/',fishbug,'/GAM/',parameter,'/',parameter,'.RDS',sep="")) %>%
      select(tnames,N,GAM_50_th,GAM_95_th) %>% 
      filter(tnames==sp())})
  output$gamTable <- DT::renderDataTable({
    req(gamStats())
#    print(head(gamStats()))
    DT::datatable(gamStats(), rownames = F,options=list(dom='t'), colnames = c('Taxa', "n",'GAM 50th', 'GAM 95th'))
  })
  
  
  #titanStats <- reactive({ 
  #  print(paste('www/FinalDataAndPlots/',fishbug,'/Titan/',parameter,'/sppmax.RDS',sep=""))
  #  readRDS(paste('www/FinalDataAndPlots/',fishbug,'/Titan/',parameter,'/sppmax.RDS',sep="")) %>%
  #    filter(Taxa==sp())%>%select(Taxa,freq,zenv.cp,purity,reliability,`5%`,`50%`,`95%`,filter)})
  
  #output$titanTable <- DT::renderDataTable({
  #  req(titanStats())
  #  DT::datatable(titanStats(), rownames = F,options=list(dom='t',scrollX = TRUE)) %>%
  #    DT::formatRound(c('zenv.cp','purity','reliability','5%','50%','95%'), 2)
  #})
  
  
  callModule(plotting,"gam",'GAM',fishbug,parameter,sp)
  #callModule(plotting,"titan",'Titan',fishbug,parameter,sp)
  
  
}




plottingSubpopUI <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns('image'),height = '500px',width = '25%')
  )
}
plottingSubpop <- function(input,output,session,model,fishbug,parameter,sp,subpop){
  output$image <- renderImage({
    filename <- normalizePath(file.path('./www/FinalDataAndPlots/Bioregion/',fishbug,subpop(),model,parameter, paste(sp(),'.jpg', sep='')))
    print(filename)
    list(src = filename,alt = paste('No ',model,' plot available for species.',sep=''))
  }, deleteFile = FALSE)
}



plotsOnTabSubpopUI <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns('CDF'),width = '500px'),
    DT::dataTableOutput(ns('cdfTable'), width = "75%"),
    #verbatimTextOutput(ns('test')),
    plottingUI(ns('gam')),
    DT::dataTableOutput(ns('gamTable'), width = "75%")#,
    #plottingUI(ns('titan')),
    #DT::dataTableOutput(ns('titanTable'), width = "75%")
  )
}
plotsOnTabSubpop <- function(input,output,session,cdfdata,fishbug,parameter,sp,subpop){
  output$CDF <- renderPlotly({CDFplot(cdfdata(),sp())})
  #callModule(plotting,"glm",'GLM',parameter,sp)
  
  cdfStats <- reactive({
    cdfstats <- select(cdfdata(), value, starts_with('csum')) %>%
      select(value, contains(sp()))
    names(cdfstats)[2] <- 'taxa'
    if(!is.nan(cdfstats[,2])){
      return(data.frame(Taxa = sp(), 
                        x5 = format(cdfstats[findInterval(0.05, cdfstats$taxa),1],digits=4),
                        x25 = format(cdfstats[findInterval(0.25, cdfstats$taxa),1],digits=4),
                        x50 = format(cdfstats[findInterval(0.5, cdfstats$taxa),1],digits=4),
                        x75 = format(cdfstats[findInterval(0.75, cdfstats$taxa),1],digits=4),
                        x95 = format(cdfstats[findInterval(0.95, cdfstats$taxa),1],digits=4)))
    }
  })
  
  
  output$cdfTable <- DT::renderDataTable({
    req(cdfStats())
    DT::datatable(cdfStats(), rownames = F,options=list(dom='t'), colnames = c('Taxa',"5th", "25th", '50th', '75th','95th'))
  })
  gamStats <- reactive({ 
    #print(paste('subpop:   ',sp(),sep=''))
    #print(paste('subpop:   ',parameter,sep=''))
    #print(paste('subpop:   ','www/FinalDataAndPlots/Bioregion/',fishbug,'/',subpop(),'/GAM/',parameter,'/',parameter,'.RDS',sep=""))
    readRDS(paste('www/FinalDataAndPlots/Bioregion/',fishbug,'/',subpop(),'/GAM/',parameter,'/',parameter,'.RDS',sep="")) %>%
      select(tnames,N,GAM_50_th,GAM_95_th) %>% 
      filter(tnames==sp())})
  
  output$gamTable <- DT::renderDataTable({
    req(gamStats())
#    print(head(gamStats()))
    DT::datatable(gamStats(), rownames = F,options=list(dom='t'), colnames = c('Taxa', "n",'GAM 50th', 'GAM 95th'))
  })
  
  
  #titanStats <- reactive({ 
  #  readRDS(paste('www/FinalDataAndPlots/Bioregion/',fishbug,'/',subpop(),'/Titan/',parameter,'/sppmax.RDS',sep="")) %>%
  #    filter(Taxa==sp())%>%select(Taxa,freq,zenv.cp,purity,reliability,`5%`,`50%`,`95%`,filter)})
  #output$titanTable <- DT::renderDataTable({
  #  req(titanStats())
  #  DT::datatable(titanStats(), rownames = F,options=list(dom='t',scrollX = TRUE))%>%
  #    DT::formatRound(c('zenv.cp','purity','reliability','5%','50%','95%'), 2)
  #})
  
  callModule(plottingSubpop,"gam",'GAM',fishbug,parameter,sp,subpop)
  #callModule(plottingSubpop,"titan",'Titan',fishbug,parameter,sp,subpop)
  
}



# Interactive CDF plot with plotly
CDFplot <- function(CDFdataset,taxaName){
  # Double spaces are a pain
  keep <- c('parameter','value', paste('csum_',taxaName,sep=""))
  ggData <- CDFdataset[,match(keep,names(CDFdataset))] %>%
    dplyr::rename(pct=!!names(.[3]))
  plot_ly(ggData) %>%
    add_trace(x = ~value, y = ~pct, mode = 'scatter', name=taxaName,
              hoverinfo="text",text=~paste(format(pct,digits=2)," , ",format(value,digits=2))) %>%
    #add_trace(x = ~value, y = 0.5, mode = 'scatter', name=taxaName,
    #          hoverinfo="text",text=~paste('50th Percentile')) %>%
    #add_trace(x = ~value, y = 0.95, mode = 'scatter', name=taxaName,
    #          hoverinfo="text",text=~paste('95th Percentile')) %>%
    layout(showlegend = FALSE,
           title=taxaName,
           yaxis = list(title = "Proportion of Total"),
           xaxis = list(title = prettyXlabel(ggData$parameter[1])))
} 

# function to get better x axis label
prettyXlabel <- function(parameter){
  if(parameter=="DO"){return('Dissolved Oxygen (mg/L)')}
  if(parameter=="pH"){return('pH (Standard Units)')}
  if(parameter=="SpCond"){return("Specific Conductivity (uS/cm)")}#expression(paste("Specific Conductivity ( ", mu, "S/cm)")))} # plotly doesnt support expression or mathjax easily
  if(parameter=="TN"){return('Total Nitrogen (mg/L)')}
  if(parameter=="TP"){return('Total Phosphorus (mg/L)')}
  if(parameter=='TDS'){return('Total Dissolved Solids (mg/L)')}
  if(parameter=="NH4"){return('Ammonia Nitrogen (mg/L)')}
  if(parameter=="NO3"){return('Total Nitrate Nitrogen (mg/L)')}
  if(parameter=="TKN"){return('Total Kjendahl Nitrogen (mg/L)')}
  if(parameter=="Ortho-P"){return("Total Ortho-Phosphorus (mg/L)")}
  if(parameter== "Turb" ){return('Turbidity (NTU)')}
  if(parameter== "TSS" ){return('Total Suspended Solids (mg/L)')}
  if(parameter=="Na"){return("Dissolved Sodium (mg/L)")}
  if(parameter=="K"){return("Dissolved Potassium (mg/L)")}
  if(parameter=="Cl"){return("Dissolved Chlorides (mg/L)")}
  if(parameter=="Sf"){return("Dissolved Sulfates (mg/L)")}
  if(parameter=="70331VFine"){return("% Fine Sediment")}
  if(parameter=="SSCCOARSE"){return("%SSC Coarse")}
  if(parameter=="SSCFINE"){return("%SSC Fine")}
  if(parameter=="SSCTOTAL" ){return("SSC Course + SSC Fine Combined")}
  if(parameter=="LRBS"){return("Logged Relative Bed Stability")}
  if(parameter=="Slope"){return("% Slope")} 
  if(parameter=="FN_PCT"){return("% Fine Sediment")}
  if(parameter=="SA_PCT"){return("% Sand")}
  if(parameter=="SA_FN_PCT"){return("%Fine + %Sand")}
  if(parameter=="LSUB_DMM"){return("Logged Mean Particle Size in Reach")}
  if(parameter=="BL_CB_GR_Embed_PCT"){return("% Embeddedness of Bolders,Cobbles, and Gravels")}
  if(parameter=="Embed_PCT"){return("% Embeddedness All Particles")}
  if(parameter=="TotHab"){return("Total Habitat Score (RBP) ")}
  if(parameter=="TotHab_wMD"){return("Total Habitat Score, with MD (RBP) ")}
  if(parameter=="Sf"){return("Sulfate (mg/L) ")}
  if(parameter=="Cl"){return("Chloride (mg/L) ")}
  if(parameter=="MetalCCU"){return("Metal chronic criterion unit (Index of multiple metals)")}
}

#titanOverviewPlottingUI <- function(id){
#  ns <- NS(id)
#  tagList(
#    plotOutput(ns('image'),height = '800px',width = '80%'),
#    fluidRow(
#      column(6,h3(strong('Gainers')),DT::dataTableOutput(ns('titanStatsWinner'),width = '95%')),
#      column(6,h3(strong('Losers')),DT::dataTableOutput(ns('titanStatsLoser'),width = '95%')))
#  )
#}

#titanOverviewPlotting <- function(input,output,session,model,fishBug,parameter){
#  output$image <- renderImage({
#    filename <- normalizePath(file.path('./www/FinalDataAndPlots/',model, paste(fishBug,'/titan',parameter(),'.jpg', sep='')))
#    print(filename)
#    list(src = filename,alt = paste('No ',model,' plot available for stressor.',sep=''))
#  }, deleteFile = FALSE)
#  
#  titanOverallStatsWinner <- reactive({
#    #print(paste('www/FinalDataAndPlots/',model,'/', fishBug,'/titan',parameter(),'.RDS',sep=""))
#    #readRDS(paste('www/FinalDataAndPlots/',model,'/', fishBug,'/titan',parameter(),'.RDS',sep=""))
#    #print(paste('www/FinalDataAndPlots/',fishBug,'/Titan/',parameter(),'/sppmax.RDS',sep=""))
#    readRDS(paste('www/FinalDataAndPlots/',fishBug,'/Titan/',parameter(),'/sppmax.RDS',sep="")) %>%filter(filter==1)})
#  titanOverallStatsLoser <- reactive({
#    #print(paste('www/FinalDataAndPlots/',model,'/', fishBug,'/titan',parameter(),'.RDS',sep=""))
#    #readRDS(paste('www/FinalDataAndPlots/',model,'/', fishBug,'/titan',parameter(),'.RDS',sep=""))
#    #print(paste('www/FinalDataAndPlots/',fishBug,'/Titan/',parameter(),'/sppmax.RDS',sep=""))
#    readRDS(paste('www/FinalDataAndPlots/',fishBug,'/Titan/',parameter(),'/sppmax.RDS',sep="")) %>%filter(filter==2)})
#  
  
#  output$titanStatsWinner <- DT::renderDataTable({
#    req(titanOverallStatsWinner())
#    #print(head(titanOverallStatsWinner()))
#    DT::datatable(titanOverallStatsWinner(), rownames = F,options=list(pageLength= nrow(titanOverallStatsWinner()),
#                                                                       dom='ft',scrollX = TRUE))#, colnames = c('Taxa', "n",'GAM 50th', 'GAM 95th'))
#  })
  
#  output$titanStatsLoser <- DT::renderDataTable({
#    req(titanOverallStatsLoser())
#    #print(head(titanOverallStatsLoser()))
#    DT::datatable(titanOverallStatsLoser(), rownames = F,options=list(pageLength= nrow(titanOverallStatsLoser()),
#                                                                      dom='ft',scrollX = TRUE))#, colnames = c('Taxa', "n",'GAM 50th', 'GAM 95th'))
#  })
#}
