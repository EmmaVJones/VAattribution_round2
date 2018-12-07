# in R 3.4.4
library(tidyverse)
library(readxl)


# Combine Virginia and Fairfax environmental data

# Bring in Fairfax dataset
FFE <- readxl::read_excel('data/FairFax/DEQ BCG Taxa Data v3.xlsx',sheet='Environmental')

# Bring in DEQ environmental data (from 2018IR)
enviroSmash <- readxl::read_excel('data/VA/Wadeable_ProbMon_2001-2016_EVJ.xlsx',
                                  sheet='Wadeable_ProbMon_2001-2016_EVJ')


# Get Fairfax variables to match Envirosmash variables
FFE_smash <- mutate(FFE,
                    EcoRegion = US_L3NAME,
                    BioRegion = ifelse(US_L3NAME=="Southeastern Plains",'Coast',"Piedmont"),
                    TotHab = TotalHab,
                    DO = DO.med,
                    SpCond = SpCond.med,
                    pH = pH.med,
                    TN = TN.med,
                    TP = TP.med) %>%
  # modeled TN if no TN measured
  mutate(TN = ifelse(is.na(TN), 0.523 + 0.897 * NOX.med, TN)) %>%
  select(DataSource,StationID,Year,LongitudeDD,LatitudeDD,EcoRegion,wshdImpPCT,
         totalArea_sqMile,sqMileImp,TotHab,DO,SpCond,pH,TN,TP)

# Add columns to FFE_smash if they arent in there 
n1 <- unique(names(enviroSmash))
n2 <- unique(names(FFE_smash))
needToAdd <- n1[!(n1 %in% n2)]
FFE_smash1 <- FFE_smash
FFE_smash1[needToAdd] <- NA
names(FFE_smash1) %in% names(enviroSmash)
FFE_smash2 <- FFE_smash1[,names(enviroSmash)]

# now drop Fairfax benthic and fish trend sites
FFE_smash3 <- full_join(FFE_smash2,FFE[,1:5],by=c("DataSource","StationID","Year")) %>%
  filter(is.na(BenthicTrend_Drop) & is.na(FishTrend_Drop)) %>%
  select(-c(BenthicTrend_Drop,FishTrend_Drop))


# match Basin information
library(rgdal)
superB <- readOGR('C:/GIS/EmmaGIS/WGS84projectionsforLeaflet','VAsuperbasins')
basins <- readOGR('C:/GIS/EmmaGIS/WGS84projectionsforLeaflet','VAbasins')

sites_shp <- select(FFE_smash3,StationID,LatitudeDD,LongitudeDD)
coordinates(sites_shp) <- ~LongitudeDD+LatitudeDD
proj4string(sites_shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")                                 
FFE_smash4 <- FFE_smash3

for(i in 1:nrow(FFE_smash4)){
  FFE_smash4$Basin[i] <- as.character(over(sites_shp[i,],superB)$SUPERBASIN)
  FFE_smash4$SubBasin[i] <- as.character(over(sites_shp[i,],basins)$BASIN)
}


# Add Fairfax stream order (only got dataset from Jonathan Witt after the workshop so wasn't reflected
#  in original workshop datasets)
FForder <- read_excel('data/Fairfax/FFX Stream Orders.xlsx')

FFE_smash5 <- full_join(FFE_smash4,FForder, by='StationID') %>%
  mutate(Order=Order.y) %>%
  select(-c(Order.x,Order.y))%>%
  select(DataSource, StationID, Year, StationID_Trend, LongitudeDD, LatitudeDD, 
         stratum, designweight, weightcategory, station, state, status, comment, 
         set, Basin, SubBasin, BayShed, BayPanel, EcoRegion, BioRegion, Panel, 
         BioPanel, Order,everything())

names(FFE_smash5) == names(enviroSmash)

# Finally Combine the two datasets
smash <- rbind(enviroSmash,FFE_smash5)

write.csv(smash,'data/processedData/VAandFairfax_envData.csv', row.names = F)
