# this script pulls together stats from teh GAM dataset for Ben's spreadsheet


library(tidyverse)

# jason fill this out ################## ######################
fileExtensionToGAMdata <- 'FinalDataAndPlots/Fish/GAM'
#fileExtensionToGAMdata <- 'FinalDataAndPlots/Bug/GAM'
#fileExtensionToGAMdata <- 'FinalDataAndPlots/BugFall/GAM'

################################################################

dataToSiftThrough <- c('DO','pH','SpCond','TP','TN','TotHab','TotHab_wMD','LRBS','wshdImpPCT','Cl','Sf')

# doing lots of stuff so keep it organized in a list of dataframes
datalist <- list()
out <- data.frame(Taxa=NA, x5=NA, x25=NA, x50=NA, x75=NA, x95=NA)

for(i in 1:length(dataToSiftThrough)){
  # bring in Fish GAM data
  dat <- readRDS(paste(fileExtensionToGAMdata,'/',dataToSiftThrough[i],'/',dataToSiftThrough[i],'.RDS',sep='')) %>%
    select(tnames,N,`GAM_50_th`,`GAM_95_th`) %>%
    mutate(Parameter = dataToSiftThrough[i])
  datalist[[dataToSiftThrough[i]]] <- dat
  
}

# now you can look at each parameter data frame by doing 
datalist[['DO']] # etc
# DO <- datalist[['DO']]

# or as one data frame. Note the rownames become the parameter.rownumber
allGAMstats <- do.call(rbind,datalist)

saveRDS(allGAMstats,paste(fileExtensionToGAMdata,'/allGAMstats.RDS',sep=''))

allGAMstats2 <- do.call(rbind,datalist) %>%
  tibble::rownames_to_column()

write.csv(allGAMstats2,paste(fileExtensionToGAMdata,'/allGAMstats.csv',sep=''),row.names = F)
