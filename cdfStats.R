# this script pulls together stats from teh cdf dataset for Ben's spreadsheet


library(tidyverse)

# jason fill this out ################## ######################
fileExtensionToCDFdata <- 'FinalDataAndPlots/Fish/CDF'
#fileExtensionToCDFdata <- 'FinalDataAndPlots/Bug/CDF'
#fileExtensionToCDFdata <- 'FinalDataAndPlots/BugFall/CDF'

################################################################

dataToSiftThrough <- c('DO','pH','SpCond','TP','TN','TotHab','TotHab_wMD','LRBS','wshdImpPCT','Cl','Sf')

# doing lots of stuff so keep it organized in a list of dataframes
datalist <- list()
out <- data.frame(Taxa=NA, x5=NA, x25=NA, x50=NA, x75=NA, x95=NA)

for(i in 1:length(dataToSiftThrough)){
  # bring in Fish cdf data
  dat <- readRDS(paste(fileExtensionToCDFdata,'/cdfdata',dataToSiftThrough[i],'.RDS',sep='')) %>%
    select(value,starts_with('csum'))
  # loop through each taxa
  for(k in 2:length(dat)){
    cdfstats <- select(dat, value, contains(names(dat)[k]))
    names(cdfstats)[2] <- 'taxa' # rename to a general column name to make looping 894389143298743 times easier
    # in case no taxa data
    if(!is.nan(unique(cdfstats$taxa)[1]) & cdfstats$taxa[1] == 0){
      dat1 <- cbind(Taxa = gsub('csum_','',names(dat)[k]), 
                    x5 = as.numeric(format(cdfstats[findInterval(0.05, cdfstats$taxa),1],digits=4)),
                    x25 = as.numeric(format(cdfstats[findInterval(0.25, cdfstats$taxa),1],digits=4)),
                    x50 = as.numeric(format(cdfstats[findInterval(0.5, cdfstats$taxa),1],digits=4)),
                    x75 = as.numeric(format(cdfstats[findInterval(0.75, cdfstats$taxa),1],digits=4)),
                    x95 = as.numeric(format(cdfstats[findInterval(0.95, cdfstats$taxa),1],digits=4)))
    }else{dat1 <- cbind(Taxa=gsub('csum_','',names(dat)[k]), x5=NA, x25=NA, x50=NA, x75=NA, x95=NA)}
    out[k,] <- dat1      
  }
  datalist[[dataToSiftThrough[i]]] <- out
  
}

# now you can look at each parameter data frame by doing 
datalist[['DO']] # etc
# DO <- datalist[['DO']]

# or as one data frame. Note the rownames become the parameter.rownumber
allCDFstats <- do.call(rbind,datalist)

saveRDS(allCDFstats,paste(fileExtensionToCDFdata,'/allCDFstats.RDS',sep=''))

allCDFstats2 <- do.call(rbind,datalist) %>%
  tibble::rownames_to_column()

write.csv(allCDFstats2,paste(fileExtensionToCDFdata,'/allCDFstats.csv',sep=''),row.names = F)
