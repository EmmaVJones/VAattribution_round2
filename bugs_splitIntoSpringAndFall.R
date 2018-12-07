# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)

# Split final version of bug data into spring and fall datasets for statistical and spatial analyses

# Bring in final bug dataset
bugs <- read_csv('data/processedData/finalBugData.csv')


# Fall only
fall <- mutate(bugs,month=month(CollDate)) %>%
  filter(month>7 & month < 12) %>% select(-month)

# How many records per partner?
nrow(filter(fall,DataSource %in% c('DEQ'))) # DEQ = 326, so low bc only started fish work in 2005
nrow(filter(fall,DataSource %in% c('Stressed Site'))) # Target Stress = 42, so low bc only started fish work in 2005
nrow(filter(fall,DataSource %in% c('RMN'))) # RMN = 3, so low bc only started fish work in 2005

nrow(filter(fall,DataSource %in% c('INSTAR'))) # VCU = 2
nrow(filter(fall,DataSource %in% c("FFX_USGS","FFX_Probabilistic","FFX_Reference" ))) # Fairfax = 0
nrow(filter(fall,DataSource %in% c("WVA_BUG"))) # WVA = 1423
nrow(filter(fall,DataSource %in% c("MD_BUG"    ))) # MD = 0



# Spring Only
spring <- mutate(bugs,month=month(CollDate)) %>%
  filter(month>2 & month < 7) %>% select(-month)

# How many records per partner?
nrow(filter(spring,DataSource %in% c('DEQ'))) # DEQ = 334
nrow(filter(spring,DataSource %in% c('Stressed Site'))) # Target Stress = 41
nrow(filter(spring,DataSource %in% c('RMN'))) # RMN = 4, 

nrow(filter(spring,DataSource %in% c('INSTAR'))) # VCU = 11
nrow(filter(spring,DataSource %in% c("FFX_USGS","FFX_Probabilistic","FFX_Reference" ))) # Fairfax = 679
nrow(filter(spring,DataSource %in% c("WVA_BUG"))) # WVA = 1635
nrow(filter(spring,DataSource %in% c("MD_BUG"    ))) # MD = 1958



write.csv(fall,'data/processedData/allFallBugs.csv',row.names = F)
write.csv(spring,'data/processedData/allSpringBugs.csv',row.names = F)

