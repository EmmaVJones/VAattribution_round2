# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)


# Combine (previously combined) VA and Fairfax environmental data with West Virginia data

# Bring in West Virginia Data
WVAE <- read_csv('data/WVA/VA BCG EnviroDataSmash.csv') %>%
  select(-c(`Ortho-PO4`))

# Bring in combined VA and Fairfax data
enviroSmash <- read_csv('data/processedData/VAandFairfax_envData.csv') %>%
  mutate(UID = paste(StationID, Year, sep='_')) %>%
  select(DataSource,UID, StationID, everything())


# Make sure every column that is in WVAE has a home in enviroSmash
names(WVAE)[!names(WVAE) %in% names(enviroSmash)]

# Add columns to WVAE if they arent in there 
n1 <- unique(names(enviroSmash))
n2 <- unique(names(WVAE))
needToAdd <- n1[!(n1 %in% n2)]
WVAE1 <- WVAE
WVAE1[needToAdd] <- NA
names(WVAE1) %in% names(enviroSmash)
WVAE2 <- WVAE1[,names(enviroSmash)]


# Now limit environmental data actually added to enviroSmash to just sites with bug and fish data that
#  meets this project's goals

# Bring in WVA bug data
WVAB <- read_csv('data/processedData/VA_FF_WVA_bugData.csv') %>%
  filter(DataSource == 'WVA_BUG') %>% 
  mutate(DataSource = 'WVA')

# Bring in WVA fish data
WVAF <- read_csv('data/processedData/VA_FF_WVA_fishData.csv') %>%
  filter(DataSource== 'WVA_FISH') %>% 
  mutate(DataSource = 'WVA')


# Filter WVAE2 to just sites with bugs
#WVAE3 <- filter(WVAE2,StationID %in% unique(WVAB$StationID))
WVAE3 <- filter(WVAE2,UID %in% unique(WVAB$UID))


# Filter WVAE2 to just sites with fish
#WVAE4 <- filter(WVAE2,StationID %in% unique(WVAF$StationID))
WVAE4 <- filter(WVAE2,UID %in% unique(WVAF$UID))


# Add those two filtered datasets together
WVAE5 <- rbind(WVAE3,WVAE4)

## Remove potential duplicates
WVAE6 <- WVAE5[!(duplicated(WVAE5$UID)),] 


# Finally combine with VA and Fairfax datasets
smash <- rbind(enviroSmash,WVAE6)


write.csv(smash,"data/processedData/VA_FF_WVA_envData.csv", row.names=F)

