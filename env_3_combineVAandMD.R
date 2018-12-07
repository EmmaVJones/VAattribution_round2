# in R 3.4.4
library(tidyverse)
library(readxl)


# Combine (previously combined) VA, Fairfax, and WVA environmental data with Maryland data

# Bring in Maryland Data
MDE <- read_excel("data/Maryland/SiteInfoChemLandCoverHabTaxa_JRH.xlsx", sheet = 'SiteInfoChemHabitatLandCover') %>%
  mutate(DataSource='MD', UID = SITEYR) %>%
  dplyr::rename(StationID = SITEYR, LongitudeDD = LongitudeWGS84, LatitudeDD = LatitudeWGS84,
                Order = ORDER, pH = PH_FLD, DO = DO_FLD, SpCond = COND_FLD, Cl = CL_LAB, Sf = SO4_LAB, 
                wshdImpPCT = IMPSURF, TotHab_wMD = TotHab) %>%
  select(DataSource, UID, StationID, Date, Year, LongitudeDD,LatitudeDD, Order, pH, DO, SpCond, Cl, Sf, wshdImpPCT, TotHab_wMD)

# Bring in combined VA, Fairfax, and WVA data
enviroSmash <- read_csv('data/processedData/VA_FF_WVA_envData.csv') %>%
  mutate(TotHab_wMD=TotHab) %>%
  select(DataSource:TotHab, TotHab_wMD, everything())


# Make sure every column that is in WVAE has a home in enviroSmash
names(MDE)[!names(MDE) %in% names(enviroSmash)]

# Add columns to MDAE if they arent in there 
n1 <- unique(names(enviroSmash))
n2 <- unique(names(MDE))
needToAdd <- n1[!(n1 %in% n2)]
MDE1 <- MDE
MDE1[needToAdd] <- NA
names(MDE1) %in% names(enviroSmash)




# Now limit environmental data actually added to enviroSmash to just sites with bug and fish data that
#  meets this project's goals


# Bring in MD bug data
MDB <- read_csv('data/processedData/VA_FF_WVA_MD_bugData.csv') %>%
  filter(DataSource == 'MD_BUG')

# Bring in MD fish data
MDF <- read_csv('data/processedData/VA_FF_WVA_MD_fishData.csv') %>%
  filter(DataSource== 'MD_FISH')

# Filter MDE2 to just sites with bugs
MDE2 <- filter(MDE1,UID %in% unique(MDB$UID)) %>%
  mutate(DataSource='MD')

# Filter WVAE2 to just sites with fish
MDE3 <- filter(MDE1,UID %in% unique(MDF$UID)) %>%
  mutate(DataSource='MD')

# Add those two filtered datasets together
MDE4 <- rbind(MDE2,MDE3)

## Remove potential duplicates
MDE5 <- MDE4[!(duplicated(MDE4$StationID)),] %>%
  select(-c(Date))

# Reorganize columns
MDE6 <- MDE5[,names(enviroSmash)]
names(MDE6) == names(enviroSmash)

# Finally combine with VA and Fairfax datasets
smash <- rbind(enviroSmash,MDE5)


write.csv(smash,"data/processedData/VA_FF_WVA_MD_envData.csv", row.names=F)

