# in R 3.4.4
library(tidyverse)
library(readxl)


# Combine EDAS genus and Fairfax bug data

# Bring in Fairfax dataset
FF <- read_excel('data/Fairfax/DEQ BCG Taxa Data v3.xlsx',sheet='Benthics')
FF <- select(FF,-c(Year))

# Remove any taxa from FF that doesnt have any records
as.data.frame(sort(colSums(FF[4:341],na.rm = T))) # Cura and Agabetes not populated


#Bring in DEQ genus EDAS template
# Originally created in 'C:/HardDriveBackup/R/BCG/VAattribution/EDASmanipulation/EDASgenus200_03122018.csv'
EDAS <- read_csv('data/VA/EDASgenus200_03122018.csv')%>%
  mutate(DataSource='DEQ')%>%select(DataSource,everything())

# EDAS has way more columns, need to map things in very carefully


# Compare which taxa are in each dataset to get an idea of what needs to be mapped manaually
n1 <- unique(names(EDAS)[6:501])
n2 <- unique(names(FF)[4:341])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 247, nice!
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#91 taxa in Fairfax dataset that I need to map to DEQ taxa


# Before doing taxa math, change NA's to 0's to avoid problems adding NAs
FF[,4:length(FF)][is.na(FF[,4:length(FF)])] <- 0

# Manually adjust Fairfax taxa names to match EDAS format
FF1 <- mutate(FF,
              BenSampID=NA, # for now, might change this to SiteID later
              RepNum=1,# pretty sure from previous analysis that all are filtered to rep 1 already
              # These need to be mapped
              Hydracarina = Acariformes + Atractides + Forelia + Lebertia + Hydrachna + Hygrobates +Limnesia + Neumania +Sperchon + Torrenticola,
              Pelecypoda = Pelecypoda + Bivalvia,
              Allocapnia = Allocapnia + Capnia,
              `Chironomidae (A)` = Chironomidae,
              Lepidoptera = Lepidoptera + Crambus,
              Tricladida = Tricladida + Dugesia + Hymanella +  Cura + Planariidae + Platyhelminthes + Turbellaria,
              Ancylidae = Ancylidae + Ferrissia + Laevapex,
              Lymnaeidae = Lymnaeidae + Fossaria + Pseudosuccinea,
              Planorbidae = Planorbidae + Gyraulus + Helisoma + Menetus + Planorbella,
              Gammaridae = Gammaridae + Haustoriidae,
              Asellidae = Asellidae + Isopoda + Lirceus + Caecidotea,
              Cambaridae = Cambaridae + Orconectes + Procambarus,
              Culicidae = Panisopsis,
              Physidae = Physidae + Physa + Physella + Aplexa,
              Plauditus = Plauditis,
              Amphipoda = Amphipoda + Pontoporeia,
              Petrophila = Pyralidae,
              Thaumaleidae = Thaumalea,
              Valvatidae = Valvatidae + Valvata) %>%
  select(-c(Acariformes, Atractides, Forelia, Lebertia, Hydrachna, Hygrobates, Limnesia, Neumania, Sperchon, Torrenticola,
            Bivalvia, Capnia, Chironomidae, Crambus, Dugesia, Hymanella,  Cura, Planariidae, Platyhelminthes, Turbellaria,
            Ferrissia, Laevapex, Fossaria, Pseudosuccinea, Gyraulus, Helisoma, Menetus, Planorbella, Haustoriidae,
            Isopoda, Lirceus, Caecidotea, Orconectes, Procambarus, Panisopsis, Physa, Physella, Aplexa, Plauditis,
            Pontoporeia, Pyralidae, Thaumalea, Valvata,
            # taxa from FF dropped
            Chyranda, Hesperophylax, Hydra, Lenarchus, Leptoconops, Microvelia,Nematomorpha,Nemoura,
            Odonata, Podmosta, Polymera, Veliidae))

# Change things in EDAS to match more common format (just Genus in places)
EDAS1 <- mutate(EDAS,
                Agabetes = `Agabetes acuductus`,
                Ancyronyx = `Ancyronyx variegatus`,
                Ansiocentropus = `Anisocentropus pyraloides`,
                Attaneuria = `Attaneuria ruralis`,
                Barbaetis = `Barbaetis benfieldi`,
                Chromagrion = `Chromagrion conditum`,
                Clioperla = `Clioperla clio`,
                Didymops = `Didymops transversa`,
                Diphetor = `Diphetor hageni`,
                Eccoptura = `Eccoptura xanthenes`,
                Habrophlebia = `Habrophlebia vibrans`,
                Hagenius = `Hagenius brevistylus`,
                Helocombus = `Helocombus bifidus`,
                Heteroplectron = `Heteroplectron americanum`,
                Hydatophylax = `Hydatophylax argus`,
                Lype = `Lype diversa`,
                Macronychus = `Macronychus glabratus`,
                Oemopteryx = `Oemopteryx contorta`,
                Pachydiplax = `Pachydiplax longipennis`,
                Penelomax = `Penelomax septentrionalis`,
                Protoplasa = `Protoplasa fitchii`,
                Sperchopsis = `Sperchopsis tessellata`,
                Taenionema = `Taenionema atlanticum`,
                Timpanoga = `Timpanoga hecuba`) %>%
  select(-c(`Agabetes acuductus`,`Ancyronyx variegatus`,`Anisocentropus pyraloides`,`Attaneuria ruralis`,
            `Barbaetis benfieldi`,`Chromagrion conditum`,`Clioperla clio`,
            `Didymops transversa`,`Diphetor hageni`,`Eccoptura xanthenes`,`Habrophlebia vibrans`,
            `Hagenius brevistylus`,`Helocombus bifidus`,`Heteroplectron americanum`,`Hydatophylax argus`,
            `Lype diversa`,`Macronychus glabratus`,`Oemopteryx contorta`,`Pachydiplax longipennis`,
            `Penelomax septentrionalis`,`Protoplasa fitchii`,`Sperchopsis tessellata`,`Taenionema atlanticum`,
            `Timpanoga hecuba`))



#FF2 <- select(FF1,-one_of(needToMap)) # get rid of Fairfax FinalID that don't match
# now add things from EDAS that weren't in Fairfax so the column numbers match
n1 <- unique(names(EDAS1))
n2 <- unique(names(FF1))
needToAdd <- n1[!(n1 %in% n2)]
stilldoesntmatch <- n2[!(n2 %in% n1)]
EDAS2 <- EDAS1
EDAS2[stilldoesntmatch] <- NA
# add things to FF that are in EDAS
FF2 <- FF1
FF2[needToAdd] <- NA
names(FF2) %in% names(EDAS2)
# reorganize to have station identifiers on left then alphabetical order
FF3 <- FF2[,sort(names(FF2))] %>% select(DataSource,StationID,CollDate,everything())
# reorganize to have identifiers on left then alphabetical order
EDAS3 <- EDAS2[,sort(names(EDAS2))] %>% select(DataSource,StationID,CollDate,everything())
# Last check to make sure everything will match up perfectly when combining EDAS and Fairfax
names(EDAS3) == names(FF3)


# Make NA's 0's
FF4 <- FF3
FF4[,4:514] <- apply(FF4[,4:514], 2, function(x){replace(x, is.na(x), 0)})



EDASandFF <- rbind(EDAS3,FF4)

rm(FF1);rm(FF2);rm(FF3);rm(FF4);rm(EDAS);rm(EDAS1);rm(EDAS2)

write.csv(EDASandFF,'data/processedData/EDASandFF_bugData.csv', row.names = F)
