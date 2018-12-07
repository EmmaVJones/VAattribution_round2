# in R 3.4.4
library(tidyverse)
library(readxl)

# Combine West Virginia bug data with the previously combined EDAS genus and Fairfax bug data

# Bring in EDAS, Fairfax, and WVA bug data
bugSmash <- read_csv('data/processedData/VA_FF_WVA_bugData.csv')


# Bring in Marlyand Bug Data
MDlong <- read_excel("data/Maryland/SiteInfoChemLandCoverHabTaxa_JRH.xlsx",
                     sheet = 'Bugs') %>%
  mutate(CollDate=as.Date(DATE_SPR, format='%Y-%m-%d'),
         UID = SITEYR) %>%
  select(-c(DATE_SPR))

# For now, remove duplicate rows so can start mapping process
MDlong1 <- MDlong[!duplicated(MDlong[,c(1:3,5:6)]),]

# Change from long to wide format
MD <- spread(MDlong1,key= FinalName, value= N_TAXA)
MD1 <- mutate(MD,DataSource="MD_BUG",month = lubridate::month(CollDate)) %>%
  #filter(month >2 & month < 7) %>% # keep fall in for now so I can split out later
  dplyr::rename(StationID = SITEYR) %>%
  select(DataSource,UID,StationID,CollDate,everything()) %>%
  select(-c(month, Year, `<NA>`))
#Made sure nothing will be duplicated with UID
View(MD1[duplicated(MD1[,2]),])

rm(MDlong);rm(MDlong1); rm(MD)


# Compare which taxa are in each dataset to get an idea of what needs to be mapped manaually
n1 <- unique(names(bugSmash)[4:532])
n2 <- unique(names(MD1)[5:528])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 328, nice!
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#196 taxa in MD dataset that I need to map to DEQ taxa

# Before doing taxa math, change NA's to 0's to avoid problems adding NAs
MD1[,5:length(MD1)][is.na(MD1[,5:length(MD1)])] <- 0


# Looks like a lot of chironomids like the WVA dataset throwing things off. First try to use the WVA mapping and go from there
MD2 <- mutate(MD1,
               Asellidae = Asellidae + Caecidotea + Lirceus,
               Cambaridae = Cambaridae + Orconectes + Procambarus,
               Crangonyctidae = Stygobromus + Synurella,
               Helobdellidae = Helobdella,
               Lymnaidae = Pseudosuccinea + Stagnicola,
               `Chironomidae (A)` = Chironomidae + Ablabesmyia + Brillia + Brundiniella + 
                 Cardiocladius + Chaetocladius + Chironominae + Chironomini + Chironomus + 
                 Cladopelma + Cladotanytarsus + Clinotanypus + Conchapelopia +  Constempellina + 
                 Cricotopus + `Cricotopus/Orthocladius` + Cryptochironomus +  
                 Diamesa + Diamesinae + DIAMESINAE + Dicrotendipes + Diplocladius +  Endochironomus + Epoicocladius + 
                 Eukiefferiella + Glyptotendipes + Corynoneura + 
                 Heleniella + Heterotrissocladius + Hydrobaenus + 
                 Krenopelopia + Labrundinia + Larsia + Limnophyes + Lopescladius + 
                 Mesocricotopus + Mesosmittia + Metriocnemus + Micropsectra + 
                 Microtendipes + Nanocladius + Natarsia + Nilotanypus + Odontomesa + 
                 Orthocladiinae + Orthocladius + Pagastia + Parachaetocladius + 
                 Parachironomus + Paracladopelma + Parakiefferiella + Paralauterborniella + 
                 Paramerina + Parametriocnemus + Paraphaenocladius + Paratanytarsus + 
                 Paratendipes + Pentaneura + Phaenopsectra + Platysmittia + 
                 Polypedilum + Potthastia + Procladius + Prodiamesa +  Psectrocladius + Psectrotanypus + 
                 Pseudochironomus + Pseudorthocladius + Pseudosmittia + Psilometriocnemus + 
                 Rheocricotopus + Rheosmittia + Rheotanytarsus + Robackia + Saetheria + Smittia + 
                 Stempellina + Stempellinella + Stenochironomus + Stictochironomus + Stilocladius + Sublettea + 
                 Sympotthastia + Synorthocladius + Tanypodinae + Tanypus + Tanytarsini + Tanytarsus + 
                 Thienemanniella + Thienemannimyia + Tribelos + Trissopelopia + 
                 Tvetenia + Unniella + Xenochironomus + Xylotopus + Zalutschia + Zavrelimyia + 
                `Thienemannimyia group` + `Thienemannimyia Group`,
              Tubificidae = Tubificidae + Branchiura + Chaetogaster + Spirosperma,
              Ephemerellidae = Ephemerellidae + EPHEMERELLIDAE,
              Gomphidae = Gomphidae + GOMPHIDAE,
              Hydropsychidae = Hydropsychidae + HYDROPSYCHIDAE,
              Leptophlebiidae = Leptophlebiidae + LEPTOPHLEBIIDAE,
              Planorbidae = Menetus + Gyraulus + Helisoma,
              Naididae = Naididae + NAIDIDAE,
              Paranemoura = Paranemoura + Nemoura,
              Phyrganaeidae = Oligostomis,
              Perlidae = Perlidae + PERLIDAE,
              Tricladida = Turbellaria,
              Viviparidae = Viviparus) %>%
  select(-c(Branchiura, Chaetogaster, Chironomidae, Corynoneura, DIAMESINAE, Gerris, GOMPHIDAE,
            HYDROPSYCHIDAE, LEPTOPHLEBIIDAE, Lymnaea, Menetus, Gyraulus, Helisoma,
            Microvelia, NAIDIDAE, Oligostomis, PERLIDAE, Spirosperma, Stagnicola,EPHEMERELLIDAE,
            `Thienemannimyia group`, `Thienemannimyia Group`, Turbellaria, Viviparus,
            Caecidotea, Lirceus, Orconectes, Procambarus, Stygobromus, Synurella, Helobdella, Pseudosuccinea,
            Ablabesmyia, Brillia, Brundiniella, Cardiocladius, Chaetocladius, Chironominae, Chironomini, Chironomus, 
              Cladopelma, Cladotanytarsus, Clinotanypus, Conchapelopia,  Constempellina, 
              Cricotopus, `Cricotopus/Orthocladius`, Cryptochironomus,  
              Diamesa, Diamesinae, Dicrotendipes, Diplocladius,  Endochironomus, Epoicocladius, 
              Eukiefferiella, Glyptotendipes, 
              Heleniella, Heterotrissocladius, Hydrobaenus, 
              Krenopelopia, Labrundinia, Larsia, Limnophyes, Lopescladius, 
              Mesocricotopus, Mesosmittia, Metriocnemus, Micropsectra, 
              Microtendipes, Nanocladius, 
              Natarsia, Nilotanypus, Odontomesa, 
              Orthocladiinae, Orthocladius, Pagastia, Parachaetocladius, 
              Parachironomus, Paracladopelma, Parakiefferiella, Paralauterborniella, 
              Paramerina, Parametriocnemus, Paraphaenocladius, Paratanytarsus, 
              Paratendipes, Pentaneura, Phaenopsectra, Platysmittia, 
              Polypedilum, Potthastia, Procladius, Prodiamesa,  Psectrocladius, Psectrotanypus, 
              Pseudochironomus, Pseudorthocladius, Pseudosmittia, Psilometriocnemus, 
              Rheocricotopus, Rheosmittia, Rheotanytarsus, Robackia, Saetheria, Smittia, 
              Stempellina, Stempellinella, Stenochironomus, Stictochironomus, Stilocladius, Sublettea, 
              Sympotthastia, Synorthocladius, Tanypodinae, Tanypus, Tanytarsini, Tanytarsus, 
              Thienemanniella, Thienemannimyia, Tribelos, Trissopelopia, 
              Tvetenia, Unniella, Xenochironomus, Xylotopus, Zalutschia, Zavrelimyia))

# Compare which taxa are in each dataset to get an idea of what needs to be mapped manaually
n1 <- unique(names(bugSmash)[5:532])
n2 <- unique(names(MD2)[5:406])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 332, nice!
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#70 taxa in MD dataset that I need to map to DEQ taxa






# Get help from biologists on mapping
# Make a csv of taxa to map to distribute to biologists
#help <- as.data.frame(as.matrix(needToMap)) %>%
#  mutate(FinalTaxaName=NA)
#names(help)[1] <- 'MD taxa that need to be translated to EDAS'
#write.csv(help,'data/processedData/MDbugTaxaToMapToEDAS.csv',row.names = F)
#write.csv(as.data.frame(as.matrix(n1)) %>% 
#            dplyr::rename(`bugSmash Taxa Options`='V1'), 'data/processedData/bugSmashTaxaOptions.csv',row.names=F)
# then I threw in EDAS master taxa list and combined both (manually) to make the helpMe_MD.xlsx


MD3 <- mutate(MD2,
              Ancylidae = Ancylidae + Ferrissia,
              Bittacomorpha = Bittacomorpha + Ptychoptera,
              Chaoboridae = Mochlonyx,
              `Chironomidae (A)` = `Chironomidae (A)` + Alotanypus + Apsectrotanypus + Cryptotendipes + 
                Georthocladius + Kiefferulus + Macropelopia + Omisus + `Orthocladiinae A` + 
                `Orthocladiinae B` + Parasmittia + Podonominae,
              Pteronarcys = Pteronarcys + Pteronarcyidae, 
              Ameletus = Ameletus + Ameletidae,
              Dytiscidae = Dytiscidae + Deronectes + Laccornis + Matus,
              Ephemeridae = Ephemeridae + Pentagenia, 
              Hydrobiidae = Hydrobiidae + Amnicola + Fossaria,
              Hirudinea = Alboglossiphonia + Mooreobdella + Piscicola + Placobdella,
              Libellulidae = Libellulidae + Plathemis,
              Nematopmorpha = Gordiidae,
              Noteridae = Hydrocanthus,
              Notonectidae = Buenoa,
              Physidae = Physidae + Physa,
              Planorbidae = Planorbidae + Planorbella + Promenetus,
              Pleuroceridae = Goniobasis + Leptoxis,
              Scirtidae = Scirtidae + Cyphon,
              Simuliidae = Greniera,
              Tipulidae = Tipulidae + Liogma + Rhabdomastix,
              Tricladida = Tricladida + Cura + Dugesiidae + Girardia + Phagocata + Planariidae,
              Tubificidae = Tubificidae + Chrysogaster + Limnodrilus + Tubifex,
              Molanna = Molannodes,
              Viviparidae = Campeloma) %>%
  select(-c(Ferrissia,Ptychoptera, Mochlonyx, Alotanypus, Apsectrotanypus, Cryptotendipes, 
              Georthocladius, Kiefferulus, Macropelopia, Omisus, `Orthocladiinae A`, 
              `Orthocladiinae B`, Parasmittia, Podonominae, Pteronarcyidae, Ameletidae, 
            Deronectes, Laccornis, Matus, Pentagenia, Amnicola, Fossaria, Alboglossiphonia, 
            Mooreobdella, Piscicola, Placobdella, Plathemis, Gordiidae, Hydrocanthus, Buenoa,
            Physa, Planorbella, Promenetus, Goniobasis, Leptoxis, Cyphon, Greniera,
            Liogma, Rhabdomastix, Cura, Dugesiidae, Girardia, Phagocata, Planariidae,
            Chrysogaster, Limnodrilus, Tubifex, Molannodes,Campeloma,
            # Taxa to remove
            Aquarius, Batracobdella, Bithynia, Bivalvia, Braconidae,Branchiobdellida,
            Collembola, Crambus, Gerridae,Isotomidae, Isotomurus,Limnoporus,
            Nematomorpha, Noctuidae, Odonata, Saldidae, Tortricidae))

# Add things to bugSmash that are in MD
bugSmash2 <- mutate(bugSmash,
                    Anisocentropus=NA, Epiaeschna=NA, Hesperocorixa=NA, Lestidae=NA,
                    Litobrancha=NA, Nemoura=NA,Potamyia=NA,Nematopmorpha=NA,Noteridae=NA)

# Compare which taxa are in each dataset to get an idea of what needs to be mapped manaually
n1 <- unique(names(bugSmash2)[5:541])
n2 <- unique(names(MD3)[5:345])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 341, nice!
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#perfect!
needToAdd <- n1[!(n1 %in% n2)]


# add things to MD that are in bugSmash
MD4 <- MD3
MD4[needToAdd] <- NA
# Double check everything in both
names(MD4) %in% names(bugSmash2)


# reorganize to have station identifiers on left then alphabetical order
MD5 <- MD4[,sort(names(MD4))] %>% select(DataSource,UID,StationID,CollDate,everything())
bugSmash3 <- bugSmash2[,sort(names(bugSmash2))] %>% select(DataSource,UID,StationID,CollDate,everything())
# Last check to make sure everything will match up perfectly when combining EDAS and Fairfax
names(MD5) == names(bugSmash3)


# Make NA's 0's
MD6 <- MD5
MD6[,5:541] <- apply(MD6[,5:541], 2, function(x){replace(x, is.na(x), 0)})

# combine (finally)
bugSmash4 <- rbind(bugSmash3,MD6)

rm(MD1);rm(MD2);rm(MD3);rm(MD4);rm(MD5);rm(MD6);rm(bugSmash);rm(bugSmash2);rm(bugSmash3)


write.csv(bugSmash4,'data/processedData/VA_FF_WVA_MD_bugData.csv', row.names = F)
