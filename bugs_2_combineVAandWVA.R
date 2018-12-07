# in R 3.4.4
library(tidyverse)
library(readxl)
library(lubridate)

# Combine West Virginia bug data with the previously combined EDAS genus and Fairfax bug data

# Bring in EDAS and Fairfax bug data
bugSmash <- read_csv('data/processedData/EDASandFF_bugData.csv') %>%
  mutate(UID = paste(StationID, year(CollDate), sep='_')) %>%
  select(DataSource,UID, StationID, CollDate, everything())


# Bring in West Virginia Bug Data
WVAlong <- read_csv("data/WVA/VA BCG Benthic Data Table.csv") %>%
  mutate(CollDate=as.Date(CollDate, format='%m/%d/%Y'))



# Bring in Mike's table with duplicated samples
dupes <- read_excel('data/WVA/Duplicate Benthos VA BCG.xlsx') %>%
  filter(DUPLICATE_NAME == 'Dup 2') # take only the duplicate samples
# Take dupes out of WVAlong
WVAlong1 <- WVAlong[!(WVAlong$UID %in% dupes$UID),]


# Change from long to wide format
WVA <- spread(WVAlong1,key= FINAL_WAB_ID, value= GLIMPSS_COUNT)
WVA1 <- mutate(WVA,DataSource="WVA_BUG",month = lubridate::month(CollDate)) %>%
  #filter(month >2 & month < 7) %>% # keep fall in for now so I can split out later
  select(-c(SAMPLE_ID,StreamName,Order,Lat,Long,Basin,Year,BENTHIC_COL_ID,month)) %>%
  select(DataSource,UID,StationID,CollDate,everything())
rm(WVAlong);rm(WVAlong1); rm(dupes)



# Compare which taxa are in each dataset to get an idea of what needs to be mapped manaually
n1 <- unique(names(bugSmash)[5:515])
n2 <- unique(names(WVA1)[5:545])
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 310, nice!
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#231 taxa in WVA dataset that I need to map to DEQ taxa


# Get help from biologists on mapping
# Make a csv of taxa to map to distribute to biologists
#help <- as.data.frame(as.matrix(needToMap)) %>%
#  mutate(FinalTaxaName=NA)
#names(help)[1] <- 'WVA taxa that need to be translated to EDAS'
#write.csv(help,'data/processedData/WVAbugTaxaToMapToEDAS.csv',row.names = F)
#write.csv(as.data.frame(as.matrix(n1)) %>% 
#            dplyr::rename(`bugSmash Taxa Options`='V1'), 'data/processedData/bugSmashTaxaOptions.csv',row.names=F)
# then I threw in EDAS master taxa list and combined both (manually) to make the helpMe.xlsx


# Before doing taxa math, change NA's to 0's to avoid problems adding NAs
WVA1[,5:length(WVA1)][is.na(WVA1[,5:length(WVA1)])] <- 0

# Incorportate Biologist calls for mapping taxa
WVA2 <- mutate(WVA1,
               Asellidae = Asellidae + Asellus + Caecidotea + Lirceus,
               #`Barbaetis benfieldi` = Barbaetis, 
               Branchiobdellidae = Branchiobdella,
               Cambaridae = Cambaridae + Orconectes + Procambarus,
               Tricorythodes = Tricorythodes + Leptohyphes,
               Crangonyctidae = Stygobromus + Synurella,
               Dytiscidae = Dytiscidae + Hydrovatus + Ilybius + Laccobius + Oreodytes,
               Empididae = Empididae + Dolichocephala + Roederiodes,
               Glossophoniidae = Glossiphonia,
               Gomphidae = Gomphidae + `Ophiogomphus/Erpetogomphus`,
               Helobdellidae = Helobdella,
               Hydracarina = Atractides + Limnesiidae + Mideopsis + Sperchonopsis + Wettina + 
                 Hygrobates + Lebertia + Sperchon + Torrenticola,
               Psephenus = Psephenus + Dicranopselaphus, 
               Lumbriculidae = Lumbriculidae + Eclipidrilus + Lumbriculus,
               Lymnaidae = Pseudosuccinea + Stagnicola,
               Naididae = Naididae + Dero + Uncinais + Nais + Ophidonais + Pristina,
               Nematomrpha = Gordius,
               Neopleiidae = Neoplea,
               Yugus = Yugus + `Malirekus/Yugus`,
               Acentrella = Acentrella + `Acentrella/Plauditus`,
               Centroptilum = Centroptilum + `Centroptilum/Procloeon`,
               Planorbidae = Menetus + Gyraulus + Helisoma,
               Pleuroceridae = Pleuroceridae + Pleurocera + Elimia + Leptoxis + Lithasia,
               Pseudocloeon = Labiobaetis,
               Scirtidae = Celina,
               Stratiomyiidae = Euparyphus,
               Tricladida = Turbellaria,
               Tubificidae = Tubificidae + Aulodrilus + Branchiura + Chaetogaster + Limnodrilus + 
                 Quistradrilus + Rhyacodrilus + Spirosperma +  Stylodrilus + Slavina,
               Valvatidae = Valvata,
               Chelifera = Chelifera + `Chelifera/Metachela`,
               Ephydridae =  Cirrula,
               Hirudinea = Hirudinea + Erpobdella,
               Ancylidae = Ferrissia + Laevapex,
               Hydrobiidae = Hydrobiidae + Fossaria,
               Limnophila = Limnophila + Lispoides,
               Physidae = Physella,
               Viviparidae = Viviparus,
               `Chironomidae (A)` = Chironomidae + Ablabesmyia + Acricotopus + Apedilum +
                 Bethbilbeckia + Brillia + Brundiniella + Bryophaenocladius + Camptocladius + 
                 Cardiocladius + Chaetocladius + Chironominae + Chironomini + Chironomus + 
                 Cladopelma + Cladotanytarsus + Clinotanypus + Compterosmittia + Conchapelopia + 
                 `Conchapelopia/Helopelopia` + Constempellina + Corynoneura + Corynoptera + 
                 Cricotopus + `Cricotopus/Orthocladius` + Cryptochironomus + Demicryptochironomus + 
                 Diamesa + Diamesinae + Dicrotendipes + Diplocladius + Djalmabatista + Doithrix + 
                 Einfeldia + Endochironomus + `Endochironomus/Tribelos` + Epoicocladius + 
                 Eukiefferiella + Euryhapsis + Glyptotendipes + Goeldichironomus + Gymnometriocnemus + 
                 Harnischia + Heleniella + Helopelopia + Heterotrissocladius + Hudsonimyia + Hydrobaenus + 
                 Krenopelopia + Krenosmittia + Labrundinia + Larsia + Limnophyes + Lopescladius + 
                 Meropelopia + Mesocricotopus + Mesosmittia + Metriocnemus + Micropsectra + 
                 `Micropsectra/Tanytarsus` + Microtendipes + Monodiamesa + Monopelopia + Nanocladius + 
                 Natarsia + Neostempellina + Neozavrelia + Nilotanypus + Nilothauma + Odontomesa + 
                 Orthocladiinae + Orthocladius + Pagastia + Paraboreochlus + Parachaetocladius + 
                 Parachironomus + Paracladopelma + Paracricotopus + Parakiefferiella + Paralauterborniella + 
                 Paramerina + Parametriocnemus + Paraphaenocladius + Parapsectra + Paratanytarsus + 
                 Paratendipes + Pentaneura + Phaenopsectra + `Phaenopsectra/Tribelos` + Platysmittia + 
                 Polypedilum + Potthastia + Procladius + Prodiamesa +  Psectrocladius + Psectrotanypus + 
                 Pseudochironomus + Pseudodiamesa + Pseudorthocladius + Pseudosmittia + Psilometriocnemus + 
                 Rheocricotopus + Rheopelopia + Rheosmittia + Rheotanytarsus + Robackia + Saetheria + Smittia + 
                 Stempellina + Stempellinella + Stenochironomus + Stictochironomus + Stilocladius + Sublettea + 
                 Sympotthastia + Synorthocladius + Tanypodinae + Tanypus + Tanytarsini + Tanytarsus + 
                 Thienemannia + Thienemanniella + Thienemannimyia + Tokunagaia + Tribelos + Trissopelopia + 
                 Tvetenia + Unniella + Xenochironomus + Xylotopus + Zalutschia + Zavrelia + Zavreliella + 
                 Zavrelimyia) %>%
  select(-c(Asellus, Caecidotea, Lirceus, Branchiobdella,Orconectes, Procambarus,Leptohyphes,
            Stygobromus, Synurella,Hydrovatus, Ilybius, Laccobius, Oreodytes, Dolichocephala, Roederiodes,
            Glossiphonia,`Ophiogomphus/Erpetogomphus`,Helobdella, Atractides, Limnesiidae, Mideopsis, Sperchonopsis, 
            Wettina, Dicranopselaphus, Eclipidrilus, Lumbriculus, 
            Pseudosuccinea, Stagnicola, Dero, Uncinais,  Gordius, Neoplea, `Malirekus/Yugus`, `Acentrella/Plauditus`,
            `Centroptilum/Procloeon`,  Menetus, Pleurocera, Labiobaetis, Celina, Euparyphus, Lipsothrix, Prionocera, Turbellaria,
            Aulodrilus, Branchiura, Chaetogaster, Limnodrilus,Quistradrilus, Rhyacodrilus, Spirosperma,  Stylodrilus, Slavina,
            Valvata, Mesovelia, Metrobates, Microvelia, Rhagovelia, `Chelifera/Metachela`, Cirrula, Clioperla , 
            Elimia, Leptoxis, Lithasia, Erpobdella, Ferrissia, Laevapex, Fossaria, Gyraulus, Helisoma,
            Hygrobates, Lebertia, Sperchon, Torrenticola, Lispoides, Nais, Ophidonais, Pristina,
            Physella, Viviparus, Ablabesmyia, Acricotopus, Apedilum,
            Bethbilbeckia, Brillia, Brundiniella, Bryophaenocladius, Camptocladius, Cardiocladius, Chaetocladius, Chironominae, 
            Chironomini, Chironomus, Cladopelma, Cladotanytarsus, Clinotanypus, Compterosmittia, Conchapelopia, 
            `Conchapelopia/Helopelopia`, Constempellina, Corynoneura, Corynoptera, Cricotopus, `Cricotopus/Orthocladius`, 
            Cryptochironomus, Demicryptochironomus, Diamesa, Diamesinae, Dicrotendipes, Diplocladius, Djalmabatista, Doithrix,
            Einfeldia, Endochironomus, `Endochironomus/Tribelos`, Epoicocladius, Eukiefferiella, Euryhapsis, Glyptotendipes, 
            Goeldichironomus, Gymnometriocnemus, Harnischia, Heleniella, Helopelopia, Heterotrissocladius, Hudsonimyia, Hydrobaenus, 
            Krenopelopia, Krenosmittia, Labrundinia, Larsia, Limnophyes, Lopescladius, Meropelopia, Mesocricotopus, Mesosmittia, 
            Metriocnemus, Micropsectra, `Micropsectra/Tanytarsus`, Microtendipes, Monodiamesa, Monopelopia, Nanocladius, 
            Natarsia, Neostempellina, Neozavrelia, Nilotanypus, Nilothauma, Odontomesa, Orthocladiinae, Orthocladius, Pagastia, 
            Paraboreochlus, Parachaetocladius, Parachironomus, Paracladopelma, Paracricotopus, Parakiefferiella, Paralauterborniella, 
            Paramerina, Parametriocnemus, Paraphaenocladius, Parapsectra, Paratanytarsus, Paratendipes, Pentaneura, Phaenopsectra, 
            `Phaenopsectra/Tribelos`, Platysmittia, Polypedilum, Potthastia, Procladius, Prodiamesa,  Psectrocladius, Psectrotanypus, 
            Pseudochironomus, Pseudodiamesa, Pseudorthocladius, Pseudosmittia, Psilometriocnemus, Rheocricotopus, Rheopelopia, 
            Rheosmittia, Rheotanytarsus, Robackia, Saetheria, Smittia, Stempellina, Stempellinella, Stenochironomus, Stictochironomus,
            Stilocladius, Sublettea, Sympotthastia, Synorthocladius, Tanypodinae, Tanypus, Tanytarsini, Tanytarsus,
            Thienemannia, Thienemanniella, Thienemannimyia, Tokunagaia, Tribelos, Trissopelopia, Tvetenia, Unniella, Xenochironomus, 
            Xylotopus, Zalutschia, Zavrelia, Zavreliella, Zavrelimyia, Chironomidae,
            Archips, Gerris, Lymnaea, Merragata, Nematoda, Nemoura, Neoleptophlebia, Platyvelia, Prionoxystus, Trepobates, 
            Veliidae))
  
# Add some things from WVA to bugSmash because we want to include new taxa
bugSmash2 <- mutate(bugSmash,
                    Helicopsyche = NA, 
                    Malirekus = NA, 
                    Teloganopsis = NA, 
                    Adicrophleps = `Adicrophleps hitchcocki`,
                    Barbaetis = NA, 
                    Pseudostenophylax = NA,
                    Basiaeschna = `Basiaeschna janata`,
                    Choroterpes = NA,
                    Crenitis = NA,
                    Dibusa = `Dibusa angata`,
                    Cyrnellus = `Cyrnellus fraternus`,
                    Rasvena = `Rasvena terna`,
                    Stratiomyiidae = NA,
                    Glossophoniidae = NA,
                    Helobdellidae = NA,
                    Homoplectra = NA,
                    Limnephildae = NA,
                    Lymnaidae = NA,
                    Nematomrpha = NA,
                    Neopleiidae = NA, 
                    Phyrganaeidae = NA,
                    Psuedocloeon = NA,
                    Oligostomis = NA,
                    Pseudocloeon = NA,
                    Lipsothrix = NA, 
                    Prionocera = NA) %>%
  select(-c(`Adicrophleps hitchcocki`,`Dibusa angata`,`Cyrnellus fraternus`,`Rasvena terna`))



# See what we are down to now
n1 <- unique(names(bugSmash2)[5:532])
n2 <- unique(names(WVA2)[5:339])
needToAdd <- n1[!(n1 %in% n2)]
dontNeedToChange <- n2[n2 %in% n1]
length(dontNeedToChange)# 335, nice! it helped
needToMap <- n2[!(n2 %in% n1)]
length(needToMap)#perfect!

# add things to WVA that are in bugSmash
WVA3 <- WVA2
WVA3[needToAdd] <- NA
# Double check everything in both
names(WVA2) %in% names(bugSmash2)


# reorganize to have station identifiers on left then alphabetical order
WVA4 <- WVA3[,sort(names(WVA3))] %>% select(DataSource,UID,StationID,CollDate,everything())
bugSmash3 <- bugSmash2[,sort(names(bugSmash2))] %>% select(DataSource,UID,StationID,CollDate,everything())
# Last check to make sure everything will match up perfectly when combining EDAS and Fairfax
names(WVA4) == names(bugSmash3)


# Make NA's 0's
WVA5 <- WVA4
WVA5[,5:532] <- apply(WVA5[,5:532], 2, function(x){replace(x, is.na(x), 0)})

# combine (finally)
bugSmash4 <- rbind(bugSmash3,WVA5)

rm(WVA); rm(WVA1); rm(WVA2); rm(WVA3); rm(WVA4);rm(WVA5);rm(bugSmash);rm(bugSmash2);rm(bugSmash3)

write.csv(bugSmash4,'data/processedData/VA_FF_WVA_bugData.csv', row.names = F)

















##_________________________________________________________________________________________________

# how i found duplicated samples before doing lots of other steps

# See if anything duplicated
z <- data.frame(StationID=NA,SampDate=NA,rep=NA)
# Any rep2's in FF data?
rep2 <- WVA2[duplicated(WVA2$CollDate),1:3]
for(i in 1:length(unique(rep2$CollDate))){
  x <- filter(rep2,CollDate %in% unique(rep2$CollDate)[i])
  date1 <- as.Date(unique(x$CollDate),format='%m/%d/%Y')
  # If more rows in x than unique stations (i.e. something is duplicated for a single date)
  if(length(unique(x$StationID)) <  nrow(x)){
    y <- cbind(StationID=as.character(x[duplicated(x$StationID),2]),SampDate=as.character(date1),rep="rep2?")
    z[i,] <- y}
}

z <- z[complete.cases(z), ]
# So there are some replicate samples in there, filter WVA1 by the names and dates of the replicate samples

reps <- filter(WVA1,StationID %in% unique(z$StationID))[,1:20]# & CollDate %in% unique(z$SampDate)
# there are different bug counts on each date so going back to Mike W to figure out which is rep1
#write.csv(z,'data/WVA/possibleRep2s.csv')

# Bring in Mike Whitman's response table
dupes <- read_excel('data/WVA/Duplicate Benthos VA BCG.xlsx')






