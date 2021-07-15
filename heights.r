library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(ggplot2)

#setwd("~/Desktop/Research/HCRP/Elsa Clean/main_dat.csv")
hdata <- read_csv("~/Desktop/Research/HCRP/Elsa Clean/main_dat.csv")
#hdata <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv") #EO
summary(hdata$dbh)

#Restrict to Latest Censuses------------
hdata <- filter(hdata, dbh >= 10)
summary(hdata$dbh)
hdata <- filter(hdata, census == "01_census_2016" | census == "02_census_2016" | 
                  census == "03_census_2016" | census == "census_2019" |
                  census == "census_2007_08" | census == "10_census_2014" |
                  census == "30_census_2015" | census == "08_census_2014")

DNM50=filter(hdata, site == "DNM50")
dan <- filter(DNM50, dbh >= quantile99dbh)
dansp <- filter (dan, species == "excelsa" | species == "johorensis" | 
                   species == "lanceolata" | species == "parvifolia" |
                   species == "pauciflora" | species == "sumatrana" |
                   species == "superba")
table(hdata$site)
LMN <- filter(hdata, site == "LHP")
lam <- filter(LMN, dbh >= quantile99dbh)
lamsp <- filter (lam, species == "excelsa" | species == "johorensis" | 
                   species == "lanceolata" | species == "parvifolia" |
                   species == "pauciflora" | species == "sumatrana" |
                   species == "superba")
#----------------------------------------------------------------------#
#-----------------------Calculate Heights-------------------------------
#----------------------------------------------------------------------#

#Height Equations------
#use the Feldpausch et al 2011 equation with the Southeast Asia regional coefficients
dbh2h_01 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){ # exclude hgt_ref here if using first dbh_crit eq.
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh)),
              exp(b1Ht + b2Ht * log(dbh_crit)))
  return(h)
}
# CASE(3,4) ## Chave et al. 2014 - assuming environmental factor = 0 ## 
dbh2h_34 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh) + (hgt_ref * log(dbh)**2)),
              exp(b1Ht + b2Ht * log(dbh_crit) + (hgt_ref * log(dbh_crit)**2)))
  return(h)
}

## Chave et al. 2014 - WITH environmental factor ##
dbh2h_ChaveE <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht,E){
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  #h <- 0.893 - E + 0.760*log(dbh)-0.0340*(log(dbh)**2)
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht - E + b2Ht * log(dbh) + hgt_ref * log(dbh)**2),
              exp(b1Ht - E + b2Ht * log(dbh_crit) + hgt_ref * log(dbh_crit)**2))
  return(h)
}

# Parameters-------
#Feldspauch - -----
b1Ht_SEA    = 0.5279284 * log(10) # Use for dbh2h_01
# SAME AS: b1Ht_SEA = 1.2156
b2Ht_SEA    = 0.5782 #"coefficient of ln(D)" # Use for dbh2h_01
hgt_ref_SEA = -0.0114
hgt_max_SEA = 100
#Chave------
b1Ht_34    =  0.893 # Use for dbh2h_34 & dbh2h_ChaveE
b2Ht_34    =  0.76 # Use for dbh2h_34 & dbh2h_ChaveE
hgt_ref_34 = -0.034 # Use for dbh2h_34 & dbh2h_ChaveE
hgt_max = 100
#E------library("raster")
library("ncdf4")
source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
# E = (0.178* TS - 0.938 & CWD - 6.61 * PS) * 10^3
# DNM, LHP, SPK, PSO, ALP, CRA
longitude=c(117.78994,114.033333,117.92806,102.3062,-73.4385,-70.2173)
latitude=c(4.955965,4.20161,5.852336,2.973,-3.9603,-12.4402)
coord=cbind(longitude,latitude); coord

E = retrieve_raster("E", coord, format="nc"); E
# Danum "E" value
E_DNM = E[1]; E_DNM #-0.0365256
# Lambir "E" value
E_LHP = E[2]
# Sepilok "E" value
E_SPK = E[3]
# Alpahuayo "E" value
#E_ALP = E[4]; E_ALP #-0.09335928
# Cicra "E" value
#E_CRA = E[5]; E_CRA #-0.04474343

#Calculate Heights-----
#Feld
hdata$heightFeld <- dbh2h_01(hdata$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(hdata$heightFeld)
#Chave
hdata$heightCh <- dbh2h_34(hdata$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
table(hdata$heightCh)
#Chave with E
# need to specify which "E" value to use - from above
hdata$heightE <- dbh2h_ChaveE(hdata$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)

#----------------------------------------------------------------------#
#-----------------------Calculate Quantiles-----------------------------
#----------------------------------------------------------------------#
table(hdata$species)
table(hdata$heightCh)
summary(hdata)
#Feld Heights Quantiles
quantile90Feld <-quantile(hdata$heightFeld, probs = 0.90, na.rm = TRUE)
quantile95Feld <-quantile(hdata$heightFeld, probs = 0.95, na.rm = TRUE)
quantile99Feld <-quantile(hdata$heightFeld, probs = 0.99, na.rm = TRUE)
#Chave Height Quantiles
quantile90Ch <-quantile(hdata$heightCh, probs = 0.90, na.rm = TRUE)
quantile95Ch <-quantile(hdata$heightCh, probs = 0.95, na.rm = TRUE)
quantile99Ch <-quantile(hdata$heightCh, probs = 0.99, na.rm = TRUE)
#Chave with E Quantiles
quantile90E <-quantile(hdata$heightE, probs = 0.90, na.rm = TRUE)
quantile95E <-quantile(hdata$heightE, probs = 0.95, na.rm = TRUE)
quantile99E <-quantile(hdata$heightE, probs = 0.99, na.rm = TRUE)

#DBH Quantiles-------
quantile90dbh <-quantile(hdata$dbh, probs = 0.90, na.rm = TRUE)
quantile95dbh <-quantile(hdata$dbh, probs = 0.95, na.rm = TRUE)
quantile99dbh <-quantile(hdata$dbh, probs = 0.99, na.rm = TRUE)

#Remove Indets--------
hdata <- filter(hdata, species != "Indet")

#----------------------------------------------------------------------#
#------------Adding Emergent/Non-emergent Labeling----------------------
#----------------------------------------------------------------------#
#quantile 90-------
#Feld
emergent90Feld <- filter(hdata, dbh >= quantile90Feld)
table(emergent90Feld$dbh)

emergent90Feld <- unique(emergent90Feld$species)
table(emergent90Feld)

hdata$tree_type90F <- ifelse(hdata$species %in% c(emergent90Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent90Ch <- filter(hdata, dbh >= quantile90Ch)
table(emergent90Ch$dbh)

emergent90Ch <- unique(emergent90Ch$species)
table(emergent90Ch)

hdata$tree_type90Ch <- ifelse(hdata$species %in% c(emergent90Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent90E <- filter(hdata, dbh >= quantile90E)
table(emergent90E$dbh)

emergent90E <- unique(emergent90E$species)
table(emergent90E)

hdata$tree_type90E <- ifelse(hdata$species %in% c(emergent90E), "emrgnt", "non_emrgnt")


#quantile 95-------
#Feld
emergent95Feld <- filter(hdata, dbh >= quantile95Feld)
table(emergent95Feld$dbh)

emergent95Feld <- unique(emergent95Feld$species)
table(emergent95Feld)

hdata$tree_type95F <- ifelse(hdata$species %in% c(emergent95Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent95Ch <- filter(hdata, dbh >= quantile95Ch)
table(emergent95Ch$dbh)

emergent95Ch <- unique(emergent95Ch$species)
table(emergent95Ch)

hdata$tree_type95Ch <- ifelse(hdata$species %in% c(emergent95Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent95E <- filter(hdata, dbh >= quantile95E)
table(emergent95E$dbh)

emergent95E <- unique(emergent95E$species)
table(emergent95E)

hdata$tree_type95E <- ifelse(hdata$species %in% c(emergent95E), "emrgnt", "non_emrgnt")

#quantile 99-------
#Feld
emergent99Feld <- filter(hdata, dbh >= quantile99Feld)
table(emergent99Feld$dbh)

emergent99Feld <- unique(emergent99Feld$species)
table(emergent99Feld)

hdata$tree_type99F <- ifelse(hdata$species %in% c(emergent99Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent99Ch <- filter(hdata, dbh >= quantile99Ch)
table(emergent99Ch$dbh)

emergent99Ch <- unique(emergent99Ch$species)
table(emergent99Ch)

hdata$tree_type99Ch <- ifelse(hdata$species %in% c(emergent99Ch), "emrgnt", "non_emrgnt")
#Chave with E
emergent99E <- filter(hdata, dbh >= quantile99E)
table(emergent99E$dbh)

emergent99E <- unique(emergent99E$species)
table(emergent99E)

hdata$tree_type99E <- ifelse(hdata$species %in% c(emergent99E), "emrgnt", "non_emrgnt")

#------------------DBH Definitions---------------------------
#90th percentile-----
emergent90dbh <- filter(hdata, dbh >= quantile90dbh)
table(emergent90dbh$dbh)

emergent90dbh <- unique(emergent90dbh$species)
table(emergent90dbh)

hdata$tree_type90dbh <- ifelse(hdata$species %in% c(emergent90dbh), "emrgnt", "non_emrgnt")

#95th percentile-----
emergent95dbh <- filter(hdata, dbh >= quantile95dbh)
table(emergent95dbh$dbh)

emergent95dbh <- unique(emergent95dbh$species)
table(emergent95dbh)

hdata$tree_type95dbh <- ifelse(hdata$species %in% c(emergent95dbh), "emrgnt", "non_emrgnt")

#99th percentile-----
emergent99dbh <- filter(hdata, dbh >= quantile99dbh)
table(emergent99dbh$dbh)

emergent99dbh <- unique(emergent99dbh$species)
table(emergent99dbh)

hdata$tree_type99dbh <- ifelse(hdata$species %in% c(emergent99dbh), "emrgnt", "non_emrgnt")

#----------------------------------------------------------------------#
#--------------------------Elsa Help------------------------------------
#How do I make a third category for emergent individuals?
#----------------------------------------------------------------------#
# use a nested ifelse statement with two conditions (emergent status & DBH) that reads as follows: 
# if an observation has tree_type99dbh == "emrgnt" and dbh >= some defined size == emrgnt_tree
# else, if an observation has tree_type99dbh == "emrgnt" and dbh < some defined size == emrgnt_spp
# else == non_emrgnt
size_threhold = quantile99dbh # replace 100 with whatever threshold you want (e.g. quantile99dbh) 
hdata$tree_type99dbhmap <- ifelse(hdata$tree_type99dbh == "emrgnt" & hdata$dbh >= size_threhold, "emrgnt_tree",
                                  ifelse(hdata$tree_type99dbh == "emrgnt" & hdata$dbh < size_threhold, "emrgnt_spp","non_emrgnt"))
table(hdata$tree_type99dbh)
table(hdata$tree_type99dbhmap)
# if you compare the two tables above, non_emergent should have the same number of observations for both variables
# tree_type99dbhmap splits tree_type99dbh == emrgnt into two variables now

ggplot(hdata, aes(x=tree_type99dbh, y=dbh, fill=tree_type99dbh)) + 
  geom_boxplot() + 
  theme_classic() + theme(legend.position = c(0.8, 0.8))

ggplot(hdata, aes(x=tree_type99dbhmap, y=dbh, fill=tree_type99dbhmap)) + 
  geom_boxplot() + 
  theme_classic() + theme(legend.position = c(0.8, 0.8))
#----------------------------------------------------------------------#
#Can stop running code here if you just want to make manuscript plots (see section at the bottom for manuscript plot code)-------------------------------
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#-------------------------Plot Parameters-------------------------------
#----------------------------------------------------------------------#
#Separate by site------
table(hdata$site)
DNM1=filter(hdata, site == "DNM1")
DNM2=filter(hdata, site == "DNM2")
DNM3=filter(hdata, site == "DNM3")
DNM50=filter(hdata, site == "DNM50")
LHP=filter(hdata, site == "LHP")
SPKA=filter(hdata, site == "SPKA")
SPKH=filter(hdata, site == "SPKH")
SPKS=filter(hdata, site == "SPKS")

#Variables----
DNM1nem90F <- filter(DNM1, tree_type90F == "non_emrgnt")
DNM1nem95F <- filter(DNM1, tree_type95F == "non_emrgnt")
DNM1nem99F <- filter(DNM1, tree_type99F == "non_emrgnt")
DNM1nem90Ch <- filter(DNM1, tree_type90Ch == "non_emrgnt")
DNM1nem95Ch <- filter(DNM1, tree_type95Ch == "non_emrgnt")
DNM1nem99Ch <- filter(DNM1, tree_type99Ch == "non_emrgnt")
DNM1nem90E <- filter(DNM1, tree_type90E == "non_emrgnt")
DNM1nem95E <- filter(DNM1, tree_type95E == "non_emrgnt")
DNM1nem99E <- filter(DNM1, tree_type99E == "non_emrgnt")

DNM2nem90F <- filter(DNM2, tree_type90F == "non_emrgnt")
DNM2nem95F <- filter(DNM2, tree_type95F == "non_emrgnt")
DNM2nem99F <- filter(DNM2, tree_type99F == "non_emrgnt")
DNM2nem90Ch <- filter(DNM2, tree_type90Ch == "non_emrgnt")
DNM2nem95Ch <- filter(DNM2, tree_type95Ch == "non_emrgnt")
DNM2nem99Ch <- filter(DNM2, tree_type99Ch == "non_emrgnt")
DNM2nem90E <- filter(DNM2, tree_type90E == "non_emrgnt")
DNM2nem95E <- filter(DNM2, tree_type95E == "non_emrgnt")
DNM2nem99E <- filter(DNM2, tree_type99E == "non_emrgnt")

DNM3nem90F <- filter(DNM3, tree_type90F == "non_emrgnt")
DNM3nem95F <- filter(DNM3, tree_type95F == "non_emrgnt")
DNM3nem99F <- filter(DNM3, tree_type99F == "non_emrgnt")
DNM3nem90Ch <- filter(DNM3, tree_type90Ch == "non_emrgnt")
DNM3nem95Ch <- filter(DNM3, tree_type95Ch == "non_emrgnt")
DNM3nem99Ch <- filter(DNM3, tree_type99Ch == "non_emrgnt")
DNM3nem90E <- filter(DNM3, tree_type90E == "non_emrgnt")
DNM3nem95E <- filter(DNM3, tree_type95E == "non_emrgnt")
DNM3nem99E <- filter(DNM3, tree_type99E == "non_emrgnt")

DNM50nem90F <- filter(DNM50, tree_type90F == "non_emrgnt")
DNM50nem95F <- filter(DNM50, tree_type95F == "non_emrgnt")
DNM50nem99F <- filter(DNM50, tree_type99F == "non_emrgnt")
DNM50nem90Ch <- filter(DNM50, tree_type90Ch == "non_emrgnt")
DNM50nem95Ch <- filter(DNM50, tree_type95Ch == "non_emrgnt")
DNM50nem99Ch <- filter(DNM50, tree_type99Ch == "non_emrgnt")
DNM50nem90E <- filter(DNM50, tree_type90E == "non_emrgnt")
DNM50nem95E <- filter(DNM50, tree_type95E == "non_emrgnt")
DNM50nem99E <- filter(DNM50, tree_type99E == "non_emrgnt")

LHPnem90F <- filter(LHP, tree_type90F == "non_emrgnt")
LHPnem95F <- filter(LHP, tree_type95F == "non_emrgnt")
LHPnem99F <- filter(LHP, tree_type99F == "non_emrgnt")
LHPnem90Ch <- filter(LHP, tree_type90Ch == "non_emrgnt")
LHPnem95Ch <- filter(LHP, tree_type95Ch == "non_emrgnt")
LHPnem99Ch <- filter(LHP, tree_type99Ch == "non_emrgnt")
LHPnem90E <- filter(LHP, tree_type90E == "non_emrgnt")
LHPnem95E <- filter(LHP, tree_type95E == "non_emrgnt")
LHPnem99E <- filter(LHP, tree_type99E == "non_emrgnt")

SPKAnem90F <- filter(SPKA, tree_type90F == "non_emrgnt")
SPKAnem95F <- filter(SPKA, tree_type95F == "non_emrgnt")
SPKAnem99F <- filter(SPKA, tree_type99F == "non_emrgnt")
SPKAnem90Ch <- filter(SPKA, tree_type90Ch == "non_emrgnt")
SPKAnem95Ch <- filter(SPKA, tree_type95Ch == "non_emrgnt")
SPKAnem99Ch <- filter(SPKA, tree_type99Ch == "non_emrgnt")
SPKAnem90E <- filter(SPKA, tree_type90E == "non_emrgnt")
SPKAnem95E <- filter(SPKA, tree_type95E == "non_emrgnt")
SPKAnem99E <- filter(SPKA, tree_type99E == "non_emrgnt")

SPKHnem90F <- filter(SPKH, tree_type90F == "non_emrgnt")
SPKHnem95F <- filter(SPKH, tree_type95F == "non_emrgnt")
SPKHnem99F <- filter(SPKH, tree_type99F == "non_emrgnt")
SPKHnem90Ch <- filter(SPKH, tree_type90Ch == "non_emrgnt")
SPKHnem95Ch <- filter(SPKH, tree_type95Ch == "non_emrgnt")
SPKHnem99Ch <- filter(SPKH, tree_type99Ch == "non_emrgnt")
SPKHnem90E <- filter(SPKH, tree_type90E == "non_emrgnt")
SPKHnem95E <- filter(SPKH, tree_type95E == "non_emrgnt")
SPKHnem99E <- filter(SPKH, tree_type99E == "non_emrgnt")

SPKSnem90F <- filter(SPKS, tree_type90F == "non_emrgnt")
SPKSnem95F <- filter(SPKS, tree_type95F == "non_emrgnt")
SPKSnem99F <- filter(SPKS, tree_type99F == "non_emrgnt")
SPKSnem90Ch <- filter(SPKS, tree_type90Ch == "non_emrgnt")
SPKSnem95Ch <- filter(SPKS, tree_type95Ch == "non_emrgnt")
SPKSnem99Ch <- filter(SPKS, tree_type99Ch == "non_emrgnt")
SPKSnem90E <- filter(SPKS, tree_type90E == "non_emrgnt")
SPKSnem95E <- filter(SPKS, tree_type95E == "non_emrgnt")
SPKSnem99E <- filter(SPKS, tree_type99E == "non_emrgnt")

hdatanem90Feld <- filter(hdata, tree_type90F == "non_emrgnt")
hdatanem95Feld <- filter(hdata, tree_type95F == "non_emrgnt")
hdatanem99Feld <- filter(hdata, tree_type99F == "non_emrgnt")
hdatanem90Ch <- filter(hdata, tree_type90Ch == "non_emrgnt")
hdatanem95Ch <- filter(hdata, tree_type95Ch == "non_emrgnt")
hdatanem99Ch <- filter(hdata, tree_type99Ch == "non_emrgnt")
hdatanem90E <- filter(hdata, tree_type90E == "non_emrgnt")
hdatanem95E <- filter(hdata, tree_type95E == "non_emrgnt")
hdatanem99E <- filter(hdata, tree_type99E == "non_emrgnt")

hdata90nemdbh <- filter(hdata, tree_type90dbh == "non_emrgnt")
hdata95nemdbh <- filter(hdata, tree_type95dbh == "non_emrgnt")
hdata99nemdbh <- filter(hdata, tree_type99dbh == "non_emrgnt")

#----------------------------------------------------------------------#
#-------------------------Random Plots----------------------------------
#----------------------------------------------------------------------#
#Everything Plot------
hdata %>%
  ggplot(aes(dbh, heightFeld, colour= "Height Defintion"))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(aes(dbh, heightCh), color="darkorchid3")+
  geom_point(aes(dbh, heightE), color="deepskyblue3")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse1")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse2")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid1")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid2")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue1")+
  geom_vline(xintercept = quantile95E, color="deepskyblue2")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')


hdata %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = hdatanem90Feld,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = hdatanem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = hdatanem90E,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

hdata %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = hdatanem95Feld,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = hdatanem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = hdatanem95E,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height")

hdata %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = hdatanem99Feld,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = hdatanem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = hdatanem99E,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#DNM1 Plot------
#All 90th Percentile
DNM1 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM1nem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM1nem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM1nem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
DNM1 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM1nem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM1nem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM1nem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
DNM1 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM1nem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM1nem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM1nem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM1 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
DNM1 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM1 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM1 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM1 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM1 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM1 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM1 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM1 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#DNM2 Plot------
#All 90th Percentile
DNM2 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM2nem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM2nem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM2nem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
DNM2 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM2nem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM2nem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM2nem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
DNM2 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM2nem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM2nem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM2nem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM2 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)

#Feldpausch Heights 95th percentile
DNM2 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM2 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM2 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM2 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM2 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM2 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM2 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM2 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#DNM3 Plot------
#All 90th Percentile
DNM3 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM3nem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM3nem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM3nem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
DNM3 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM3nem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM3nem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM3nem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
DNM3 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM3nem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM3nem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM3nem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM3 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
DNM3 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM3 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM3 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM3 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM3 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM3 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM3 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM3 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#DNM50 Plot------
DNM50 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM50nem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM50nem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM50nem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
DNM50 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM50nem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM50nem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM50nem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
DNM50 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = DNM50nem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = DNM50nem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = DNM50nem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM50 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
DNM50 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM50 %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM50 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM50 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM50 %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM50 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM50 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM50 %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#LHP Plot------
LHP %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = LHPnem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = LHPnem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = LHPnem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
LHP %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = LHPnem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = LHPnem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = LHPnem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
LHP %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = LHPnem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = LHPnem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = LHPnem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
LHP %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = LHPnem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
LHP %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = LHPnem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
LHP %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = LHPnem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
LHP %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
LHP %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
LHP %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
LHP %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = LHPnem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
LHP %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = LHPnem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
LHP %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = LHPnem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#SPKA Plot------
#All 90th Percentile
SPKA %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKAnem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKAnem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKAnem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
SPKA %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKAnem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKAnem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKAnem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
SPKA %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKAnem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKAnem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKAnem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKA %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
SPKA %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
SPKA %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKA %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
SPKA %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
SPKA %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKA %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
SPKA %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
SPKA %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#SPKH Plot------
#All 90th Percentile
SPKH %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKHnem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKHnem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKHnem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
SPKH %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKHnem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKHnem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKHnem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
SPKH %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKHnem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKHnem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKHnem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKH %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
SPKH %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
SPKH %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKH %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
SPKH %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
SPKH %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKH %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
SPKH %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
SPKH %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#SPKS Plot------
#All 90th Percentile
SPKS %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKSnem90F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKSnem90Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKSnem90Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile90Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile90Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile90E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height90")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 95th Percentile
SPKS %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKSnem95F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKSnem95Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKSnem95Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile95Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile95Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile95E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height95")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#All 99th Percentile
SPKS %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(data = SPKSnem99F,mapping = aes(dbh, heightFeld), color="chartreuse4")+
  geom_point(aes(dbh, heightCh), color="darkorchid2")+
  geom_point(data = SPKSnem99Ch,mapping = aes(dbh, heightCh), color="darkorchid4")+
  geom_point(aes(dbh, heightE), color="deepskyblue2")+
  geom_point(data = SPKSnem99Ch,mapping = aes(dbh, heightE), color="deepskyblue4")+
  geom_vline(xintercept = quantile99Feld, color="chartreuse4")+
  geom_vline(xintercept = quantile99Ch, color="darkorchid4")+
  geom_vline(xintercept = quantile99E, color="deepskyblue4")+
  xlab("DBH")+
  ylab("Height99")+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKS %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem90F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
SPKS %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem95F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
SPKS %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem99F,mapping = aes(dbh, heightFeld), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKS %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem90Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
SPKS %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem95Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
SPKS %>%
  ggplot(aes(dbh, heightCh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem99Ch,mapping = aes(dbh, heightCh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKS %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem90E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
SPKS %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem95E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
SPKS %>%
  ggplot(aes(dbh, heightE))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem99E,mapping = aes(dbh, heightE), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

#Histogram of height------
#Feld
hdata %>%
  ggplot(hdata, mapping = aes(x=heightFeld))+
  geom_histogram()+
  geom_vline(xintercept = quantile90Feld) +
  geom_vline(xintercept = quantile95Feld) +
  geom_vline(xintercept = quantile99Feld)

#Chave w/o E
hdata %>%
  ggplot(hdata, mapping = aes(x=heightCh))+
  geom_histogram()+
  geom_vline(xintercept = quantile90Ch) +
  geom_vline(xintercept = quantile95Ch) +
  geom_vline(xintercept = quantile99Ch)

#Chave with E
hdata %>%
  ggplot(hdata, mapping = aes(x=heightE))+
  geom_histogram()+
  geom_vline(xintercept = quantile90E) +
  geom_vline(xintercept = quantile95E) +
  geom_vline(xintercept = quantile99E)
colnames(hdata)
#Basal Area and stem density stacked plots-------
#90th percentile
par(mfrow=c(1,3))
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type90F, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type90Ch, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type90E, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
#95
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type95F, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type95Ch, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type95E, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
#99
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type99F, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type99Ch, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type99E, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")

summary(hdata$stem_BA)
sum <- sum(hdata$stem_BA)

#90th percentile
par(mfrow=c(1,3))
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type90F, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type90Ch, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type90E, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
#95
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type95F, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type95Ch, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type95E, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
#99
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type99F, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type99Ch, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
hdata %>%
  ggplot(hdata, mapping = aes(fill=tree_type99E, y=stem_BA, x=site))+
  geom_col(position="stack", stat="identity")
table(hdata$site)
#----------------------------------------------------------------------#
#--------------------------------Maps------------------------------------
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#--------------------------Elsa Help------------------------------------
#Map Scripts (pertain to first Elsa Help, where I just need a third category)
#----------------------------------------------------------------------#
# filter all using: tree_type99dbhmap instead of tree_type99dbh
#DNM1-----
DNM1 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

# emapdat <- filter(DNM1, dbh >= tree_type99dbhmap)
# DNM1 %>%
#   ggplot(mapping = aes(y=plot_y, x=plot_x))+
#   geom_point(aes(col=tree_type99dbhmap))+
#   geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
#   theme_classic()

#DNM2-----
emapdat <- filter(DNM2, dbh >= quantile99dbh)
DNM2 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x))+
  geom_point(aes(col=tree_type99dbh))+
  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
  theme_classic()

#DNM3-----
emapdat <- filter(DNM3, dbh >= quantile99dbh)
DNM3 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x))+
  geom_point(aes(col=tree_type99dbh))+
  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
  theme_classic()

#DNM50-----
DNM50 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.5) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()+
  geom_point(emapdat, mapping=aes(y=plot_y, x=plot_x))
emapdat <- filter(DNM50, dbh >= quantile99dbh)
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=species))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=genus))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()
#DNM50 %>%
#  ggplot(mapping = aes(y=plot_y, x=plot_x))+
#  geom_point(aes(col=tree_type99dbh))+
#  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
#  theme_classic()

#LHP-----
emapdat <- filter(LHP, dbh >= quantile99dbh)
LHP %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=1.5, alpha=0.5) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50"))+ 
  theme_classic()+
  geom_point(emapdat, mapping=aes(y=plot_y, x=plot_x))
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=species))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=genus))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()

#LHP %>%
#  ggplot(mapping = aes(y=plot_y, x=plot_x))+
#  geom_point(aes(col=tree_type99dbh))+
#  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
#  theme_classic()
SPKA
#SPKA-----
emapdat <- filter(SPKA, dbh >= quantile99dbh)
SPKA %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x))+
  geom_point(aes(col=tree_type99dbh))+
  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
  theme_classic()

#SPKH-----
emapdat <- filter(SPKH, dbh >= quantile99dbh)
SPKH %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x))+
  geom_point(aes(col=tree_type99dbh))+
  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
  theme_classic()

#SPKS-----
emapdat <- filter(SPKS, dbh >= quantile99dbh)
SPKS %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x))+
  geom_point(aes(col=tree_type99dbh))+
  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
  theme_classic()

#----------------------------------------------------------------------#
#-------------------Plots for manuscript-------------------------------
#----------------------------------------------------------------------#
#DBH Histogram-------
hdata %>%
  ggplot(hdata, mapping = aes(x=dbh))+
  geom_histogram()+
  geom_vline(xintercept = quantile90dbh) +
  geom_vline(xintercept = quantile95dbh) +
  geom_vline(xintercept = quantile99dbh)

#All three height calculations on one plot
hdata %>%
  ggplot(aes(dbh, heightFeld, colour= "Height Defintion"))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(aes(dbh, heightCh), color="darkorchid3")+
  geom_point(aes(dbh, heightE), color="deepskyblue3")+
  geom_hline(yintercept = quantile90Feld, color="chartreuse4")+
  annotate("text", y= quantile90Feld+2,x=230,label=round(quantile90Feld, digits=3))+
  geom_hline(yintercept = quantile95Feld, color="chartreuse4")+
  annotate("text", y= quantile95Feld+2,x=230,label=round(quantile95Feld, digits=3))+
  geom_hline(size=1, yintercept = quantile99Feld, color="chartreuse4")+
  annotate("text", y= quantile99Feld+2,x=230,label=round(quantile99Feld, digits=3))+
  geom_vline(xintercept = quantile90dbh, color="black")+
  annotate("text", x= quantile90dbh+5,y=5,label=round(quantile90dbh, digits=3))+
  geom_vline(xintercept = quantile95dbh, color="black")+
  annotate("text", x= quantile95dbh+7,y=5,label=round(quantile95dbh, digits=3))+
  geom_vline(size=1, xintercept = quantile99dbh, color="black")+
  annotate("text", x= quantile99dbh+5,y=5,label=round(quantile99dbh, digits=3))+
  xlab("DBH")+
  ylab("Height")
#----- PLOT COLOR PALETTE -----


#99th percentile DBH and All Height Calculations Plot with Emergent species colored darker
#----------------------------------------------------------------------#
#--------------------------Elsa Help------------------------------------
#How do I change colors so that each line is different and each set of emergents are different?
#----------------------------------------------------------------------#

# for colors, see ggnewscale (https://github.com/eliocamp/ggnewscale)
# i just came across it and it works beautifully for this
library(ggnewscale)

# also, remember that tree_type99dbhmap is determined based on my arbitrary size threshold of dbh = 100
# once you update it to quantile99dbh, the colors will correspond to the vertical line in this plot

hdata %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(col=tree_type99dbhmap))+
  scale_color_manual("Feldpausch allom", values=c("green","chartreuse","darkgreen")) + 
  new_scale_color() + # from ggnewscale - everything after this requires a new color scale
  geom_point(aes(col=tree_type99dbhmap, dbh, heightCh))+
  scale_color_manual("Chave allom", values=c("#1d91c0","#41b6c4","#253494")) + 
  new_scale_color() +
  geom_point(aes(col=tree_type99dbhmap, dbh, heightE))+
  scale_color_manual("Chave+E allom", values=c("#7a0177","#ae017e","#810f7c")) + 
  geom_vline(xintercept = quantile99dbh, color="black")+
  xlab("DBH")+
  ylab("Height")+
  theme_classic() 

# hdata %>%
#   ggplot(aes(dbh, heightFeld))+
#   geom_point(aes(col=tree_type99F))+
#   geom_point(aes(col=tree_type99Ch, dbh, heightCh))+
#   geom_point(aes(col=tree_type99E, dbh, heightE))+
#   geom_vline(xintercept = quantile99dbh, color="black")+
#   xlab("DBH")+
#   ylab("Height")+
#   theme_classic()

#Same plot as last one, but with only Feld Height Calculation
hdata %>%
  ggplot(aes(dbh, heightFeld, col=tree_type99F))+
  geom_point()+
  geom_vline(xintercept = quantile99dbh, color="black")+
  xlab("DBH")+
  ylab("Height")

#Stem_BA---
plotdata <- filter(hdata, DFstatus == "A")
table(plotdata$DFstatus)
plotdat <- plotdata %>% group_by(site, tree_type99dbh) %>% summarize(n_stems=n(), plot_BA = sum(stem_BA, na.rm=T, 
                                                                                                mean_stem_BA = mean(stem_BA, na.rm=T)))

plotdat$area <- c(1,1,1,1,1,1,50,50,52,52,8,8,12,12,4,4)

plotdat$stemBAha <- plotdat$plot_BA/plotdat$area

plotdat$stemdens <- plotdat$n_stems/plotdat$area
table(LHP$DFstatus)
table(DNM50$DFstatus)
#re
#plotdat$cluster <- factor(plot_dat2$cluster, levels=c("nonemrgnt","emrgnt"),
#labels=c("Nonemergent","Emergent" ))
#----------------------------------------------------------------------#
#--------------------------Elsa Help------------------------------------
#Sepiloks densities are weird
#----------------------------------------------------------------------#
#Stem_BAs plot------
plotdat %>%
  ggplot(plotdat, mapping = aes(fill=tree_type99dbh, y=stemBAha, x=site))+
  geom_col(position="stack")

#Stem density plot------
plotdat %>%
  ggplot(plotdat, mapping = aes(fill=tree_type99dbh, y=stemdens, x=site))+
  geom_col(position="stack")

