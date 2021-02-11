library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)

data <- read_csv(here("Desktop", "Research", "R", "Data", "data_clean.csv"))

#Height Equations------
#use the Feldpuasch et al 2011 equation with the Southeast Asia regional coefficients
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
E_DNM = E[1]; E_DNM #-0.0365256
E_LHP = E[2]
E_SPK = E[3]
E_ALP = E[4]; E_ALP #-0.09335928
E_CRA = E[5]; E_CRA #-0.04474343

#Calculate Heights-----
#Feld
data$heightFeld <- dbh2h_01(data$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
 table(data$heightFeld)
#Chave
data$heightCh <- dbh2h_34(data$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
table(data$heightCh)
#Chave with E
data$heightE <- dbh2h_ChaveE(data$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E)


#Quantiles------
table(data$species)
table(data$heightCh)
summary(data)
data <- filter(data, species != "Indet")
#Feld Heights Quantiles
quantile90Feld <-quantile(data$heightFeld, probs = 0.90, na.rm = TRUE)
quantile95Feld <-quantile(data$heightFeld, probs = 0.95, na.rm = TRUE)
quantile99Feld <-quantile(data$heightFeld, probs = 0.99, na.rm = TRUE)
#Chave Height Quantiles
quantile90Ch <-quantile(data$heightCh, probs = 0.90, na.rm = TRUE)
quantile95Ch <-quantile(data$heightCh, probs = 0.95, na.rm = TRUE)
quantile99Ch <-quantile(data$heightCh, probs = 0.99, na.rm = TRUE)
#Chave with E Quantiles
quantile90E <-quantile(data$heightE, probs = 0.90, na.rm = TRUE)
quantile95E <-quantile(data$heightE, probs = 0.95, na.rm = TRUE)
quantile99E <-quantile(data$heightE, probs = 0.99, na.rm = TRUE)


#quantile 90-------
#Feld
emergent90Feld <- filter(data, dbh >= quantile90Feld)
table(emergent90Feld$dbh)

emergent90Feld <- unique(emergent90Feld$species)
table(emergent90Feld)

data$tree_type90F <- ifelse(data$species %in% c(emergent90Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent90Ch <- filter(data, dbh >= quantile90Ch)
table(emergent90Ch$dbh)

emergent90Ch <- unique(emergent90Ch$species)
table(emergent90Ch)

data$tree_type90Ch <- ifelse(data$species %in% c(emergent90Ch), "emrgnt", "non_emrgnt")

#Chave with E
#emergent90E <- filter(data, dbh >= quantile90E)
#table(emergent90E$dbh)

#emergent90E <- unique(emergent90E$species)
#table(emergent90E)

#data$tree_type90E <- ifelse(data$species %in% c(emergent90E), "emrgnt", "non_emrgnt")


#quantile 95-------
#Feld
emergent95Feld <- filter(data, dbh >= quantile95Feld)
table(emergent95Feld$dbh)

emergent95Feld <- unique(emergent95Feld$species)
table(emergent95Feld)

data$tree_type95F <- ifelse(data$species %in% c(emergent95Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent95Ch <- filter(data, dbh >= quantile95Ch)
table(emergent95Ch$dbh)

emergent95Ch <- unique(emergent95Ch$species)
table(emergent95Ch)

data$tree_type95Ch <- ifelse(data$species %in% c(emergent95Ch), "emrgnt", "non_emrgnt")

##Chave with E
#emergent95E <- filter(data, dbh >= quantile95E)
#table(emergent95E$dbh)
#
#emergent95E <- unique(emergent95E$species)
#table(emergent95E)
#
#data$tree_type95E <- ifelse(data$species %in% c(emergent95E), "emrgnt", "non_emrgnt")

#quantile 99-------
#Feld
emergent99Feld <- filter(data, dbh >= quantile99Feld)
table(emergent99Feld$dbh)

emergent99Feld <- unique(emergent99Feld$species)
table(emergent99Feld)

data$tree_type99F <- ifelse(data$species %in% c(emergent99Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent99Ch <- filter(data, dbh >= quantile99Ch)
table(emergent99Ch$dbh)

emergent99Ch <- unique(emergent99Ch$species)
table(emergent99Ch)

data$tree_type95Ch <- ifelse(data$species %in% c(emergent99Ch), "emrgnt", "non_emrgnt")

#Chave with E
#emergent99E <- filter(data, dbh >= quantile99E)
#table(emergent99E$dbh)
#
#emergent99E <- unique(emergent99E$species)
#table(emergent99E)
#
#data$tree_type99E <- ifelse(data$species %in% c(emergent99E), "emrgnt", "non_emrgnt")#
