library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)

data <- read_csv(here("Elsa Clean", "main_dat.csv"))

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
data$heightFeld <- dbh2h_01(data$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(data$heightFeld)
#Chave
data$heightCh <- dbh2h_34(data$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
table(data$heightCh)
#Chave with E
# need to specify which "E" value to use - from above
data$heightE <- dbh2h_ChaveE(data$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)


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
emergent90E <- filter(data, dbh >= quantile90E)
table(emergent90E$dbh)

emergent90E <- unique(emergent90E$species)
table(emergent90E)

data$tree_type90E <- ifelse(data$species %in% c(emergent90E), "emrgnt", "non_emrgnt")


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

#Chave with E
emergent95E <- filter(data, dbh >= quantile95E)
table(emergent95E$dbh)

emergent95E <- unique(emergent95E$species)
table(emergent95E)

data$tree_type95E <- ifelse(data$species %in% c(emergent95E), "emrgnt", "non_emrgnt")

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

data$tree_type99Ch <- ifelse(data$species %in% c(emergent99Ch), "emrgnt", "non_emrgnt")
#Chave with E
emergent99E <- filter(data, dbh >= quantile99E)
table(emergent99E$dbh)

emergent99E <- unique(emergent99E$species)
table(emergent99E)

data$tree_type99E <- ifelse(data$species %in% c(emergent99E), "emrgnt", "non_emrgnt")

#Binning-----------
data$size_class <- 
  cut(data$dbh, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,95,100,
                              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,195,200,205,max(data$dbh, na.rm=T)),
  )

table(data$site)

#Plots------
#Separate by site------
table(data$site)
DNM1=filter(data, site == "DNM1")
DNM2=filter(data, site == "DNM2")
DNM3=filter(data, site == "DNM3")
DNM50=filter(data, site == "DNM50")
LHP=filter(data, site == "LHP")
SPKA=filter(data, site == "SPKA")
SPKH=filter(data, site == "SPKH")
SPKS=filter(data, site == "SPKS")

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

#DNM1 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM1 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
DNM1 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
DNM1 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM1 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
DNM1 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
DNM1 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM1 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
DNM1 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
DNM1 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM1nem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#DNM2 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM2 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
DNM2 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
DNM2 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM2 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
DNM2 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
DNM2 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM2 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
DNM2 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
DNM2 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM2nem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#DNM3 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM3 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
DNM3 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
DNM3 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM3 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
DNM3 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
DNM3 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM3 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
DNM3 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
DNM3 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#DNM50 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM50 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
DNM50 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
DNM50 %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM3nem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM50 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
DNM50 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
DNM50 %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM50 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
DNM50 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
DNM50 %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = DNM50nem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#LHP Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
LHP %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
LHP %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
LHP %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
LHP %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
LHP %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
LHP %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
LHP %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
LHP %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
LHP %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = LHPnem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#SPKA Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKA %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
SPKA %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
SPKA %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKA %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
SPKA %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
SPKA %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKA %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
SPKA %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
SPKA %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKAnem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#SPKH Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKH %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
SPKH %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
SPKH %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKH %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
SPKH %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
SPKH %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKH %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
SPKH %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
SPKH %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKHnem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#SPKS Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKS %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem90F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile90Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 95th percentile
SPKS %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem95F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile95Feld)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Feldpausch Heights 99th percentile
SPKS %>%
  ggplot(aes(heightFeld, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem99F,mapping = aes(heightFeld, dbh), color="blue")+
  geom_vline(xintercept = quantile99Feld) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKS %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem90Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile90Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 95th percentile
SPKS %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem95Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile95Ch)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave Heights 99th percentile
SPKS %>%
  ggplot(aes(heightCh, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem99Ch,mapping = aes(heightCh, dbh), color="blue")+
  geom_vline(xintercept = quantile99Ch) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKS %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem90E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile90E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 95th percentile
SPKS %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem95E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile95E)+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')

#Chave E Heights 99th percentile
SPKS %>%
  ggplot(aes(heightE, dbh))+
  geom_point(color = "red")+
  geom_point(data = SPKSnem99E,mapping = aes(heightE, dbh), color="blue")+
  geom_vline(xintercept = quantile99E) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')


