#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# Diameter to Height Allometric Equations
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#

#---------------------------------------------------------------------#
# Default ED 2.1 -- log-log
#---------------------------------------------------------------------#

#Import data------------------
library(tidyverse)
library(here)
library(skimr)
data <- read_csv(here("Desktop", "Research", "DNM_SPK_forest_inventory_data_all_census_wDATE.csv"))

#Basics of data----------
View(data)
dim(data)
head(data)
str(data)
skim(data)

# CASE(0,1)
dbh2h_01 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){ # exclude hgt_ref here if using first dbh_crit eq.
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh)),
              exp(b1Ht + b2Ht * log(dbh_crit)))
  return(h)
}

#---------------------------------------------------------------------#
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
#---------------------------------------------------------------------#


#---------------------------------------------------------------------#
# Load the environmental stress factor "E" as described in Chave et al 2014 and:
# http://chave.ups-tlse.fr/pantropical_allometry.htm
#---------------------------------------------------------------------#
library("raster")
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

#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
#  current allometry parameters (b1Ht = intercept | b2Ht = slope)
#---------------------------------------------------------------------#

#---------------------------------------------------------------------#
# Feldpausch et al 2011 - Appendix A, Table A2 (previously from Table 3)
#---------------------------------------------------------------------#
# Use with dbh2h_01()
#---------------------------------------------------------------------#
# Southeast Asia regional parameters
b1Ht_SEA    = 0.5279284 * log(10) # Use for dbh2h_01
# SAME AS: b1Ht_SEA = 1.2156
b2Ht_SEA    = 0.5782 #"coefficient of ln(D)" # Use for dbh2h_01
hgt_ref_SEA = -0.0114

# Pantropical parameters
b1Ht_pan    = 1.2229 # Use for dbh2h_01
b2Ht_pan    = 0.5320 #"coefficient of ln(D)" # Use for dbh2h_01

# Western Amazon regional parameters
b1Ht_WAM    = 1.4799
b2Ht_WAM    = 0.4669 #"coefficient of ln(D)"


#---------------------------------------------------------------------#
# Use with dbh2h_34() and dbh2h_ChaveE()
#---------------------------------------------------------------------#
# Chave et al 2014
#---------------------------------------------------------------------#
b1Ht_34    =  0.893 # Use for dbh2h_34 & dbh2h_ChaveE
b2Ht_34    =  0.76 # Use for dbh2h_34 & dbh2h_ChaveE
hgt_ref_34 = -0.034 # Use for dbh2h_34 & dbh2h_ChaveE

hgt_max = 100

#Find Heights----------

data$height2h_01 <- dbh2h_01 (data$dbh,hgt_max,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA)
data$height2h_34 <- dbh2h_34 (data$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
data$heightch <- dbh2h_ChaveE (data$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E)

#Plot from dbh2h_01----------
data%>%
  na.omit()%>%
  filter (census == "04_census_2001")%>%
  ggplot(aes(x = dbh, y = height2h_01))+
  geom_histogram(stat = 'identity')+
  xlab("DBH")+
  ylab("Height")

#Plot from dbh2h_34----------------
data%>%
  na.omit()%>%
  filter (census == "04_census_2001")%>%
  ggplot(aes(x = dbh, y = height2h_34))+
  geom_histogram(stat = 'identity')+
  xlab("DBH")+
  ylab("Height")

#Plot from dbh2h_ChaveE-------------
data%>%
  na.omit()%>%
  filter (census == "04_census_2001")%>%
  ggplot(aes(x = dbh, y = heightch))+
  geom_histogram(stat = 'identity') +
  xlab("DBH")+
  ylab("Height")

#Growth Rate--------------------------
library(fgeo)
library(dplyr)

growdata <-rename (data, censusID = census, stemID = X1, dbh = dbh, date = JulianDate, status = DFstatus)

growdata %>% 
  select(censusID, treeID, stemID, status) %>% 
  add_status_tree()

census1 <- filter(growdata, censusID == "05_census_2001" | censusID == "04_census_2001")

census2 <- filter(growdata, censusID == "04_census_2008" | censusID == "05_census_2008")

as_tibble(growth_ctfs(census1, census2, method = "E"))

as_tibble(growth_ctfs(census1, census2, method = "I"))

names(growdata)

example_path()












