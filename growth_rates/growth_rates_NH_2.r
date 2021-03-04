library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)
library(ggplot2)
#Load Data----------
growdata <- read_csv(here("Elsa Clean", "growth_dat.csv"))

#Adding height info for plots---------
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
growdata$heightFeld <- dbh2h_01(growdata$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(growdata$heightFeld)
#Chave
growdata$heightCh <- dbh2h_34(growdata$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
table(growdata$heightCh)
#Chave with E
# need to specify which "E" value to use - from above
growdata$heightE <- dbh2h_ChaveE(growdata$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)


#Quantiles------
table(growdata$species)
table(growdata$heightCh)
summary(growdata)
growdata <- filter(growdata, species != "Indet")
#Feld Heights Quantiles
quantile90Feld <-quantile(growdata$heightFeld, probs = 0.90, na.rm = TRUE)
quantile95Feld <-quantile(growdata$heightFeld, probs = 0.95, na.rm = TRUE)
quantile99Feld <-quantile(growdata$heightFeld, probs = 0.99, na.rm = TRUE)
#Chave Height Quantiles
quantile90Ch <-quantile(growdata$heightCh, probs = 0.90, na.rm = TRUE)
quantile95Ch <-quantile(growdata$heightCh, probs = 0.95, na.rm = TRUE)
quantile99Ch <-quantile(growdata$heightCh, probs = 0.99, na.rm = TRUE)
#Chave with E Quantiles
quantile90E <-quantile(growdata$heightE, probs = 0.90, na.rm = TRUE)
quantile95E <-quantile(growdata$heightE, probs = 0.95, na.rm = TRUE)
quantile99E <-quantile(growdata$heightE, probs = 0.99, na.rm = TRUE)


#quantile 90-------
#Feld
emergent90Feld <- filter(growdata, dbh >= quantile90Feld)
table(emergent90Feld$dbh)

emergent90Feld <- unique(emergent90Feld$species)
table(emergent90Feld)

growdata$tree_type90F <- ifelse(growdata$species %in% c(emergent90Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent90Ch <- filter(growdata, dbh >= quantile90Ch)
table(emergent90Ch$dbh)

emergent90Ch <- unique(emergent90Ch$species)
table(emergent90Ch)

growdata$tree_type90Ch <- ifelse(growdata$species %in% c(emergent90Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent90E <- filter(growdata, dbh >= quantile90E)
table(emergent90E$dbh)

emergent90E <- unique(emergent90E$species)
table(emergent90E)

growdata$tree_type90E <- ifelse(growdata$species %in% c(emergent90E), "emrgnt", "non_emrgnt")


#quantile 95-------
#Feld
emergent95Feld <- filter(growdata, dbh >= quantile95Feld)
table(emergent95Feld$dbh)

emergent95Feld <- unique(emergent95Feld$species)
table(emergent95Feld)

growdata$tree_type95F <- ifelse(growdata$species %in% c(emergent95Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent95Ch <- filter(growdata, dbh >= quantile95Ch)
table(emergent95Ch$dbh)

emergent95Ch <- unique(emergent95Ch$species)
table(emergent95Ch)

growdata$tree_type95Ch <- ifelse(growdata$species %in% c(emergent95Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent95E <- filter(growdata, dbh >= quantile95E)
table(emergent95E$dbh)

emergent95E <- unique(emergent95E$species)
table(emergent95E)

growdata$tree_type95E <- ifelse(growdata$species %in% c(emergent95E), "emrgnt", "non_emrgnt")

#quantile 99-------
#Feld
emergent99Feld <- filter(growdata, dbh >= quantile99Feld)
table(emergent99Feld$dbh)

emergent99Feld <- unique(emergent99Feld$species)
table(emergent99Feld)

growdata$tree_type99F <- ifelse(growdata$species %in% c(emergent99Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent99Ch <- filter(growdata, dbh >= quantile99Ch)
table(emergent99Ch$dbh)

emergent99Ch <- unique(emergent99Ch$species)
table(emergent99Ch)

growdata$tree_type99Ch <- ifelse(growdata$species %in% c(emergent99Ch), "emrgnt", "non_emrgnt")
#Chave with E
emergent99E <- filter(growdata, dbh >= quantile99E)
table(emergent99E$dbh)

emergent99E <- unique(emergent99E$species)
table(emergent99E)

growdata$tree_type99E <- ifelse(growdata$species %in% c(emergent99E), "emrgnt", "non_emrgnt")


#SPKS08 first interval------------

census1 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2001")
census2 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2009")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS08_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2001_2009) 

# notice that in 'SPKS08_2001_2009' dbh.x is dbh at census 1 and dbh.y is dbh at census 2

# calculate time difference and convert time from days to years  
time <- (SPKS08_2001_2009$date.y-SPKS08_2001_2009$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS08_2001_2009$dbh.y
size1 <- SPKS08_2001_2009$dbh.x

# calculate growth rates: 
SPKS08_2001_2009$annual_increment <- (size2 - size1)/time
SPKS08_2001_2009$relative_gr      <- (log(size2) - log(size1))/time

SPKS08_2001_2009 <- filter(SPKS08_2001_2009, SPKS08_2001_2009$annual_increment >= 0
                           & SPKS08_2001_2009$annual_increment < 7.5)
summary(SPKS08_2001_2009)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(SPKS08_2001_2009$annual_increment)
summary(SPKS08_2001_2009$relative_gr )

par(mfrow=c(1,2))
hist(SPKS08_2001_2009$relative_gr , xlab="SPKS08 2001-09 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2001_2009$annual_increment, xlab="SPKS08 2001-09 Annual increment (cm)", col="grey", main="")

#SPKS081 Plot------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKS08_2001_2009$dbh.x, SPKS08_2001_2009$dbh.y, pch=19, 
     xlab="DBH SPKS08 2001 (cm)", ylab="DBH SPKS08 2009 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKS08 Second interval--------
SPKS08 <- filter(growdata, plot == "SPKS_08" )
table(SPKS08$censusID)

census1 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2009")
census2 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS08_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2009_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKS08_2009_2014$date.y-SPKS08_2009_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS08_2009_2014$dbh.y
size1 <- SPKS08_2009_2014$dbh.x

# calculate growth rates: 
SPKS08_2009_2014$annual_increment <- (size2 - size1)/time
SPKS08_2009_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKS08_2009_2014 <- filter(SPKS08_2009_2014, SPKS08_2009_2014$annual_increment >= 0
                           & SPKS08_2009_2014$annual_increment < 7.5)

summary(SPKS08_2009_2014$annual_increment)
summary(SPKS08_2009_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(SPKS08_2001_2009$relative_gr , xlab="SPKS08 2001-09 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2001_2009$annual_increment, xlab="SPKS08 2001-09 Annual increment (cm)", col="grey", main="")
hist(SPKS08_2009_2014$relative_gr , xlab="SPKS08 2009-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2009_2014$annual_increment, xlab="SPKS08 2009-14 Annual increment (cm)", col="grey", main="")

#SPKS082 Plot------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(SPKS08_2009_2014$dbh.x, SPKS08_2009_2014$dbh.y, pch=19, 
     xlab="DBH SPKS08 2009 (cm)", ylab="DBH SPKS08 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2)
plot(SPKS08_2001_2009$dbh.x, SPKS08_2001_2009$dbh.y, pch=19, 
     xlab="DBH SPKS08 2001 (cm)", ylab="DBH SPKS08 2009 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKS08 Large interval SPKS08--------
str(census1)
SPKS8 <- filter(growdata, plot == "SPKS_08")

SPKS8cen1 <- filter(SPKS8, censusID == "08_census_2001")
table(SPKS8cen1$stemID)

SPKS01$pool_stem_ID <- paste0(SPKA01$stemID, "_1a")
SPKS091$pool_stem_ID <- paste0(SPKA091$stemID, "_1b")
SPKS092$pool_census_ID <- paste0(SPKA092$stemID, "_2a")
SPKS14$pool_census_ID <- paste0(SPKA14$stemID, "_2b")
#recombine into 2 new pooled dataset - row bind
#census 1 <- SPKS01 SPKS092
#census 2 <- SPKS091 SPKS14
#have 4 distinct time periods

SPKS801 <- filter(SPKS8, censusID == "08_census_2001")
SPKS809 <- filter(SPKS8, censusID == "08_census_2009")
SPKS814 <- filter(SPKS8, censusID == "08_census_2014")

census1 <- rbind(SPKS801, SPKS809)
census2 <- rbind(SPKS809, SPKS814)

SPKS08_2001_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2001_2014) 
dim(census1)
dim(census2)
dim(SPKS8)
dim(SPKS809)
head(SPKS08_2001_2014)
table(SPKS8$census)
table(SPKS08_2001_2014$censusID.x)
table(SPKS08_2001_2014$censusID.y)
# notice that in 'SPKS08_2001_2009' dbh.x is dbh at census 1 and dbh.y is dbh at census 2

# calculate time difference and convert time from days to years  
time <- (SPKS08_2001_2014$date.y-SPKS08_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS08_2001_2014$dbh.y
size1 <- SPKS08_2001_2014$dbh.x

# calculate growth rates: 
SPKS08_2001_2014$annual_increment <- (size2 - size1)/time
SPKS08_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

census1 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2001")
census2 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS08_2001_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2001_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKS08_2001_2014$date.y-SPKS08_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS08_2001_2014$dbh.y
size1 <- SPKS08_2001_2014$dbh.x

# calculate growth rates: 
SPKS08_2001_2014$annual_increment <- (size2 - size1)/time
SPKS08_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKS08_2001_2014 <- filter(SPKS08_2001_2014, SPKS08_2001_2014$annual_increment >= 0
                           & SPKS08_2001_2014$annual_increment < 7.5)

summary(SPKS08_2001_2014$annual_increment)
summary(SPKS08_2001_2014$relative_gr )

# take a look at the values
par(mfrow=c(1,2))
hist(SPKS08_2001_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2001_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKS08L Plot------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKS08_2001_2014$dbh.x, SPKS08_2001_2014$dbh.y, pch=19, 
     xlab="DBH SPKS08 2001 (cm)", ylab="DBH SPKS08 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2)


#DNM1 First interval --------

DNM1 <- filter(growdata, plot == "DNM1_01" )
table(DNM1$censusID)

census1 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2006")
census2 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2013")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2006_2013) 

# calculate time difference and convert time from days to years  
time <- (DNM1_2006_2013$date.y-DNM1_2006_2013$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM1_2006_2013$dbh.y
size1 <- DNM1_2006_2013$dbh.x

# calculate growth rates: 
DNM1_2006_2013$annual_increment <- (size2 - size1)/time
DNM1_2006_2013$relative_gr      <- (log(size2) - log(size1))/time

DNM1_2006_2013 <- filter(DNM1_2006_2013, DNM1_2006_2013$annual_increment >= 0
                         & DNM1_2006_2013$annual_increment < 7.5)

summary(DNM1_2006_2013$annual_increment)
summary(DNM1_2006_2013$relative_gr  )

# take a look at the values
par(mfrow=c(1,2))
hist(DNM1_2006_2013$relative_gr, xlab="DNM1 2006-13 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2006_2013$annual_increment, xlab="DNM1 2006-13 Annual increment (cm)", col="grey", main="")


#DNM11 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM1_2006_2013$dbh.x, DNM1_2006_2013$dbh.y, pch=19, 
     xlab="DBH DNM1 2006 (cm)", ylab="DBH DNM1 2013 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM1 Second interval --------

census1 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2013")
census2 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2016")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2013_2016) 

# calculate time difference and convert time from days to years  
time <- (DNM1_2013_2016$date.y-DNM1_2013_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM1_2013_2016$dbh.y
size1 <- DNM1_2013_2016$dbh.x

# calculate growth rates: 
DNM1_2013_2016$annual_increment <- (size2 - size1)/time
DNM1_2013_2016$relative_gr      <- (log(size2) - log(size1))/time

DNM1_2013_2016 <- filter(DNM1_2013_2016, DNM1_2013_2016$annual_increment >= 0
                         & DNM1_2013_2016$annual_increment < 7.5)

summary(DNM1_2013_2016$annual_increment)
summary(DNM1_2013_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(DNM1_2006_2013$relative_gr, xlab="DNM1 2006-13 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2006_2013$annual_increment, xlab="DNM1 2006-13 Annual increment (cm)", col="grey", main="")
hist(DNM1_2013_2016$relative_gr, xlab="DNM1 2013-16 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2013_2016$annual_increment, xlab="DNM1 2013-16 Annual increment (cm)", col="grey", main="")

#DNM12 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(DNM1_2013_2016$dbh.x, DNM1_2013_2016$dbh.y, pch=19, 
     xlab="DBH DNM1 2013 (cm)", ylab="DBH DNM1 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(DNM1_2006_2013$dbh.x, DNM1_2006_2013$dbh.y, pch=19, 
     xlab="DBH DNM1 2006 (cm)", ylab="DBH DNM1 2013 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM1 Large interval --------

census1 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2006")
census2 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2016")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2006_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2006_2016) 

# calculate time difference and convert time from days to years  
time <- (DNM1_2006_2016$date.y-DNM1_2006_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM1_2006_2016$dbh.y
size1 <- DNM1_2006_2016$dbh.x

# calculate growth rates: 
DNM1_2006_2016$annual_increment <- (size2 - size1)/time
DNM1_2006_2016$relative_gr      <- (log(size2) - log(size1))/time

DNM1_2006_2016 <- filter(DNM1_2006_2016, DNM1_2006_2016$annual_increment >= 0
                         & DNM1_2006_2016$annual_increment < 7.5)

summary(DNM1_2006_2016$annual_increment)
summary(DNM1_2006_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM1_2006_2016$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2006_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM1L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM1_2006_2016$dbh.x, DNM1_2006_2016$dbh.y, pch=19, 
     xlab="DBH DNM1 2006 (cm)", ylab="DBH DNM1 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#DNM2 First interval --------
DNM2 <- filter(growdata, plot == "DNM2_02" )
table(DNM2$censusID)

census1 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2006")
census2 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2013")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2006_2013) 

# calculate time difference and convert time from days to years  
time <- (DNM2_2006_2013$date.y-DNM2_2006_2013$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM2_2006_2013$dbh.y
size1 <- DNM2_2006_2013$dbh.x

# calculate growth rates: 
DNM2_2006_2013$annual_increment <- (size2 - size1)/time
DNM2_2006_2013$relative_gr      <- (log(size2) - log(size1))/time

DNM2_2006_2013 <- filter(DNM2_2006_2013, DNM2_2006_2013$annual_increment >= 0
                         & DNM2_2006_2013$annual_increment < 7.5)

summary(DNM2_2006_2013$annual_increment)
summary(DNM2_2006_2013$relative_gr )

# take a look at the values
par(mfrow=c(1,2))
hist(DNM2_2006_2013$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM2_2006_2013$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM21 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM2_2006_2013$dbh.x, DNM2_2006_2013$dbh.y, pch=19, 
     xlab="DBH DNM2 2006 (cm)", ylab="DBH DNM2 2013 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM2 Second interval --------
DNM2 <- filter(growdata, plot == "DNM2_02" )
table(DNM2$censusID)

census1 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2013")
census2 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2016")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2013_2016) 

# calculate time difference and convert time from days to years  
time <- (DNM2_2013_2016$date.y-DNM2_2013_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM2_2013_2016$dbh.y
size1 <- DNM2_2013_2016$dbh.x

# calculate growth rates: 
DNM2_2013_2016$annual_increment <- (size2 - size1)/time
DNM2_2013_2016$relative_gr      <- (log(size2) - log(size1))/time

DNM2_2013_2016 <- filter(DNM2_2013_2016, DNM2_2013_2016$annual_increment >= 0
                         & DNM2_2013_2016$annual_increment < 7.5)

summary(DNM2_2013_2016$annual_increment)
summary(DNM2_2013_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(DNM2_2006_2013$relative_gr , xlab="DNM2 2006 - 13 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM2_2006_2013$annual_increment, xlab="DNM2 2006 - 13 Annual increment (cm)", col="grey", main="")
hist(DNM2_2013_2016$relative_gr, xlab="DNM2 2013 - 06 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM2_2013_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM22 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(DNM2_2013_2016$dbh.x, DNM2_2013_2016$dbh.y, pch=19, 
     xlab="DBH DNM2 2013 (cm)", ylab="DBH DNM2 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(DNM2_2006_2013$dbh.x, DNM2_2006_2013$dbh.y, pch=19, 
     xlab="DBH DNM2 2006 (cm)", ylab="DBH DNM2 2013 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#DNM2 Large interval --------
DNM2 <- filter(growdata, plot == "DNM2_02" )
table(DNM2$censusID)
census1 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2006")
census2 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2016")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2006_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2006_2016) 

# calculate time difference and convert time from days to years  
time <- (DNM2_2006_2016$date.y-DNM2_2006_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM2_2006_2016$dbh.y
size1 <- DNM2_2006_2016$dbh.x

# calculate growth rates: 
DNM2_2006_2016$annual_increment <- (size2 - size1)/time
DNM2_2006_2016$relative_gr      <- (log(size2) - log(size1))/time

DNM2_2006_2016 <- filter(DNM2_2006_2016, DNM2_2006_2016$annual_increment >= 0
                         & DNM2_2006_2016$annual_increment < 7.5)

summary(DNM2_2006_2016$annual_increment)
summary(DNM2_2006_2016$relative_gr )

# take a look at the values
par(mfrow=c(1,2))
hist(DNM2_2006_2016$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM2_2006_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM2L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM2_2006_2016$dbh.x, DNM2_2006_2016$dbh.y, pch=19, 
     xlab="DBH DNM2 2006 (cm)", ylab="DBH DNM2 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM3 First interval --------
DNM3 <- filter(growdata, plot == "DNM3_03" )
table(DNM3$censusID)

census1 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2006")
census2 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2013")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2006_2013) 

# calculate time difference and convert time from days to years  
time <- (DNM3_2006_2013$date.y-DNM3_2006_2013$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM3_2006_2013$dbh.y
size1 <- DNM3_2006_2013$dbh.x

# calculate growth rates: 
DNM3_2006_2013$annual_increment <- (size2 - size1)/time
DNM3_2006_2013$relative_gr      <- (log(size2) - log(size1))/time

DNM3_2006_2013 <- filter(DNM3_2006_2013, DNM3_2006_2013$annual_increment >= 0
                         & DNM3_2006_2013$annual_increment < 7.5)

summary(DNM3_2006_2013$annual_increment)
summary(DNM3_2006_2013$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM3_2006_2013$relative_gr, xlab="DNM2 2006 -13 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2006_2013$annual_increment, xlab="DNM2 2006 -13 Annual increment (cm)", col="grey", main="")

#DNM31 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM3_2006_2013$dbh.x, DNM3_2006_2013$dbh.y, pch=19, 
     xlab="DBH DNM3 2006 (cm)", ylab="DBH DNM3 2013 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM3 Second interval --------
DNM3 <- filter(growdata, plot == "DNM3_03" )
table(DNM3$censusID)

census1 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2013")
census2 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2016")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2013_2016) 

# calculate time difference and convert time from days to years  
time <- (DNM3_2013_2016$date.y-DNM3_2013_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM3_2013_2016$dbh.y
size1 <- DNM3_2013_2016$dbh.x

# calculate growth rates: 
DNM3_2013_2016$annual_increment <- (size2 - size1)/time
DNM3_2013_2016$relative_gr      <- (log(size2) - log(size1))/time

DNM3_2013_2016 <- filter(DNM3_2013_2016, DNM3_2013_2016$annual_increment >= 0
                         & DNM3_2013_2016$annual_increment < 7.5)


summary(DNM3_2013_2016$annual_increment)
summary(DNM3_2013_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(DNM3_2006_2013$relative_gr, xlab="DNM2 2006 - 13 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2006_2013$annual_increment, xlab="DNM2 2006 - 13 Annual increment (cm)", col="grey", main="")
hist(DNM3_2013_2016$relative_gr, xlab="DNM2 2013 - 16 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2013_2016$annual_increment, xlab="DNM2 2013 - 16 Annual increment (cm)", col="grey", main="")

#DNM32 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(DNM3_2006_2013$dbh.x, DNM3_2006_2013$dbh.y, pch=19, 
     xlab="DBH DNM3 2006 (cm)", ylab="DBH DNM3 2013 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(DNM3_2013_2016$dbh.x, DNM3_2013_2016$dbh.y, pch=19, 
     xlab="DBH DNM3 2013 (cm)", ylab="DBH DNM3 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM3 Large interval --------
DNM3 <- filter(growdata, plot == "DNM3_03" )
table(DNM3$censusID)

census1 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2006")
census2 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2016")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2006_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2006_2016) 

# calculate time difference and convert time from days to years  
time <- (DNM3_2006_2016$date.y-DNM3_2006_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM3_2006_2016$dbh.y
size1 <- DNM3_2006_2016$dbh.x

# calculate growth rates: 
DNM3_2006_2016$annual_increment <- (size2 - size1)/time
DNM3_2006_2016$relative_gr      <- (log(size2) - log(size1))/time

DNM3_2006_2016 <- filter(DNM3_2006_2016, DNM3_2006_2016$annual_increment >= 0
                         & DNM3_2006_2016$annual_increment < 7.5)

summary(DNM3_2006_2016$annual_increment)
summary(DNM3_2006_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM3_2006_2016$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2006_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM3L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM3_2006_2016$dbh.x, DNM3_2006_2016$dbh.y, pch=19, 
     xlab="DBH DNM3 2006 (cm)", ylab="DBH DNM3 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM50 Large interval --------
DNM50 <- filter(growdata, plot == "DNM50_FGEO" )
table(DNM50$censusID)
table(DNM50$status)

census1 <- filter(growdata, plot == "DNM50_FGEO" & censusID == "census_2011_15")
census2 <- filter(growdata, plot == "DNM50_FGEO" & censusID == "census_2019")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM50_2011_2019 <- inner_join(census1, census2, by="stemID")
dim(DNM50_2011_2019) 

# calculate time difference and convert time from days to years  
time <- (DNM50_2011_2019$date.y-DNM50_2011_2019$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM50_2011_2019$dbh.y
size1 <- DNM50_2011_2019$dbh.x

# calculate growth rates: 
DNM50_2011_2019$annual_increment <- (size2 - size1)/time
DNM50_2011_2019$relative_gr      <- (log(size2) - log(size1))/time

DNM50_2011_2019 <-  filter(DNM50_2011_2019, DNM50_2011_2019$annual_increment >= 0
                           & DNM50_2011_2019$annual_increment < 7.5)


summary(DNM50_2011_2019$annual_increment)
summary(DNM50_2011_2019$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM50_2011_2019$relative_gr, xlab="DNM50 2011-19 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM50_2011_2019$annual_increment, xlab="DNM50 2011-19 Annual increment (cm)", col="grey", main="")

#DNM50 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM50_2011_2019$dbh.x,DNM50_2011_2019$dbh.y, pch=19, 
     xlab="DBH DNM50 2011 (cm)", ylab="DBH DNM50 2019 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA9 First interval --------
SPKA9 <- filter(growdata, plot == "SPKA_09" )
table(SPKA9$censusID)

census1 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2001")
census2 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2009")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2001_2009) 

# calculate time difference and convert time from days to years  
time <- (SPKA9_2001_2009$date.y-SPKA9_2001_2009$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA9_2001_2009$dbh.y
size1 <- SPKA9_2001_2009$dbh.x

# calculate growth rates: 
SPKA9_2001_2009$annual_increment <- (size2 - size1)/time
SPKA9_2001_2009$relative_gr      <- (log(size2) - log(size1))/time

SPKA9_2001_2009 <- filter(SPKA9_2001_2009, SPKA9_2001_2009$annual_increment >= 0
                          & SPKA9_2001_2009$annual_increment < 7.5)

summary(SPKA9_2001_2009$annual_increment)
summary(SPKA9_2001_2009$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA9_2001_2009$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA9_2001_2009$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA91 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKA9_2001_2009$dbh.x,SPKA9_2001_2009$dbh.y, pch=19, 
     xlab="DBH SPKA9 2001 (cm)", ylab="DBH SPKA9 2001 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA9 Second interval --------
SPKA9 <- filter(growdata, plot == "SPKA_09" )
table(SPKA9$censusID)

census1 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2009")
census2 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2009_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKA9_2009_2014$date.y-SPKA9_2009_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA9_2009_2014$dbh.y
size1 <- SPKA9_2009_2014$dbh.x

# calculate growth rates: 
SPKA9_2009_2014$annual_increment <- (size2 - size1)/time
SPKA9_2009_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKA9_2009_2014 <- filter(SPKA9_2009_2014, SPKA9_2009_2014$annual_increment >= 0
                          & SPKA9_2009_2014$annual_increment < 7.5)

summary(SPKA9_2009_2014$annual_increment)
summary(SPKA9_2009_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(SPKA9_2001_2009$relative_gr, xlab="SPKA9 2001-09 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA9_2001_2009$annual_increment, xlab="SPKA9 2001-09 Annual increment (cm)", col="grey", main="")
hist(SPKA9_2009_2014$relative_gr, xlab="SPKA9 2009-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA9_2009_2014$annual_increment, xlab="SPKA9 2009-14 Annual increment (cm)", col="grey", main="")

#SPKA92 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(SPKA9_2009_2014$dbh.x,SPKA9_2009_2014$dbh.y, pch=19, 
     xlab="DBH SPKA9 2009 (cm)", ylab="DBH SPKA9 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(SPKA9_2001_2009$dbh.x,SPKA9_2001_2009$dbh.y, pch=19, 
     xlab="DBH SPKA9 2001 (cm)", ylab="DBH SPKA9 2001 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA9 Large interval --------
SPKA9 <- filter(growdata, plot == "SPKA_09" )
table(SPKA9$censusID)

census1 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2001")
census2 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2001_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2001_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKA9_2001_2014$date.y-SPKA9_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA9_2001_2014$dbh.y
size1 <- SPKA9_2001_2014$dbh.x

# calculate growth rates: 
SPKA9_2001_2014$annual_increment <- (size2 - size1)/time
SPKA9_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKA9_2001_2014 <- filter(SPKA9_2001_2014, SPKA9_2001_2014$annual_increment >= 0
                          & SPKA9_2001_2014$annual_increment < 7.5)

summary(SPKA9_2001_2014$annual_increment)
summary(SPKA9_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA9_2001_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA9_2001_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA9L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKA9_2001_2014$dbh.x,SPKA9_2001_2014$dbh.y, pch=19, 
     xlab="DBH SPKA9 2001 (cm)", ylab="DBH SPKA9 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA10 First interval --------
SPKA10 <- filter(growdata, plot == "SPKA_10" )
table(SPKA10$censusID)

census1 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2001" & status == "A")
census2 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2009" & status == "A")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2001_2009) 

# calculate time difference and convert time from days to years  
time <- (SPKA10_2001_2009$date.y-SPKA10_2001_2009$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA10_2001_2009$dbh.y
size1 <- SPKA10_2001_2009$dbh.x

# calculate growth rates: 
SPKA10_2001_2009$annual_increment <- (size2 - size1)/time
SPKA10_2001_2009$relative_gr      <- (log(size2) - log(size1))/time

SPKA10_2001_2009 <- filter(SPKA10_2001_2009, SPKA10_2001_2009$annual_increment >= 0
                           & SPKA10_2001_2009$annual_increment < 7.5)

summary(SPKA10_2001_2009$annual_increment)
summary(SPKA10_2001_2009$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA10_2001_2009$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA10_2001_2009$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA101 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKA10_2001_2009$dbh.x,SPKA10_2001_2009$dbh.y, pch=19, 
     xlab="DBH SPKA10 2001 (cm)", ylab="DBH SPKA10 2009 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA10 Second interval --------
SPKA10 <- filter(growdata, plot == "SPKA_10" )
table(SPKA10$censusID)

census1 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2009")
census2 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2009_2014 ) 

# calculate time difference and convert time from days to years  
time <- (SPKA10_2009_2014$date.y-SPKA10_2009_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA10_2009_2014$dbh.y
size1 <- SPKA10_2009_2014$dbh.x

# calculate growth rates: 
SPKA10_2009_2014$annual_increment <- (size2 - size1)/time
SPKA10_2009_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKA10_2009_2014 <- filter(SPKA10_2009_2014, SPKA10_2009_2014$annual_increment >= 0
                           & SPKA10_2009_2014$annual_increment < 7.5)

summary(SPKA10_2009_2014$annual_increment)
summary(SPKA10_2009_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(SPKA10_2001_2009$relative_gr, xlab="SPKA10 2001-09 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA10_2001_2009$annual_increment, xlab="SPKA10 2001-09 Annual increment (cm)", col="grey", main="")
hist(SPKA10_2009_2014$relative_gr, xlab="SPKA10 2009-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA10_2009_2014$annual_increment, xlab="SPKA10 2009-14 Annual increment (cm)", col="grey", main="")

#SPKA102 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(SPKA10_2009_2014$dbh.x,SPKA10_2009_2014$dbh.y, pch=19, 
     xlab="DBH SPKA10 2009 (cm)", ylab="DBH SPKA10 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(SPKA10_2001_2009$dbh.x,SPKA10_2001_2009$dbh.y, pch=19, 
     xlab="DBH SPKA10 2001 (cm)", ylab="DBH SPKA10 2009 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA10 Large interval --------
SPKA10 <- filter(growdata, plot == "SPKA_10" )
table(SPKA10$censusID)

census1 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2001")
census2 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2001_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2001_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKA10_2001_2014$date.y-SPKA10_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA10_2001_2014$dbh.y
size1 <- SPKA10_2001_2014$dbh.x

# calculate growth rates: 
SPKA10_2001_2014$annual_increment <- (size2 - size1)/time
SPKA10_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKA10_2001_2014 <- filter(SPKA10_2001_2014, SPKA10_2001_2014$annual_increment >= 0
                           & SPKA10_2001_2014$annual_increment < 7.5)

summary(SPKA10_2001_2014$annual_increment)
summary(SPKA10_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA10_2001_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA10_2001_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA10L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKA10_2001_2014$dbh.x,SPKA10_2001_2014$dbh.y, pch=19, 
     xlab="DBH SPKA10 2001 (cm)", ylab="DBH SPKA10 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKH4 First interval --------
SPKH4 <- filter(growdata, plot == "SPKH_04")
table(SPKH4$censusID)

census1 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2001")
census2 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2008")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2001_2008 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2001_2008) 

# calculate time difference and convert time from days to years  
time <- (SPKH4_2001_2008$date.y-SPKH4_2001_2008$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH4_2001_2008$dbh.y
size1 <- SPKH4_2001_2008$dbh.x

# calculate growth rates: 
SPKH4_2001_2008$annual_increment <- (size2 - size1)/time
SPKH4_2001_2008$relative_gr      <- (log(size2) - log(size1))/time

SPKH4_2001_2008 <- filter(SPKH4_2001_2008, SPKH4_2001_2008$annual_increment >= 0
                          & SPKH4_2001_2008$annual_increment < 7.5)

summary(SPKH4_2001_2008$annual_increment)
summary(SPKH4_2001_2008$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH4_2001_2008$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH4_2001_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKH41 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH4_2001_2008$dbh.x,SPKH4_2001_2008$dbh.y, pch=19, 
     xlab="DBH SPKH4 2001 (cm)", ylab="DBH SPKH4 2008 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKH4 Second interval --------
SPKH4 <- filter(growdata, plot == "SPKH_04")
table(SPKH4$censusID)

census1 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2008")
census2 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2014")
table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2008_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2008_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKH4_2008_2014$date.y-SPKH4_2008_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH4_2008_2014$dbh.y
size1 <- SPKH4_2008_2014$dbh.x

# calculate growth rates: 
SPKH4_2008_2014$annual_increment <- (size2 - size1)/time
SPKH4_2008_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKH4_2008_2014 <- filter(SPKH4_2008_2014, SPKH4_2008_2014$annual_increment >= 0
                          & SPKH4_2008_2014$annual_increment < 7.5)

summary(SPKH4_2008_2014$annual_increment)
summary(SPKH4_2008_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(SPKH4_2001_2008$relative_gr, xlab="SPKH4 2001-08 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH4_2001_2008$annual_increment, xlab="SPKH4 2001-08 Annual increment (cm)", col="grey", main="")
hist(SPKH4_2008_2014$relative_gr, xlab="SPKH4 2008-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH4_2008_2014$annual_increment, xlab="SPKH4 2008-14 Annual increment (cm)", col="grey", main="")

#SPKH42 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(SPKH4_2001_2008$dbh.x,SPKH4_2001_2008$dbh.y, pch=19, 
     xlab="DBH SPKH4 2001 (cm)", ylab="DBH SPKH4 2008 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(SPKH4_2008_2014$dbh.x,SPKH4_2008_2014$dbh.y, pch=19, 
     xlab="DBH SPKH4 2008 (cm)", ylab="DBH SPKH4 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKH4 Large interval --------
SPKH4 <- filter(growdata, plot == "SPKH_04")
table(SPKH4$censusID)

census1 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2001")
census2 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2001_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2001_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKH4_2001_2014$date.y-SPKH4_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH4_2001_2014$dbh.y
size1 <- SPKH4_2001_2014$dbh.x

# calculate growth rates: 
SPKH4_2001_2014$annual_increment <- (size2 - size1)/time
SPKH4_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKH4_2001_2014 <- filter(SPKH4_2001_2014, SPKH4_2001_2014$annual_increment >= 0
                          & SPKH4_2001_2014$annual_increment < 7.5)

summary(SPKH4_2001_2014$annual_increment)
summary(SPKH4_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH4_2001_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH4_2001_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKH4L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH4_2001_2014$dbh.x,SPKH4_2001_2014$dbh.y, pch=19, 
     xlab="DBH SPKH4 2001 (cm)", ylab="DBH SPKH4 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKH5 First interval --------
SPKH5 <- filter(growdata, plot == "SPKH_05")
table(SPKH5$censusID)

census1 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2001")
census2 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2008")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2001_2008 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2001_2008) 

# calculate time difference and convert time from days to years  
time <- (SPKH5_2001_2008$date.y-SPKH5_2001_2008$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH5_2001_2008$dbh.y
size1 <- SPKH5_2001_2008$dbh.x

# calculate growth rates: 
SPKH5_2001_2008$annual_increment <- (size2 - size1)/time
SPKH5_2001_2008$relative_gr      <- (log(size2) - log(size1))/time

SPKH5_2001_2008 <- filter(SPKH5_2001_2008, SPKH5_2001_2008$annual_increment >= 0
                          & SPKH5_2001_2008$annual_increment < 7.5)

summary(SPKH5_2001_2008$annual_increment)
summary(SPKH5_2001_2008$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH5_2001_2008$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2001_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKH51 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH5_2001_2008$dbh.x,SPKH5_2001_2008$dbh.y, pch=19, 
     xlab="DBH SPKH5 2001 (cm)", ylab="DBH SPKH5 2008 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKH5 Second interval --------
SPKH5 <- filter(growdata, plot == "SPKH_05")
table(SPKH5$censusID)

census1 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2008")
census2 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2014")
table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2008_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2008_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKH5_2008_2014$date.y-SPKH5_2008_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH5_2008_2014$dbh.y
size1 <- SPKH5_2008_2014$dbh.x

# calculate growth rates: 
SPKH5_2008_2014$annual_increment <- (size2 - size1)/time
SPKH5_2008_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKH5_2008_2014 <- filter(SPKH5_2008_2014, SPKH5_2008_2014$annual_increment >= 0
                          & SPKH5_2008_2014$annual_increment < 7.5)

summary(SPKH5_2008_2014$annual_increment)
summary(SPKH5_2008_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(SPKH5_2001_2008$relative_gr, xlab="SPKH5 2001-08 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2001_2008$annual_increment, xlab="SPKH5 2001-08 Annual increment (cm)", col="grey", main="")
hist(SPKH5_2008_2014$relative_gr, xlab="SPKH5 2008-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2008_2014$annual_increment, xlab="SPKH5 2008-14 Annual increment (cm)", col="grey", main="")

#SPKH52 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(SPKH5_2001_2008$dbh.x,SPKH5_2001_2008$dbh.y, pch=19, 
     xlab="DBH SPKH5 2001 (cm)", ylab="DBH SPKH5 2008 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(SPKH5_2008_2014$dbh.x,SPKH5_2008_2014$dbh.y, pch=19, 
     xlab="DBH SPKH5 2008 (cm)", ylab="DBH SPKH5 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKH5 Large interval --------
SPKH5 <- filter(growdata, plot == "SPKH_05")
table(SPKH5$censusID)

census1 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2001")
census2 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2014")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2001_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2001_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKH5_2001_2014$date.y-SPKH4_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH5_2001_2014$dbh.y
size1 <- SPKH5_2001_2014$dbh.x

# calculate growth rates: 
SPKH5_2001_2014$annual_increment <- (size2 - size1)/time
SPKH5_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

SPKH5_2001_2014 <- filter(SPKH5_2001_2014, SPKH5_2001_2014$annual_increment >= 0
                          & SPKH5_2001_2014$annual_increment < 7.5)

summary(SPKH5_2001_2014$annual_increment)
summary(SPKH5_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH5_2001_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2001_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKH5L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH5_2001_2014$dbh.x,SPKH5_2001_2014$dbh.y, pch=19, 
     xlab="DBH SPKH5 2001 (cm)", ylab="DBH SPKH5 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKH30 First interval --------
SPKH30 <- filter(growdata, plot == "SPKH_30")
table(SPKH30$censusID)

census1 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2001")
census2 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2010")
table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2001_2010 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2001_2010) 

# calculate time difference and convert time from days to years  
time <- (SPKH30_2001_2010$date.y-SPKH30_2001_2010$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH30_2001_2010$dbh.y
size1 <- SPKH30_2001_2010$dbh.x

# calculate growth rates: 
SPKH30_2001_2010$annual_increment <- (size2 - size1)/time
SPKH30_2001_2010$relative_gr      <- (log(size2) - log(size1))/time

SPKH30_2001_2010 <- filter(SPKH30_2001_2010, SPKH30_2001_2010$annual_increment >= 0
                           & SPKH30_2001_2010$annual_increment < 7.5)

summary(SPKH30_2001_2010$annual_increment)
summary(SPKH30_2001_2010$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH30_2001_2010$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2001_2010$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKH301 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH30_2001_2010$dbh.x,SPKH30_2001_2010$dbh.y, pch=19, 
     xlab="DBH SPKH30 2001 (cm)", ylab="DBH SPKH30 2010 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKH30 Second interval --------
SPKH30 <- filter(growdata, plot == "SPKH_30")
table(SPKH30$censusID)

census1 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2010")
census2 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2015")
table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2010_2015 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2010_2015) 

# calculate time difference and convert time from days to years  
time <- (SPKH30_2010_2015$date.y-SPKH30_2010_2015$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH30_2010_2015$dbh.y
size1 <- SPKH30_2010_2015$dbh.x

# calculate growth rates: 
SPKH30_2010_2015$annual_increment <- (size2 - size1)/time
SPKH30_2010_2015$relative_gr      <- (log(size2) - log(size1))/time

SPKH30_2010_2015 <- filter(SPKH30_2010_2015, SPKH30_2010_2015$annual_increment >= 0
                           & SPKH30_2010_2015$annual_increment < 7.5)

summary(SPKH30_2010_2015$annual_increment)
summary(SPKH30_2010_2015$relative_gr)

# take a look at the values
par(mfrow=c(1,4))
hist(SPKH30_2001_2010$relative_gr, xlab="SPKH30 2001-10 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2001_2010$annual_increment, xlab="SPKH30 2001-10 Annual increment (cm)", col="grey", main="")
hist(SPKH30_2010_2015$relative_gr, xlab="SPKH30 2010-15 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2010_2015$annual_increment, xlab="SPKH30 2010-15 Annual increment (cm)", col="grey", main="")

#SPKH302 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(SPKH30_2001_2010$dbh.x,SPKH30_2001_2010$dbh.y, pch=19, 
     xlab="DBH SPKH30 2001 (cm)", ylab="DBH SPKH30 2010 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
plot(SPKH30_2010_2015$dbh.x,SPKH30_2010_2015$dbh.y, pch=19, 
     xlab="DBH SPKH30 2010 (cm)", ylab="DBH SPKH30 2015 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKH30 Large interval --------
SPKH30 <- filter(growdata, plot == "SPKH_30")
table(SPKH30$censusID)

census1 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2001")
census2 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2015")
table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2001_2015 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2001_2015) 

# calculate time difference and convert time from days to years  
time <- (SPKH30_2001_2015$date.y-SPKH30_2001_2015$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH30_2001_2015$dbh.y
size1 <- SPKH30_2001_2015$dbh.x

# calculate growth rates: 
SPKH30_2001_2015$annual_increment <- (size2 - size1)/time
SPKH30_2001_2015$relative_gr      <- (log(size2) - log(size1))/time

SPKH30_2001_2015 <- filter(SPKH30_2001_2015, SPKH30_2001_2015$annual_increment >= 0
                           & SPKH30_2001_2015$annual_increment < 7.5)

summary(SPKH30_2001_2015$annual_increment)
summary(SPKH30_2001_2015$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH30_2001_2015$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2001_2015$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA30L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH30_2001_2015$dbh.x,SPKH30_2001_2015$dbh.y, pch=19, 
     xlab="DBH SPKH30 2001 (cm)", ylab="DBH SPKH30 2015 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#Create Combined Dataset of all growth rates-----
growthr <- rbind(SPKS08_2001_2009, SPKS08_2009_2014, DNM1_2006_2013, DNM1_2013_2016,
                 DNM2_2006_2013, DNM2_2013_2016, DNM3_2006_2013, DNM3_2013_2016,
                 DNM50_2011_2019, SPKA9_2001_2009, SPKA9_2009_2014, SPKA10_2001_2009, SPKA10_2009_2014,
                 SPKH4_2001_2008, SPKH4_2008_2014, SPKH5_2001_2008, 
                 SPKH5_2008_2014, SPKH30_2001_2010, SPKH30_2010_2015)

growthp <- rbind(SPKS08_2001_2014, DNM1_2006_2016, DNM2_2006_2016, DNM3_2006_2016, DNM50_2011_2019,
                      SPKA9_2001_2014, SPKA10_2001_2014, SPKH4_2001_2014, SPKH5_2001_2014, SPKH30_2001_2015)

#Unpooled plots---------
#growth rate~height
par(mfrow=c(1,2))
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, annual_increment))
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, relative_gr))+
        coord_cartesian(ylim = c(0, .6))

growthr$size_class <- 
        cut(growthr$dbh.y, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,95,100,
                                    105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,195,200,205,max(growthr$dbh.y, na.rm=T)),
        )

plot(growthr$size_class, growthr$annual_increment, pch=19, 
     xlab="DBH", ylab="annual increment", )+
        + scale_y_log10()

#growth rate~height
plot(growthr$size_class, growthr$annual_increment, pch=19, 
     xlab="DBH", ylab="annual increment", )

#Pooled plots---------
par(mfrow=c(1,2))
ggplot() +
        geom_boxplot(growthp, mapping = aes(site.x, annual_increment))+
        scale_x_continuous(trans = 'log2') +
        scale_y_continuous(trans = 'log2')
ggplot() +
        geom_boxplot(growthp, mapping = aes(site.x, relative_gr))+
        coord_cartesian(ylim = c(0, .6))+
        scale_x_continuous(trans = 'log2') +
        scale_y_continuous(trans = 'log2')

growthp$size_class <- 
        cut(growthp$dbh.y, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,95,100,
                                    105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,195,200,205,max(growthr$dbh.y, na.rm=T)),
        )

plot(growthp$size_class, growthp$annual_increment, pch=19, 
     xlab="DBH", ylab="annual increment", )

table(growthp$site.x)

#Sapling Stuff------
DNM <- filter(growthr, plot.x == "DNM1_01" | plot.x == "DNM2_02" | plot.x == "DNM3_03")
DNMsap <- filter(DNM, dbh.x >= 1 & dbh.x <= 4.9 & dbh.y >= 1 & dbh.y <= 4.9)
table(DNMsap$site.x)
table(DNMsap$dbh.y)
summary(DNMsap$annual_increment)
summary(DNMsap$relative_gr)
summary(DNM$relative_gr)

DNM50 <- filter(growthr, plot.x == "DNM50_FGEO")
DNMsap <- filter(DNM50, dbh.x >= 1 & dbh.x <= 4.9 & dbh.y >= 1 & dbh.y <= 4.9)

SPKA <- filter(growthr, site.x == "SPKA")
SPKAsap <- filter(SPKA, dbh.x >= 1 & dbh.x <= 4.9 & dbh.y >= 1 & dbh.y <= 4.9)
table(SPKAsap$site.x)
table(SPKAsap$dbh.y)
summary(SPKAsap$annual_increment)
summary(SPKAsap$relative_gr)

SPKH <- filter(growthr, site.x == "SPKH")
SPKHsap <- filter(SPKH, dbh.x >= 1 & dbh.x <= 4.9 & dbh.y >= 1 & dbh.y <= 4.9)
table(SPKHsap$site.x)
table(SPKHsap$dbh.y)
summary(SPKHsap$annual_increment)
summary(SPKHsap$relative_gr)

SPKS <- filter(growthr, site.x == "SPKS")
SPKSsap <- filter(SPKS, dbh.x >= 1 & dbh.x <= 4.9 & dbh.y >= 1 & dbh.y <= 4.9)
table(SPKSsap$site.x)
table(SPKSsap)
summary(SPKSsap$annual_increment)
summary(SPKSsap$relative_gr)


