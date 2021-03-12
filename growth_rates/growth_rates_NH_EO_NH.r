library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)
#-------------------------------------------------------------------#
#----------------First Run Heights.r script!------------------------#
#-------------------------------------------------------------------#

#Load Data----------
growdata <- read_csv(here("Elsa Clean", "growth_dat.csv"))

growdata <- filter(growdata, dbh >= 10)

#---------------------------------------------------------------------------#
#--------------------------Growth Rate Calcutlations------------------------
#---------------------------------------------------------------------------#

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


summary(SPKS08_2001_2009)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(SPKS08_2001_2009$annual_increment)
summary(SPKS08_2001_2009$relative_gr )

par(mfrow=c(1,2))
hist(SPKS08_2001_2009$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2001_2009$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

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


summary(SPKS08_2009_2014$annual_increment)
summary(SPKS08_2009_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKS08_2009_2014$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2009_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKS082 Plot------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKS08_2009_2014$dbh.x, SPKS08_2009_2014$dbh.y, pch=19, 
     xlab="DBH SPKS08 2009 (cm)", ylab="DBH SPKS08 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2)

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#SPKS08 Large interval SPKS08--------
SPKS8 <- filter(growdata, plot == "SPKS_08")

SPKS801  <- filter(SPKS8, censusID == "08_census_2001")
SPKS8091 <- filter(SPKS8, censusID == "08_census_2009")
SPKS8092 <- filter(SPKS8, censusID == "08_census_2009")
SPKS814  <- filter(SPKS8, censusID == "08_census_2014")

SPKS801$pool_stem_ID    <- paste0(SPKS801$stemID, "_1") # you're using "SPKS01" and "SPKA01"
SPKS8091$pool_stem_ID   <- paste0(SPKS8091$stemID, "_2")
SPKS8092$pool_stem_ID <- paste0(SPKS8092$stemID, "_1") # also changed "pool_census_ID" to "pool_stem_ID"
SPKS814$pool_stem_ID  <- paste0(SPKS814$stemID, "_2")

census1 <- rbind(SPKS801, SPKS8091)
census2 <- rbind(SPKS8092, SPKS814)
#---------------------------------------------------------------------------------------------#
# I updated some errors in the code above and changed the suffix so that "_1" and "_2"
# correspond to the datasets that are associated with census 1 and census 2. 
#---------------------------------------------------------------------------------------------#

SPKS08_2001_2014 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKS08_2001_2014) 
dim(census1)
dim(census2)
dim(SPKS8)
dim(SPKS8091)
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


summary(SPKS08_2001_2014$annual_increment)
summary(SPKS08_2001_2014$relative_gr )

# take a look at the values
par(mfrow=c(1,2))
hist(SPKS08_2001_2014$relative_gr, xlab="SPKS8 2001-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS08_2001_2014$annual_increment, xlab="SPKS8 2001-14 Annual increment (cm)", col="grey", main="")

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

summary(DNM1_2006_2013$annual_increment)
summary(DNM1_2006_2013$relative_gr  )

# take a look at the values
par(mfrow=c(1,2))
hist(DNM1_2006_2013$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2006_2013$annual_increment, xlab="Annual increment (cm)", col="grey", main="")


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

summary(DNM1_2013_2016$annual_increment)
summary(DNM1_2013_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM1_2013_2016$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2013_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM12 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM1_2013_2016$dbh.x, DNM1_2013_2016$dbh.y, pch=19, 
     xlab="DBH DNM1 2013 (cm)", ylab="DBH DNM1 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM1 Large interval --------
DNM1 <- filter(growdata, plot == "DNM1_01")
table(DNM1$censusID)

DNM106  <- filter(DNM1, censusID == "01_census_2006")
DNM1131 <- filter(DNM1, censusID == "01_census_2013")
DNM1132 <- filter(DNM1, censusID == "01_census_2013")
DNM116  <- filter(DNM1, censusID == "01_census_2016")

DNM106$pool_stem_ID    <- paste0(DNM106$stemID, "_1") # you're using "SPKS01" and "SPKA01"
DNM1131$pool_stem_ID   <- paste0(DNM1131$stemID, "_2")
DNM1132$pool_stem_ID <- paste0(DNM1132$stemID, "_1") # also changed "pool_census_ID" to "pool_stem_ID"
DNM116$pool_stem_ID  <- paste0(DNM116$stemID, "_2")

census1 <- rbind(DNM106, DNM1131)
census2 <- rbind(DNM1132, DNM116)
#---------------------------------------------------------------------------------------------#
# I updated some errors in the code above and changed the suffix so that "_1" and "_2"
# correspond to the datasets that are associated with census 1 and census 2. 
#---------------------------------------------------------------------------------------------#

DNM1_2006_2016 <- inner_join(census1, census2, by="pool_stem_ID")
dim(DNM1_2006_2016) 
dim(census1)
dim(census2)
dim(DNM1)
dim(DNM1131)
head(DNM1_2006_2016)
table(DNM1_2006_2016$censusID.x)
table(DNM1_2006_2016$censusID.y)
# notice that in 'SPKS08_2001_2009' dbh.x is dbh at census 1 and dbh.y is dbh at census 2

# calculate time difference and convert time from days to years  
time <- (DNM1_2006_2016$date.y-DNM1_2006_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM1_2006_2016$dbh.y
size1 <- DNM1_2006_2016$dbh.x

# calculate growth rates: 
DNM1_2006_2016$annual_increment <- (size2 - size1)/time
DNM1_2006_2016$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM1_2006_2016$annual_increment)
summary(DNM1_2006_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM1_2006_2016$relative_gr, xlab="DNM1 2006 - 16 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM1_2006_2016$annual_increment, xlab="DNM1 2006 - 16 Annual increment (cm)", col="grey", main="")

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

summary(DNM2_2013_2016$annual_increment)
summary(DNM2_2013_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM2_2013_2016$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM2_2013_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM22 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM2_2013_2016$dbh.x, DNM2_2013_2016$dbh.y, pch=19, 
     xlab="DBH DNM2 2013 (cm)", ylab="DBH DNM2 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#DNM2 Large interval --------
DNM2 <- filter(growdata, plot == "DNM2_02" )
table(DNM2$censusID)

DNM206  <- filter(DNM2, censusID == "02_census_2006")
DNM2131 <- filter(DNM2, censusID == "02_census_2013")
DNM2132 <- filter(DNM2, censusID == "02_census_2013")
DNM216  <- filter(DNM2, censusID == "02_census_2016")

DNM206$pool_stem_ID    <- paste0(DNM206$stemID, "_1") # you're using "SPKS01" and "SPKA01"
DNM2131$pool_stem_ID   <- paste0(DNM2131$stemID, "_2")
DNM2132$pool_stem_ID <- paste0(DNM2132$stemID, "_1") # also changed "pool_census_ID" to "pool_stem_ID"
DNM216$pool_stem_ID  <- paste0(DNM216$stemID, "_2")

census1 <- rbind(DNM206, DNM2131)
census2 <- rbind(DNM2132, DNM216)

DNM2_2006_2016 <- inner_join(census1, census2, by="pool_stem_ID")
dim(DNM2_2006_2016) 
dim(census1)
dim(census2)
head(DNM2_2006_2016)
table(DNM2_2006_2016$censusID.x)
table(DNM2_2006_2016$censusID.y)

# calculate time difference and convert time from days to years  
time <- (DNM2_2006_2016$date.y-DNM2_2006_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM2_2006_2016$dbh.y
size1 <- DNM2_2006_2016$dbh.x

# calculate growth rates: 
DNM2_2006_2016$annual_increment <- (size2 - size1)/time
DNM2_2006_2016$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM2_2006_2016$annual_increment)
summary(DNM2_2006_2016$relative_gr )

# take a look at the values
par(mfrow=c(1,2))
hist(DNM2_2006_2016$relative_gr, xlab="DNM2 2006 - 16 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM2_2006_2016$annual_increment, xlab="DNM2 2006 - 16 Annual increment (cm)", col="grey", main="")

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

summary(DNM3_2006_2013$annual_increment)
summary(DNM3_2006_2013$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM3_2006_2013$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2006_2013$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

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


summary(DNM3_2013_2016$annual_increment)
summary(DNM3_2013_2016$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM3_2013_2016$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2013_2016$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM32 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM3_2013_2016$dbh.x, DNM3_2013_2016$dbh.y, pch=19, 
     xlab="DBH DNM3 2013 (cm)", ylab="DBH DNM3 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM3 Large interval --------
DNM3 <- filter(growdata, plot == "DNM3_03")
table(DNM3$censusID)

DNM306  <- filter(DNM3, censusID == "03_census_2006")
DNM3131 <- filter(DNM3, censusID == "03_census_2013")
DNM3132 <- filter(DNM3, censusID == "03_census_2013")
DNM316  <- filter(DNM3, censusID == "03_census_2016")

DNM306$pool_stem_ID    <- paste0(DNM306$stemID, "_1") 
DNM3131$pool_stem_ID   <- paste0(DNM3131$stemID, "_2")
DNM3132$pool_stem_ID <- paste0(DNM3132$stemID, "_1") 
DNM316$pool_stem_ID  <- paste0(DNM316$stemID, "_2")

census1 <- rbind(DNM306, DNM3131)
census2 <- rbind(DNM3132, DNM316)

DNM3_2006_2016 <- inner_join(census1, census2, by="pool_stem_ID")

# calculate time difference and convert time from days to years  
time <- (DNM3_2006_2016$date.y-DNM3_2006_2016$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM3_2006_2016$dbh.y
size1 <- DNM3_2006_2016$dbh.x

# calculate growth rates: 
DNM3_2006_2016$annual_increment <- (size2 - size1)/time
DNM3_2006_2016$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM3_2006_2016$annual_increment)
summary(DNM3_2006_2016$relative_gr)
colnames(DNM3_2006_2016)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM3_2006_2016$relative_gr, xlab="DNM3 2006 - 16 Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM3_2006_2016$annual_increment, xlab="DNM3 2006 - 16 Annual increment (cm)", col="grey", main="")

#DNM3L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM3_2006_2016$dbh.x, DNM3_2006_2016$dbh.y, pch=19, 
     xlab="DBH DNM3 2006 (cm)", ylab="DBH DNM3 2016 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM50 Large interval Unpooled--------
DNM50 <- filter(growdata, plot == "DNM50_FGEO" )
table(DNM50$censusID)
table(DNM50$DFstatus)

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

summary(DNM50_2011_2019$annual_increment)
summary(DNM50_2011_2019$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(DNM50_2011_2019$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM50_2011_2019$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM50 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(DNM50_2011_2019$dbh.x,DNM50_2011_2019$dbh.y, pch=19, 
     xlab="DBH DNM50 2011 (cm)", ylab="DBH DNM50 2019 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#DNM50 Large interval Pooled--------
DNM50 <- filter(growdata, plot == "DNM50_FGEO" )
table(DNM50$censusID)
table(DNM50$DFstatus)

census1 <- filter(growdata, plot == "DNM50_FGEO" & censusID == "census_2011_15")
census2 <- filter(growdata, plot == "DNM50_FGEO" & censusID == "census_2019")

census1$pool_stem_ID <- census1$stemID
census2$pool_stem_ID <- census2$stemID
table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM50_2011_2019p <- inner_join(census1, census2, by="pool_stem_ID")
dim(DNM50_2011_2019p) 

# calculate time difference and convert time from days to years  
time <- (DNM50_2011_2019p$date.y-DNM50_2011_2019p$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- DNM50_2011_2019p$dbh.y
size1 <- DNM50_2011_2019p$dbh.x

# calculate growth rates: 
DNM50_2011_2019p$annual_increment <- (size2 - size1)/time
DNM50_2011_2019p$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM50_2011_2019p$annual_increment)
summary(DNM50_2011_2019p$relative_gr)

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

summary(SPKA9_2009_2014$annual_increment)
summary(SPKA9_2009_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA9_2009_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA9_2009_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA92 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKA9_2009_2014$dbh.x,SPKA9_2009_2014$dbh.y, pch=19, 
     xlab="DBH SPKA9 2009 (cm)", ylab="DBH SPKA9 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA9 Large interval --------
SPKA9 <- filter(growdata, plot == "SPKA_09" )
table(SPKA9$censusID)

SPKA901  <- filter(SPKA9, censusID == "09_census_2001")
SPKA9091 <- filter(SPKA9, censusID == "09_census_2009")
SPKA9092 <- filter(SPKA9, censusID == "09_census_2009")
SPKA914  <- filter(SPKA9, censusID == "09_census_2014")

SPKA901$pool_stem_ID    <- paste0(SPKA901$stemID, "_1") 
SPKA9091$pool_stem_ID   <- paste0(SPKA9091$stemID, "_2")
SPKA9092$pool_stem_ID <- paste0(SPKA9092$stemID, "_1") 
SPKA914$pool_stem_ID  <- paste0(SPKA914$stemID, "_2")

census1 <- rbind(SPKA901, SPKA9091)
census2 <- rbind(SPKA9092, SPKA914)

SPKA9_2001_2014 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKA9_2001_2014) 
dim(census1)
dim(census2)
head(SPKA9_2001_2014)
table(SPKA9_2001_2014$censusID.x)
table(SPKA9_2001_2014$censusID.y)

# calculate time difference and convert time from days to years  
time <- (SPKA9_2001_2014$date.y-SPKA9_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA9_2001_2014$dbh.y
size1 <- SPKA9_2001_2014$dbh.x

# calculate growth rates: 
SPKA9_2001_2014$annual_increment <- (size2 - size1)/time
SPKA9_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKA9_2001_2014$annual_increment)
summary(SPKA9_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA9_2001_2014$relative_gr, xlab="SPKA 2009 - 14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA9_2001_2014$annual_increment, xlab="SPKA 2009 - 14 Annual increment (cm)", col="grey", main="")

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

summary(SPKA10_2009_2014$annual_increment)
summary(SPKA10_2009_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA10_2009_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA10_2009_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA102 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKA10_2009_2014$dbh.x,SPKA10_2009_2014$dbh.y, pch=19, 
     xlab="DBH SPKA10 2009 (cm)", ylab="DBH SPKA10 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKA10 Large interval --------
SPKA10 <- filter(growdata, plot == "SPKA_10" )
table(SPKA10$censusID)

SPKA1001  <- filter(SPKA10, censusID == "10_census_2001")
SPKA10091 <- filter(SPKA10, censusID == "10_census_2009")
SPKA10092 <- filter(SPKA10, censusID == "10_census_2009")
SPKA1014  <- filter(SPKA10, censusID == "10_census_2014")

SPKA1001$pool_stem_ID    <- paste0(SPKA1001$stemID, "_1") 
SPKA10091$pool_stem_ID   <- paste0(SPKA10091$stemID, "_2")
SPKA10092$pool_stem_ID <- paste0(SPKA10092$stemID, "_1") 
SPKA1014$pool_stem_ID  <- paste0(SPKA1014$stemID, "_2")

census1 <- rbind(SPKA1001, SPKA10091)
census2 <- rbind(SPKA10092, SPKA1014)

SPKA10_2001_2014 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKA10_2001_2014) 
dim(census1)
dim(census2)
head(SPKA10_2001_2014)
table(SPKA10_2001_2014$censusID.x)
table(SPKA10_2001_2014$censusID.y)

# calculate time difference and convert time from days to years  
time <- (SPKA10_2001_2014$date.y-SPKA10_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKA10_2001_2014$dbh.y
size1 <- SPKA10_2001_2014$dbh.x

# calculate growth rates: 
SPKA10_2001_2014$annual_increment <- (size2 - size1)/time
SPKA10_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKA10_2001_2014$annual_increment)
summary(SPKA10_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKA10_2001_2014$relative_gr, xlab="SPKA10 2001-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKA10_2001_2014$annual_increment, xlab="SPKA10 2001-14 Annual increment (cm)", col="grey", main="")

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

summary(SPKH4_2008_2014$annual_increment)
summary(SPKH4_2008_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH4_2008_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH4_2008_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKH42 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH4_2008_2014$dbh.x,SPKH4_2008_2014$dbh.y, pch=19, 
     xlab="DBH SPKH4 2008 (cm)", ylab="DBH SPKH4 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKH4 Large interval --------
SPKH4 <- filter(growdata, plot == "SPKH_04")
table(SPKH4$censusID)

SPKH401  <- filter(SPKH4, censusID == "04_census_2001")
SPKH4081 <- filter(SPKH4, censusID == "04_census_2008")
SPKH4082 <- filter(SPKH4, censusID == "04_census_2008")
SPKH414  <- filter(SPKH4, censusID == "04_census_2014")

SPKH401$pool_stem_ID    <- paste0(SPKH401$stemID, "_1") 
SPKH4081$pool_stem_ID   <- paste0(SPKH4081$stemID, "_2")
SPKH4082$pool_stem_ID <- paste0(SPKH4082$stemID, "_1") 
SPKH414$pool_stem_ID  <- paste0(SPKH414$stemID, "_2")

census1 <- rbind(SPKH401, SPKH4081)
census2 <- rbind(SPKH4082, SPKH414)

SPKH4_2001_2014 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKH4_2001_2014) 
dim(census1)
dim(census2)
head(SPKH4_2001_2014)
table(SPKH4_2001_2014$censusID.x)
table(SPKH4_2001_2014$censusID.y)

# calculate time difference and convert time from days to years  
time <- (SPKH4_2001_2014$date.y-SPKH4_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH4_2001_2014$dbh.y
size1 <- SPKH4_2001_2014$dbh.x

# calculate growth rates: 
SPKH4_2001_2014$annual_increment <- (size2 - size1)/time
SPKH4_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH4_2001_2014$annual_increment)
summary(SPKH4_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH4_2001_2014$relative_gr, xlab="SPKH4 2001-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH4_2001_2014$annual_increment, xlab="SPKH4 2001-14 Annual increment (cm)", col="grey", main="")

#SPKA4L Plot----------
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

summary(SPKH5_2001_2008$annual_increment)
summary(SPKH5_2001_2008$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH5_2001_2008$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2001_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA51 Plot----------
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

summary(SPKH5_2008_2014$annual_increment)
summary(SPKH5_2008_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH5_2008_2014$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2008_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA52 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH5_2008_2014$dbh.x,SPKH5_2008_2014$dbh.y, pch=19, 
     xlab="DBH SPKH5 2008 (cm)", ylab="DBH SPKH5 2014 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#SPKH5 Large interval --------
SPKH5 <- filter(growdata, plot == "SPKH_05")
table(SPKH5$censusID)

SPKH501  <- filter(SPKH5, censusID == "05_census_2001")
SPKH5081 <- filter(SPKH5, censusID == "05_census_2008")
SPKH5082 <- filter(SPKH5, censusID == "05_census_2008")
SPKH514  <- filter(SPKH5, censusID == "05_census_2014")

SPKH501$pool_stem_ID    <- paste0(SPKH501$stemID, "_1") 
SPKH5081$pool_stem_ID   <- paste0(SPKH5081$stemID, "_2")
SPKH5082$pool_stem_ID <- paste0(SPKH5082$stemID, "_1") 
SPKH514$pool_stem_ID  <- paste0(SPKH514$stemID, "_2")

census1 <- rbind(SPKH501, SPKH5081)
census2 <- rbind(SPKH5082, SPKH514)

SPKH5_2001_2014 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKH5_2001_2014) 
dim(census1)
dim(census2)
head(SPKH5_2001_2014)
table(SPKH5_2001_2014$censusID.x)
table(SPKH5_2001_2014$censusID.y)

# calculate time difference and convert time from days to years  
time <- (SPKH5_2001_2014$date.y-SPKH5_2001_2014$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH5_2001_2014$dbh.y
size1 <- SPKH5_2001_2014$dbh.x

# calculate growth rates: 
SPKH5_2001_2014$annual_increment <- (size2 - size1)/time
SPKH5_2001_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH5_2001_2014$annual_increment)
summary(SPKH5_2001_2014$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH5_2001_2014$relative_gr, xlab="SPKH5 2001-14 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH5_2001_2014$annual_increment, xlab="SPKH5 2001-14 Annual increment (cm)", col="grey", main="")

#SPKA5L Plot----------
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

summary(SPKH30_2001_2010$annual_increment)
summary(SPKH30_2001_2010$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH30_2001_2010$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2001_2010$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA301 Plot----------
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

summary(SPKH30_2010_2015$annual_increment)
summary(SPKH30_2010_2015$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH30_2010_2015$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2010_2015$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKA302 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH30_2010_2015$dbh.x,SPKH30_2010_2015$dbh.y, pch=19, 
     xlab="DBH SPKH30 2010 (cm)", ylab="DBH SPKH30 2015 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 


#SPKH30 Large interval --------
SPKH30 <- filter(growdata, plot == "SPKH_30")
table(SPKH30$censusID)

SPK3001  <- filter(SPKH30, censusID == "30_census_2001")
SPKH30101 <- filter(SPKH30, censusID == "30_census_2010")
SPKH30102 <- filter(SPKH30, censusID == "30_census_2010")
SPKH3015  <- filter(SPKH30, censusID == "30_census_2015")

SPK3001$pool_stem_ID    <- paste0(SPK3001$stemID, "_1") 
SPKH30101$pool_stem_ID   <- paste0(SPKH30101$stemID, "_2")
SPKH30102$pool_stem_ID <- paste0(SPKH30102$stemID, "_1") 
SPKH3015$pool_stem_ID  <- paste0(SPKH3015$stemID, "_2")

census1 <- rbind(SPK3001, SPKH30101)
census2 <- rbind(SPKH30102, SPKH3015)

SPKH30_2001_2015 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKH30_2001_2015) 
dim(census1)
dim(census2)
head(SPKH30_2001_2015)
table(SPKH30_2001_2015$censusID.x)
table(SPKH30_2001_2015$censusID.y)

# calculate time difference and convert time from days to years  
time <- (SPKH30_2001_2015$date.y-SPKH30_2001_2015$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKH30_2001_2015$dbh.y
size1 <- SPKH30_2001_2015$dbh.x

# calculate growth rates: 
SPKH30_2001_2015$annual_increment <- (size2 - size1)/time
SPKH30_2001_2015$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH30_2001_2015$annual_increment)
summary(SPKH30_2001_2015$relative_gr)

# take a look at the values
par(mfrow=c(1,2))
hist(SPKH30_2001_2015$relative_gr, xlab="SPKH30 2001-15 Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKH30_2001_2015$annual_increment, xlab="SPKH30 2001-15 Annual increment (cm)", col="grey", main="")

#SPKA30L Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKH30_2001_2015$dbh.x,SPKH30_2001_2015$dbh.y, pch=19, 
     xlab="DBH SPKH30 2001 (cm)", ylab="DBH SPKH30 2015 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 

#LHP_clay first interval------------
LHP <- filter(growdata, site == "LHP")
table(LHP$plot)
LHP_clay <- filter(growdata, plot=="LH_clay")
table(LHP_clay$censusID)

census1 <- filter(growdata, plot == "LH_clay" & censusID == "census_1991")
census2 <- filter(growdata, plot == "LH_clay" & censusID == "census_1997")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_clay_1991_1997 <- inner_join(census1, census2, by="stemID")
dim(LH_clay_1991_1997) 

# calculate time difference and convert time from days to years  
time <- (LH_clay_1991_1997$date.y-LH_clay_1991_1997$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_clay_1991_1997$dbh.y
size1 <- LH_clay_1991_1997$dbh.x

# calculate growth rates: 
LH_clay_1991_1997$annual_increment <- (size2 - size1)/time
LH_clay_1991_1997$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_clay_1991_1997)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_clay_1991_1997$annual_increment)
summary(LH_clay_1991_1997$relative_gr )

par(mfrow=c(1,2))
hist(LH_clay_1991_1997$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_clay_1991_1997$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_clay second interval------------
table(LHP_clay$censusID)

census1 <- filter(growdata, plot == "LH_clay" & censusID == "census_1997")
census2 <- filter(growdata, plot == "LH_clay" & censusID == "census_2003")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_clay_1997_2003 <- inner_join(census1, census2, by="stemID")
dim(LH_clay_1997_2003) 

# calculate time difference and convert time from days to years  
time <- (LH_clay_1997_2003$date.y-LH_clay_1997_2003$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_clay_1997_2003$dbh.y
size1 <- LH_clay_1997_2003$dbh.x

# calculate growth rates: 
LH_clay_1997_2003$annual_increment <- (size2 - size1)/time
LH_clay_1997_2003$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_clay_1997_2003)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_clay_1997_2003$annual_increment)
summary(LH_clay_1997_2003$relative_gr )

par(mfrow=c(1,2))
hist(LH_clay_1997_2003$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_clay_1997_2003$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_clay third interval------------
table(LHP_clay$censusID)

census1 <- filter(growdata, plot == "LH_clay" & censusID == "census_2003")
census2 <- filter(growdata, plot == "LH_clay" & censusID == "census_2007_08")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_clay_2003_2008 <- inner_join(census1, census2, by="stemID")
dim(LH_clay_2003_2008) 

# calculate time difference and convert time from days to years  
time <- (LH_clay_2003_2008$date.y-LH_clay_2003_2008$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_clay_2003_2008$dbh.y
size1 <- LH_clay_2003_2008$dbh.x

# calculate growth rates: 
LH_clay_2003_2008$annual_increment <- (size2 - size1)/time
LH_clay_2003_2008$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_clay_2003_2008)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_clay_2003_2008$annual_increment)
summary(LH_clay_2003_2008$relative_gr )

par(mfrow=c(1,2))
hist(LH_clay_2003_2008$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_clay_2003_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_fineloam first interval------------
LHP <- filter(growdata, site == "LHP")
table(LHP$plot)
LHP_fineloam <- filter(growdata, plot=="LH_fineloam")
table(LHP_fineloam$censusID)

census1 <- filter(growdata, plot == "LH_fineloam" & censusID == "census_1991")
census2 <- filter(growdata, plot == "LH_fineloam" & censusID == "census_1997")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_fineloam_1991_1997 <- inner_join(census1, census2, by="stemID")
dim(LH_fineloam_1991_1997) 

# calculate time difference and convert time from days to years  
time <- (LH_fineloam_1991_1997$date.y-LH_fineloam_1991_1997$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_fineloam_1991_1997$dbh.y
size1 <- LH_fineloam_1991_1997$dbh.x

# calculate growth rates: 
LH_fineloam_1991_1997$annual_increment <- (size2 - size1)/time
LH_fineloam_1991_1997$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_fineloam_1991_1997)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_fineloam_1991_1997$annual_increment)
summary(LH_fineloam_1991_1997$relative_gr )

par(mfrow=c(1,2))
hist(LH_fineloam_1991_1997$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_fineloam_1991_1997$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_fineloam second interval------------
table(LHP_fineloam$censusID)

census1 <- filter(growdata, plot == "LH_fineloam" & censusID == "census_1997")
census2 <- filter(growdata, plot == "LH_fineloam" & censusID == "census_2003")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_fineloam_1997_2003 <- inner_join(census1, census2, by="stemID")
dim(LH_fineloam_1997_2003) 

# calculate time difference and convert time from days to years  
time <- (LH_fineloam_1997_2003$date.y-LH_fineloam_1997_2003$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_fineloam_1997_2003$dbh.y
size1 <- LH_fineloam_1997_2003$dbh.x

# calculate growth rates: 
LH_fineloam_1997_2003$annual_increment <- (size2 - size1)/time
LH_fineloam_1997_2003$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_fineloam_1997_2003)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_fineloam_1997_2003$annual_increment)
summary(LH_fineloam_1997_2003$relative_gr )

par(mfrow=c(1,2))
hist(LH_fineloam_1997_2003$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_fineloam_1997_2003$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_fineloam third interval------------
table(LHP_fineloam$censusID)

census1 <- filter(growdata, plot == "LH_fineloam" & censusID == "census_2003")
census2 <- filter(growdata, plot == "LH_fineloam" & censusID == "census_2007_08")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_fineloam_2003_2008 <- inner_join(census1, census2, by="stemID")
dim(LH_fineloam_2003_2008) 

# calculate time difference and convert time from days to years  
time <- (LH_fineloam_2003_2008$date.y-LH_fineloam_2003_2008$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_fineloam_2003_2008$dbh.y
size1 <- LH_fineloam_2003_2008$dbh.x

# calculate growth rates: 
LH_fineloam_2003_2008$annual_increment <- (size2 - size1)/time
LH_fineloam_2003_2008$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_fineloam_2003_2008)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_fineloam_2003_2008$annual_increment)
summary(LH_fineloam_2003_2008$relative_gr )

par(mfrow=c(1,2))
hist(LH_fineloam_2003_2008$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_fineloam_2003_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_loam first interval------------
LHP <- filter(growdata, site == "LHP")
table(LHP$plot)
LHP_loam <- filter(growdata, plot=="LH_loam")
table(LHP_loam$censusID)

census1 <- filter(growdata, plot == "LH_loam" & censusID == "census_1991")
census2 <- filter(growdata, plot == "LH_loam" & censusID == "census_1997")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_loam_1991_1997 <- inner_join(census1, census2, by="stemID")
dim(LH_loam_1991_1997) 

# calculate time difference and convert time from days to years  
time <- (LH_loam_1991_1997$date.y-LH_loam_1991_1997$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_loam_1991_1997$dbh.y
size1 <- LH_loam_1991_1997$dbh.x

# calculate growth rates: 
LH_loam_1991_1997$annual_increment <- (size2 - size1)/time
LH_loam_1991_1997$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_loam_1991_1997)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_loam_1991_1997$annual_increment)
summary(LH_loam_1991_1997$relative_gr )

par(mfrow=c(1,2))
hist(LH_loam_1991_1997$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_loam_1991_1997$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_loam second interval------------
table(LHP_loam$censusID)

census1 <- filter(growdata, plot == "LH_loam" & censusID == "census_1997")
census2 <- filter(growdata, plot == "LH_loam" & censusID == "census_2003")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_loam_1997_2003 <- inner_join(census1, census2, by="stemID")
dim(LH_loam_1997_2003) 

# calculate time difference and convert time from days to years  
time <- (LH_loam_1997_2003$date.y-LH_loam_1997_2003$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_loam_1997_2003$dbh.y
size1 <- LH_loam_1997_2003$dbh.x

# calculate growth rates: 
LH_loam_1997_2003$annual_increment <- (size2 - size1)/time
LH_loam_1997_2003$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_loam_1997_2003)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_loam_1997_2003$annual_increment)
summary(LH_loam_1997_2003$relative_gr )

par(mfrow=c(1,2))
hist(LH_loam_1997_2003$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_loam_1997_2003$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_loam third interval------------
table(LHP_loam$censusID)

census1 <- filter(growdata, plot == "LH_loam" & censusID == "census_2003")
census2 <- filter(growdata, plot == "LH_loam" & censusID == "census_2007_08")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_loam_2003_2008 <- inner_join(census1, census2, by="stemID")
dim(LH_loam_2003_2008) 

# calculate time difference and convert time from days to years  
time <- (LH_loam_2003_2008$date.y-LH_loam_2003_2008$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_loam_2003_2008$dbh.y
size1 <- LH_loam_2003_2008$dbh.x

# calculate growth rates: 
LH_loam_2003_2008$annual_increment <- (size2 - size1)/time
LH_loam_2003_2008$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_loam_2003_2008)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_loam_2003_2008$annual_increment)
summary(LH_loam_2003_2008$relative_gr )

par(mfrow=c(1,2))
hist(LH_loam_2003_2008$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_loam_2003_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_sandstone first interval------------
LHP <- filter(growdata, site == "LHP")
table(LHP$plot)
LHP_sandstone <- filter(growdata, plot=="LH_sandstone")
table(LHP_sandstone$censusID)

census1 <- filter(growdata, plot == "LH_sandstone" & censusID == "census_1991")
census2 <- filter(growdata, plot == "LH_sandstone" & censusID == "census_1997")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_sandstone_1991_1997 <- inner_join(census1, census2, by="stemID")
dim(LH_sandstone_1991_1997) 

# calculate time difference and convert time from days to years  
time <- (LH_sandstone_1991_1997$date.y-LH_sandstone_1991_1997$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_sandstone_1991_1997$dbh.y
size1 <- LH_sandstone_1991_1997$dbh.x

# calculate growth rates: 
LH_sandstone_1991_1997$annual_increment <- (size2 - size1)/time
LH_sandstone_1991_1997$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_sandstone_1991_1997)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_sandstone_1991_1997$annual_increment)
summary(LH_sandstone_1991_1997$relative_gr )

par(mfrow=c(1,2))
hist(LH_sandstone_1991_1997$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_sandstone_1991_1997$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_sandstone second interval------------
table(LHP_sandstone$censusID)

census1 <- filter(growdata, plot == "LH_sandstone" & censusID == "census_1997")
census2 <- filter(growdata, plot == "LH_sandstone" & censusID == "census_2003")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_sandstone_1997_2003 <- inner_join(census1, census2, by="stemID")
dim(LH_sandstone_1997_2003) 

# calculate time difference and convert time from days to years  
time <- (LH_sandstone_1997_2003$date.y-LH_sandstone_1997_2003$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_sandstone_1997_2003$dbh.y
size1 <- LH_sandstone_1997_2003$dbh.x

# calculate growth rates: 
LH_sandstone_1997_2003$annual_increment <- (size2 - size1)/time
LH_sandstone_1997_2003$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_sandstone_1997_2003)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_sandstone_1997_2003$annual_increment)
summary(LH_sandstone_1997_2003$relative_gr )

par(mfrow=c(1,2))
hist(LH_sandstone_1997_2003$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_sandstone_1997_2003$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#LHP_sandstone third interval------------
table(LHP_sandstone$censusID)

census1 <- filter(growdata, plot == "LH_sandstone" & censusID == "census_2003")
census2 <- filter(growdata, plot == "LH_sandstone" & censusID == "census_2007_08")

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LH_sandstone_2003_2008 <- inner_join(census1, census2, by="stemID")
dim(LH_sandstone_2003_2008) 

# calculate time difference and convert time from days to years  
time <- (LH_sandstone_2003_2008$date.y-LH_sandstone_2003_2008$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- LH_sandstone_2003_2008$dbh.y
size1 <- LH_sandstone_2003_2008$dbh.x

# calculate growth rates: 
LH_sandstone_2003_2008$annual_increment <- (size2 - size1)/time
LH_sandstone_2003_2008$relative_gr      <- (log(size2) - log(size1))/time

summary(LH_sandstone_2003_2008)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(LH_sandstone_2003_2008$annual_increment)
summary(LH_sandstone_2003_2008$relative_gr )

par(mfrow=c(1,2))
hist(LH_sandstone_2003_2008$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(LH_sandstone_2003_2008$annual_increment, xlab="Annual increment (cm)", col="grey", main="")



#Create Combined Dataset of all growth rates-----

growthr <- rbind(SPKS08_2001_2009, SPKS08_2009_2014, DNM1_2006_2013, DNM1_2013_2016,
      DNM2_2006_2013, DNM2_2013_2016, DNM3_2006_2013, DNM3_2013_2016, DNM50_2011_2019, 
      SPKA9_2001_2009, SPKA9_2009_2014,SPKA10_2001_2009, SPKA10_2009_2014,
      SPKH4_2001_2008, SPKH4_2008_2014, SPKH5_2001_2008, 
      SPKH5_2008_2014, SPKH30_2001_2010, SPKH30_2010_2015,
      LH_sandstone_2003_2008, LH_sandstone_1997_2003, LH_sandstone_1991_1997,
      LH_clay_2003_2008, LH_clay_1997_2003, LH_clay_1991_1997,
      LH_fineloam_2003_2008, LH_fineloam_1997_2003, LH_fineloam_1991_1997,
      LH_loam_2003_2008, LH_loam_1997_2003, LH_loam_1991_1997)

growthp <- rbind(SPKS08_2001_2014, DNM1_2006_2016,
                 DNM2_2006_2016, DNM3_2006_2016,
                 DNM50_2011_2019p, SPKA9_2001_2014,
                 SPKA10_2001_2014, SPKH4_2001_2014, 
                 SPKH5_2001_2014,SPKH30_2001_2015)


#---------------------------------------------------------------------------#
#--------------------------Add Height info to growthr------------------------
#---------------------------------------------------------------------------#
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
colnames(growthr)
#Calculate Heights-----
#Feld
growthr$heightFeld <- dbh2h_01(growthr$dbh.y, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(growthr$heightFeld)
#Chave
growthr$heightCh <- dbh2h_34(growthr$dbh.y,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
table(growthr$heightCh)
#Chave with E
# need to specify which "E" value to use - from above
growthr$heightE <- dbh2h_ChaveE(growthr$dbh.y,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)

#Source Height Quantiles------
source("heights.r")

#Exclude Indets???? ASK------
growthr <- filter(growthr, species.y != "Indet")
#quantile 90-------
#Feld
growthr$tree_type90F <- ifelse(growthr$species.y %in% c(emergent90Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
growthr$tree_type90Ch <- ifelse(growthr$species.y %in% c(emergent90Ch), "emrgnt", "non_emrgnt")

#Chave with E
growthr$tree_type90E <- ifelse(growthr$species.y %in% c(emergent90E), "emrgnt", "non_emrgnt")


#quantile 95-------
#Feld
growthr$tree_type95F <- ifelse(growthr$species.y %in% c(emergent95Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
growthr$tree_type95Ch <- ifelse(growthr$species.y %in% c(emergent95Ch), "emrgnt", "non_emrgnt")

#Chave with E
growthr$tree_type95E <- ifelse(growthr$species.y %in% c(emergent95E), "emrgnt", "non_emrgnt")

#quantile 99-------
#Feld
growthr$tree_type99F <- ifelse(growthr$species.y %in% c(emergent99Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
growthr$tree_type99Ch <- ifelse(growthr$species.y %in% c(emergent99Ch), "emrgnt", "non_emrgnt")
#Chave with E
growthr$tree_type99E <- ifelse(growthr$species.y %in% c(emergent99E), "emrgnt", "non_emrgnt")

#DBH------
#90th percentile-----
growthr$tree_type90dbh <- ifelse(growthr$species.y %in% c(emergent90dbh), "emrgnt", "non_emrgnt")

#95th percentile-----
growthr$tree_type95dbh <- ifelse(growthr$species.y %in% c(emergent95dbh), "emrgnt", "non_emrgnt")

#99th percentile-----
growthr$tree_type99dbh <- ifelse(growthr$species.y %in% c(emergent99dbh), "emrgnt", "non_emrgnt")


#---------------------------------------------------------------------------#
#-------------------------Plot Variables-------------------------------------
#---------------------------------------------------------------------------#

#Growth~Height Definitions-------
#height bins------
growthr$heightFeld_class <- 
        cut(growthr$heightFeld, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                         110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(growthr$dbh.y, na.rm=T)),
        )
growthr$heightCh_class <- 
        cut(growthr$heightCh, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                       110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(growthr$dbh.y, na.rm=T)),
        )
growthr$heightE_class <- 
        cut(growthr$heightE, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                      110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(growthr$dbh.y, na.rm=T)),
        )
#Separate by site------
table(growthr$site.x)
DNM1=filter(growthr, site.x == "DNM1")
DNM2=filter(growthr, site.x == "DNM2")
DNM3=filter(growthr, site.x == "DNM3")
DNM50=filter(growthr, site.x == "DNM50")
LHP=filter(growthr, site.x == "LHP")
SPKA=filter(growthr, site.x == "SPKA")
SPKH=filter(growthr, site.x == "SPKH")
SPKS=filter(growthr, site.x == "SPKS")


DNM1GRnem90F <- filter(DNM1, tree_type90F == "non_emrgnt")
DNM1GRnem95F <- filter(DNM1, tree_type95F == "non_emrgnt")
DNM1GRnem99F <- filter(DNM1, tree_type99F == "non_emrgnt")
DNM1GRnem90Ch <- filter(DNM1, tree_type90Ch == "non_emrgnt")
DNM1GRnem95Ch <- filter(DNM1, tree_type95Ch == "non_emrgnt")
DNM1GRnem99Ch <- filter(DNM1, tree_type99Ch == "non_emrgnt")
DNM1GRnem90E <- filter(DNM1, tree_type90E == "non_emrgnt")
DNM1GRnem95E <- filter(DNM1, tree_type95E == "non_emrgnt")
DNM1GRnem99E <- filter(DNM1, tree_type99E == "non_emrgnt")

DNM2GRnem90F <- filter(DNM2, tree_type90F == "non_emrgnt")
DNM2GRnem95F <- filter(DNM2, tree_type95F == "non_emrgnt")
DNM2GRnem99F <- filter(DNM2, tree_type99F == "non_emrgnt")
DNM2GRnem90Ch <- filter(DNM2, tree_type90Ch == "non_emrgnt")
DNM2GRnem95Ch <- filter(DNM2, tree_type95Ch == "non_emrgnt")
DNM2GRnem99Ch <- filter(DNM2, tree_type99Ch == "non_emrgnt")
DNM2GRnem90E <- filter(DNM2, tree_type90E == "non_emrgnt")
DNM2GRnem95E <- filter(DNM2, tree_type95E == "non_emrgnt")
DNM2GRnem99E <- filter(DNM2, tree_type99E == "non_emrgnt")

DNM3GRnem90F <- filter(DNM3, tree_type90F == "non_emrgnt")
DNM3GRnem95F <- filter(DNM3, tree_type95F == "non_emrgnt")
DNM3GRnem99F <- filter(DNM3, tree_type99F == "non_emrgnt")
DNM3GRnem90Ch <- filter(DNM3, tree_type90Ch == "non_emrgnt")
DNM3GRnem95Ch <- filter(DNM3, tree_type95Ch == "non_emrgnt")
DNM3GRnem99Ch <- filter(DNM3, tree_type99Ch == "non_emrgnt")
DNM3GRnem90E <- filter(DNM3, tree_type90E == "non_emrgnt")
DNM3GRnem95E <- filter(DNM3, tree_type95E == "non_emrgnt")
DNM3GRnem99E <- filter(DNM3, tree_type99E == "non_emrgnt")

DNM50GRnem90F <- filter(DNM50, tree_type90F == "non_emrgnt")
DNM50GRnem95F <- filter(DNM50, tree_type95F == "non_emrgnt")
DNM50GRnem99F <- filter(DNM50, tree_type99F == "non_emrgnt")
DNM50GRnem90Ch <- filter(DNM50, tree_type90Ch == "non_emrgnt")
DNM50GRnem95Ch <- filter(DNM50, tree_type95Ch == "non_emrgnt")
DNM50GRnem99Ch <- filter(DNM50, tree_type99Ch == "non_emrgnt")
DNM50GRnem90E <- filter(DNM50, tree_type90E == "non_emrgnt")
DNM50GRnem95E <- filter(DNM50, tree_type95E == "non_emrgnt")
DNM50GRnem99E <- filter(DNM50, tree_type99E == "non_emrgnt")

LHPGRnem90F <- filter(LHP, tree_type90F == "non_emrgnt")
LHPGRnem95F <- filter(LHP, tree_type95F == "non_emrgnt")
LHPGRnem99F <- filter(LHP, tree_type99F == "non_emrgnt")
LHPGRnem90Ch <- filter(LHP, tree_type90Ch == "non_emrgnt")
LHPGRnem95Ch <- filter(LHP, tree_type95Ch == "non_emrgnt")
LHPGRnem99Ch <- filter(LHP, tree_type99Ch == "non_emrgnt")
LHPGRnem90E <- filter(LHP, tree_type90E == "non_emrgnt")
LHPGRnem95E <- filter(LHP, tree_type95E == "non_emrgnt")
LHPGRnem99E <- filter(LHP, tree_type99E == "non_emrgnt")

SPKAGRnem90F <- filter(SPKA, tree_type90F == "non_emrgnt")
SPKAGRnem95F <- filter(SPKA, tree_type95F == "non_emrgnt")
SPKAGRnem99F <- filter(SPKA, tree_type99F == "non_emrgnt")
SPKAGRnem90Ch <- filter(SPKA, tree_type90Ch == "non_emrgnt")
SPKAGRnem95Ch <- filter(SPKA, tree_type95Ch == "non_emrgnt")
SPKAGRnem99Ch <- filter(SPKA, tree_type99Ch == "non_emrgnt")
SPKAGRnem90E <- filter(SPKA, tree_type90E == "non_emrgnt")
SPKAGRnem95E <- filter(SPKA, tree_type95E == "non_emrgnt")
SPKAGRnem99E <- filter(SPKA, tree_type99E == "non_emrgnt")

SPKHGRnem90F <- filter(SPKH, tree_type90F == "non_emrgnt")
SPKHGRnem95F <- filter(SPKH, tree_type95F == "non_emrgnt")
SPKHGRnem99F <- filter(SPKH, tree_type99F == "non_emrgnt")
SPKHGRnem90Ch <- filter(SPKH, tree_type90Ch == "non_emrgnt")
SPKHGRnem95Ch <- filter(SPKH, tree_type95Ch == "non_emrgnt")
SPKHGRnem99Ch <- filter(SPKH, tree_type99Ch == "non_emrgnt")
SPKHGRnem90E <- filter(SPKH, tree_type90E == "non_emrgnt")
SPKHGRnem95E <- filter(SPKH, tree_type95E == "non_emrgnt")
SPKHGRnem99E <- filter(SPKH, tree_type99E == "non_emrgnt")

SPKSGRnem90F <- filter(SPKS, tree_type90F == "non_emrgnt")
SPKSGRnem95F <- filter(SPKS, tree_type95F == "non_emrgnt")
SPKSGRnem99F <- filter(SPKS, tree_type99F == "non_emrgnt")
SPKSGRnem90Ch <- filter(SPKS, tree_type90Ch == "non_emrgnt")
SPKSGRnem95Ch <- filter(SPKS, tree_type95Ch == "non_emrgnt")
SPKSGRnem99Ch <- filter(SPKS, tree_type99Ch == "non_emrgnt")
SPKSGRnem90E <- filter(SPKS, tree_type90E == "non_emrgnt")
SPKSGRnem95E <- filter(SPKS, tree_type95E == "non_emrgnt")
SPKSGRnem99E <- filter(SPKS, tree_type99E == "non_emrgnt")


growthrGRnem90F <- filter(growthr, tree_type90F == "non_emrgnt")
growthrGRnem95F <- filter(growthr, tree_type95F == "non_emrgnt")
growthrGRnem99F <- filter(growthr, tree_type99F == "non_emrgnt")
growthrGRnem90Ch <- filter(growthr, tree_type90Ch == "non_emrgnt")
growthrGRnem95Ch <- filter(growthr, tree_type95Ch == "non_emrgnt")
growthrGRnem99Ch <- filter(growthr, tree_type99Ch == "non_emrgnt")
growthrGRnem90E <- filter(growthr, tree_type90E == "non_emrgnt")
growthrGRnem95E <- filter(growthr, tree_type95E == "non_emrgnt")
growthrGRnem99E <- filter(growthr, tree_type99E == "non_emrgnt")

growthrGRnem99dbh <- filter(growthr, tree_type99dbh == "non_emrgnt")

#---------------------------------------------------------------------------#
#--------------------------Random Plots-------------------------------------
#---------------------------------------------------------------------------#
#Unpooled Plots-------
#Growth~Diameter-----
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, log(annual_increment)))
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, annual_increment))
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, log(relative_gr)))

growthr$size_class <- 
        cut(growthr$dbh.y, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                    110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(growthr$dbh.y, na.rm=T)),
        )
growthr$size_class2 <- 
        cut(growthr$dbh.y, breaks=c(0,10,20,30,40,50,60,70,80,90,100,
                                    110,120,130,140,150,160,170,180,190,200,210,max(growthr$dbh.y, na.rm=T)),
        )

par(mfrow=c(1,2))
plot(growthr$size_class, log(growthr$annual_increment), pch=19, 
     xlab="DBH", ylab="annual increment", )
plot(growthr$size_class2, log(growthr$annual_increment), pch=19, 
     xlab="DBH", ylab="annual increment", )

par(mfrow=c(1,2))
plot(growthr$size_class, log(growthr$relative_gr), pch=19, 
     xlab="DBH", ylab="relative", )
plot(growthr$size_class2, log(growthr$relative_gr), pch=19, 
     xlab="DBH", ylab="relative", )

#everything scatter plot-------
#90th percentile
#all
growthr %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "chartreuse3")+
        geom_point(data = growthrGRnem90F,color = "chartreuse4")+
        geom_vline(xintercept = quantile90Feld, color="black")
growthr %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "darkorchid2")+
        geom_point(data = growthrGRnem90Ch,color = "darkorchid4")+
        geom_vline(xintercept = quantile90Ch, color="black")
growthr %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "deepskyblue2")+
        geom_point(data = growthrGRnem90E,color = "deepskyblue4")+
        geom_vline(xintercept = quantile90E, color="black")
#95th percentile
growthr %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "chartreuse3")+
        geom_point(data = growthrGRnem95F,color = "chartreuse4")+
        geom_vline(xintercept = quantile95Feld, color="black")
growthr %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "darkorchid2")+
        geom_point(data = growthrGRnem95Ch,color = "darkorchid4")+
        geom_vline(xintercept = quantile95Ch, color="black")
growthr %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "deepskyblue2")+
        geom_point(data = growthrGRnem95E,color = "deepskyblue4")+
        geom_vline(xintercept = quantile95E, color="black")
#99th percentile
growthr %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "chartreuse3")+
        geom_point(data = growthrGRnem99F,color = "chartreuse4")+
        geom_vline(xintercept = quantile99Feld, color="green")
growthr %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "darkorchid2")+
        geom_point(data = growthrGRnem99Ch,color = "darkorchid4")+
        geom_vline(xintercept = quantile99Ch, color="black")
growthr %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "deepskyblue2")+
        geom_point(data = growthrGRnem99E,color = "deepskyblue4")+
        geom_vline(xintercept = quantile99E, color="black")
#everything box plot-------
#annual increment
growthr %>%
        ggplot(aes(heightFeld_class, annual_increment))+
        geom_boxplot(color = "blue")
growthr %>%
        ggplot(aes(heightCh_class, annual_increment))+
        geom_boxplot(color = "blue")
growthr %>%
        ggplot(aes(heightE_class, annual_increment))+
        geom_boxplot(color = "blue")
#rel growth
growthr %>%
        ggplot(aes(heightFeld_class, relative_gr))+
        geom_boxplot(color = "blue")
growthr %>%
        ggplot(aes(heightCh_class, relative_gr))+
        geom_boxplot(color = "blue")
growthr %>%
        ggplot(aes(heightE_class, relative_gr))+
        geom_boxplot(color = "blue")
#emergents and non emergents separated-------
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type90F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type90Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type90E))+
        geom_boxplot()

growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type95F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type95Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type95E))+
        geom_boxplot()

growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type99F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type99Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type99E))+
        geom_boxplot()
#annual increment
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type90F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type90Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type90E))+
        geom_boxplot()

growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type95F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type95Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type95E))+
        geom_boxplot()

growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()

#annual increment
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type90F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type90Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type90E))+
        geom_boxplot()

growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type95F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type95Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type95E))+
        geom_boxplot()

growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()
#Some plots----
DNM1 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type90F))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type90Ch))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type90E))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type95F))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type95Ch))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type95E))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=heightCh_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
DNM1 %>%
        ggplot(aes(x=heightE_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()
DNM50 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
DNM50 %>%
        ggplot(aes(x=heightCh_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
DNM50 %>%
        ggplot(aes(x=heightE_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()


par(mfrow=c(1,3))
DNM2 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
DNM2 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
DNM2 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()

par(mfrow=c(1,3))
DNM3 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
DNM3 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
DNM3 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()
par(mfrow=c(1,3))
DNM50 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
DNM50 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
DNM50 %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()
par(mfrow=c(1,3))
LHP %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
LHP %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
LHP %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()
par(mfrow=c(1,3))
SPKA %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99F))+
        geom_boxplot()
SPKA %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99Ch))+
        geom_boxplot()
SPKA %>%
        ggplot(aes(x=heightFeld_class, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()

#DNM1 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile

ggplot(aes(heightFeld_class, annual_increment))+
        geom_boxplot(color = "blue")+
        geom_boxplot(data = DNM1em90F,mapping = aes(heightFeld_class, annual_increment), color="red")+
        geom_vline(xintercept = quantile90Feld)

#Feldpausch Heights 95th percentile
DNM1 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM1 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM1 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM1 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM1 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM1 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM1 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM1 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM1GRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#DNM2 Plot------
DNM2 %>%
        ggplot(aes(x=dbh.y, y=log(annual_increment), fill=tree_type99E))+
        geom_boxplot()
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM2 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
DNM2 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM2 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM2 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM2 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM2 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM2 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM2 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM2 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM2GRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#DNM3 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM3 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
DNM3 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM3 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM3 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM3 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM3 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM3 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM3 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM3 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#DNM50 Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
DNM50 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
DNM50 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
DNM50 %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM3GRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
DNM50 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM50GRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
DNM50 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM50GRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
DNM50 %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM50GRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
DNM50 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM50GRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
DNM50 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM50GRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
DNM50 %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = DNM50GRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#LHP Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
LHP %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
LHP %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
LHP %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
LHP %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
LHP %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
LHP %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
LHP %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
LHP %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
LHP %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = LHPGRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#SPKA Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKA %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
SPKA %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
SPKA %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKA %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
SPKA %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
SPKA %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKA %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
SPKA %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
SPKA %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKAGRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#SPKH Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKH %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
SPKH %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
SPKH %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKH %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
SPKH %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
SPKH %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKH %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
SPKH %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
SPKH %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKHGRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#SPKS Plot------
#Feld-----
par(mfrow=c(3,1))
#Feldpausch Heights 90th percentile
SPKS %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem90F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 95th percentile
SPKS %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem95F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Feld)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Feldpausch Heights 99th percentile
SPKS %>%
        ggplot(aes(heightFeld, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem99F,mapping = aes(heightFeld, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Feld) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w/o E-----
par(mfrow=c(3,1))
#Chave Heights 90th percentile
SPKS %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem90Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 95th percentile
SPKS %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem95Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95Ch)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave Heights 99th percentile
SPKS %>%
        ggplot(aes(heightCh, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem99Ch,mapping = aes(heightCh, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99Ch) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave w E-----
par(mfrow=c(3,1))
#Chave E Heights 90th percentile
SPKS %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem90E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile90E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 95th percentile
SPKS %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem95E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile95E)+
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#Chave E Heights 99th percentile
SPKS %>%
        ggplot(aes(heightE, annual_increment))+
        geom_point(color = "red")+
        geom_point(data = SPKSGRnem99E,mapping = aes(heightE, annual_increment), color="blue")+
        geom_vline(xintercept = quantile99E) +
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log')

#----------------------------------------------------------------------#
#-------------------Plots for manuscript-------------------------------
#----------------------------------------------------------------------#
bincheck <- filter(growthr, dbh.y>=124 & dbh.y<134)
bincheck <- filter(growthr, dbh.y>=134 & dbh.y<156)
bincheck <- filter(growthr, dbh.y>=156 & dbh.y<300)
#Binned Box Plots
growthr$size_class <- 
        cut(growthr$dbh.y, breaks=c(seq(0,116,by=4), 124, 134, 156, 300)
        )
plot(growthr$size_class, log(growthr$annual_increment), pch=19, 
     xlab="DBH", ylab="annual increment", )

plot(growthr$size_class, log(growthr$relative_gr), pch=19, 
     xlab="DBH", ylab="relative growth", )

#Site and Growth Rate Box Plots ASK ABOUT ANOVA!
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, log(annual_increment)))
an_gr.aov <- aov(annual_increment ~ site.x, data = growthr)
summary(an_gr.aov)
ggplot() +
        geom_boxplot(growthr, mapping = aes(site.x, log(relative_gr)))
rel_gr.aov <- aov(relative_gr ~ site.x, data = growthr)
summary(rel_gr.aov)
#DBH and Growth Rate Scatter Plot
growthr %>%
        ggplot(aes(dbh.y, log(annual_increment)))+
        geom_point(color = "chartreuse3")+
        geom_point(data = growthrGRnem99dbh,color = "chartreuse4")+
        geom_vline(xintercept = quantile99dbh, color="black")
growthr %>%
        ggplot(aes(dbh.y, log(relative_gr)))+
        geom_point(color = "chartreuse3")+
        geom_point(data = growthrGRnem99dbh,color = "chartreuse4")+
        geom_vline(xintercept = quantile99dbh, color="black")

#Box Plots Separated by emergent definitions
growthr %>%
        ggplot(aes(x=site.x, y=log(relative_gr), fill=tree_type99dbh))+
        geom_boxplot()
growthr %>%
        ggplot(aes(x=site.x, y=log(annual_increment), fill=tree_type99dbh))+
        geom_boxplot()




