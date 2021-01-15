library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)
#Load Data----------
#data <- read_csv(here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))
data <- read_csv("G:/My Drive/Harvard/Emergent_project/Data/data_first_clean.csv")

colnames(data)
growdata <-rename (data, censusID = census, dbh = dbh, date = JulianDate, status = DFstatus)
colnames(growdata)

#Update DFstatus---------
library(stringr)

growdata$status <- gsub("0", "y", growdata$status)
growdata$status <- gsub("dead", "y", growdata$status)
growdata$status<- gsub("broken below", "yy", growdata$status)
growdata$status <- gsub("^b.*", "yy", growdata$status)
growdata$status <- gsub("missing", "yyy", growdata$status)

growdata$status<- gsub("[^y]+", "A", growdata$status)

growdata$status <- gsub("yyy","missing", growdata$status)
growdata$status<- gsub("yy", "B", growdata$status)
growdata$status <- gsub("y", "D", growdata$status)

table(growdata$status)
table(data$DFstatus)

growdata$hom <- rep(130, length(growdata$dbh));

growdata <- filter(growdata, status == "A")

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

SPKS08_2009_2014 <- filter(SPKS08_2009_2014, SPKS08_2009_2014$annual_increment >= 0
                           & SPKS08_2009_2014$annual_increment < 7.5)

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

# SPKS8cen1 <- filter(SPKS8, censusID == "08_census_2001")
# table(SPKS8cen1$stemID)
#I dunnooo----------

#recombine into 2 new pooled dataset - row bind
#census 1 <- SPKS01 SPKS092
#census 2 <- SPKS091 SPKS14
#have 4 distinct time periods

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

DNM1_2013_2016 <- filter(DNM1_2013_2016, DNM1_2013_2016$annual_increment >= 0
                         & DNM1_2013_2016$annual_increment < 7.5)

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

DNM3_2013_2016 <- filter(DNM3_2013_2016, DNM3_2013_2016$annual_increment >= 0
                         & DNM3_2013_2016$annual_increment < 7.5)


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

DNM50_2011_2019 <-  filter(DNM50_2011_2019, DNM50_2011_2019$annual_increment >= 0
                           & DNM50_2011_2019$annual_increment < 7.5)


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

SPKH5_2001_2008 <- filter(SPKH5_2001_2008, SPKH5_2001_2008$annual_increment >= 0
                          & SPKH5_2001_2008$annual_increment < 7.5)

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

SPKH5_2008_2014 <- filter(SPKH5_2008_2014, SPKH5_2008_2014$annual_increment >= 0
                          & SPKH5_2008_2014$annual_increment < 7.5)

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

SPKH30_2001_2010 <- filter(SPKH30_2001_2010, SPKH30_2001_2010$annual_increment >= 0
                           & SPKH30_2001_2010$annual_increment < 7.5)

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

SPKH30_2010_2015 <- filter(SPKH30_2010_2015, SPKH30_2010_2015$annual_increment >= 0
                          & SPKH30_2010_2015$annual_increment < 7.5)

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
growthr <- rbind(SPKS08_2001_2009, SPKS08_2009_2014, SPKS08_2001_2014, DNM1_2006_2013, DNM1_2013_2016, DNM1_2006_2016,
      DNM2_2006_2013, DNM2_2013_2016, DNM2_2006_2016, DNM3_2006_2013, DNM3_2013_2016, DNM3_2006_2016,
      DNM50_2011_2019, SPKA9_2001_2009, SPKA9_2009_2014, SPKA9_2001_2014, SPKA10_2001_2009, SPKA10_2009_2014,
      SPKA10_2001_2014, SPKH4_2001_2008, SPKH4_2008_2014, SPKH4_2001_2014, SPKH5_2001_2008, 
      SPKH5_2008_2014, SPKH5_2001_2014, SPKH30_2001_2010, SPKH30_2010_2015, SPKH30_2001_2015)

