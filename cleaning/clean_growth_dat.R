## Clean inventory plot data for growth rates analysis
## Remove based on growth rate outliers 
## 01-21-2021

library(dplyr)
library(tidyverse)
library(here)
library(skimr)
library(stringr)

#---------------------------------------------------------------------------------------------#
# Load data                                                                                   # 
#---------------------------------------------------------------------------------------------#
#data <- read_csv(here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))
data <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv")

clean_dat <-rename (data, censusID = census, dbh = dbh, date = JulianDate, status = DFstatus)
#---------------------------------------------------------------------------------------------#

# test <- subset(clean_dat, site == "DNM50")
# test <- subset(test, status != "A")
# summary(test$dbh)
# table(test$status)

summary(clean_dat$dbh)
clean_dat %>% group_by(plot) %>% summarize(min_dbh = min(dbh, na.rm=T))

ggplot(clean_dat, aes(dbh)) + 
        geom_histogram() +
        facet_wrap(~plot, scales="free") +
        geom_vline(xintercept = 5, col="red", lty=2) + 
        geom_vline(xintercept = 10, col="red") + 
        theme_classic()
# 8x5 Harvard/Figures/site_min_dbh

#---------------------------------------------------------------------------------------------#
# Keep only alive stems
#---------------------------------------------------------------------------------------------#
growdata <- filter(clean_dat, status == "A")
table(growdata$status)
summary(growdata$dbh)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# FILTER BASED ON GROWTH RATES (see Condit et al 2006 Supplementary Material)
# exclude stems if:
#  shrunk > 25% of initial DBH (rgr < -0.25)
#  grew > 7.5 mm (annual increment > 7.5)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM1 First interval 
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2006")
census2 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2013")

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2006_2013) 

time <- (DNM1_2006_2013$date.y-DNM1_2006_2013$date.x)/365
size2 <- DNM1_2006_2013$dbh.y
size1 <- DNM1_2006_2013$dbh.x

# calculate growth rates: 
DNM1_2006_2013$annual_increment <- (size2 - size1)/time
DNM1_2006_2013$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM1_2006_2013$annual_increment)
summary(DNM1_2006_2013$relative_gr  )
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
DNM1_2006_2013remove <- filter(DNM1_2006_2013, annual_increment >= 7.5
                               | relative_gr < -0.25)

summary(DNM1_2006_2013)
summary(DNM1_2006_2013remove)
dim(DNM1_2006_2013) 
dim(DNM1_2006_2013remove)
dim(DNM1_2006_2013remove)[[1]]/dim(DNM1_2006_2013)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# DNM1 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2013")
census2 <- filter(growdata, plot == "DNM1_01" & censusID == "01_census_2016")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2013_2016) 

time <- (DNM1_2013_2016$date.y-DNM1_2013_2016$date.x)/365
size2 <- DNM1_2013_2016$dbh.y
size1 <- DNM1_2013_2016$dbh.x

# calculate growth rates: 
DNM1_2013_2016$annual_increment <- (size2 - size1)/time
DNM1_2013_2016$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM1_2013_2016$annual_increment)
summary(DNM1_2013_2016$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
DNM1_2013_2016remove <- filter(DNM1_2013_2016, annual_increment >= 7.5
                               | relative_gr < -0.25)

summary(DNM1_2013_2016)
summary(DNM1_2013_2016remove)
dim(DNM1_2013_2016) 
dim(DNM1_2013_2016remove)
dim(DNM1_2013_2016remove)[[1]]/dim(DNM1_2013_2016)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM2 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2006")
census2 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2013")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2006_2013) 

time <- (DNM2_2006_2013$date.y-DNM2_2006_2013$date.x)/365
size2 <- DNM2_2006_2013$dbh.y
size1 <- DNM2_2006_2013$dbh.x

# calculate growth rates: 
DNM2_2006_2013$annual_increment <- (size2 - size1)/time
DNM2_2006_2013$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM2_2006_2013$annual_increment)
summary(DNM2_2006_2013$relative_gr )
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
DNM2_2006_2013remove <- filter(DNM2_2006_2013, annual_increment >= 7.5
                               | relative_gr < -0.25)

summary(DNM2_2006_2013)
summary(DNM2_2006_2013remove)
dim(DNM2_2006_2013) 
dim(DNM2_2006_2013remove)
dim(DNM2_2006_2013remove)[[1]]/dim(DNM2_2006_2013)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# DNM2 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2013")
census2 <- filter(growdata, plot == "DNM2_02" & censusID == "02_census_2016")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2013_2016) 

time <- (DNM2_2013_2016$date.y-DNM2_2013_2016$date.x)/365
size2 <- DNM2_2013_2016$dbh.y
size1 <- DNM2_2013_2016$dbh.x

# calculate growth rates: 
DNM2_2013_2016$annual_increment <- (size2 - size1)/time
DNM2_2013_2016$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM2_2013_2016$annual_increment)
summary(DNM2_2013_2016$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
DNM2_2013_2016remove <- filter(DNM2_2013_2016, annual_increment >= 7.5
                               | relative_gr < -0.25)

summary(DNM2_2013_2016)
summary(DNM2_2013_2016remove)
dim(DNM2_2013_2016) 
dim(DNM2_2013_2016remove)
dim(DNM2_2013_2016remove)[[1]]/dim(DNM2_2013_2016)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM3 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2006")
census2 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2013")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2006_2013) 

time <- (DNM3_2006_2013$date.y-DNM3_2006_2013$date.x)/365
size2 <- DNM3_2006_2013$dbh.y
size1 <- DNM3_2006_2013$dbh.x

# calculate growth rates: 
DNM3_2006_2013$annual_increment <- (size2 - size1)/time
DNM3_2006_2013$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM3_2006_2013$annual_increment)
summary(DNM3_2006_2013$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
DNM3_2006_2013remove <- filter(DNM3_2006_2013, annual_increment >= 7.5
                               | relative_gr < -0.25)

summary(DNM3_2006_2013)
summary(DNM3_2006_2013remove)
dim(DNM3_2006_2013) 
dim(DNM3_2006_2013remove)
dim(DNM3_2006_2013remove)[[1]]/dim(DNM3_2006_2013)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# DNM3 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2013")
census2 <- filter(growdata, plot == "DNM3_03" & censusID == "03_census_2016")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2013_2016) 

time <- (DNM3_2013_2016$date.y-DNM3_2013_2016$date.x)/365
size2 <- DNM3_2013_2016$dbh.y
size1 <- DNM3_2013_2016$dbh.x

# calculate growth rates: 
DNM3_2013_2016$annual_increment <- (size2 - size1)/time
DNM3_2013_2016$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM3_2013_2016$annual_increment)
summary(DNM3_2013_2016$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
DNM3_2013_2016remove <- filter(DNM3_2013_2016, annual_increment >= 7.5
                               | relative_gr < -0.25)

summary(DNM3_2013_2016)
summary(DNM3_2013_2016remove)
dim(DNM3_2013_2016) 
dim(DNM3_2013_2016remove)
dim(DNM3_2013_2016remove)[[1]]/dim(DNM3_2013_2016)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# DNM50 
#---------------------------------------------------------------------------------------------#
DNM50 <- filter(growdata, plot == "DNM50_FGEO" )
table(DNM50$censusID)
table(DNM50$status)

census1 <- filter(growdata, plot == "DNM50_FGEO" & censusID == "census_2011_15")
census2 <- filter(growdata, plot == "DNM50_FGEO" & censusID == "census_2019")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM50_2011_2019 <- inner_join(census1, census2, by="stemID")
dim(DNM50_2011_2019) 

time <- (DNM50_2011_2019$date.y-DNM50_2011_2019$date.x)/365
size2 <- DNM50_2011_2019$dbh.y
size1 <- DNM50_2011_2019$dbh.x

# calculate growth rates: 
DNM50_2011_2019$annual_increment <- (size2 - size1)/time
DNM50_2011_2019$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM50_2011_2019$annual_increment)
summary(DNM50_2011_2019$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
#DNM50_2011_2019remove <- filter(DNM50_2011_2019, annual_increment >= 7.5)
#DNM50_2011_2019remove <- filter(DNM50_2011_2019, annual_increment < 0)
DNM50_2011_2019remove <- filter(DNM50_2011_2019, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(DNM50_2011_2019)
summary(DNM50_2011_2019remove)
dim(DNM50_2011_2019) 
dim(DNM50_2011_2019remove)
dim(DNM50_2011_2019remove)[[1]]/dim(DNM50_2011_2019)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 20 stems removed (0.01% ALIVE stems removed) 
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKS06 first interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKS_06" & censusID == "06_census_2003")
census2 <- filter(growdata, plot == "SPKS_06" & censusID == "06_census_2014")
table(growdata$plot)
table(census2$censusID)
summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS06_2003_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKS06_2003_2014) 

# calculate time difference and convert time from days to years  
time <- (SPKS06_2003_2014$date.y-SPKS06_2003_2014$date.x)/365
# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS06_2003_2014$dbh.y
size1 <- SPKS06_2003_2014$dbh.x

# calculate growth rates: 
SPKS06_2003_2014$annual_increment <- (size2 - size1)/time
SPKS06_2003_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKS06_2003_2014)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(SPKS06_2003_2014$annual_increment)
summary(SPKS06_2003_2014$relative_gr )

#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKS06_2003_2014remove <- filter(SPKS06_2003_2014, annual_increment >= 7.5
                                 | relative_gr < -0.25)
summary(SPKS06_2003_2014)
summary(SPKS06_2003_2014remove)
dim(SPKS06_2003_2014) 
dim(SPKS06_2003_2014remove) 
dim(SPKS06_2003_2014remove)[[1]]/dim(SPKS06_2003_2014)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
par(mfrow=c(1,2))
hist(SPKS06_2003_2014$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS06_2003_2014$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKS081 Plot------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKS06_2003_2014$dbh.x, SPKS06_2003_2014$dbh.y, pch=19, 
     xlab="DBH SPKS08 2001 (cm)", ylab="DBH SPKS08 2009 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKS07 first interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKS_07" & censusID == "07_census_2008")
census2 <- filter(growdata, plot == "SPKS_07" & censusID == "07_census_2013")
table(growdata$plot)
table(census2$censusID)
summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS07_2008_2013 <- inner_join(census1, census2, by="stemID")
dim(SPKS07_2008_2013) 

# calculate time difference and convert time from days to years  
time <- (SPKS07_2008_2013$date.y-SPKS07_2008_2013$date.x)/365
# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS07_2008_2013$dbh.y
size1 <- SPKS07_2008_2013$dbh.x

# calculate growth rates: 
SPKS07_2008_2013$annual_increment <- (size2 - size1)/time
SPKS07_2008_2013$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKS07_2008_2013)
# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(SPKS07_2008_2013$annual_increment)
summary(SPKS07_2008_2013$relative_gr)

#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKS07_2008_2013remove <- filter(SPKS07_2008_2013, annual_increment >= 7.5
                                 | relative_gr < -0.25)
summary(SPKS07_2008_2013)
summary(SPKS07_2008_2013remove)
dim(SPKS07_2008_2013) 
dim(SPKS07_2008_2013remove) 
dim(SPKS07_2008_2013remove)[[1]]/dim(SPKS07_2008_2013)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
par(mfrow=c(1,2))
hist(SPKS07_2008_2013$relative_gr , xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(SPKS07_2008_2013$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#SPKS081 Plot------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKS07_2008_2013$dbh.x, SPKS07_2008_2013$dbh.y, pch=19, 
     xlab="DBH SPKS08 2001 (cm)", ylab="DBH SPKS08 2009 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKS08 first interval
#---------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKS08_2001_2009remove <- filter(SPKS08_2001_2009, annual_increment >= 7.5
                                 | relative_gr < -0.25)
summary(SPKS08_2001_2009)
summary(SPKS08_2001_2009remove)
dim(SPKS08_2001_2009) 
dim(SPKS08_2001_2009remove) 
dim(SPKS08_2001_2009remove)[[1]]/dim(SPKS08_2001_2009)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKS08 Second interval
#---------------------------------------------------------------------------------------------#
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

time <- (SPKS08_2009_2014$date.y-SPKS08_2009_2014$date.x)/365
size2 <- SPKS08_2009_2014$dbh.y
size1 <- SPKS08_2009_2014$dbh.x

# calculate growth rates: 
SPKS08_2009_2014$annual_increment <- (size2 - size1)/time
SPKS08_2009_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKS08_2009_2014$annual_increment)
summary(SPKS08_2009_2014$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKS08_2009_2014remove <- filter(SPKS08_2009_2014, annual_increment >= 7.5
                                 | relative_gr < -0.25)

summary(SPKS08_2009_2014)
summary(SPKS08_2009_2014remove)
dim(SPKS08_2009_2014) 
dim(SPKS08_2009_2014remove) 
dim(SPKS08_2009_2014remove)[[1]]/dim(SPKS08_2009_2014)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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


#---------------------------------------------------------------------------------------------#
# SPKA9 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2001")
census2 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2009")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2001_2009) 

time <- (SPKA9_2001_2009$date.y-SPKA9_2001_2009$date.x)/365
size2 <- SPKA9_2001_2009$dbh.y
size1 <- SPKA9_2001_2009$dbh.x

# calculate growth rates: 
SPKA9_2001_2009$annual_increment <- (size2 - size1)/time
SPKA9_2001_2009$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKA9_2001_2009$annual_increment)
summary(SPKA9_2001_2009$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKA9_2001_2009remove <- filter(SPKA9_2001_2009, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(SPKA9_2001_2009)
summary(SPKA9_2001_2009remove)
dim(SPKA9_2001_2009) 
dim(SPKA9_2001_2009remove)
dim(SPKA9_2001_2009remove)[[1]]/dim(SPKA9_2001_2009)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0.0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKA9 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2009")
census2 <- filter(growdata, plot == "SPKA_09" & censusID == "09_census_2014")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2009_2014) 

time <- (SPKA9_2009_2014$date.y-SPKA9_2009_2014$date.x)/365
size2 <- SPKA9_2009_2014$dbh.y
size1 <- SPKA9_2009_2014$dbh.x

# calculate growth rates: 
SPKA9_2009_2014$annual_increment <- (size2 - size1)/time
SPKA9_2009_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKA9_2009_2014$annual_increment)
summary(SPKA9_2009_2014$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKA9_2009_2014remove <- filter(SPKA9_2009_2014, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(SPKA9_2009_2014)
summary(SPKA9_2009_2014remove)
dim(SPKA9_2009_2014) 
dim(SPKA9_2009_2014remove)
dim(SPKA9_2009_2014remove)[[1]]/dim(SPKA9_2009_2014)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKA10 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2001" & status == "A")
census2 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2009" & status == "A")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2001_2009) 

time <- (SPKA10_2001_2009$date.y-SPKA10_2001_2009$date.x)/365
size2 <- SPKA10_2001_2009$dbh.y
size1 <- SPKA10_2001_2009$dbh.x

# calculate growth rates: 
SPKA10_2001_2009$annual_increment <- (size2 - size1)/time
SPKA10_2001_2009$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKA10_2001_2009$annual_increment)
summary(SPKA10_2001_2009$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKA10_2001_2009remove <- filter(SPKA10_2001_2009, annual_increment >= 7.5
                                 | relative_gr < -0.25)

summary(SPKA10_2001_2009)
summary(SPKA10_2001_2009remove)
dim(SPKA10_2001_2009) 
dim(SPKA10_2001_2009remove)
dim(SPKA10_2001_2009remove)[[1]]/dim(SPKA10_2001_2009)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKA10 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2009")
census2 <- filter(growdata, plot == "SPKA_10" & censusID == "10_census_2014")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2009_2014 ) 

time <- (SPKA10_2009_2014$date.y-SPKA10_2009_2014$date.x)/365
size2 <- SPKA10_2009_2014$dbh.y
size1 <- SPKA10_2009_2014$dbh.x

# calculate growth rates: 
SPKA10_2009_2014$annual_increment <- (size2 - size1)/time
SPKA10_2009_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKA10_2009_2014$annual_increment)
summary(SPKA10_2009_2014$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKA10_2009_2014remove <- filter(SPKA10_2009_2014, annual_increment >= 7.5
                                 | relative_gr < -0.25)

summary(SPKA10_2009_2014)
summary(SPKA10_2009_2014remove)
dim(SPKA10_2009_2014) 
dim(SPKA10_2009_2014remove)
dim(SPKA10_2009_2014remove)[[1]]/dim(SPKA10_2009_2014)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKA12 First interval
#---------------------------------------------------------------------------------------------#
# census1 <- filter(growdata, plot == "SPKA_12" & censusID == "12_census_XXXX")
# census2 <- filter(growdata, plot == "SPKA_12" & censusID == "12_census_2013")
# 
# # check the number of unique stems in each dataset and compare between datasets
# length(unique(census1$stemID)); dim(census1)
# length(unique(census2$stemID)); dim(census2)
# 
# # restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
# SPKA12_XXXX_2013 <- inner_join(census1, census2, by="stemID")
# dim(SPKA10_2001_2009) 
# 
# time <- (SPKA10_2001_2009$date.y-SPKA10_2001_2009$date.x)/365
# size2 <- SPKA10_2001_2009$dbh.y
# size1 <- SPKA10_2001_2009$dbh.x
# 
# # calculate growth rates: 
# SPKA10_2001_2009$annual_increment <- (size2 - size1)/time
# SPKA10_2001_2009$relative_gr      <- (log(size2) - log(size1))/time
# 
# summary(SPKA10_2001_2009$annual_increment)
# summary(SPKA10_2001_2009$relative_gr)
# #---------------------------------------------------------------------------------------------#
# # set aside stems to remove: 
# SPKA10_2001_2009remove <- filter(SPKA10_2001_2009, annual_increment >= 7.5
#                                  | relative_gr < -0.25)
# 
# summary(SPKA10_2001_2009)
# summary(SPKA10_2001_2009remove)
# dim(SPKA10_2001_2009) 
# dim(SPKA10_2001_2009remove)
# dim(SPKA10_2001_2009remove)[[1]]/dim(SPKA10_2001_2009)[[1]]*100
# #---------------------------------------------------------------------------------------------#
# # 0 stems removed (0% ALIVE stems removed)
# #---------------------------------------------------------------------------------------------#
# # take a look at the values
# par(mfrow=c(1,2))
# hist(SPKA10_2001_2009$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
# hist(SPKA10_2001_2009$annual_increment, xlab="Annual increment (cm)", col="grey", main="")
# 
# #SPKA101 Plot----------
# # look at the change in DBH from census 1 to census 2
# par(mfrow=c(1,1))
# plot(SPKA10_2001_2009$dbh.x,SPKA10_2001_2009$dbh.y, pch=19, 
#      xlab="DBH SPKA10 2001 (cm)", ylab="DBH SPKA10 2009 (cm)")
# #add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
# abline(0,1, col="red", lwd=2) 
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKH4 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2001")
census2 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2008")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2001_2008 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2001_2008) 

time <- (SPKH4_2001_2008$date.y-SPKH4_2001_2008$date.x)/365
size2 <- SPKH4_2001_2008$dbh.y
size1 <- SPKH4_2001_2008$dbh.x

# calculate growth rates: 
SPKH4_2001_2008$annual_increment <- (size2 - size1)/time
SPKH4_2001_2008$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH4_2001_2008$annual_increment)
summary(SPKH4_2001_2008$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKH4_2001_2008remove <- filter(SPKH4_2001_2008, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(SPKH4_2001_2008)
summary(SPKH4_2001_2008remove)
dim(SPKH4_2001_2008) 
dim(SPKH4_2001_2008remove)
dim(SPKH4_2001_2008remove)[[1]]/dim(SPKH4_2001_2008)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------#
# SPKH4 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2008")
census2 <- filter(growdata, plot == "SPKH_04" & censusID == "04_census_2014")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2008_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2008_2014) 

time <- (SPKH4_2008_2014$date.y-SPKH4_2008_2014$date.x)/365
size2 <- SPKH4_2008_2014$dbh.y
size1 <- SPKH4_2008_2014$dbh.x

# calculate growth rates: 
SPKH4_2008_2014$annual_increment <- (size2 - size1)/time
SPKH4_2008_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH4_2008_2014$annual_increment)
summary(SPKH4_2008_2014$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKH4_2008_2014remove <- filter(SPKH4_2008_2014, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(SPKH4_2008_2014)
summary(SPKH4_2008_2014remove)
dim(SPKH4_2008_2014) 
dim(SPKH4_2008_2014remove)
dim(SPKH4_2008_2014remove)[[1]]/dim(SPKH4_2008_2014)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 1 stems removed (0.02% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKH5 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2001")
census2 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2008")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2001_2008 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2001_2008) 

time <- (SPKH5_2001_2008$date.y-SPKH5_2001_2008$date.x)/365
size2 <- SPKH5_2001_2008$dbh.y
size1 <- SPKH5_2001_2008$dbh.x

# calculate growth rates: 
SPKH5_2001_2008$annual_increment <- (size2 - size1)/time
SPKH5_2001_2008$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH5_2001_2008$annual_increment)
summary(SPKH5_2001_2008$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKH5_2001_2008remove <- filter(SPKH5_2001_2008, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(SPKH5_2001_2008)
summary(SPKH5_2001_2008remove)
dim(SPKH5_2001_2008) 
dim(SPKH5_2001_2008remove)
dim(SPKH5_2001_2008remove)[[1]]/dim(SPKH5_2001_2008)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKH5 Second interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2008")
census2 <- filter(growdata, plot == "SPKH_05" & censusID == "05_census_2014")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2008_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2008_2014) 

time <- (SPKH5_2008_2014$date.y-SPKH5_2008_2014$date.x)/365
size2 <- SPKH5_2008_2014$dbh.y
size1 <- SPKH5_2008_2014$dbh.x

# calculate growth rates: 
SPKH5_2008_2014$annual_increment <- (size2 - size1)/time
SPKH5_2008_2014$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH5_2008_2014$annual_increment)
summary(SPKH5_2008_2014$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKH5_2008_2014remove <- filter(SPKH5_2008_2014, annual_increment >= 7.5
                                | relative_gr < -0.25)

summary(SPKH5_2008_2014)
summary(SPKH5_2008_2014remove)
dim(SPKH5_2008_2014) 
dim(SPKH5_2008_2014remove)
dim(SPKH5_2008_2014remove)[[1]]/dim(SPKH5_2008_2014)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 1 stems removed (0.02% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKH30 First interval
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2001")
census2 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2010")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2001_2010 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2001_2010) 

time <- (SPKH30_2001_2010$date.y-SPKH30_2001_2010$date.x)/365
size2 <- SPKH30_2001_2010$dbh.y
size1 <- SPKH30_2001_2010$dbh.x

# calculate growth rates: 
SPKH30_2001_2010$annual_increment <- (size2 - size1)/time
SPKH30_2001_2010$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH30_2001_2010$annual_increment)
summary(SPKH30_2001_2010$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKH30_2001_2010remove <- filter(SPKH30_2001_2010, annual_increment >= 7.5
                                 | relative_gr < -0.25)

summary(SPKH30_2001_2010)
summary(SPKH30_2001_2010remove)
dim(SPKH30_2001_2010) 
dim(SPKH30_2001_2010remove)
dim(SPKH30_2001_2010remove)[[1]]/dim(SPKH30_2001_2010)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 0 stems removed (0.0% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# SPKH30 Second interval 
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2010")
census2 <- filter(growdata, plot == "SPKH_30" & censusID == "30_census_2015")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2010_2015 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2010_2015) 

time <- (SPKH30_2010_2015$date.y-SPKH30_2010_2015$date.x)/365
size2 <- SPKH30_2010_2015$dbh.y
size1 <- SPKH30_2010_2015$dbh.x

# calculate growth rates: 
SPKH30_2010_2015$annual_increment <- (size2 - size1)/time
SPKH30_2010_2015$relative_gr      <- (log(size2) - log(size1))/time

summary(SPKH30_2010_2015$annual_increment)
summary(SPKH30_2010_2015$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
SPKH30_2010_2015remove <- filter(SPKH30_2010_2015, annual_increment >= 7.5
                                 | relative_gr < -0.25)

summary(SPKH30_2010_2015)
summary(SPKH30_2010_2015remove)
dim(SPKH30_2010_2015) 
dim(SPKH30_2010_2015remove)
dim(SPKH30_2010_2015remove)[[1]]/dim(SPKH30_2010_2015)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 12 stems removed (0.18% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# Lambir First interval 
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, site == "LHP" & censusID == "census_1991")
census2 <- filter(growdata, site == "LHP" & censusID == "census_1997")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LHC_91_97 <- inner_join(census1, census2, by="stemID")
dim(LHC_91_97) 

time <- (LHC_91_97$date.y-LHC_91_97$date.x)/365
size2 <- LHC_91_97$dbh.y
size1 <- LHC_91_97$dbh.x

# calculate growth rates: 
LHC_91_97$annual_increment <- (size2 - size1)/time
LHC_91_97$relative_gr      <- (log(size2) - log(size1))/time

summary(LHC_91_97$annual_increment)
summary(LHC_91_97$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
LHC_91_97remove <- filter(LHC_91_97, annual_increment >= 7.5
                          | relative_gr < -0.25)

summary(LHC_91_97)
summary(LHC_91_97remove)
dim(LHC_91_97) 
dim(LHC_91_97remove)
dim(LHC_91_97remove)[[1]]/dim(LHC_91_97)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 11 stems removed (0.00% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Lambir Second interval 
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, site == "LHP" & censusID == "census_1997")
census2 <- filter(growdata, site == "LHP" & censusID == "census_2003")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LHC_97_03 <- inner_join(census1, census2, by="stemID")
dim(LHC_97_03) 

time <- (LHC_97_03$date.y-LHC_97_03$date.x)/365
size2 <- LHC_97_03$dbh.y
size1 <- LHC_97_03$dbh.x

# calculate growth rates: 
LHC_97_03$annual_increment <- (size2 - size1)/time
LHC_97_03$relative_gr      <- (log(size2) - log(size1))/time

summary(LHC_97_03$annual_increment)
summary(LHC_97_03$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
LHC_97_037remove <- filter(LHC_97_03, annual_increment >= 7.5
                           | relative_gr < -0.25)

summary(LHC_97_03)
summary(LHC_97_037remove)
dim(LHC_97_03) 
dim(LHC_97_037remove)
dim(LHC_97_037remove)[[1]]/dim(LHC_97_03)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 10 stems removed (0.00% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# LHC third interval 
#---------------------------------------------------------------------------------------------#
census1 <- filter(growdata, site == "LHP" & censusID == "census_2003")
census2 <- filter(growdata, site == "LHP" & censusID == "census_2007_08")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
LHC_03_08 <- inner_join(census1, census2, by="stemID")
dim(LHC_03_08) 

time <- (LHC_03_08$date.y-LHC_03_08$date.x)/365
size2 <- LHC_03_08$dbh.y
size1 <- LHC_03_08$dbh.x

# calculate growth rates: 
LHC_03_08$annual_increment <- (size2 - size1)/time
LHC_03_08$relative_gr      <- (log(size2) - log(size1))/time

summary(LHC_03_08$annual_increment)
summary(LHC_03_08$relative_gr)
#---------------------------------------------------------------------------------------------#
# set aside stems to remove: 
#LHC_03_08remove <- filter(LHC_03_08, annual_increment >= 7.5)
#LHC_03_08remove <- filter(LHC_03_08, annual_increment < 0)
LHC_03_08remove <- filter(LHC_03_08, annual_increment >= 7.5
                          | relative_gr < -0.25)

summary(LHC_03_08)
summary(LHC_03_08remove)
dim(LHC_03_08) 
dim(LHC_03_08remove)
dim(LHC_03_08remove)[[1]]/dim(LHC_03_08)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 16 stems removed (0.01% ALIVE stems removed)
#---------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# Create Combined Dataset of all growth rates
#---------------------------------------------------------------------------------------------#
removal_ID <- rbind(DNM1_2006_2013remove, DNM1_2013_2016remove,DNM2_2006_2013remove, 
                    DNM2_2013_2016remove, DNM3_2006_2013remove, DNM3_2013_2016remove,
                    DNM50_2011_2019remove, SPKS08_2001_2009remove, SPKS08_2009_2014remove, 
                    SPKS06_2003_2014remove, SPKS07_2008_2013remove, 
                      SPKA9_2001_2009remove, SPKA9_2009_2014remove, SPKA10_2001_2009remove, 
                      SPKA10_2009_2014remove, SPKH4_2001_2008remove, SPKH4_2008_2014remove, 
                      SPKH5_2001_2008remove, SPKH5_2008_2014remove, SPKH30_2001_2010remove, 
                      SPKH30_2010_2015remove, LHC_91_97remove, LHC_97_037remove, LHC_03_08remove)
str(removal_ID)
dim(removal_ID) # n = 67
dim(growdata)   # n = 2,056,491
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#write.csv(removal_ID, here("Desktop", "Research", "R", "Data", "removal_IDS.csv"))
write.csv(removal_ID, "G:/My Drive/Harvard/Tall_trees_Borneo_project/Data/removal_IDS.csv")
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Apply removal IDs
#---------------------------------------------------------------------------------------------#
#removal_ID <- read_csv(here("Desktop", "Research", "R", "Data", "removal_IDS.csv"))
#removal_ID <- read_csv("G:/My Drive/Harvard/Tall_trees_Borneo_project/Data/removal_IDS.csv")

removal_ID$new_ID <- with(removal_ID, paste0(stemID, plot.x))
tree_IDs_remove <- removal_ID$new_ID
growdata$new_ID <- with(growdata, paste0(stemID, plot))

str(growdata$new_ID)
str(tree_IDs_remove)

withremoval <- growdata[!growdata$new_ID %in% tree_IDs_remove, ] 

# test <- subset(growdata, new_ID == "1404482SPKH_05")
# test <- subset(withremoval, new_ID == "1404482SPKH_05")

SPKA <- subset(withremoval, site == "SPKA"); table(SPKA$status)
SPKS <- subset(withremoval, site == "SPKS"); table(SPKS$status)
SPKH <- subset(withremoval, site == "SPKH"); table(SPKH$status)
DNM50 <- subset(withremoval, site == "DNM50"); table(SPKH$status)
summary(DNM50$dbh)
LHP <- subset(growdata, site == "LHP"); table(LHP$status)

#---------------------------------------------------------------------------------------------#
dim(growdata)    # n = 2,056,486
dim(withremoval) # n = 2,056,296
dim(growdata)[[1]] - length(tree_IDs_remove) # 2,056,419
#---------------------------------------------------------------------------------------------#
# I think 'withremoval' is smaller than ('growdata' - the number of 'tree_IDs_remove') because
# tree_IDs_remove is by plot, not by census. So it's removing the plot-level treeIDs each time
# they occur in a plot (~2-3 times depending on the number of censuses). 
#---------------------------------------------------------------------------------------------#

table(withremoval$status)

#---------------------------------------------------------------------------------------------#
write.csv(withremoval, "G:/My Drive/Harvard/Plot_Data/clean_inventory_data/growth_dat.csv")
#---------------------------------------------------------------------------------------------#

