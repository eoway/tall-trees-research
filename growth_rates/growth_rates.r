## Calculate census-level growth rates
## 11-9-2020

library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)

#Load Data----------
growdata <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/growth_dat.csv")

#---------------------------------------------------------------------------------------------#
table(growdata$status)
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

#---------------------------------------------------------------------------------------------#
# DNM50 
#---------------------------------------------------------------------------------------------#
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

time <- (DNM50_2011_2019$date.y-DNM50_2011_2019$date.x)/365
size2 <- DNM50_2011_2019$dbh.y
size1 <- DNM50_2011_2019$dbh.x

# calculate growth rates: 
DNM50_2011_2019$annual_increment <- (size2 - size1)/time
DNM50_2011_2019$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM50_2011_2019$annual_increment)
summary(DNM50_2011_2019$relative_gr)
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
#---------------------------------------------------------------------------------------------#




#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# Create Combined Dataset of all growth rates
#---------------------------------------------------------------------------------------------#
growthr <- rbind(SPKS08_2001_2009, SPKS08_2009_2014, SPKS06_2003_2014, SPKS07_2008_2013, 
                 DNM1_2006_2013, DNM1_2013_2016, DNM2_2006_2013, DNM2_2013_2016, 
                 DNM3_2006_2013, DNM3_2013_2016, DNM50_2011_2019, SPKA9_2001_2009, 
                 SPKA9_2009_2014, SPKA10_2001_2009, SPKA10_2009_2014, SPKH4_2001_2008, 
                 SPKH4_2008_2014, SPKH5_2001_2008, SPKH5_2008_2014, SPKH30_2001_2010, 
                 SPKH30_2010_2015)

summary(growthr$annual_increment) # max should be 7.5
summary(growthr$relative_gr) # min should be -0.25

#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# GROWTH RATES ANALYSIS (see Condit et al 2006 Supplementary Material)
# growth_dat already excludes stems if:
#  shrunk > 25% of initial DBH (rgr < -0.25)
#  grew > 7.5 mm (annual increment > 7.5)
# 
# analysis_v1: negative growth rates converted to 0
# analysis_v2: negative growth rates recalculated to DBHj+1 = 0.05 + DBHj (when DBH is in cm)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# create growth_dat_v1 & growth_dat_v2
# growth_dat_v1: negative growth rates recalculated to 0
# growth_dat_v2: negative growth rates recalculated to DBHj+1 = 0.05 + DBHj (when DBH is in cm) - from Condit et al 2006 Supplementary Material
#---------------------------------------------------------------------------------------------#
growth_dat_v1 <- growthr
growth_dat_v2 <- growthr

growth_dat_v1$annual_increment <- ifelse(growth_dat_v1$annual_increment < 0, 0, growth_dat_v1$annual_increment) 
growth_dat_v2$annual_increment <- ifelse(growth_dat_v2$annual_increment < 0, 0.05, growth_dat_v2$annual_increment)


summary(growthr$annual_increment) # max should be 7.5
summary(growth_dat_v1$annual_increment)
summary(growth_dat_v2$annual_increment)

summary(growthr$relative_gr) # min should be -0.25
summary(growth_dat_v1$relative_gr) 
summary(growth_dat_v2$relative_gr) 

par(mfrow=c(1,3))
hist(growthr$annual_increment, xlab="Annual increment (cm)", col="grey", main="")
hist(growth_dat_v1$annual_increment, xlab="Annual increment (cm)", col="grey", main="")
hist(growth_dat_v2$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

hist(growthr$relative_gr, col="grey", main="")
hist(growth_dat_v1$relative_gr, col="grey", main="")
hist(growth_dat_v2$relative_gr, col="grey", main="")
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# plot by site
#---------------------------------------------------------------------------------------------#
library(RColorBrewer)
pal <- brewer.pal(3, "Dark2")
pal2 <- brewer.pal(5, "BrBG")

# growth_dat_v1; growth_dat_v2
plot_dat <- growth_dat_v1 %>% group_by(site.x) %>% summarize(n = n(), 
                                                             growth = median(relative_gr, na.rm=T),
                                                             sd_growth = sd(relative_gr, na.rm=T),
                                                             SEgrowth = sd_growth/sqrt(n),
                                                             t_score = qt(p=0.05/2, df=n-1, lower.tail=F),
                                                             mlow = growth-(SEgrowth*t_score),
                                                             mup = growth+(SEgrowth*t_score))
ggplot(plot_dat, aes(x=site.x,y=growth, fill=site.x)) + 
  geom_errorbar(aes(ymin=mlow, ymax=mup), width=.2, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=5, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4], "blue")) +
  labs(x="", y=expression(Relative~growth~rate~(yr^{-1}))) +
  #scale_x_discrete(labels = c("SPKa","DNM50","LHc","SPKs","LHs","SPKh")) +
  theme_classic() + 
  theme(legend.position = "none") 


