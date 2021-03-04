<<<<<<< HEAD
library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)

#Load Data----------
data <- read_csv(here("Elsa Clean", "mort_dat.csv"))

colnames(data)
#Plots--------------
#DNM1
DNM1 <- filter(data, plot == "DNM1_01")
table(DNM1$census)
DNM106 <- filter(DNM1, census == "01_census_2006")
DNM113 <- filter(DNM1, census == "01_census_2013")
DNM116 <- filter(DNM1, census == "01_census_2016")
#DNM2
DNM2 <- filter(data, plot == "DNM2_02")
table(DNM2$census)
DNM206 <- filter(DNM2, census == "02_census_2006")
DNM213 <- filter(DNM2, census == "02_census_2013")
DNM216 <- filter(DNM2, census == "02_census_2016")
#DNM3
DNM3 <- filter(data, plot == "DNM3_03")
table(DNM3$census)
DNM306 <- filter(DNM3, census == "03_census_2006")
DNM313 <- filter(DNM3, census == "03_census_2013")
DNM316 <- filter(DNM3, census == "03_census_2016")
#DNM50
DNM50 <- filter(data, plot == "DNM50_FGEO")
table(DNM50$census)
DNM5011 <- filter(DNM50, census == "census_2011_15")
DNM5019 <- filter(DNM50, census == "census_2019")
#SPKA9
SPKA9 <- filter(data, plot == "SPKA_09")
table(SPKA9$census)
SPKA901 <- filter(SPKA9, census == "09_census_2001")
SPKA909 <- filter(SPKA9, census == "09_census_2009")
SPKA914 <- filter(SPKA9, census == "09_census_2014")
#SPKA10
SPKA10 <- filter(data, plot == "SPKA_10")
table(SPKA10$census)
SPKA1001 <- filter(SPKA10, census == "10_census_2001")
SPKA1009 <- filter(SPKA10, census == "10_census_2009")
SPKA1014 <- filter(SPKA10, census == "10_census_2014")
#SPKH4
SPKH4 <- filter(data, plot == "SPKH_04")
table(SPKH4$census)
SPKH401 <- filter(SPKH4, census == "04_census_2001")
SPKH408 <- filter(SPKH4, census == "04_census_2008")
SPKH414 <- filter(SPKH4, census == "04_census_2014")
#SPKH5
SPKH5 <- filter(data, plot == "SPKH_05")
table(SPKH5$census)
SPKH501 <- filter(SPKH5, census == "05_census_2001")
SPKH508 <- filter(SPKH5, census == "05_census_2008")
SPKH514 <- filter(SPKH5, census == "05_census_2014")
#SPKH30
SPKH30 <- filter(data, plot == "SPKH_30")
table(SPKH30$census)
SPKH3001 <- filter(SPKH5, census == "30_census_2001")
SPKH3010 <- filter(SPKH5, census == "30_census_2010")
SPKH3015 <- filter(SPKH5, census == "30_census_2015")
#SPKS8
SPKS8 <- filter(data, plot == "SPKS_08")
table(SPKS8$census)
SPKS801 <- filter(SPKS8, census == "08_census_2001")
SPKS801p <- filter(SPKS8, census == "08_census_2001")
SPKS809 <- filter(SPKS8, census == "08_census_2009")
SPKS8091 <- filter(SPKS8, census == "08_census_2009")
SPKS8092 <- filter(SPKS8, census == "08_census_2009")
SPKS814 <- filter(SPKS8, census == "08_census_2014")
SPKS814p <- filter(SPKS8, census == "08_census_2014")
SPKS801p$pool_stem_ID    <- paste0(SPKS801$stemID, "_1")
SPKS8091$pool_stem_ID   <- paste0(SPKS8091$stemID, "_2")
SPKS8092$pool_stem_ID <- paste0(SPKS8092$stemID, "_1") 
SPKS814p$pool_stem_ID  <- paste0(SPKS814$stemID, "_2")

#DNM1 First interval------------

census1 <- DNM106
census2 <- DNM113
colnames(census2)
table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2006_2013) 

#N = number of trees at first census
N = length(census1$stemID)
N
#S = Number of trees that survived
dead = filter(DNM1_2006_2013, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(dead$stemID)
S
# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM1_2006_2013$date.y-DNM1_2006_2013$date.x)/365
meantime[1]
#calculate mortality rate
DNM1_2006_2013$mortrate <- (log(N) - log(S))/meantime

#DNM1 Second interval------------

census1 <- DNM113
census2 <- DNM116

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM1_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM1_2013_2016) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(DNM1_2013_2016, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM1_2013_2016$date.y-DNM1_2013_2016$date.x)/365
meantime[1]
#calculate mortality rate
DNM1_2013_2016$mortrate <- (log(N) - log(S))/meantime

#DNM2 First interval------------

census1 <- DNM201
census2 <- DNM213

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2006_2013) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(DNM2_2006_2013, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM2_2006_2013$date.y-DNM2_2006_2013$date.x)/365
meantime[1]
#calculate mortality rate
DNM2_2006_2013$mortrate <- (log(N) - log(S))/meantime

#DNM2 Second interval------------

census1 <- DNM213
census2 <- DNM216

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM2_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM2_2013_2016) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(DNM2_2013_2016, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM2_2013_2016$date.y-DNM2_2013_2016$date.x)/365
meantime[1]
#calculate mortality rate
DNM2_2013_2016$mortrate <- (log(N) - log(S))/meantime

#DNM3 First interval------------

census1 <- DNM301
census2 <- DNM313

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2006_2013 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2006_2013) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(DNM3_2006_2013, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM3_2006_2013$date.y-DNM3_2006_2013$date.x)/365
meantime[1]
#calculate mortality rate
DNM3_2006_2013$mortrate <- (log(N) - log(S))/meantime

#DNM3 Second interval------------

census1 <- DNM313
census2 <- DNM316

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM3_2013_2016 <- inner_join(census1, census2, by="stemID")
dim(DNM3_2013_2016) 

#N = number of trees at first census
N = length(unique(census3$stemID))

#S = Number of trees that survived
dead = filter(DNM3_2013_2016, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM3_2013_2016$date.y-DNM3_2013_2016$date.x)/365
meantime[1]
#calculate mortality rate
DNM3_2013_2016$mortrate <- (log(N) - log(S))/meantime

#DNM50 Large interval------------

census1 <- DNM5011
census2 <- DNM5019

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM50_2011_2019 <- inner_join(census1, census2, by="stemID")
dim(DNM50_2011_2019) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(DNM50_2011_2019, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (DNM50_2011_2019$date.y-DNM50_2011_2019$date.x)/365
meantime[1]
#calculate mortality rate
DNM50_2011_2019$mortrate <- (log(N) - log(S))/meantime

#SPKA9 First interval------------

census1 <- SPKA901
census2 <- SPKA909

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2001_2009) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKA9_2001_2009, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKA9_2001_2009$date.y-SPKA9_2001_2009$date.x)/365
meantime[1]
#calculate mortality rate
SPKA9_2001_2009$mortrate <- (log(N) - log(S))/meantime

#SPKA9 Second interval------------

census1 <- SPKA909
census2 <- SPKA914

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA9_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA9_2009_2014) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKA9_2009_2014, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKA9_2009_2014$date.y-SPKA9_2009_2014$date.x)/365
meantime[1]
#calculate mortality rate
SPKA9_2009_2014$mortrate <- (log(N) - log(S))/meantime

#SPKA10 First interval------------

census1 <- SPKA1001
census2 <- SPKA1009

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2001_2009) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKA10_2001_2009, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKA10_2001_2009$date.y-SPKA10_2001_2009$date.x)/365
meantime[1]
#calculate mortality rate
SPKA10_2001_2009$mortrate <- (log(N) - log(S))/meantime

#SPKA10 Second interval------------

census1 <- SPKA1009
census2 <- SPKA1014

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKA10_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKA10_2009_2014) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKA10_2009_2014, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKA10_2009_2014$date.y-SPKA10_2009_2014$date.x)/365
meantime[1]
#calculate mortality rate
SPKA10_2009_2014$mortrate <- (log(N) - log(S))/meantime

#SPKH4 First interval------------

census1 <- SPKH401
census2 <- SPKH408

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2001_2008 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2001_2008) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKH4_2001_2008, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKH4_2001_2008$date.y-SPKH4_2001_2008$date.x)/365
meantime[1]
#calculate mortality rate
SPKH4_2001_2008$mortrate <- (log(N) - log(S))/meantime

#SPKH4 Second interval------------

census1 <- SPKH408
census2 <- SPKH414

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH4_2008_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH4_2008_2014) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKH4_2008_2014, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKH4_2008_2014$date.y-SPKH4_2008_2014$date.x)/365
meantime[1]
#calculate mortality rate
SPKH4_2008_2014$mortrate <- (log(N) - log(S))/meantime

#SPKH5 First interval------------

census1 <- SPKH501
census2 <- SPKH508

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2001_2008 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2001_2008) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKH5_2001_2008, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKH5_2001_2008$date.y-SPKH5_2001_2008$date.x)/365
meantime[1]
#calculate mortality rate
SPKH5_2001_2008$mortrate <- (log(N) - log(S))/meantime

#SPKH5 Second interval------------

census1 <- SPKH508
census2 <- SPKH514

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH5_2008_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKH5_2008_2014) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKH5_2008_2014, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKH5_2008_2014$date.y-SPKH5_2008_2014$date.x)/365
meantime[1]
#calculate mortality rate
SPKH5_2008_2014$mortrate <- (log(N) - log(S))/meantime

#SPKH30 First interval------------

census1 <- SPKH3001
census2 <- SPKH3010

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2001_2010 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2001_2010) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKH30_2001_2010, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKH30_2001_2010$date.y-SPKH30_2001_2010$date.x)/365
meantime[1]
#calculate mortality rate
SPKH30_2001_2010$mortrate <- (log(N) - log(S))/meantime

#SPKH30 Second interval------------

census1 <- SPKH3010
census2 <- SPKH3015

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKH30_2010_2015 <- inner_join(census1, census2, by="stemID")
dim(SPKH30_2010_2015) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKH30_2010_2015, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKH30_2010_2015$date.y-SPKH30_2010_2015$date.x)/365
meantime[1]
#calculate mortality rate
SPKH30_2010_2015$mortrate <- (log(N) - log(S))/meantime

#SPKS08 first interval------------

census1 <- SPKS801
census2 <- SPKS809

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS08_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2001_2009) 

#N = number of trees at first census
N = length(unique(census1$stemID))
N
#S = Number of trees that survived
dead = filter(SPKS08_2001_2009, status.y == "D")
table(dead$status.y)
table(census2$status.y)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKS08_2001_2009$date.y-SPKS08_2001_2009$date.x)/365

#calculate mortality rate
SPKS08_2001_2009$mortrate <- (log(N) - log(S))/meantime

#SPKS08 second interval------------

census1 <- SPKS809
census2 <- SPKS814

table(census2$census)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS08_2009_2014 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2009_2014) 

#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKS08_2009_2014, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID))
S = N - length(unique(dead$stemID))

# calculate meantime difference and convert meantime from days to years  
meantime <- (SPKS08_2009_2014$date.y-SPKS08_2009_2014$date.x)/365
meantime[1]
#calculate mortality rate
SPKS08_2009_2014$mortrate <- (log(N) - log(S))/meantime

#SPKS8 Large Interval--------
census1 <- rbind(SPKS801p, SPKS8091)
census2 <- rbind(SPKS8092, SPKS814p)
head(SPKS08_2001_2014$date.x)
SPKS08_2001_2014 <- inner_join(census1, census2, by="pool_stem_ID")
dim(SPKS08_2001_2014) 
dim(census1)
dim(census2)
dim(SPKS8)
dim(SPKS8091)
head(SPKS08_2001_2014)
table(SPKS8$census)
table(SPKS08_2001_2014$census.x)
table(SPKS08_2001_2014$census.y)

# calculate time difference and convert time from days to years  
SPKS08_2001_2014$meantime <- (SPKS08_2001_2014$date.y-SPKS08_2001_2014$date.x)/365
head(SPKS08_2001_2014$meantime)
table(meantime)
length(unique(census2$stemID))
#N = number of trees at first census
N = length(unique(census1$stemID))

#S = Number of trees that survived
dead = filter(SPKS08_2001_2014, status.y == "D")
table(dead$status.y)
table(census2$status)
length(unique(dead$stemID.y))
S = N - length(unique(dead$stemID.y))

#calculate mortality rate
SPKS08_2001_2014$mortrate <- (log(N) - log(S))/SPKS08_2001_2014$meantime
table(SPKS08_2001_2014$mortrate)

mortrates <- rbind(SPKS08_2001_2009, SPKS08_2009_2014, DNM1_2006_2013, DNM1_2013_2016,
                 DNM2_2006_2013, DNM2_2013_2016, DNM3_2006_2013, DNM3_2013_2016, DNM50_2011_2019, 
                 SPKA9_2001_2009, SPKA9_2009_2014,SPKA10_2001_2009, SPKA10_2009_2014,
                 SPKH4_2001_2008, SPKH4_2008_2014, SPKH5_2001_2008, 
                 SPKH5_2008_2014, SPKH30_2001_2010, SPKH30_2010_2015)

ggplot() +
  geom_boxplot(mortrates, mapping = aes(site.x, mortrate))

mortrates$size_class <- 
  cut(mortrates$dbh.y, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                              110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(mortrates$dbh.y, na.rm=T)),
  )
mortrates$size_class2 <- 
  cut(mortrates$dbh.y, breaks=c(0,10,20,30,40,50,60,70,80,90,100,
                              110,120,130,140,150,160,170,180,190,200,210,max(mortrates$dbh.y, na.rm=T)),
  )

par(mfrow=c(1,2))
plot(mortrates$size_class, log(mortrates$mortrate), pch=19, 
     xlab="DBH", ylab="mortality rate", )
plot(mortrates$size_class2, log(mortrates$mortrate), pch=19, 
     xlab="DBH", ylab="mortality rate", )

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
colnames(mortrates)
#Calculate Heights-----
#Feld
mortrates$heightFeld <- dbh2h_01(mortrates$dbh.x, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(mortrates$heightFeld)
#Chave
mortrates$heightCh <- dbh2h_34(mortrates$dbh.x,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
table(mortrates$heightCh)
#Chave with E
# need to specify which "E" value to use - from above
mortrates$heightE <- dbh2h_ChaveE(mortrates$dbh.x,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)

#Source Height Quantiles------
source("heights.r")

#Exclude Indets???? ASK------
mortrates <- filter(mortrates, species.x != "Indet")
mortrates <- filter(mortrates, species.y != "Indet")

#quantile 90-------
#Feld
mortrates$tree_type90F <- ifelse(mortrates$species.x %in% c(emergent90Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
mortrates$tree_type90Ch <- ifelse(mortrates$species.x %in% c(emergent90Ch), "emrgnt", "non_emrgnt")

#Chave with E
mortrates$tree_type90E <- ifelse(mortrates$species.x %in% c(emergent90E), "emrgnt", "non_emrgnt")


#quantile 95-------
#Feld
mortrates$tree_type95F <- ifelse(mortrates$species.x %in% c(emergent95Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
mortrates$tree_type95Ch <- ifelse(mortrates$species.x %in% c(emergent95Ch), "emrgnt", "non_emrgnt")

#Chave with E
mortrates$tree_type95E <- ifelse(mortrates$species.x %in% c(emergent95E), "emrgnt", "non_emrgnt")

#quantile 99-------
#Feld
mortrates$tree_type99F <- ifelse(mortrates$species.x %in% c(emergent99Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
mortrates$tree_type99Ch <- ifelse(mortrates$species.x %in% c(emergent99Ch), "emrgnt", "non_emrgnt")
#Chave with E
mortrates$tree_type99E <- ifelse(mortrates$species.x %in% c(emergent99E), "emrgnt", "non_emrgnt")



#Box plots-------
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type90F))+
  geom_boxplot()
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type90Ch))+
  geom_boxplot()
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type90E))+
  geom_boxplot()

mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type95F))+
  geom_boxplot()
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type95Ch))+
  geom_boxplot()
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type95E))+
  geom_boxplot()

mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type99F))+
  geom_boxplot()
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type99Ch))+
  geom_boxplot()
mortrates %>%
  ggplot(aes(x=site.x, y=mortrate, fill=tree_type99E))+
  geom_boxplot()

#height bins------
mortrates$heightFeld_class <- 
  cut(mortrates$heightFeld, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                   110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(mortrates$dbh.y, na.rm=T)),
  )
mortrates$heightCh_class <- 
  cut(mortrates$heightCh, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                 110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(mortrates$dbh.y, na.rm=T)),
  )
mortrates$heightE_class <- 
  cut(mortrates$heightE, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                                110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,max(mortrates$dbh.y, na.rm=T)),
  )
#Definitions------
mortratesnem90F <- filter(mortrates, tree_type90F == "non_emrgnt")
mortratesnem95F <- filter(mortrates, tree_type95F == "non_emrgnt")
mortratesnem99F <- filter(mortrates, tree_type99F == "non_emrgnt")
mortratesnem90Ch <- filter(mortrates, tree_type90Ch == "non_emrgnt")
mortratesnem95Ch <- filter(mortrates, tree_type95Ch == "non_emrgnt")
mortratesnem99Ch <- filter(mortrates, tree_type99Ch == "non_emrgnt")
mortratesnem90E <- filter(mortrates, tree_type90E == "non_emrgnt")
mortratesnem95E <- filter(mortrates, tree_type95E == "non_emrgnt")
mortratesnem99E <- filter(mortrates, tree_type99E == "non_emrgnt")

mortrates %>%
  ggplot(aes(heightFeld, mortrate))+
  geom_point(color = "chartreuse3")+
  geom_point(data = mortratesnem90F,color = "chartreuse4")+
  geom_vline(xintercept = quantile90Feld, color="black")
mortrates %>%
  ggplot(aes(heightCh, mortrate))+
  geom_point(color = "darkorchid2")+
  geom_point(data = mortratesnem90Ch,color = "darkorchid4")+
  geom_vline(xintercept = quantile90Ch, color="black")
mortrates %>%
  ggplot(aes(heightE, mortrate))+
  geom_point(color = "deepskyblue2")+
  geom_point(data = mortratesnem90E,color = "deepskyblue4")+
  geom_vline(xintercept = quantile90E, color="black")
#95th percentile
mortrates %>%
  ggplot(aes(heightFeld, mortrate))+
  geom_point(color = "chartreuse3")+
  geom_point(data = mortratesnem95F,color = "chartreuse4")+
  geom_vline(xintercept = quantile95Feld, color="black")
mortrates %>%
  ggplot(aes(heightCh, mortrate))+
  geom_point(color = "darkorchid2")+
  geom_point(data = mortratesnem95Ch,color = "darkorchid4")+
  geom_vline(xintercept = quantile95Ch, color="black")
mortrates %>%
  ggplot(aes(heightE, mortrate))+
  geom_point(color = "deepskyblue2")+
  geom_point(data = mortratesnem95E,color = "deepskyblue4")+
  geom_vline(xintercept = quantile95E, color="black")
#99th percentile
mortrates %>%
  ggplot(aes(heightFeld, mortrate))+
  geom_point(color = "chartreuse3")+
  geom_point(data = mortratesnem99F,color = "chartreuse4")+
  geom_vline(xintercept = quantile99Feld, color="green")
mortrates %>%
  ggplot(aes(heightCh, mortrate))+
  geom_point(color = "darkorchid2")+
  geom_point(data = mortratesnem99Ch,color = "darkorchid4")+
  geom_vline(xintercept = quantile99Ch, color="black")
mortrates %>%
  ggplot(aes(heightE, mortrate))+
  geom_point(color = "deepskyblue2")+
  geom_point(data = mortratesnem99E,color = "deepskyblue4")+
  geom_vline(xintercept = quantile99E, color="black")
=======
#----------------------------------------------------------------------------------------#
# Read [clean] inventory plot data and calculate mortality rates
#----------------------------------------------------------------------------------------#
setwd("G:/My Drive") # Google Drive

library(dplyr); library(ggplot2); library(viridis); library(stringr)
library(ggfortify); library(cowplot); require(data.table); 
library(tidyverse)
library(raster); library(rgdal); library(sp); library(GISTools); library(sf)
library(vegan); library(RColorBrewer); library(splus2R); library(fgeo)
# install.packages("devtools")
# devtools::install_github("forestgeo/ctfs")
# library(ctfs)
# edit(mortality.calculation)

pal <- brewer.pal(3, "Dark2")
pal2 <- brewer.pal(5, "BrBG")

#---------------------------------------------------------------------------------------------#
# Load data                                                                                   # 
#---------------------------------------------------------------------------------------------#
#data <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv")
clean_dat <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/mort_dat.csv")

colnames(clean_dat)
#clean_dat <-rename (data, censusID = census, dbh = dbh, date = JulianDate, status = DFstatus)
#---------------------------------------------------------------------------------------------#
table(clean_dat$status)
#clean_dat_dead <- filter(clean_dat, status == "D" | status == "B") # excludes "missing"
#clean_dat <- filter(clean_dat, status == "A")

length(unique(clean_dat$treeID))

table(clean_dat$site)
table(clean_dat$plot)
#----------------------------------------------------------------------------------------#
# update site column so that it differentiates between lambir soil types
#----------------------------------------------------------------------------------------#
clean_dat$site <- substr(clean_dat$plot, 1, 4)
table(clean_dat$site)
clean_dat$site <- factor(clean_dat$site, 
                         levels = c("LH_c","LH_l","LH_f","LH_s","SPKA","DNM5","DNM3","SPKS","DNM2","DNM1","SPKH"),
                         labels = c("LHPc","LHPl","LHPf","LHPs","SPKA","DNM50","DNM3","SPKS","DNM2","DNM1","SPKH"))
table(clean_dat$site)
#----------------------------------------------------------------------------------------#

# reorganize data to define status based on treeID, not stemID 
# if ALL stems == Dead, tree is dead
# if only some stems == Dead, tree is alive
# edit(pick_main_stem)
# pick_main_stem()

# create clean_mort_dat.R file 
# select the largest stem (see code from Naomi)
# how many stems? just largest?


#----------------------------------------------------------------------------------------#
# http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/packages/CTFS.pdf
#----------------------------------------------------------------------------------------#
# search for "mortality.calculation'
# N  = number of trees at the first census
# S  = number of trees that survived to the second census, N - D
# meantime = interval between censuses in years for trees in the first census
#----------------------------------------------------------------------------------------#
# also see: 
# fgeo::recruitment_ctfs, mortality_ctfs, growth_ctfs
# ?mortality_ctfs
# edit(mortality_ctfs)
#----------------------------------------------------------------------------------------#


mortality_rate <- function (N, S, meantime) 
{
  lower.ci = find.climits(N, (N - S), kind = "lower")
  upper.ci = find.climits(N, (N - S), kind = "upper")
  mort.rate = (log(N) - log(S))/meantime
  upper.rate = (log(N) - log(N - upper.ci))/meantime
  lower.rate = (log(N) - log(N - lower.ci))/meantime
  mort.rate[S == 0] = upper.rate[S == 0] = Inf
  upper.rate[upper.ci == N] = Inf
  lower.rate[lower.ci == N] = 0
  mort.rate[N == 0] = lower.rate[N == 0] = upper.rate[N == 
                                                        0] = NA
  if (is.null(dim(N))) 
    return(data.frame(N = N, S = S, D = N - S, rate = mort.rate, 
                      lowerCI = lower.rate, upperCI = upper.rate, time = meantime))
  else return(list(N = N, S = S, D = N - S, rate = mort.rate, 
                   lowerCI = lower.rate, upperCI = upper.rate, time = meantime))
}
#----------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
# see: https://www.forestplots.net/upload/ManualsEnglish/RAINFOR_data_codes_EN.pdf
table(clean_dat$status)

dat <- clean_dat
#dat <- subset(clean_dat, dbh >= 2 | is.na(dbh))
# dat <- subset(clean_dat, dbh >= 4 | is.na(dbh)) #dbh >= 5
# dat <- subset(clean_dat, dbh >= 10 | is.na(dbh))
# dat <- subset(clean_dat, dbh >= 30 | is.na(dbh))
table(dat$status)

# test <- subset(clean_dat, site == "DNM50" & status == "D")
# test <- subset(clean_dat, site == "LHPc" & status == "D")
# table(test$status)
# summary(test$dbh)

dem_dat <- dat
#dem_dat <- filter(dat, status != "missing" & status != "B")
#dem_dat <- filter(dat, status != "missing" & status != "prior")
#dem_dat <- filter(dat, status != "missing" & status != "prior" & status != "stem_gone")
#dem_dat <- filter(dat, status != "missing" & status != "prior" & status != "stem_gone" & status != "B")
#dem_dat <- dat

table(dem_dat$status)
#dem_dat$status <- gsub("B", "A", dem_dat$status)
#dem_dat$status <- gsub("stem_gone", "D", dem_dat$status)
#dem_dat$status <- gsub("B", "D", dem_dat$status)
#dem_dat$status <- gsub("missing", "A", dem_dat$status)
table(dem_dat$status)

# test <- filter(dem_dat, DFstatus == "prior"); tail(test)
# table(dem_dat$DFstatus)
# test <- filter(dem_dat, DFstatus == "D"); dim(test); summary(test$dbh)

colnames(dem_dat)
# Change JulianDate & DFstatus to date & status
#colnames(dem_dat)[13:14] <- c("date","status")
#dem_dat$hom <- rep(130, length(dem_dat$dbh))

table(dem_dat$status)

(table(dem_dat$status)[[2]]/table(dem_dat$status)[[1]])*100 # 11.94


# # DANUM CLEAN STEMS TO USE - from Lucie data cleaning based on growth rates
# cleaned_dat <- read.csv("G:/My Drive/Harvard/Lucie_Internship/Data/Census_2_dnm50_clean_main_stem.csv")
# clean_stems <- cleaned_dat$stemID
#dnm_dat <- subset(uncleaned_dat, stemID %in% clean_stems)

# dnm_dat <- subset(ForestGEO_dat, site == "DNM50_NA")
# dnm_fgeo <- fgeo.tool::pick_main_stem(dnm_dat)
dnm_fgeo <- subset(dem_dat, site == "DNM50")
table(dnm_fgeo$census)
table(dnm_fgeo$status)
(table(dnm_fgeo$status)[[2]]/table(dnm_fgeo$status)[[1]])*100 # 5.06

# table(dnm_fgeo$census)
# DNM_c1 <- subset(dnm_fgeo, census == "census_2011_15")
# DNM_c2 <- subset(dnm_fgeo, census == "census_2019")
# table(DNM_c1$status); table(DNM_c2$status)

# entire lambir 52 ha plot
lh_fgeo <- subset(dem_dat, plot == "LH_clay" | plot == "LH_fineLoam" | plot == "LH_loam" | plot == "LH_sandstone")
table(lh_fgeo$census)
table(lh_fgeo$status)
(table(lh_fgeo$status)[[2]]/table(lh_fgeo$status)[[1]])*100 # 14.31

# lhc_dat <- subset(ForestGEO_dat, site == "LHPc")
# lhc_fgeo <- fgeo.tool::pick_main_stem(lhc_dat)
lhc_fgeo <- subset(dem_dat, plot == "LH_clay")
table(lhc_fgeo$census)
table(lhc_fgeo$status)
(table(lhc_fgeo$status)[[2]]/table(lhc_fgeo$status)[[1]])*100 # 20.52

# lhs_dat <- subset(ForestGEO_dat, site == "LHPs")
# lhs_fgeo <- fgeo.tool::pick_main_stem(lhs_dat)
lhs_fgeo <- subset(dem_dat, plot == "LH_sandstone")
table(lhs_fgeo$census)
table(lhs_fgeo$status)
(table(lhs_fgeo$status)[[2]]/table(lhs_fgeo$status)[[1]])*100 # 11.14

# FOREST PLOTS MORTALITY 
table(dem_dat$status)
# see: https://www.forestplots.net/upload/ManualsEnglish/RAINFOR_data_codes_EN.pdf
# ForestPlots_dat$status <- ifelse(ForestPlots_dat$DFstatus == '0', "D","A")
# table(ForestPlots_dat$status)

table(dem_dat$site)
SPKA <- subset(dem_dat, site == "SPKA"); table(SPKA$status)
SPKS <- subset(dem_dat, site == "SPKS"); table(SPKS$status)
SPKH <- subset(dem_dat, site == "SPKH"); table(SPKH$status)
#----------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------#
# separate sites by census
#----------------------------------------------------------------------------------------#
table(dnm_fgeo$census)
DNM_c1 <- subset(dnm_fgeo, census == "census_2011_15")
DNM_c2 <- subset(dnm_fgeo, census == "census_2019")

table(lh_fgeo$census)
LH_c1 <- subset(lh_fgeo, census == "census_1991")
LH_c2 <- subset(lh_fgeo, census == "census_1997")
LH_c3 <- subset(lh_fgeo, census == "census_2003")
LH_c4 <- subset(lh_fgeo, census == "census_2007_08")

table(lhc_fgeo$census)
LHC_c1 <- subset(lhc_fgeo, census == "census_1991")
LHC_c2 <- subset(lhc_fgeo, census == "census_1997")
LHC_c3 <- subset(lhc_fgeo, census == "census_2003")
LHC_c4 <- subset(lhc_fgeo, census == "census_2007_08")

table(lhs_fgeo$census)
LHS_c1 <- subset(lhs_fgeo, census == "census_1991")
LHS_c2 <- subset(lhs_fgeo, census == "census_1997")
LHS_c3 <- subset(lhs_fgeo, census == "census_2003")
LHS_c4 <- subset(lhs_fgeo, census == "census_2007_08")

table(SPKA$census)
SPKA_09_c1 <- subset(SPKA, census == "09_census_2001")
SPKA_09_c2 <- subset(SPKA, census == "09_census_2009")
SPKA_09_c3 <- subset(SPKA, census == "09_census_2014")

SPKA_10_c1 <- subset(SPKA, census == "10_census_2001") 
SPKA_10_c2 <- subset(SPKA, census == "10_census_2009")
SPKA_10_c3 <- subset(SPKA, census == "10_census_2014") 

table(SPKS$census)
SPKS_08_c1 <- subset(SPKS, census == "08_census_2001")
SPKS_08_c2 <- subset(SPKS, census == "08_census_2009")
SPKS_08_c3 <- subset(SPKS, census == "08_census_2014")

table(SPKH$census)
SPKH_04_c1 <- subset(SPKH, census == "04_census_2001")
SPKH_04_c2 <- subset(SPKH, census == "04_census_2008")
SPKH_04_c3 <- subset(SPKH, census == "04_census_2014")

SPKH_05_c1 <- subset(SPKH, census == "05_census_2001")
SPKH_05_c2 <- subset(SPKH, census == "05_census_2008")
SPKH_05_c3 <- subset(SPKH, census == "05_census_2014")

SPKH_30_c1 <- subset(SPKH, census == "30_census_2001") 
SPKH_30_c2 <- subset(SPKH, census == "30_census_2010")
SPKH_30_c3 <- subset(SPKH, census == "30_census_2015") 

#--------------------------------------------------------------------------------------------------#
# calculate by hand
#--------------------------------------------------------------------------------------------------#
# N  = number of trees at the first census
# S  = number of trees that survived to the second census, N - D
# meantime = interval between censuses in years for trees in the first census
#----------------------------------------------------------------------------------------#
census_dat1 <- DNM_c1 # LH_c3
census_dat2 <- DNM_c2 # LH_c4

table(census_dat1$status); table(census_dat2$status)

census_dat1_alive <- filter(census_dat1, status == "A")
join_dat <- inner_join(census_dat1_alive, census_dat2, by="treeID")
N <- table(join_dat$status.x)[1]
S <- table(join_dat$status.y)[1]
mean_time <- mean(join_dat$date.y-join_dat$date.x)

(log(N) - log(S))/(mean_time/365)

join_dat <- semi_join(census_dat2, census_dat1_alive, by="treeID")
N <- table(census_dat1_alive$status)[1]
S <- table(join_dat$status)[1]
mean_time <- mean(join_dat$date-census_dat1_alive$date)

(log(N) - log(S))/(mean_time/365)

#test <- mortality_rate(table(join_dat$status.x)[1], table(join_dat$status.y)[1], meantime=mean_time) 
## can't find function 'find.climits'
#----------------------------------------------------------------------------------------#
  


#--------------------------------------------------------------------------------------------------#
# calculate using mortality_ctfs() function
#--------------------------------------------------------------------------------------------------#
DNM_mort1 <- mortality_ctfs(DNM_c1, DNM_c2) # mort rate = 0.0038 (95CI: 0.0009, 0.0213)
DNM_mort1$rate*100; DNM_mort1$lower*100; DNM_mort1$upper*100

# Lambir - entire plot
LH_mort1 <- mortality_ctfs(LH_c1, LH_c2) # mort rate = )
LH_mort1$rate*100; LH_mort1$lower*100; LH_mort1$upper*100
LH_mort2 <- mortality_ctfs(LH_c2, LH_c3) # mort rate =  (95CI: )
LH_mort2$rate*100; LH_mort2$lower*100; LH_mort2$upper*100
LH_mort3 <- mortality_ctfs(LH_c3, LH_c4) # mort rate =  (95CI: )
LH_mort3$rate*100; LH_mort3$lower*100; LH_mort3$upper*100

LH_mort1$rate; LH_mort2$rate; LH_mort3$rate

LHC_mort1 <- mortality_ctfs(LHC_c1, LHC_c2) # mort rate = 0.0093 (95CI: 0.0029, 0.0335)
LHC_mort1$rate*100; LHC_mort1$lower*100; LHC_mort1$upper*100
LHC_mort2 <- mortality_ctfs(LHC_c2, LHC_c3) # mort rate =  (95CI: )
LHC_mort2$rate*100; LHC_mort2$lower*100; LHC_mort2$upper*100
LHC_mort3 <- mortality_ctfs(LHC_c3, LHC_c4) # mort rate =  (95CI: )
LHC_mort3$rate*100; LHC_mort3$lower*100; LHC_mort3$upper*100

LHS_mort1 <- mortality_ctfs(LHS_c1, LHS_c2) # mort rate =  (95CI: )
LHS_mort1$rate*100; LHS_mort1$lower*100; LHS_mort1$upper*100
LHS_mort2 <- mortality_ctfs(LHS_c2, LHS_c3) # mort rate =  (95CI: )
LHS_mort2$rate*100; LHS_mort2$lower*100; LHS_mort2$upper*100
LHS_mort3 <- mortality_ctfs(LHS_c3, LHS_c4) # mort rate =  (95CI: )
LHS_mort3$rate*100; LHS_mort3$lower*100; LHS_mort3$upper*100


# SPKA_09_c1$date <- rep(14933,length(SPKA_09_c1$dbh)) #"2000-11-19"
# SPKA_09_c2$date <- rep(18229,length(SPKA_09_c2$dbh)) #"2009-11-28"
# SPKA_09_c3$date <- rep(19920,length(SPKA_09_c3$dbh)) #"2014-07-16"
# 
# SPKA_10_c1$date <- rep(14924,length(SPKA_10_c1$dbh)) #"2000-11-10"
# SPKA_10_c2$date <- rep(18112,length(SPKA_10_c2$dbh)) #"2009-08-03"
# SPKA_10_c3$date <- rep(19883,length(SPKA_10_c3$dbh)) #"2014-06-09"

SPKA_09_mort1 <- mortality_ctfs(SPKA_09_c1, SPKA_09_c2) # mort rate = 0.0210 (95CI: 0.0196, 0.0225)
SPKA_09_mort1$rate; SPKA_09_mort1$lower; SPKA_09_mort1$upper
SPKA_09_mort2 <- mortality_ctfs(SPKA_09_c2, SPKA_09_c3) # mort rate = 0.0211 (95CI: 0.0189, 0.0234)
SPKA_09_mort2$rate; SPKA_09_mort2$lower; SPKA_09_mort2$upper
SPKA_10_mort1 <- mortality_ctfs(SPKA_10_c1, SPKA_10_c2) # mort rate = 0.0212 (95CI: 0.0196, 0.0228)
SPKA_10_mort1$rate; SPKA_10_mort1$lower; SPKA_10_mort1$upper
SPKA_10_mort2 <- mortality_ctfs(SPKA_10_c2, SPKA_10_c3) # mort rate = 0.0216 (95CI: 0.0194, 0.0241)
SPKA_10_mort2$rate; SPKA_10_mort2$lower; SPKA_10_mort2$upper

# SPKS_08_c1$date <- rep(14994,length(SPKS_08_c1$dbh)) #"2001-01-19"
# SPKS_08_c2$date <- rep(18034,length(SPKS_08_c2$dbh)) #"2009-05-17"
# SPKS_08_c3$date <- rep(19899,length(SPKS_08_c3$dbh)) #"2014-06-25"

SPKS_08_mort1 <- mortality_ctfs(SPKS_08_c1, SPKS_08_c2) # mort rate = 0.0111 (95CI: 0.0102, 0.0121)
SPKS_08_mort1$rate; SPKS_08_mort1$lower; SPKS_08_mort1$upper
SPKS_08_mort2 <- mortality_ctfs(SPKS_08_c2, SPKS_08_c3) # mort rate = 0.0142 (95CI: 0.0128, 0.0157)
SPKS_08_mort2$rate; SPKS_08_mort2$lower; SPKS_08_mort2$upper

# SPKH_04_c1$date <- rep(15125,length(SPKH_04_c1$dbh)) #"2001-05-30"
# SPKH_04_c2$date <- rep(17754,length(SPKH_04_c2$dbh)) #"2008-08-10"
# SPKH_04_c3$date <- rep(20060,length(SPKH_04_c3$dbh)) #"2014-12-03"
# 
# SPKH_05_c1$date <- rep(15124,length(SPKH_05_c1$dbh)) #"2001-05-29"
# SPKH_05_c2$date <- rep(17772,length(SPKH_05_c2$dbh)) #"2008-08-28"
# SPKH_05_c3$date <- rep(20004,length(SPKH_05_c3$dbh)) #"2014-10-08"
# 
# SPKH_30_c1$date <- rep(15097,length(SPKH_30_c1$dbh)) #"2001-05-02"
# SPKH_30_c2$date <- rep(18433,length(SPKH_30_c2$dbh)) #"2010-06-20"
# SPKH_30_c3$date <- rep(20127,length(SPKH_30_c3$dbh)) #"2015-02-08"

SPKH_04_mort1 <- mortality_ctfs(SPKH_04_c1, SPKH_04_c2) # mort rate = 0.0212 (95CI: 0.0198, 0.0227)
SPKH_04_mort1$rate; SPKH_04_mort1$lower; SPKH_04_mort1$upper
SPKH_04_mort2 <- mortality_ctfs(SPKH_04_c2, SPKH_04_c3) # mort rate = 0.0226 (95CI: 0.0210, 0.0242)
SPKH_04_mort2$rate; SPKH_04_mort2$lower; SPKH_04_mort2$upper
SPKH_05_mort1 <- mortality_ctfs(SPKH_05_c1, SPKH_05_c2) # mort rate = 0.0180 (95CI: 0.0169, 0.0191)
SPKH_05_mort1$rate; SPKH_05_mort1$lower; SPKH_05_mort1$upper
SPKH_05_mort2 <- mortality_ctfs(SPKH_05_c2, SPKH_05_c3) # mort rate = 0.0221 (95CI: 0.0208, 0.0236)
SPKH_05_mort2$rate; SPKH_05_mort2$lower; SPKH_05_mort2$upper
SPKH_30_mort1 <- mortality_ctfs(SPKH_30_c1, SPKH_30_c2) # mort rate = 0.0156 (95CI: 0.0147, 0.0166)
SPKH_30_mort1$rate; SPKH_30_mort1$lower; SPKH_30_mort1$upper
SPKH_30_mort2 <- mortality_ctfs(SPKH_30_c2, SPKH_30_c3) # mort rate = 0.0140 (95CI: 0.0128, 0.0154)
SPKH_30_mort2$rate; SPKH_30_mort2$lower; SPKH_30_mort2$upper
#--------------------------------------------------------------------------------------------------#

DNM_mort <- as.data.frame(rbind(c(DNM_mort1$rate, DNM_mort1$lower, DNM_mort1$upper,"DNM")))

LHC_mort <- as.data.frame(rbind(c(LHC_mort1$rate, LHC_mort1$lower, LHC_mort1$upper,"LHC"),
                                c(LHC_mort2$rate, LHC_mort2$lower, LHC_mort2$upper,"LHC"),
                                c(LHC_mort3$rate, LHC_mort3$lower, LHC_mort3$upper,"LHC")))

LHS_mort <- as.data.frame(rbind(c(LHS_mort1$rate, LHS_mort1$lower, LHS_mort1$upper,"LHS"),
                                c(LHS_mort2$rate, LHS_mort2$lower, LHS_mort2$upper,"LHS"),
                                c(LHS_mort3$rate, LHS_mort3$lower, LHS_mort3$upper,"LHS")))


SPKA_mort <- as.data.frame(rbind(c(SPKA_09_mort1$rate, SPKA_09_mort1$lower, SPKA_09_mort1$upper,"SPKA"),
                                 c(SPKA_09_mort2$rate, SPKA_09_mort2$lower, SPKA_09_mort2$upper,"SPKA"),
                                 c(SPKA_10_mort1$rate, SPKA_10_mort1$lower, SPKA_10_mort1$upper,"SPKA"),
                                 c(SPKA_10_mort2$rate, SPKA_10_mort2$lower, SPKA_10_mort2$upper,"SPKA")))
SPKS_mort <- as.data.frame(rbind(c(SPKS_08_mort1$rate, SPKS_08_mort1$lower, SPKS_08_mort1$upper,"SPKS"),
                                 c(SPKS_08_mort1$rate, SPKS_08_mort1$lower, SPKS_08_mort1$upper,"SPKS")))
SPKH_mort <- as.data.frame(rbind(c(SPKH_04_mort1$rate, SPKH_04_mort1$lower, SPKH_04_mort1$upper,"SPKH"),
                                 c(SPKH_04_mort2$rate, SPKH_04_mort2$lower, SPKH_04_mort2$upper,"SPKH"),
                                 c(SPKH_05_mort1$rate, SPKH_05_mort1$lower, SPKH_05_mort1$upper,"SPKH"),
                                 c(SPKH_05_mort2$rate, SPKH_05_mort2$lower, SPKH_05_mort2$upper,"SPKH"),
                                 c(SPKH_30_mort1$rate, SPKH_30_mort1$lower, SPKH_30_mort1$upper,"SPKH"),
                                 c(SPKH_30_mort2$rate, SPKH_30_mort2$lower, SPKH_30_mort2$upper,"SPKH")))

SPK_mort <- rbind(DNM_mort, LHC_mort, LHS_mort, SPKA_mort, SPKS_mort, SPKH_mort)
colnames(SPK_mort) <- c("mort", "lower", "upper", "site")
SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.character)
SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.numeric)


# SPKA_mort <- as.data.frame(cbind(mort=c(0.0210,0.0211,0.0212,0.0216), lower=c(0.0196,0.0189,0.0196,0.0194), upper=c(0.0225,0.0234,0.0228,0.0241), site=rep("SPKA",4)))
# SPKS_mort <- as.data.frame(cbind(mort=c(0.0111,0.0142), lower=c(0.0102,0.0128), upper=c(0.0121,0.0157), site=rep("SPKS",2)))
# SPKH_mort <- as.data.frame(cbind(mort=c(0.0212,0.0226,0.0180,0.0221,0.0156,0.0140), lower=c(0.0198,0.0210,0.0169,0.0208,0.0147,0.0128), upper=c(0.0227,0.0242,0.0191,0.0236,0.0166,0.0154), site=rep("SPKH",6)))
# 
# SPK_mort <- rbind(SPKA_mort, SPKS_mort, SPKH_mort)
# SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.character)
# SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.numeric)

#SPK_mort$site <- factor(SPK_mort$site, levels = c("SPKA", "SPKS", "SPKH"))
SPK_mort$site <- factor(SPK_mort$site, levels = c("SPKA", "DNM", "LHC", "SPKS", "LHS", "SPKH"))

ggplot(SPK_mort, aes(x=site,y=mort, fill=site)) + geom_point(size=6, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4])) +
  #  scale_fill_manual(values=pal) +
  labs(x="", y=expression(Mortality~rate~(yr^{-1}))) +
  #scale_x_discrete(labels = c("SPK_alluvial","SPK_sandstone","SPK_heath")) +
  theme(legend.position = "none")
# 5x5 ED2_sites_mort_rates_by_census_5cm / ED2_sites_mort_rates_by_census_10cm / ED2_sites_mort_rates_by_census_30cm

plot_dat <- SPK_mort %>% group_by(site) %>% summarize(mmort = median(mort),
                                                      mlow = median(lower),
                                                      mup = median(upper))

ggplot(plot_dat, aes(x=site,y=mmort, fill=site)) + 
  geom_errorbar(aes(ymin=mlow, ymax=mup), width=.2, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=5, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4])) +
  labs(x="", y=expression(Mortality~rate~(yr^{-1}))) +
  scale_x_discrete(labels = c("SPKa","DNM50","LHc","SPKs","LHs","SPKh")) +
  theme(legend.position = "none") #+ 
#  ylim(0,0.05)
# 5x5 ED2_sites_mort_rates_5cm / ED2_sites_mort_rates_10cm / ED2_sites_mort_rates_30cm

# JUST SEPILOK
plot_dat <- subset(plot_dat, site == "SPKA" | site == "SPKS" | site == "SPKH")
ggplot(plot_dat, aes(x=site,y=mmort, fill=site)) + 
  geom_errorbar(aes(ymin=mlow, ymax=mup), width=.2, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=5, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[2],rev(pal2)[4])) +
  labs(x="", y="Mean annual mortality rate") +
  scale_x_discrete(labels = c("SPKa","SPKs","SPKh")) +
  theme(legend.position = "none")
# 5x5 SPK_sites_mort_rates


#------------------------------------------------------------------------#
# plot mortality BINNED by DBH
#------------------------------------------------------------------------#
library(Hmisc)

# constants #
FGEO_breaks   = c(seq(0,9,1),seq(10,30,10),seq(40,100,20),300)
FPlots_breaks = c(0,seq(6,9,1),seq(10,30,10),seq(40,100,20),181)
ylower        = 0.001
yupper        = 0.15

binned_mort <- function(census1_dat, census2_dat, bin_breaks=bin_breaks, g=50){
  listofdfs <- list()
  
  # omit DBH == NA from dat_c1_prelim
  dat_c1 <- census1_dat[complete.cases(census1_dat$dbh),]
  
  ## create DBH bins
  dat_c1$dbh_bin <- as.factor(cut(dat_c1$dbh, breaks = bin_breaks)); table(dat_c1$dbh_bin)

  # old version where g = # intervals and intervals have roughly same # obs
  #dat_c1$dbh_bin <- as.factor(cut2(dat_c1$dbh, g=g)); table(dat_c1$dbh_bin)
  
  dat_c1$dbh_bin_start <- gsub("(.*?),.*", "\\1",  dat_c1$dbh_bin); table(dat_c1$dbh_bin_start)
  dat_c1$dbh_bin_start <- gsub("[()]", "",dat_c1$dbh_bin_start); table(dat_c1$dbh_bin_start)
  dat_c1$dbh_bin_start <- gsub("\\[", "",dat_c1$dbh_bin_start); table(dat_c1$dbh_bin_start)
  dat_c1$dbh_bin_start <- as.numeric(dat_c1$dbh_bin_start); table(dat_c1$dbh_bin_start)
  
  dat_c1$dbh_bin_end <- gsub(".*,(.*?)", "\\1",  dat_c1$dbh_bin); table(dat_c1$dbh_bin_end)
  dat_c1$dbh_bin_end <- gsub("[()]", "",dat_c1$dbh_bin_end); table(dat_c1$dbh_bin_end)
  dat_c1$dbh_bin_end <- gsub("\\]", "",dat_c1$dbh_bin_end); table(dat_c1$dbh_bin_end)
  dat_c1$dbh_bin_end <- as.numeric(dat_c1$dbh_bin_end); table(dat_c1$dbh_bin_end)
  
  dat_c1$dbh_bin_val <- rowMeans(dat_c1[,c('dbh_bin_start', 'dbh_bin_end')], na.rm=TRUE); table(dat_c1$dbh_bin_val)
  dat_c1$dbh_bin_ID <- as.factor(dat_c1$dbh_bin_val); table(dat_c1$dbh_bin_ID)
  dat_c1 <- subset(dat_c1, dbh_bin_ID != "NaN"); table(dat_c1$dbh_bin_ID)
  dat_c1$dbh_bin_ID <- as.numeric(dat_c1$dbh_bin_ID); table(dat_c1$dbh_bin_ID)
  
  for(i in 1:max(dat_c1$dbh_bin_ID, na.rm=T)){
    print(i)
    dat1    <- subset(dat_c1, dbh_bin_ID == i)
    dat2    <- inner_join(select(dat1, treeID), census2_dat, by="treeID")
    if(dim(dat1)[[1]] > 0){
      mort_dat  <- mortality_ctfs(dat1, dat2)
      final_dat <- cbind(i, mort_dat$rate, mort_dat$lower, mort_dat$upper)
    }
    else {
      final_dat <- cbind(i, NA, NA, NA)
    }
    listofdfs[[i]] <- final_dat # save dataframes into the list
  }
  clean_list <- listofdfs[!sapply(listofdfs, is.null)] # remove NULL list elements (B)
  # COMBINE all lists into single dataframe
  output_dat <- do.call(rbind, clean_list) # less efficient way of concatenating list elements into df, but works with spatial df's
  output_dat <- as.data.frame(output_dat)
  bin_names <- as.data.frame(table(dat_c1$dbh_bin_val))
  output_dat$dbh_bin <- as.character(bin_names$Var1)
  output_dat$dbh_bin <- as.numeric(output_dat$dbh_bin)
  
  colnames(output_dat) <- c("bin", "mort_rate","lower","upper", "dbh_bin")
  return(output_dat)
}


lambir_cp1 <- binned_mort(LH_c1, LH_c2, bin_breaks=FGEO_breaks)
# test2 <- na.omit(test); head(test2)
# bin_names <- as.data.frame(table(dat_c1$dbh_bin_val))
# test2$dbh_bin <- as.character(bin_names$Var1); head(test2)
# #test2$dbh_bin <- c(as.character(bin_names$Var1),NA); head(test2)
# test2$dbh_bin <- as.numeric(test2$dbh_bin); head(test2)

p1 <- ggplot(lambir_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
#  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  #scale_y_continuous(trans='log10', limits=c(ylower,yupper)) + 
#  ylim(0.002,0.1) + 
#  scale_y_continuous(breaks=c(0.002, 1.00)) + 
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Lambir (c1-c2)") +
  theme(legend.position = "none")

lambir_cp2 <- binned_mort(LH_c2, LH_c3, bin_breaks=FGEO_breaks)
p2 <- ggplot(lambir_cp2, aes(x=dbh_bin,y=mort_rate)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Lambir (c2-c3)") +
  theme(legend.position = "none")

lambir_cp3 <- binned_mort(LH_c3, LH_c4, bin_breaks=FGEO_breaks)
p3 <- ggplot(lambir_cp3, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Lambir (c3-c4)") +
  theme(legend.position = "none")

danum <- binned_mort(DNM_c1, DNM_c2, bin_breaks=FGEO_breaks)
p4 <- ggplot(danum, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Danum (c1-c2)") +
  theme(legend.position = "none")

plot_grid(p1,p2,p3,p4, ncol=2)


# LAMBIR CLAY & SANDSTONE 
lambirC_cp1 <- binned_mort(LHC_c1, LHC_c2, bin_breaks=FGEO_breaks)
p1 <- ggplot(lambirC_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Clay (c1-c2)") +
  theme(legend.position = "none")

lambirC_cp2 <- binned_mort(LHC_c2, LHC_c3, bin_breaks=FGEO_breaks)
p2 <- ggplot(lambirC_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Clay (c2-c3)") +
  theme(legend.position = "none")

lambirC_cp3 <- binned_mort(LHC_c3, LHC_c4, bin_breaks=FGEO_breaks)
p3 <- ggplot(lambirC_cp3, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Clay (c3-c4)") +
  theme(legend.position = "none")


lambirS_cp1 <- binned_mort(LHS_c1, LHS_c2, bin_breaks=FGEO_breaks)
p4 <- ggplot(lambirS_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Sandstone (c1-c2)") +
  theme(legend.position = "none")

lambirS_cp2 <- binned_mort(LHS_c2, LHS_c3, bin_breaks=FGEO_breaks)
p5 <- ggplot(lambirS_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Sandstone (c2-c3)") +
  theme(legend.position = "none")

lambirS_cp3 <- binned_mort(LHS_c3, LHS_c4, bin_breaks=FGEO_breaks)
p6 <- ggplot(lambirS_cp3, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Sandstone (c3-c4)") +
  theme(legend.position = "none")

plot_grid(p1,p2,p3,p4,p5,p6,ncol=3)


# SEPILOK ALLUVIAL 
# triple check that treeID's are unique for different plots
summary(SPKA_09_c1$treeID); summary(SPKA_10_c1$treeID)
SPKA_c1 <- rbind(SPKA_09_c1, SPKA_10_c1)
SPKA_c2 <- rbind(SPKA_09_c2, SPKA_10_c2)
SPKA_c3 <- rbind(SPKA_09_c3, SPKA_10_c3)

spka_cp1 <- binned_mort(SPKA_c1, SPKA_c2, bin_breaks=FPlots_breaks)
spka_cp1$upper <- ifelse(spka_cp1$upper > 0.1, 0.1, spka_cp1$upper)
sp1 <- ggplot(spka_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Alluvial (c1-c2)") +
  theme(legend.position = "none")

spka_cp2 <- binned_mort(SPKA_c2, SPKA_c3, bin_breaks=FPlots_breaks)
spka_cp2$upper <- ifelse(spka_cp2$upper > 0.1, 0.1, spka_cp2$upper)
sp2 <- ggplot(spka_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Alluvial (c2-c3)") +
  theme(legend.position = "none")


# SEPILOK SANDSTONE
spks_cp1 <- binned_mort(SPKS_08_c1, SPKS_08_c2, bin_breaks=FPlots_breaks)
spks_cp1$upper <- ifelse(spks_cp1$upper > 0.1, 0.1, spks_cp1$upper)
sp3 <- ggplot(spks_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Sandstone (c1-c2)") +
  theme(legend.position = "none")

spks_cp2 <- binned_mort(SPKS_08_c2, SPKS_08_c3, bin_breaks=FPlots_breaks)
spks_cp2$upper <- ifelse(spks_cp2$upper > 0.1, 0.1, spks_cp2$upper)
sp4 <- ggplot(spks_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Sandstone (c2-c3)") +
  theme(legend.position = "none")


# SEPILOK HEATH 
# triple check that treeID's are unique for different plots
summary(SPKH_04_c1$treeID); summary(SPKH_05_c1$treeID); summary(SPKH_30_c1$treeID) 
SPKH_c1 <- rbind(SPKH_04_c1, SPKH_05_c1, SPKH_30_c1)
SPKH_c2 <- rbind(SPKH_04_c2, SPKH_05_c2, SPKH_30_c2)
SPKH_c3 <- rbind(SPKH_04_c3, SPKH_05_c3, SPKH_30_c3)

spkh_cp1 <- binned_mort(SPKH_c1, SPKH_c2, bin_breaks=FPlots_breaks)
spkh_cp1$upper <- ifelse(spkh_cp1$upper > 0.1, 0.1, spkh_cp1$upper)
sp5 <- ggplot(spkh_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Heath (c1-c2)") +
  theme(legend.position = "none")

spkh_cp2 <- binned_mort(SPKH_c2, SPKH_c3, bin_breaks=FPlots_breaks)
spkh_cp2$upper <- ifelse(spkh_cp2$upper > 0.1, 0.1, spkh_cp2$upper)
sp6 <- ggplot(spkh_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Heath (c2-c3)") +
  theme(legend.position = "none")

plot_grid(sp1,sp3,sp5,sp2,sp4,sp6,ncol=3)
#------------------------------------------------------------------------#


#----------------------------- CANOPY GAP SIZE FREQUENCY DISTRIBUTION ---------------------------------
# Zeta distribution (lambda) ia a metric to quantify and compare the negative relationship
# between canopy gap frequency and size across sites
# code from Asner et al. 2013 PLOSone
## see Gap_size_freq_dist_ForestGapR.R 

# combine lambdas & boostrapped SE

#DNM50 & ForestPlots - 2 m
plot_dat <- as.data.frame(cbind(lambda=c(1.512,1.422,1.533,1.501), lower=c(1.494947,1.406294,1.532976,1.472568), upper=c(1.532976,1.437786,1.532976,1.528418), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#DNM50 & ForestPlots - 5 m
plot_dat <- as.data.frame(cbind(lambda=c(1.452,1.386,1.513,1.488), lower=c(1.426651,1.374719,1.494947,1.466253), upper=c(1.480466,1.400118,1.532976,1.514177), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#DNM50 & ForestPlots - 10 m
plot_dat <- as.data.frame(cbind(lambda=c(1.378,1.352,1.437,1.435), lower=c(1.360905,1.343113,1.398154,1.420566), upper=c(1.398311,1.360533,1.464974,1.450949), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#DNM50 & ForestPlots - 20 m
plot_dat <- as.data.frame(cbind(lambda=c(1.3602,1.357,1.41303,1.36009), lower=c(1.343011,1.347129,1.384092,1.347281), upper=c(1.377477,1.366647,1.444553,1.372631), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#RS data - 2 m
# plot_dat <- as.data.frame(cbind(lambda=c(1.445,1.441,1.42), lower=c(1.426242,1.431351,1.395217), upper=c(1.461195,1.451024,1.438839), site=c("SPKA","SPKS","SPKH")))
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#RS data - 5 m
# plot_dat <- as.data.frame(cbind(lambda=c(1.414,1.412,1.425), lower=c(1.405226,1.405435,1.412781), upper=c(1.422847,1.419298,1.439280), site=c("SPKA","SPKS","SPKH")))
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#RS data - 10 m
# plot_dat <- as.data.frame(cbind(lambda=c(1.388,1.396,1.414), lower=c(1.381752,1.391436,1.407099), upper=c(1.393949,1.400844,1.421779), site=c("SPKA","SPKS","SPKH")))
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

plot_dat$site <- factor(plot_dat$site, levels = c("SPKA", "DNM50", "SPKS", "SPKH"))

#scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4])) +

ggplot(plot_dat, aes(x=site,y=lambda, fill=site)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(0.05), lwd=1) +
  geom_point(size=6, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],rev(pal2)[2],rev(pal2)[4])) +
  #  scale_fill_manual("",values=pal) +
  #  ylim(1.34,1.55) + 
  labs(x="", y=expression(lambda)) +
  scale_x_discrete(labels = c("SPKa","DNM50","SPKs","SPKh")) +
  theme(legend.position = "none")
# jpg 450x400 SPK_lambda_5m; SPK_ForPlots_lambda_5m





#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
>>>>>>> 2b2a82f617e3a1dfeb2aa972a51bbd30ce0c8cec
