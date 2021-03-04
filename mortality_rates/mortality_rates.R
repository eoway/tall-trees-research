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
