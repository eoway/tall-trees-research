library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)

edit(mortality_ctfs)

#Load Data----------
data <- read_csv(here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))

colnames(data)
deaddata <-rename (data, censusID = census, dbh = dbh, date = JulianDate, status = DFstatus)
colnames(deaddata)

#Update DFstatus---------
library(stringr)
deaddata$status <- gsub("0", "y", deaddata$status)
deaddata$status <- gsub("dead", "y", deaddata$status)
deaddata$status<- gsub("broken below", "yy", deaddata$status)
deaddata$status <- gsub("^b.*", "yy", deaddata$status)
deaddata$status <- gsub("missing", "yyy", deaddata$status)

deaddata$status<- gsub("[^y]+", "A", deaddata$status)

deaddata$status <- gsub("yyy","missing", deaddata$status)
deaddata$status<- gsub("yy", "B", deaddata$status)
deaddata$status <- gsub("y", "D", deaddata$status)

table(deaddata$status)
table(data$DFstatus)

#Plots--------------
#DNM1
DNM1 <- filter(deaddata, plot == "DNM1_01")
table(DNM1$censusID)
DNM106 <- filter(DNM1, censusID == "01_census_2006")
DNM113 <- filter(DNM1, censusID == "01_census_2013")
DNM116 <- filter(DNM1, censusID == "01_census_2016")
#DNM2
DNM2 <- filter(deaddata, plot == "DNM2_02")
table(DNM2$censusID)
DNM206 <- filter(DNM2, censusID == "02_census_2006")
DNM213 <- filter(DNM2, censusID == "02_census_2013")
DNM216 <- filter(DNM2, censusID == "02_census_2016")
#DNM3
DNM3 <- filter(deaddata, plot == "DNM3_03")
table(DNM3$censusID)
DNM306 <- filter(DNM3, censusID == "03_census_2006")
DNM313 <- filter(DNM3, censusID == "03_census_2013")
DNM316 <- filter(DNM3, censusID == "03_census_2016")
#DNM50
DNM50 <- filter(deaddata, plot == "DNM50_FGEO")
table(DNM50$censusID)
DNM5011 <- filter(DNM50, censusID == "census_2011_15")
DNM5019 <- filter(DNM50, censusID == "census_2019")
#SPKA9
SPKA9 <- filter(deaddata, plot == "SPKA_09")
table(SPKA9$censusID)
SPKA901 <- filter(SPKA9, censusID == "09_census_2001")
SPKA909 <- filter(SPKA9, censusID == "09_census_2009")
SPKA914 <- filter(SPKA9, censusID == "09_census_2014")
#SPKA10
SPKA10 <- filter(deaddata, plot == "SPKA_10")
table(SPKA10$censusID)
SPKA1001 <- filter(SPKA10, censusID == "10_census_2001")
SPKA1009 <- filter(SPKA10, censusID == "10_census_2009")
SPKA1014 <- filter(SPKA10, censusID == "10_census_2014")
#SPKH4
SPKH4 <- filter(deaddata, plot == "SPKH_04")
table(SPKH4$censusID)
SPKH401 <- filter(SPKH4, censusID == "04_census_2001")
SPKH408 <- filter(SPKH4, censusID == "04_census_2008")
SPKH414 <- filter(SPKH4, censusID == "04_census_2014")
#SPKH5
SPKH5 <- filter(deaddata, plot == "SPKH_05")
table(SPKH5$censusID)
SPKH501 <- filter(SPKH5, censusID == "05_census_2001")
SPKH508 <- filter(SPKH5, censusID == "05_census_2008")
SPKH514 <- filter(SPKH5, censusID == "05_census_2014")
#SPKH30
SPKH30 <- filter(deaddata, plot == "SPKH_30")
table(SPKH30$censusID)
SPKH3001 <- filter(SPKH5, censusID == "30_census_2001")
SPKH3010 <- filter(SPKH5, censusID == "30_census_2010")
SPKH3015 <- filter(SPKH5, censusID == "30_census_2015")
#SPKS8
SPKS8 <- filter(deaddata, plot == "SPKS_08")
table(SPKS8$censusID)
SPKS801 <- filter(SPKS8, censusID == "08_census_2001")
SPKS801p <- filter(SPKS8, censusID == "08_census_2001")
SPKS809 <- filter(SPKS8, censusID == "08_census_2009")
SPKS8091 <- filter(SPKS8, censusID == "08_census_2009")
SPKS8092 <- filter(SPKS8, censusID == "08_census_2009")
SPKS814 <- filter(SPKS8, censusID == "08_census_2014")
SPKS814p <- filter(SPKS8, censusID == "08_census_2014")
SPKS801p$pool_stem_ID    <- paste0(SPKS801$stemID, "_1")
SPKS8091$pool_stem_ID   <- paste0(SPKS8091$stemID, "_2")
SPKS8092$pool_stem_ID <- paste0(SPKS8092$stemID, "_1") 
SPKS814p$pool_stem_ID  <- paste0(SPKS814$stemID, "_2")

#DNM1 First interval------------

census1 <- DNM106
census2 <- DNM113
colnames(census2)
table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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

table(census2$censusID)

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
table(SPKS8$censusID)
table(SPKS08_2001_2014$censusID.x)
table(SPKS08_2001_2014$censusID.y)

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
plot(mortrates$size_class, mortrates$mortrate, pch=19, 
     xlab="DBH", ylab="mortality rate", )
plot(mortrates$size_class2, mortrates$mortrate, pch=19, 
     xlab="DBH", ylab="mortality rate", )
