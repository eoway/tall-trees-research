#Growth Rate--------------------------
library(fgeo)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)

#data <- read_csv(here("Desktop", "Research", "R", "DNM_SPK_forest_inventory_data_all_census_wDATE.csv"))
data <- read.csv("G:/My Drive/Harvard/Emergent_project/Naomi/Data/DNM_SPK_forest_inventory_data_all_census_wDATE.csv")

growdata <-rename (data, censusID = census, stemID = treeID, dbh = dbh, date = JulianDate, status = DFstatus)

# add "treeid" column for fgeo growth rate function below
growdata$treeid <- growdata$stemID

growdata$hom <- rep(130, length(growdata$dbh));

growdata$status <- toupper(growdata$status)

census1 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2001" & status == "A")
census2 <- filter(growdata, plot == "SPKS_08" & censusID == "08_census_2009" & status == "A")

table(growdata$plot)

table(census2$censusID)

summary(census1)

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)
# census 1 has 5271 stems; census 2 has 5017 stems

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
SPKS08_2001_2009 <- inner_join(census1, census2, by="stemID")
dim(SPKS08_2001_2009) # this restricts the dataset to 4850 unique stems that have measurements in both datasets

# notice that in 'SPKS08_2001_2009' dbh.x is dbh at census 1 and dbh.y is dbh at census 2

#------------------------------------------------------------------------------------------------------#
#                                 First calculate growth rates by hand                                 #
#------------------------------------------------------------------------------------------------------#
# method = "I" - calculates annual dbh increment (dbh2 - dbh1)/time - units = cm yr-1 or mm yr-2 depending on dbg units
# method = "E" - calculates relative growth rate (log(dbh2) - log(dbh1)) / time - unitless; a rate

# So, after the inner_join above: 
# calculate time difference and convert time from days to years  
time <- (SPKS08_2001_2009$date.y-SPKS08_2001_2009$date.x)/365

# assign dbh at time 1 (size1) and time 2 (size2)
size2 <- SPKS08_2001_2009$dbh.y
size1 <- SPKS08_2001_2009$dbh.x

# calculate growth rates: 
annual_increment <- (size2 - size1)/time
relative_gr      <- (log(size2) - log(size1))/time

# take a look at the values - how do these compare to values and distributions in Condit et al 2006?
summary(annual_increment)
summary(relative_gr)

par(mfrow=c(1,2))
hist(relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(annual_increment, xlab="Annual increment (cm)", col="grey", main="")

# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,1))
plot(SPKS08_2001_2009$dbh.x, SPKS08_2001_2009$dbh.y, pch=19, 
     xlab="DBH census 1 (cm)", ylab="DBH census 2 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
#------------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------------#
#                                     Now use FGEO and compare results                                 #
#------------------------------------------------------------------------------------------------------#
# split 'SPKS08_2001_2009' into census 1 and census 2 
# select first X columns for census 1 and last X columns for census 2
c1 <- SPKS08_2001_2009[,1:17]
c2 <- SPKS08_2001_2009[,c(18:21,5,22:33)] # also include stemID here
dim(c1); dim(c2)
colnames(c1) <- colnames(census1)
colnames(c2) <- colnames(census1)
table(c1$censusID); table(c2$censusID)

#method I = annual dbh increment & method E = relative growth rate-----
rgr <- growth_ctfs(as.tibble(c1), as.tibble(c2), method = "E", dbhunit = "cm")
as_tibble(rgr)

increment <- growth_ctfs(as.tibble(c1), as.tibble(c2), method = "I", dbhunit = "cm")
as_tibble(increment)

mean(relative_gr); rgr$rate
mean(annual_increment); increment$rate
#------------------------------------------------------------------------------------------------------#




#------------------------------------------------------------------------------------------------------#
#                                 code from previous attempt                                           #
#------------------------------------------------------------------------------------------------------#
#method I = annual dbh increment & method E = relative growth rate-----
growdata$increment <- as_tibble(growth_ctfs(census1, census2, method = "E", dbhunit = "cm"))
summary(growdata$increment)

growdata$relative <- as_tibble(growth_ctfs(census1, census2, method = "I", dbhunit = "cm"))
summary(growdata$relative)


?growth_ctfs

names(growdata)

#plot growth rate------
#increment-----
growdata%>%
  na.omit()%>%
  filter (growdata, censusID == "05_census_2001" | censusID == "04_census_2001" | censusID == "04_census_2008" | censusID == "05_census_2008")%>%
  ggplot(aes(x = dbh, y = increment))+
  geom_histogram(stat = 'identity') +
  xlab("DBH")+
  ylab("Growth Rate")

growdata%>%
  na.omit()%>%
  filter (growdata, censusID == "05_census_2001" | censusID == "04_census_2001" | censusID == "04_census_2008" | censusID == "05_census_2008")%>%
  ggplot(aes(x = height2h_01, y = increment))+
  geom_histogram(stat = 'identity') +
  xlab("Height")+
  ylab("Growth Rate")

#relative------
growdata%>%
  na.omit()%>%
  filter (growdata, censusID == "05_census_2001" | censusID == "04_census_2001" | censusID == "04_census_2008" | censusID == "05_census_2008")%>%
  ggplot(aes(x = dbh, y = relative))+
  geom_histogram(stat = 'identity') +
  xlab("dbh")+
  ylab("Growth Rate")

growdata%>%
  na.omit()%>%
  filter (growdata, censusID == "05_census_2001" | censusID == "04_census_2001" | censusID == "04_census_2008" | censusID == "05_census_2008")%>%
  ggplot(aes(x = height2h_01, y = relative))+
  geom_histogram(stat = 'identity') +
  xlab("Height")+
  ylab("Growth Rate")