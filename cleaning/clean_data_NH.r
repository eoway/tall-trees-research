library(tidyverse)
library(here)
library(skimr)

#Load Data----------

head(data)
summary(data)
data <- read_csv(here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))

head(data)

table(data)

table(data$DFstatus)

#Update DFstatus---------
library(stringr)

data$DFstatus <- gsub("0", "y", data$DFstatus)
data$DFstatus <- gsub("dead", "y", data$DFstatus)
data$DFstatus <- gsub("broken below", "yy", data$DFstatus)
data$DFstatus <- gsub("^b.*", "yy", data$DFstatus)
data$DFstatus <- gsub("missing", "yyy", data$DFstatus)

data$DFstatus <- gsub("[^y]+", "A", data$DFstatus)

data$DFstatus <- gsub("^y$", "D", data$DFstatus)
data$DFstatus <- gsub("^yy$", "B", data$DFstatus)
data$DFstatus <- gsub("^yyy$","missing", data$DFstatus)

table(data$DFstatus)

#Separate by plot---------
table(data$plot)

DNM1 <- filter(data, plot == "DNM1_01")
DNM2 <- filter(data, plot == "DNM2_02")
DNM3 <- filter(data, plot == "DNM3_03")
DNM50 <- filter(data, plot == "DNM50_FGEO")
SPKA9 <- filter(data, plot == "SPKA_09")
SPKA10 <- filter(data, plot == "SPKA_10")
SPKH4 <- filter(data, plot == "SPKH_04")
SPKH5 <- filter(data, plot == "SPKH_05")
SPKH30 <- filter(data, plot == "SPKH_30")
SPKS8 <- filter(data, plot == "SPKS_08")

#DNM1-------
#DNM1: Check for Duplicate Records-------
table(DNM1$census)

DNM106 <- filter(DNM1, census == "01_census_2006")
DNM106$treeID[duplicated(DNM106$stemID)]

DNM106help <- filter(DNM106, stemID == "297184")

DNM113 <- filter(DNM1, census == "01_census_2013")
DNM113$treeID[duplicated(DNM113$treeID)]

DNM116 <- filter(DNM1, census == "01_census_2016")
DNM116$treeID[duplicated(DNM116$treeID)]

#DNM1: Check for Strange Taxonomic Entries--------
table(DNM1$family)
table(DNM1$genus)
table(DNM1$species)

#DNM1: Check for Outliers-------
library(outliers)

outlier(DNM1$dbh)
outlier(DNM1$dbh, opposite = TRUE)
boxplot(DNM1$dbh)

#DNM2----
table(DNM2$census)
#DNM2: Check for Duplicates-------
DNM206 <- filter(DNM2, census == "02_census_2006")
DNM206$treeID[duplicated(DNM206$treeID)]

DNM213 <- filter(DNM2, census == "02_census_2013")
DNM213$treeID[duplicated(DNM213$treeID)]

DNM216 <- filter(DNM2, census == "02_census_2016")
DNM216$treeID[duplicated(DNM216$treeID)]

#DNM2: Check for Strange Taxonomic Entries--------
table(DNM2$family)
table(DNM2$genus)
table(DNM2$species)

#DNM2: Check for Outliers-------
outlier(DNM2$dbh)
outlier(DNM2$dbh, opposite = TRUE)

boxplot(DNM2$dbh)

#DNM3--------
table(DNM3$census)
#DNM3: Check for duplicates-------
DNM306 <- filter(DNM1, census == "03_census_2006")
DNM306$treeID[duplicated(DNM106$treeID)]

DNM313 <- filter(DNM1, census == "03_census_2013")
DNM313$treeID[duplicated(DNM113$treeID)]

DNM316 <- filter(DNM1, census == "03_census_2016")
DNM316$treeID[duplicated(DNM116$treeID)]

#DNM3: Check for Strange Taxonomic Entries--------
table(DNM3$family)
table(DNM3$genus)
table(DNM3$species)

#DNM3: Check for Outliers-------
outlier(DNM3$dbh)
outlier(DNM3$dbh, opposite = TRUE)

boxplot(DNM3$dbh)

#DNM50--------
table(DNM50$census)
#DNM50: Check for duplicates------
library(janitor)
#Duplicate Help Needed here---------------
DNM5011 <- filter(DNM50, census == "census_2011_15")
#checks for duplicates
DNM5011$treeID[duplicated(DNM5011$treeID)]

#makes a dataset of DNM5011 + a column called dupe_count
DNM50dupes1 <- get_dupes(DNM5011, treeID)
length(DNM50dupes1$dbh)
#orders the data set by dbh- I think I found this somewhere so that when it removes the duplicates it removes them from smallest to largest
DNM50dupes1 <- DNM50dupes1[order(DNM50dupes1$dbh)]

#length(DNM50dupes1$dbh)
summary(DNM50dupes1)

#My idea was to have a while loop (below) go through each of the dupe_count values and if it had a dupe_count > 4, it would remove the smallest duplicate 
#and recalculate dupe_count and repeat until it was down to 4. I think there is a problem with how I am trying to remove the duplicates? Or just with all of it I dunno
for (val in DNM50dupes1$dupe_count){
  while (DNM50dupes1$dupe_count > 4) {
    #Remove highest duplicate
    DNM50dupes1 <- DNM50dupes1$treeID[!duplicated(DNM50dupes1, DNM50dupes1$treeID)]
    DNM50dupes1$dupe_count <- get_dupes(DNM5011, treeID)
  }
}
  
summary(DNM50dupes1)
hist(DNM50dupes1$dupe_count)
boxplot(DNM50dupes1$dupe_count)

#then I would repeat this process for the secxond census's duplicates
DNM5019 <- filter(DNM50, census == "census_2019")
DNM5019$treeID[duplicated(DNM5019$treeID)]

#DNM50 <- rbind(DNM50dupes1, DNM50dupes2)

#DNM50: Check for Strange Taxonomic Entries--------
table(DNM50$family)
table(DNM50$genus)
table(DNM50$species)

#DNM50: Check for Outliers-------
outlier(DNM50$dbh)
outlier(DNM50$dbh, opposite = TRUE)

boxplot(DNM50$dbh)

#How to remove outliers-------
#DNM50out <- subset(DNM50, dbh == outlier(DNM50$dbh))
#DNM50full <- subset(DNM50, X1 != DNM50out$X1)

head(DNM50out)
summary(DNM50$dbh)
summary(DNM50full$dbh)

dim(DNM50);dim(DNM50full)

boxplot(DNM50$dbh)

#SPKA9----------
table(SPKA9$census)
#SPKA9: Check for duplicates----------
SPKA901 <- filter(SPKA9, census == "09_census_2001")
SPKA901$treeID[duplicated(SPKA901$treeID)]

SPKA909 <- filter(SPKA9, census == "09_census_2009")
SPKA909$treeID[duplicated(SPKA909$treeID)]

SPKA914 <- filter(SPKA9, census == "09_census_2014")
SPKA914$treeID[duplicated(SPKA914$treeID)]

#SPKA9: Check for Strange Taxonomic Entries--------
table(SPKA9$family)
table(SPKA9$genus)
table(SPKA9$species)

#SPKA9: Check for Outliers-------
outlier(SPKA9$dbh)
outlier(SPKA9$dbh, opposite = TRUE)

boxplot(SPKA9$dbh)

#SPKA10----------
table(SPKA10$census)
#SPKA10: Check for duplicates----------
SPKA1001 <- filter(SPKA10, census == "10_census_2001")
SPKA1001$treeID[duplicated(SPKA1001$treeID)]

SPKA1009 <- filter(SPKA10, census == "10_census_2009")
SPKA1009$treeID[duplicated(SPKA1009$treeID)]

SPKA1014 <- filter(SPKA10, census == "10_census_2014")
SPKA1014$treeID[duplicated(SPKA1014$treeID)]

#SPKA10: Check for Strange Taxonomic Entries--------
table(SPKA10$family)
table(SPKA10$genus)
table(SPKA10$species)

#SPKA10: Check for Outliers-------
outlier(SPKA10$dbh)
outlier(SPKA10$dbh, opposite = TRUE)

boxplot(SPKA10$dbh)

#SPKH4----------
table(SPKH4$census)
#SPKH4: Check for duplicates----------
SPKH401 <- filter(SPKH4, census == "04_census_2001")
SPKH401$treeID[duplicated(SPKH401$treeID)]

SPKH408 <- filter(SPKH4, census == "04_census_2008")
SPKH408$treeID[duplicated(SPKH408$treeID)]

SPKH414 <- filter(SPKH4, census == "04_census_2014")
SPKH414$treeID[duplicated(SPKH414$treeID)]

#SPKH4: Check for Strange Taxonomic Entries--------
table(SPKH4$family)
table(SPKH4$genus)
table(SPKH4$species)

#SPKH4: Check for Outliers-------
outlier(SPKH4$dbh)
outlier(SPKH4$dbh, opposite = TRUE)

boxplot(SPKH4$dbh)

#SPKH5----------
table(SPKH5$census)
#SPKH5: Check for duplicates----------
SPKH501 <- filter(SPKH5, census == "05_census_2001")
SPKH501$treeID[duplicated(SPKH501$treeID)]

SPKH508 <- filter(SPKH5, census == "05_census_2008")
SPKH508$treeID[duplicated(SPKH508$treeID)]

SPKH514 <- filter(SPKH5, census == "05_census_2014")
SPKH514$treeID[duplicated(SPKH514$treeID)]

#SPKH5: Check for Strange Taxonomic Entries--------
table(SPKH5$family)
table(SPKH5$genus)
table(SPKH5$species)

#SPKH5: Check for Outliers-------
outlier(SPKH5$dbh)
outlier(SPKH5$dbh, opposite = TRUE)

boxplot(SPKH5$dbh)

#SPKH30----------
table(SPKH30$census)
#SPKH30: Check for duplicates----------
SPKH3001 <- filter(SPKH30, census == "30_census_2001")
SPKH3001$treeID[duplicated(SPKH3001$treeID)]

SPKH3010 <- filter(SPKH30, census == "30_census_2010")
SPKH3010$treeID[duplicated(SPKH3010$treeID)]

SPKH3015 <- filter(SPKH30, census == "30_census_2015")
SPKH3015$treeID[duplicated(SPKH3015$treeID)]

#SPKH30: Check for Strange Taxonomic Entries--------
table(SPKH30$family)
table(SPKH30$genus)
table(SPKH30$species)

#SPKH30: Check for Outliers-------
outlier(SPKH30$dbh)
outlier(SPKH30$dbh, opposite = TRUE)

boxplot(SPKH30$dbh)

#SPKS8----------
table(SPKS8$census)
#SPKS8: Check for duplicates----------
SPKS801 <- filter(SPKS8, census == "08_census_2001")
SPKS801$treeID[duplicated(SPKS801$treeID)]

SPKS809 <- filter(SPKS8, census == "08_census_2009")
SPKS809$treeID[duplicated(SPKS809$treeID)]

SPKS814 <- filter(SPKS8, census == "08_census_2014")
SPKS814$treeID[duplicated(SPKS814$treeID)]

#SPKS8: Check for Strange Taxonomic Entries--------
table(SPKS8$family)
table(SPKS8$genus)
table(SPKS8$species)

#SPKS8: Check for Outliers-------
outlier(SPKS8$dbh)
outlier(SPKS8$dbh, opposite = TRUE)

boxplot(SPKS8$dbh)

#Combine all plots---------
cleandata <- rbind(DNM1, DNM2, DNM3, DNM50, SPKA9, SPKA10, SPKH4, SPKH5, SPKH30, SPKS8)

str(cleandata)

removal_ID <- read_csv(here("Desktop", "Research", "R", "Data", "removal_IDS.csv"))

removal_ID$new_ID <- with(removal_ID, paste0(stemID, plot.x))

tree_IDs_remove <- removal_ID$new_ID

cleandata$new_ID <- with(cleandata, paste0(stemID, plot))

withremoval <- cleandata[cleandata$new_ID !=  tree_IDs_remove, ]
str(cleandata$new_ID)
str(tree_IDs_remove)
dim(withremoval)
dim (cleandata)
  
