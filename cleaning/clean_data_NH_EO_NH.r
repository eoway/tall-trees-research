library(tidyverse)
library(here)
library(skimr)

#Load Data----------
data <- read_csv(here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))
#data <- read_csv("G:/My Drive/Harvard/Emergent_project/Data/data_first_clean.csv")

head(data)
summary(data)

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

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
DNM106 <- filter(DNM1, census == "01_census_2006")
DNM106$treeID[duplicated(DNM106$stemID)] # EO EDIT: you're using treeID AND stemID here, not sure
# if you meant to do that, but I think you want to use the same column name in both cases
DNM106$stemID[duplicated(DNM106$stemID)] # EO EDIT: you're using treeID AND stemID here, not sure

DNM106help <- filter(DNM106, stemID == "297184")
#---------------------------------------------------------------------------------------------#
# Instead of using treeID, let's use stemID to check for duplicates. My thought process here is 
# that we actually might expect to have multiple stems per tree (as we do in DNM50), but we 
# would not expect to have stemID duplicates, so we want to check for any errors there.
# Sorry for the confusion around treeID and stemID from the beginning!
#
# I changed all instances of 'treeID' to 'stemID' below when checking for duplicates
#---------------------------------------------------------------------------------------------#

DNM113 <- filter(DNM1, census == "01_census_2013")
DNM113$stemID[duplicated(DNM113$stemID)]

DNM116 <- filter(DNM1, census == "01_census_2016")
DNM116$stemID[duplicated(DNM116$stemID)]

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
DNM206$stemID[duplicated(DNM206$stemID)]

DNM213 <- filter(DNM2, census == "02_census_2013")
DNM213$stemID[duplicated(DNM213$stemID)]

DNM216 <- filter(DNM2, census == "02_census_2016")
DNM216$stemID[duplicated(DNM216$stemID)]

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
DNM306$stemID[duplicated(DNM106$stemID)]

DNM313 <- filter(DNM1, census == "03_census_2013")
DNM313$stemID[duplicated(DNM113$stemID)]

DNM316 <- filter(DNM1, census == "03_census_2016")
DNM316$stemID[duplicated(DNM116$stemID)]

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

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#checks for duplicates
DNM5011$treeID[duplicated(DNM5011$treeID)]
#---------------------------------------------------------------------------------------------#
DNM5011$stemID[duplicated(DNM5011$stemID)] # EO EDIT
#---------------------------------------------------------------------------------------------#
# Use stemID here instead of treeID, although I left the treeID check in case we do want to 
# filter stems where there are more than N duplicates (e.g. 4 in the example below)
#---------------------------------------------------------------------------------------------#

#makes a dataset of DNM5011 + a column called dupe_count
DNM50dupes1 <- get_dupes(DNM5011, treeID)
length(DNM50dupes1$dbh)
#orders the data set by dbh- I think I found this somewhere so that when it removes the duplicates it removes them from smallest to largest
# DNM50dupes1 <- DNM50dupes1[order(DNM50dupes1$dbh)] # EO EDIT: this needs a comma, see my version in the next line
# EO EDIT: I also sorted by treeID and added the "-" in front of dbh so that it's sorted with the largest DBH up top
DNM50dupes1 <- DNM50dupes1[order(DNM50dupes1$treeID, -DNM50dupes1$dbh),] 

#length(DNM50dupes1$dbh)
summary(DNM50dupes1)

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#My idea was to have a while loop (below) go through each of the dupe_count values and if it had a dupe_count > 4, it would remove the smallest duplicate 
#and recalculate dupe_count and repeat until it was down to 4. I think there is a problem with how I am trying to remove the duplicates? Or just with all of it I dunno
# for (val in DNM50dupes1$dupe_count){
#   while (DNM50dupes1$dupe_count > 4) {
#     #Remove highest duplicate
#     DNM50dupes1 <- DNM50dupes1$treeID[!duplicated(DNM50dupes1, DNM50dupes1$treeID)]
#     DNM50dupes1$dupe_count <- get_dupes(DNM5011, treeID)
#   }
# }

dim(DNM50dupes1)
summary(DNM50dupes1$dupe_count)

# set aside obs to keep (where dupe_count <= 4)
dat_keep <- filter(DNM50dupes1, dupe_count <= 4)
# set aside obs to update (where dupe_count > 4)
dat_update <- filter(DNM50dupes1, dupe_count > 4)
# remove duplicates
dat_1 <- dat_update[ !duplicated(dat_update$treeID), ] 
# combine data that was kept with updated data
new_dat <- bind_rows(dat_1,dat_keep)
# recalculate # duplicates
new_dat2 <- get_dupes(new_dat, treeID)

summary(new_dat2$dupe_count)
hist(new_dat2$dupe_count)
boxplot(new_dat2$dupe_count)
# summary(DNM50dupes1)
# hist(DNM50dupes1$dupe_count)
# boxplot(DNM50dupes1$dupe_count)

#---------------------------------------------------------------------------------------------#
# I think the while loop was over complicating things because dat_update[ !duplicated(dat_update$treeID), ]
# removes all duplicates. If you specify that this should only happen if dupe_count > 4, then you take 
# care of all the duplicates at once.
#---------------------------------------------------------------------------------------------#

#then I would repeat this process for the secxond census's duplicates
DNM5019 <- filter(DNM50, census == "census_2019")
#DNM5019$treeID[duplicated(DNM5019$treeID)]
DNM5019$stemID[duplicated(DNM5019$stemID)]

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
SPKA901$stemID[duplicated(SPKA901$stemID)]

SPKA909 <- filter(SPKA9, census == "09_census_2009")
SPKA909$stemID[duplicated(SPKA909$stemID)]

SPKA914 <- filter(SPKA9, census == "09_census_2014")
SPKA914$stemID[duplicated(SPKA914$stemID)]

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
SPKA1001$stemID[duplicated(SPKA1001$stemID)]

SPKA1009 <- filter(SPKA10, census == "10_census_2009")
SPKA1009$stemID[duplicated(SPKA1009$stemID)]

SPKA1014 <- filter(SPKA10, census == "10_census_2014")
SPKA1014$stemID[duplicated(SPKA1014$stemID)]

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
SPKH401$stemID[duplicated(SPKH401$stemID)]

SPKH408 <- filter(SPKH4, census == "04_census_2008")
SPKH408$stemID[duplicated(SPKH408$stemID)]

SPKH414 <- filter(SPKH4, census == "04_census_2014")
SPKH414$stemID[duplicated(SPKH414$stemID)]

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
SPKH501$stemID[duplicated(SPKH501$stemID)]

SPKH508 <- filter(SPKH5, census == "05_census_2008")
SPKH508$stemID[duplicated(SPKH508$stemID)]

SPKH514 <- filter(SPKH5, census == "05_census_2014")
SPKH514$stemID[duplicated(SPKH514$stemID)]

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
SPKH3001$stemID[duplicated(SPKH3001$stemID)]

SPKH3010 <- filter(SPKH30, census == "30_census_2010")
SPKH3010$stemID[duplicated(SPKH3010$stemID)]

SPKH3015 <- filter(SPKH30, census == "30_census_2015")
SPKH3015$stemID[duplicated(SPKH3015$stemID)]

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
SPKS801$stemID[duplicated(SPKS801$stemID)]

SPKS809 <- filter(SPKS8, census == "08_census_2009")
SPKS809$stemID[duplicated(SPKS809$stemID)]

SPKS814 <- filter(SPKS8, census == "08_census_2014")
SPKS814$stemID[duplicated(SPKS814$stemID)]

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

#removal_ID <- read_csv(here("Desktop", "Research", "R", "Data", "removal_IDS.csv"))
removal_ID <- read_csv("G:/My Drive/Harvard/Emergent_project/Data/removal_IDS.csv")

removal_ID$new_ID <- with(removal_ID, paste0(stemID, plot.x))

tree_IDs_remove <- removal_ID$new_ID

cleandata$new_ID <- with(cleandata, paste0(stemID, plot))

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#withremoval <- cleandata[cleandata$new_ID !=  tree_IDs_remove, ]
withremoval <- cleandata[!cleandata$new_ID %in% tree_IDs_remove, ] 
# EO EDIT: I just gave you the wrong syntax previously. You still want the "!" to exclude any 
# new_ID that equals 'tree_IDs_remove', and using %in% tells the subsetting function to look for 
# matches within 'tree_IDs_remove'
#---------------------------------------------------------------------------------------------#

str(cleandata$new_ID)
str(tree_IDs_remove)
dim(cleandata) # 643638
dim(withremoval) # 604968
dim(cleandata)[[1]] - length(tree_IDs_remove) # 630246
#---------------------------------------------------------------------------------------------#
# I think 'withremoval' is smaller than ('cleandata' - the number of 'tree_IDs_remove') because
# tree_IDs_remove is by plot, not by census. So it's removing the plot-level treeIDs each time
# they occur in a plot (~2-3 times depending on the number of censuses). Does that sound right 
# to you?
#---------------------------------------------------------------------------------------------#

write.csv(cleandata, here("Desktop", "Research", "R", "Data", "data_clean.csv"))