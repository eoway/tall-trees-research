## Secondary cleaning of ForestGEO and ForestPlots inventory data for mortality rate calculation
## 1-11-2020

# select the largest stem (see code from Naomi)
# how many stems? just largest?
 
library(dplyr)
library(tidyverse)
library(here)
library(skimr)
library(stringr)
library(janitor)

#---------------------------------------------------------------------------------------------#
# Load data                                                                                   # 
#---------------------------------------------------------------------------------------------#
#data <- read_csv(here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))
data <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv")

clean_dat <-rename(data, date = JulianDate, status = DFstatus)
#---------------------------------------------------------------------------------------------#

head(clean_dat)
summary(clean_dat)
table(clean_dat$status)
#---------------------------------------------------------------------------------------------#
# Separate by plot
#---------------------------------------------------------------------------------------------#
table(clean_dat$plot)

LHP52  <- filter(clean_dat, plot == "LH_clay" | plot == "LH_fineLoam" | plot == "LH_loam" | plot == "LH_sandstone")
DNM50  <- filter(clean_dat, plot == "DNM50_FGEO")
table(DNM50$status)

#---------------------------------------------------------------------------------------------#
DNM_c1 <- subset(DNM50, census == "census_2011_15")
DNM_c2 <- subset(DNM50, census == "census_2019")
# THESE SHOULDN'T HAVE THE SAME NUMBER OF ALIVE TREES IN EACH CENSUS 
table(DNM_c1$status); table(DNM_c2$status)
#---------------------------------------------------------------------------------------------#

DNM1   <- filter(clean_dat, plot == "DNM1_01")
DNM2   <- filter(clean_dat, plot == "DNM2_02")
DNM3   <- filter(clean_dat, plot == "DNM3_03")
SPKA9  <- filter(clean_dat, plot == "SPKA_09")
SPKA10 <- filter(clean_dat, plot == "SPKA_10")
SPKH4  <- filter(clean_dat, plot == "SPKH_04")
SPKH5  <- filter(clean_dat, plot == "SPKH_05")
SPKH30 <- filter(clean_dat, plot == "SPKH_30")
SPKS8  <- filter(clean_dat, plot == "SPKS_08")
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# CONSOLIDATE STEMS BASED ON TREEID
# REMOVE status == 'prior' OBSERVATIONS (relevant to Lambir only)
#       Prior = a stem that is not in the current census, but will appear in a future census. 
# UPDATE status == "stem_gone" OBSERVATIONS (relevant to Lambir only)
# UPDATE status == "broken below" OBSERVATIONS (relevant to Danum 50)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# LHP52
#---------------------------------------------------------------------------------------------#
table(LHP52$census)
#---------------------------------------------------------------------------------------------#
LHP91_1 <- filter(LHP52, census == "census_1991")
# remove status == 'prior' obs
LHP91 <- filter(LHP91_1, status != "prior")
dim(LHP91_1); dim(LHP91) 
table(LHP91_1$status); table(LHP91$status)

# deal with broken below 
# ifelse(any status %in% any of the same tree ID == "A", status == A, status = D)
LHP91_keep_v1 <- LHP91 %>% group_by(treeID) %>% summarize(X1 = X1,
                                                              plot_x = plot_x,
                                                              plot_y = plot_y,
                                                              hom = mean(hom, na.rm=T),
                                                              quadrat = mean(quadrat, na.rm=T),
                                                              family = family,
                                                              genus = genus, 
                                                              species = species,
                                                              stemID = stemID,
                                                              dbh = max(dbh, na.rm=T),
                                                              Date = mean(Date, na.rm=T),
                                                              date = mean(date, na.rm=T),
                                                              status = case_when(sum(ifelse(status == "A", 1, 0)) > 0 ~ "A", 
                                                                                 TRUE ~ "D"),
                                                              IDlevel = IDlevel,
                                                              site = site, 
                                                              census = census, 
                                                              plot = plot, 
                                                              Shade.Tol = Shade.Tol, 
                                                              soil = soil, 
                                                              stem_BA = stem_BA,
                                                              mean_wd = mean(mean_wd, na.rm=T))

## LHP91_keep_v1 still same dimensions as LHP91, so needs to be reduced to treeIDs only (n = )


LHP91_keep_v1$treeID[duplicated(LHP91_keep_v1$treeID)] 
length(unique(LHP91_keep_v1$stemID))
length(unique(LHP91_keep_v1$treeID))

dim(LHP91_keep_v1)
LHP91_keep_v1 <- LHP91_keep_v1[order(LHP91_keep_v1$treeID, -LHP91_keep_v1$dbh),]
dim(LHP91_keep_v1)

length(LHP91_keep_v1$dbh)
summary(LHP91_keep_v1)
dim(LHP91_keep_v1)

# Keep only the first row for each duplicate of LHP91_keep_v1$treeID
# this row will have the largest value for LHP91_keep_v1$dbh
LHP91_keep <- LHP91_keep_v1[!duplicated(LHP91_keep_v1$treeID),]
### may want to revise this so that: 
### I'm selecting the largest stem AND determining DEAD status based on ALL stems 
length(LHP91_keep_v1$dbh); length(LHP91_keep$dbh)
max(LHP91_keep_v1$dbh, na.rm=T); max(LHP91_keep$dbh, na.rm=T)

# reorder LHP91_keep columns to original order
LHP91_keep <- as.data.frame(select(LHP91_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)) 

#---------------------------------------------------------------------------------------------#
# old code from when based on stemID - keep only trees with ~4 stems
#---------------------------------------------------------------------------------------------#
# # set aside obs to keep (where dupe_count <= 4)
# dat_keep <- filter(LHP91dupes1, dupe_count == 4)
# # set aside obs to update (where dupe_count > 4)
# dat_update <- filter(LHP91dupes1, dupe_count > 4); dim(dat_update)
# # remove duplicates
# dat_1 <- dat_update[ !duplicated(dat_update$treeID), ]
# # combine data that was kept with updated data
# new_dat <- bind_rows(dat_1,dat_keep)
# # recalculate # duplicates
# new_dat2 <- get_dupes(new_dat, treeID)

# summary(new_dat2$dupe_count)
# hist(new_dat2$dupe_count)
# boxplot(new_dat2$dupe_count)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
LHP97_1 <- filter(LHP52, census == "census_1997")
LHP97 <- filter(LHP97_1, status != "prior")
dim(LHP97_1); dim(LHP97) 
table(LHP97_1$status); table(LHP97$status)

# deal with broken below 
# ifelse(any status %in% any of the same tree ID == "A", status == A, status = D)
LHP97_keep_v1 <- LHP97 %>% group_by(treeID) %>% summarize(X1 = X1,
                                                          plot_x = plot_x,
                                                          plot_y = plot_y,
                                                          hom = mean(hom, na.rm=T),
                                                          quadrat = mean(quadrat, na.rm=T),
                                                          family = family,
                                                          genus = genus, 
                                                          species = species,
                                                          stemID = stemID,
                                                          dbh = max(dbh, na.rm=T),
                                                          Date = mean(Date, na.rm=T),
                                                          date = mean(date, na.rm=T),
                                                          status = case_when(sum(ifelse(status == "A", 1, 0)) > 0 ~ "A", 
                                                                             TRUE ~ "D"),
                                                          IDlevel = IDlevel,
                                                          site = site, 
                                                          census = census, 
                                                          plot = plot, 
                                                          Shade.Tol = Shade.Tol, 
                                                          soil = soil, 
                                                          stem_BA = stem_BA,
                                                          mean_wd = mean(mean_wd, na.rm=T))

## LHP97_keep_v1 still same dimensions as LHP97, so needs to be reduced to treeIDs only (n = )

LHP97_keep_v1$treeID[duplicated(LHP97_keep_v1$treeID)] 

length(LHP97_keep_v1$dbh)
LHP97_keep_v1 <- LHP97_keep_v1[order(LHP97_keep_v1$treeID, -LHP97_keep_v1$dbh),]
LHP97_keep <- LHP97_keep_v1[!duplicated(LHP97_keep_v1$treeID),]
length(LHP97_keep_v1$dbh); length(LHP97_keep$dbh)
max(LHP97_keep_v1$dbh, na.rm=T); max(LHP97_keep$dbh, na.rm=T)

# reorder LHP97_keep columns to original order
LHP97_keep <- as.data.frame(select(LHP97_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)) 
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
LHP03_1 <- filter(LHP52, census == "census_2003")
LHP03 <- filter(LHP03_1, status != "prior")
dim(LHP03_1); dim(LHP03) 
table(LHP03_1$status); table(LHP03$status)

# deal with broken below 
# ifelse(any status %in% any of the same tree ID == "A", status == A, status = D)
LHP03_keep_v1 <- LHP03 %>% group_by(treeID) %>% summarize(X1 = X1,
                                                          plot_x = plot_x,
                                                          plot_y = plot_y,
                                                          hom = mean(hom, na.rm=T),
                                                          quadrat = mean(quadrat, na.rm=T),
                                                          family = family,
                                                          genus = genus, 
                                                          species = species,
                                                          stemID = stemID,
                                                          dbh = max(dbh, na.rm=T),
                                                          Date = mean(Date, na.rm=T),
                                                          date = mean(date, na.rm=T),
                                                          status = case_when(sum(ifelse(status == "A", 1, 0)) > 0 ~ "A", 
                                                                             TRUE ~ "D"),
                                                          IDlevel = IDlevel,
                                                          site = site, 
                                                          census = census, 
                                                          plot = plot, 
                                                          Shade.Tol = Shade.Tol, 
                                                          soil = soil, 
                                                          stem_BA = stem_BA,
                                                          mean_wd = mean(mean_wd, na.rm=T))

## LHP03_keep_v1 still same dimensions as LHP03, so needs to be reduced to treeIDs only (n = )

LHP03_keep_v1$treeID[duplicated(LHP03_keep_v1$treeID)] 

LHP03_keep_v1 <- LHP03_keep_v1[order(LHP03_keep_v1$treeID, -LHP03_keep_v1$dbh),]
LHP03_keep <- LHP03_keep_v1[!duplicated(LHP03_keep_v1$treeID),]
length(LHP03_keep_v1$dbh); length(LHP03_keep$dbh)
max(LHP03_keep_v1$dbh, na.rm=T); max(LHP03_keep$dbh, na.rm=T)

# reorder LHP03_keep columns to original order
LHP03_keep <- as.data.frame(select(LHP03_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)) 
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
LHP08_1 <- filter(LHP52, census == "census_2007_08")
LHP08 <- filter(LHP08_1, status != "prior") # not actually relevant for this census
dim(LHP08_1); dim(LHP08) 
table(LHP08_1$status); table(LHP08$status)

# deal with stem_gone 
# ifelse(any status %in% any of the same tree ID == "A", status == A, status = D)
LHP08_keep_v1 <- LHP08 %>% group_by(treeID) %>% summarize(X1 = X1,
                                                          plot_x = plot_x,
                                                          plot_y = plot_y,
                                                          hom = mean(hom, na.rm=T),
                                                          quadrat = mean(quadrat, na.rm=T),
                                                          family = family,
                                                          genus = genus, 
                                                          species = species,
                                                          stemID = stemID,
                                                          dbh = max(dbh, na.rm=T),
                                                          Date = mean(Date, na.rm=T),
                                                          date = mean(date, na.rm=T),
                                                          status = case_when(sum(ifelse(status == "A", 1, 0)) > 0 ~ "A", 
                                                                             TRUE ~ "D"),
                                                          IDlevel = IDlevel,
                                                          site = site, 
                                                          census = census, 
                                                          plot = plot, 
                                                          Shade.Tol = Shade.Tol, 
                                                          soil = soil, 
                                                          stem_BA = stem_BA,
                                                          mean_wd = mean(mean_wd, na.rm=T))

## LHP08_keep_v1 still same dimensions as LHP08, so needs to be reduced to treeIDs only (n = 493357)

LHP08_keep_v1$treeID[duplicated(LHP08_keep_v1$treeID)] 

LHP08_keep_v1 <- LHP08_keep_v1[order(LHP08_keep_v1$treeID, -LHP08_keep_v1$dbh),]
LHP08_keep <- LHP08_keep_v1[!duplicated(LHP08_keep_v1$treeID),]
length(LHP08_keep_v1$dbh); length(LHP08_keep$dbh)
max(LHP08_keep_v1$dbh, na.rm=T); max(LHP08_keep$dbh, na.rm=T)

# dim(LHP08); dim(LHP08_keep); dim(LHP08_keep_v1)
# table(LHP08$status); table(LHP08_keep$status); table(LHP08_keep_v1$status)
# 
# length(unique(LHP08$stemID)); length(unique(LHP08$treeID))
# length(unique(LHP08_keep$stemID)); length(unique(LHP08_keep$treeID))
# length(unique(LHP08_keep_v1$stemID)); length(unique(LHP08_keep_v1$treeID))

# reorder LHP08_keep columns to original order
LHP08_keep <- as.data.frame(select(LHP08_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)) 

#---------------------------------------------------------------------------------------------#
LHP_keep <- rbind(LHP91_keep, LHP97_keep, LHP03_keep, LHP08_keep)
#---------------------------------------------------------------------------------------------#
str(LHP_keep)
#LHP_keep$status <- as.factor(LHP_keep$status)
table(LHP_keep$status)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM50
#---------------------------------------------------------------------------------------------#
table(DNM50$census)
DNM5011 <- filter(DNM50, census == "census_2011_15")

table(DNM5011$status)

# deal with broken below 
# ifelse(any status %in% any of the same tree ID == "A", status == A, status = D)
DNM5011_keep_v1 <- DNM5011 %>% group_by(treeID) %>% summarize(X1 = X1,
                                                          plot_x = plot_x,
                                                          plot_y = plot_y,
                                                          hom = mean(hom, na.rm=T),
                                                          quadrat = mean(quadrat, na.rm=T),
                                                          family = family,
                                                          genus = genus, 
                                                          species = species,
                                                          stemID = stemID,
                                                          dbh = max(dbh, na.rm=T),
                                                          Date = mean(Date, na.rm=T),
                                                          date = mean(date, na.rm=T),
                                                          status = case_when(sum(ifelse(status == "A", 1, 0)) > 0 ~ "A", 
                                                                             TRUE ~ "D"),
                                                          IDlevel = IDlevel,
                                                          site = site, 
                                                          census = census, 
                                                          plot = plot, 
                                                          Shade.Tol = Shade.Tol, 
                                                          soil = soil, 
                                                          stem_BA = stem_BA,
                                                          mean_wd = mean(mean_wd, na.rm=T))

## DNM5011_keep_v1 still same dimensions as DNM5011, so needs to be reduced to treeIDs only (n = 209912)

DNM5011_keep_v1$treeID[duplicated(DNM5011_keep_v1$treeID)] 

DNM5011_keep_v1 <- DNM5011_keep_v1[order(DNM5011_keep_v1$treeID, -DNM5011_keep_v1$dbh),]
DNM5011_keep <- DNM5011_keep_v1[!duplicated(DNM5011_keep_v1$treeID),]
length(DNM5011_keep_v1$dbh); length(DNM5011_keep$dbh)
max(DNM5011_keep_v1$dbh, na.rm=T); max(DNM5011_keep$dbh, na.rm=T)

# dim(DNM5011); dim(DNM5011_keep); dim(DNM5011_keep_v1)
# table(DNM5011$status); table(DNM5011_keep$status); table(DNM5011_keep_v1$status)
# 
# length(unique(DNM5011$stemID)); length(unique(DNM5011$treeID))
# length(unique(DNM5011_keep$stemID)); length(unique(DNM5011_keep$treeID))
# length(unique(DNM5011_keep_v1$stemID)); length(unique(DNM5011_keep_v1$treeID))

# reorder DNM5011_keep columns to original order
DNM5011_keep <- as.data.frame(select(DNM5011_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)) 
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM5019 <- filter(DNM50, census == "census_2019")
table(DNM5019$status)

# deal with broken below 
# ifelse(any status %in% any of the same tree ID == "A", status == A, status = D)
DNM5019_keep_v1 <- DNM5019 %>% group_by(treeID) %>% summarize(X1 = X1,
                                                              plot_x = plot_x,
                                                              plot_y = plot_y,
                                                              hom = mean(hom, na.rm=T),
                                                              quadrat = mean(quadrat, na.rm=T),
                                                              family = family,
                                                              genus = genus, 
                                                              species = species,
                                                              stemID = stemID,
                                                              dbh = max(dbh, na.rm=T),
                                                              Date = mean(Date, na.rm=T),
                                                              date = mean(date, na.rm=T),
                                                              status = case_when(sum(ifelse(status == "A", 1, 0)) > 0 ~ "A", 
                                                                                 TRUE ~ "D"),
                                                              IDlevel = IDlevel,
                                                              site = site, 
                                                              census = census, 
                                                              plot = plot, 
                                                              Shade.Tol = Shade.Tol, 
                                                              soil = soil, 
                                                              stem_BA = stem_BA,
                                                              mean_wd = mean(mean_wd, na.rm=T))

## DNM5019_keep_v1 still same dimensions as DNM5019, so needs to be reduced to treeIDs only (n = 209912)

DNM5019_keep_v1$treeID[duplicated(DNM5019_keep_v1$treeID)] 

DNM5019_keep_v1 <- DNM5019_keep_v1[order(DNM5019_keep_v1$treeID, -DNM5019_keep_v1$dbh),]
DNM5019_keep <- DNM5019_keep_v1[!duplicated(DNM5019_keep_v1$treeID),]
length(DNM5019_keep_v1$dbh); length(DNM5019_keep$dbh)
max(DNM5019_keep_v1$dbh, na.rm=T); max(DNM5019_keep$dbh, na.rm=T)

# dim(DNM5019); dim(DNM5019_keep); dim(DNM5019_keep_v1)
# table(DNM5019$status); table(DNM5019_keep$status); table(DNM5019_keep_v1$status)
# 
# length(unique(DNM5019$stemID)); length(unique(DNM5019$treeID))
# length(unique(DNM5019_keep$stemID)); length(unique(DNM5019_keep$treeID))
# length(unique(DNM5019_keep_v1$stemID)); length(unique(DNM5019_keep_v1$treeID))

# reorder DNM5011_keep columns to original order
DNM5019_keep <- as.data.frame(select(DNM5019_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)) 
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM50_keep <- rbind(DNM5011_keep, DNM5019_keep)
#---------------------------------------------------------------------------------------------#
table(DNM50_keep$status)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Use stemID here instead of treeID, although I left the treeID check in case we do want to 
# filter stems where there are more than N duplicates (e.g. 4 in the example below)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# DNM5011$treeID[duplicated(DNM5011$treeID)]
# #makes a dataset of DNM5011 + a column called dupe_count
# DNM50dupes1 <- get_dupes(DNM5011, treeID)
# length(DNM50dupes1$dbh)
# DNM50dupes1 <- DNM50dupes1[order(DNM50dupes1$treeID, -DNM50dupes1$dbh),] 
# 
# #length(DNM50dupes1$dbh)
# summary(DNM50dupes1)
# 
# dim(DNM50dupes1)
# summary(DNM50dupes1$dupe_count)
# 
# # set aside obs to keep (where dupe_count <= 4)
# dat_keep <- filter(DNM50dupes1, dupe_count <= 4)
# # set aside obs to update (where dupe_count > 4)
# dat_update <- filter(DNM50dupes1, dupe_count > 4); dim(dat_update)
# # remove duplicates
# dat_1 <- dat_update[ !duplicated(dat_update$treeID), ] 
# # combine data that was kept with updated data
# new_dat <- bind_rows(dat_1,dat_keep)
# # recalculate # duplicates
# new_dat2 <- get_dupes(new_dat, treeID)
# 
# summary(new_dat2$dupe_count)
# hist(new_dat2$dupe_count)
# boxplot(new_dat2$dupe_count)
# #---------------------------------------------------------------------------------------------#
# #then repeat this process for the secxond census's duplicates
# DNM5019 <- filter(DNM50, census == "census_2019")
# #DNM5019$treeID[duplicated(DNM5019$treeID)]
# DNM5019$stemID[duplicated(DNM5019$stemID)]
# #DNM50 <- rbind(DNM50dupes1, DNM50dupes2)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM1
#---------------------------------------------------------------------------------------------#
table(DNM1$census)
DNM106 <- filter(DNM1, census == "01_census_2006")
DNM106$treeID[duplicated(DNM106$treeID)] 

DNM106 <- DNM106[order(DNM106$treeID, -DNM106$dbh),]
DNM106_keep <- DNM106[!duplicated(DNM106$treeID),]
length(DNM106$dbh); length(DNM106_keep$dbh)
max(DNM106$dbh, na.rm=T); max(DNM106_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM113 <- filter(DNM1, census == "01_census_2013")
DNM113$treeID[duplicated(DNM113$treeID)]

DNM113 <- DNM113[order(DNM113$treeID, -DNM113$dbh),]
DNM113_keep <- DNM113[!duplicated(DNM113$treeID),]
length(DNM113$dbh); length(DNM113_keep$dbh)
max(DNM113$dbh, na.rm=T); max(DNM113_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM116 <- filter(DNM1, census == "01_census_2016")
DNM116$treeID[duplicated(DNM116$treeID)]

DNM116 <- DNM116[order(DNM116$treeID, -DNM116$dbh),]
DNM116_keep <- DNM116[!duplicated(DNM116$treeID),]
length(DNM116$dbh); length(DNM116_keep$dbh)
max(DNM116$dbh, na.rm=T); max(DNM116_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
DNM1_keep <- rbind(DNM106_keep, DNM113_keep, DNM116_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM2
#---------------------------------------------------------------------------------------------#
table(DNM2$census)

DNM206 <- filter(DNM2, census == "02_census_2006")
DNM206$treeID[duplicated(DNM206$treeID)]

DNM206 <- DNM206[order(DNM206$treeID, -DNM206$dbh),]
DNM206_keep <- DNM206[!duplicated(DNM206$treeID),]
length(DNM206$dbh); length(DNM206_keep$dbh)
max(DNM206$dbh, na.rm=T); max(DNM206_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM213 <- filter(DNM2, census == "02_census_2013")
DNM213$treeID[duplicated(DNM213$treeID)]

DNM213 <- DNM213[order(DNM213$treeID, -DNM213$dbh),]
DNM213_keep <- DNM213[!duplicated(DNM213$treeID),]
length(DNM213$dbh); length(DNM213_keep$dbh)
max(DNM213$dbh, na.rm=T); max(DNM213_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM216 <- filter(DNM2, census == "02_census_2016")
DNM216$treeID[duplicated(DNM216$treeID)]

DNM216 <- DNM216[order(DNM216$treeID, -DNM216$dbh),]
DNM216_keep <- DNM216[!duplicated(DNM216$treeID),]
length(DNM216$dbh); length(DNM216_keep$dbh)
max(DNM216$dbh, na.rm=T); max(DNM216_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
DNM2_keep <- rbind(DNM206_keep, DNM213_keep, DNM216_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM3
#---------------------------------------------------------------------------------------------#
table(DNM3$census)

DNM306 <- filter(DNM3, census == "03_census_2006")
DNM306$treeID[duplicated(DNM106$treeID)]

DNM306 <- DNM306[order(DNM306$treeID, -DNM306$dbh),]
DNM306_keep <- DNM306[!duplicated(DNM306$treeID),]
length(DNM306$dbh); length(DNM306_keep$dbh)
max(DNM306$dbh, na.rm=T); max(DNM306_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM313 <- filter(DNM3, census == "03_census_2013")
DNM313$treeID[duplicated(DNM113$treeID)]

DNM313 <- DNM313[order(DNM313$treeID, -DNM313$dbh),]
DNM313_keep <- DNM313[!duplicated(DNM313$treeID),]
length(DNM313$dbh); length(DNM313_keep$dbh)
max(DNM313$dbh, na.rm=T); max(DNM313_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
DNM316 <- filter(DNM3, census == "03_census_2016")
DNM316$treeID[duplicated(DNM116$treeID)]

DNM316 <- DNM316[order(DNM316$treeID, -DNM316$dbh),]
DNM316_keep <- DNM316[!duplicated(DNM316$treeID),]
length(DNM316$dbh); length(DNM316_keep$dbh)
max(DNM316$dbh, na.rm=T); max(DNM316_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
DNM3_keep <- rbind(DNM306_keep, DNM313_keep, DNM316_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKA9
#---------------------------------------------------------------------------------------------#
table(SPKA9$census)

SPKA901 <- filter(SPKA9, census == "09_census_2001")
SPKA901$treeID[duplicated(SPKA901$treeID)]

SPKA901 <- SPKA901[order(SPKA901$treeID, -SPKA901$dbh),]
SPKA901_keep <- SPKA901[!duplicated(SPKA901$treeID),]
length(SPKA901$dbh); length(SPKA901_keep$dbh)
max(SPKA901$dbh, na.rm=T); max(SPKA901_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

SPKA909 <- filter(SPKA9, census == "09_census_2009")
SPKA909$treeID[duplicated(SPKA909$treeID)]

SPKA909 <- SPKA909[order(SPKA909$treeID, -SPKA909$dbh),]
SPKA909_keep <- SPKA909[!duplicated(SPKA909$treeID),]
length(SPKA909$dbh); length(SPKA909_keep$dbh)
max(SPKA909$dbh, na.rm=T); max(SPKA909_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

SPKA914 <- filter(SPKA9, census == "09_census_2014")
SPKA914$treeID[duplicated(SPKA914$treeID)]

SPKA914 <- SPKA914[order(SPKA914$treeID, -SPKA914$dbh),]
SPKA914_keep <- SPKA914[!duplicated(SPKA914$treeID),]
length(SPKA914$dbh); length(SPKA914_keep$dbh)
max(SPKA914$dbh, na.rm=T); max(SPKA914_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
SPKA9_keep <- rbind(SPKA901_keep, SPKA909_keep, SPKA914_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKA10
#---------------------------------------------------------------------------------------------#
table(SPKA10$census)

SPKA1001 <- filter(SPKA10, census == "10_census_2001")
SPKA1001$treeID[duplicated(SPKA1001$treeID)]

SPKA1001 <- SPKA1001[order(SPKA1001$treeID, -SPKA1001$dbh),]
SPKA1001_keep <- SPKA1001[!duplicated(SPKA1001$treeID),]
length(SPKA1001$dbh); length(SPKA1001_keep$dbh)
max(SPKA1001$dbh, na.rm=T); max(SPKA1001_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKA1009 <- filter(SPKA10, census == "10_census_2009")
SPKA1009$treeID[duplicated(SPKA1009$treeID)]

SPKA1009 <- SPKA1009[order(SPKA1009$treeID, -SPKA1009$dbh),]
SPKA1009_keep <- SPKA1009[!duplicated(SPKA1009$treeID),]
length(SPKA1009$dbh); length(SPKA1009_keep$dbh)
max(SPKA1009$dbh, na.rm=T); max(SPKA1009_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKA1014 <- filter(SPKA10, census == "10_census_2014")
SPKA1014$treeID[duplicated(SPKA1014$treeID)]

SPKA1014 <- SPKA1014[order(SPKA1014$treeID, -SPKA1014$dbh),]
SPKA1014_keep <- SPKA1014[!duplicated(SPKA1014$treeID),]
length(SPKA1014$dbh); length(SPKA1014_keep$dbh)
max(SPKA1014$dbh, na.rm=T); max(SPKA1014_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
SPKA10_keep <- rbind(SPKA1001_keep, SPKA1009_keep, SPKA1014_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKH4
#---------------------------------------------------------------------------------------------#
table(SPKH4$census)

SPKH401 <- filter(SPKH4, census == "04_census_2001")
SPKH401$treeID[duplicated(SPKH401$treeID)]

SPKH401 <- SPKH401[order(SPKH401$treeID, -SPKH401$dbh),]
SPKH401_keep <- SPKH401[!duplicated(SPKH401$treeID),]
length(SPKH401$dbh); length(SPKH401_keep$dbh)
max(SPKH401$dbh, na.rm=T); max(SPKH401_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKH408 <- filter(SPKH4, census == "04_census_2008")
SPKH408$treeID[duplicated(SPKH408$treeID)]

SPKH408 <- SPKH408[order(SPKH408$treeID, -SPKH408$dbh),]
SPKH408_keep <- SPKH408[!duplicated(SPKH408$treeID),]
length(SPKH408$dbh); length(SPKH408_keep$dbh)
max(SPKH408$dbh, na.rm=T); max(SPKH408_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKH414 <- filter(SPKH4, census == "04_census_2014")
SPKH414$treeID[duplicated(SPKH414$treeID)]

SPKH414 <- SPKH414[order(SPKH414$treeID, -SPKH414$dbh),]
SPKH414_keep <- SPKH414[!duplicated(SPKH414$treeID),]
length(SPKH414$dbh); length(SPKH414_keep$dbh)
max(SPKH414$dbh, na.rm=T); max(SPKH414_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
SPKH4_keep <- rbind(SPKH401_keep, SPKH408_keep, SPKH414_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKH5
#---------------------------------------------------------------------------------------------#
table(SPKH5$census)

SPKH501 <- filter(SPKH5, census == "05_census_2001")
SPKH501$treeID[duplicated(SPKH501$treeID)]

SPKH501 <- SPKH501[order(SPKH501$treeID, -SPKH501$dbh),]
SPKH501_keep <- SPKH501[!duplicated(SPKH501$treeID),]
length(SPKH501$dbh); length(SPKH501_keep$dbh)
max(SPKH501$dbh, na.rm=T); max(SPKH501_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKH508 <- filter(SPKH5, census == "05_census_2008")
SPKH508$treeID[duplicated(SPKH508$treeID)]

SPKH508 <- SPKH508[order(SPKH508$treeID, -SPKH508$dbh),]
SPKH508_keep <- SPKH508[!duplicated(SPKH508$treeID),]
length(SPKH508$dbh); length(SPKH508_keep$dbh)
max(SPKH508$dbh, na.rm=T); max(SPKH508_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKH514 <- filter(SPKH5, census == "05_census_2014")
SPKH514$treeID[duplicated(SPKH514$treeID)]

SPKH514 <- SPKH514[order(SPKH514$treeID, -SPKH514$dbh),]
SPKH514_keep <- SPKH514[!duplicated(SPKH514$treeID),]
length(SPKH514$dbh); length(SPKH514_keep$dbh)
max(SPKH514$dbh, na.rm=T); max(SPKH514_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
SPKH5_keep <- rbind(SPKH501_keep, SPKH508_keep, SPKH514_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKH30
#---------------------------------------------------------------------------------------------#
table(SPKH30$census)

SPKH3001 <- filter(SPKH30, census == "30_census_2001")
SPKH3001$treeID[duplicated(SPKH3001$treeID)]

SPKH3001 <- SPKH3001[order(SPKH3001$treeID, -SPKH3001$dbh),]
SPKH3001_keep <- SPKH3001[!duplicated(SPKH3001$treeID),]
length(SPKH3001$dbh); length(SPKH3001_keep$dbh)
max(SPKH3001$dbh, na.rm=T); max(SPKH3001_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKH3010 <- filter(SPKH30, census == "30_census_2010")
SPKH3010$treeID[duplicated(SPKH3010$treeID)]

SPKH3010 <- SPKH3010[order(SPKH3010$treeID, -SPKH3010$dbh),]
SPKH3010_keep <- SPKH3010[!duplicated(SPKH3010$treeID),]
length(SPKH3010$dbh); length(SPKH3010_keep$dbh)
max(SPKH3010$dbh, na.rm=T); max(SPKH3010_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKH3015 <- filter(SPKH30, census == "30_census_2015")
SPKH3015$treeID[duplicated(SPKH3015$treeID)]

SPKH3015 <- SPKH3015[order(SPKH3015$treeID, -SPKH3015$dbh),]
SPKH3015_keep <- SPKH3015[!duplicated(SPKH3015$treeID),]
length(SPKH3015$dbh); length(SPKH3015_keep$dbh)
max(SPKH3015$dbh, na.rm=T); max(SPKH3015_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
SPKH30_keep <- rbind(SPKH3001_keep, SPKH3010_keep, SPKH3015_keep)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# SPKS8
#---------------------------------------------------------------------------------------------#
table(SPKS8$census)

SPKS801 <- filter(SPKS8, census == "08_census_2001")
SPKS801$treeID[duplicated(SPKS801$treeID)]

SPKS801 <- SPKS801[order(SPKS801$treeID, -SPKS801$dbh),]
SPKS801_keep <- SPKS801[!duplicated(SPKS801$treeID),]
length(SPKS801$dbh); length(SPKS801_keep$dbh)
max(SPKS801$dbh, na.rm=T); max(SPKS801_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKS809 <- filter(SPKS8, census == "08_census_2009")
SPKS809$treeID[duplicated(SPKS809$treeID)]

SPKS809 <- SPKS809[order(SPKS809$treeID, -SPKS809$dbh),]
SPKS809_keep <- SPKS809[!duplicated(SPKS809$treeID),]
length(SPKS809$dbh); length(SPKS809_keep$dbh)
max(SPKS809$dbh, na.rm=T); max(SPKS809_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
SPKS814 <- filter(SPKS8, census == "08_census_2014")
SPKS814$treeID[duplicated(SPKS814$treeID)]

SPKS814 <- SPKS814[order(SPKS814$treeID, -SPKS814$dbh),]
SPKS814_keep <- SPKS814[!duplicated(SPKS814$treeID),]
length(SPKS814$dbh); length(SPKS814_keep$dbh)
max(SPKS814$dbh, na.rm=T); max(SPKS814_keep$dbh, na.rm=T)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
SPKS8_keep <- rbind(SPKS801_keep, SPKS809_keep, SPKS814_keep)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# Combine all plots
#---------------------------------------------------------------------------------------------#

#LHP_2 <- select(LHP_keep, X1, plot_x, plot_y, hom, quadrat, family, genus, species, treeID, stemID, dbh, Date, date, status, IDlevel, site, census, plot, Shade.Tol, soil, stem_BA, mean_wd)
#cleandata <- rbind(LHP_2, DNM50, DNM1, DNM2, DNM3, SPKA9, SPKA10, SPKH4, SPKH5, SPKH30, SPKS8)

cleandata <- rbind(LHP_keep, DNM50_keep, DNM1_keep, DNM2_keep, DNM3_keep, SPKA9_keep,
                   SPKA10_keep, SPKH4_keep, SPKH5_keep, SPKH30_keep, SPKS8_keep)
cleandata$dbh <- ifelse(cleandata$dbh == '-Inf',NA, cleandata$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dim(clean_dat) # 2739063 - original
dim(cleandata) # 2246092 - reduced
#dim(cleandata)[[1]] - length(tree_IDs_remove) # 630246
#---------------------------------------------------------------------------------------------#

table(cleandata$status)

#---------------------------------------------------------------------------------------------#
danum_50_ha <- filter(cleandata, site == "DNM50" & census == "census_2019")
table(danum_50_ha$status)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
write.csv(cleandata, "G:/My Drive/Harvard/Plot_Data/clean_inventory_data/mort_dat.csv")
#---------------------------------------------------------------------------------------------#
