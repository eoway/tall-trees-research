library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)

#Dan50------
# dnm1 <- read_tsv(here("Desktop", "Research", "R", "Data", "Dirty", "PlotDataReport02-14-2019_1491847679_census1.txt"))
# dnm2 <- read_tsv(here("Desktop", "Research", "R", "Data", "Dirty", "PlotDataReport04-17-2020_1896600351_census2.txt"))
# tax <- read_tsv(here("Desktop", "Research", "R", "Data", "Dirty", "TaxonomyReport02-14-2019_844633710.txt"))

dnm1 <- read_tsv("G:/My Drive/Harvard/Plot_Data/CTFS_ForestGEO/Data/PlotDataReport02-14-2019_1491847679_census1.txt")
dnm2 <- read_tsv("G:/My Drive/Harvard/Plot_Data/CTFS_ForestGEO/Data/PlotDataReport04-17-2020_1896600351_census2.txt")
tax <- read_tsv("G:/My Drive/Harvard/Plot_Data/CTFS_ForestGEO/Data/TaxonomyReport02-14-2019_844633710.txt")

#Join with taxonomic info-----
dnm1 <- left_join(dnm1, tax, by = "Mnemonic")
dnm2 <- left_join(dnm2, tax, by = "Mnemonic")                

#Determine IDlevel------
dnm1$IDlevel <- ifelse(dnm1$Family == "Unknown","unknown",
                       ifelse(str_detect(dnm1$Species, "species") | dnm1$Species == "sp", "genus", "species"))
dnm2$IDlevel <- ifelse(dnm2$Family == "Unknown","unknown",
                       ifelse(str_detect(dnm2$Species, "species") | dnm2$Species == "sp", "genus", "species"))
#Fix HOMs (?????????)------
#table(dnm1$HOM)
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
# Fix HOM where clearly entered in wrong units (1.3) or with note-taking error (1130)
dnm1$HOM <- ifelse(dnm1$HOM == 1.3 | dnm1$HOM == 1130, 130, dnm1$HOM)
dnm2$HOM <- ifelse(dnm2$HOM == 1.3 | dnm2$HOM == 1130, 130, dnm2$HOM)
#---------------------------------------------------------------------------------------------#
# Still lots of strange HOM values (e.g. 4.5 or 853). I'm leaving those for now. 
#---------------------------------------------------------------------------------------------#

#add site column
dnm1$site <- rep("DNM50",length(dnm1$DBH))
dnm2$site <- rep("DNM50",length(dnm2$DBH))

#add census column
dnm1$census <- rep("census_2011_15",length(dnm1$DBH))
dnm2$census <- rep("census_2019",length(dnm2$DBH))

#add plot column
dnm1$plot <- rep("DNM50_FGEO",length(dnm1$DBH))
dnm2$plot <- rep("DNM50_FGEO",length(dnm2$DBH))

# convert DBH from mm to cm-------
dnm1$dbh <- dnm1$DBH*0.1
dnm2$dbh <- dnm2$DBH*0.1

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#Calculate Julian Date-------
dnm1$JulianDate <- julian.Date(dnm1$Date)
dnm1$Date <- as.character(dnm1$Date)

dnm2$JulianDate <- julian.Date(dnm2$Date)
dnm2$Date <- as.character(dnm2$Date)
#---------------------------------------------------------------------------------------------#
# plot Julian date (as color) by plot X & plot Y locations
# ...these plots take a minute because there are so many observations...
ggplot() + 
  geom_point(data=dnm1, aes(PX, PY, col=JulianDate), pch=21) + 
  scale_colour_gradient() + 
  theme_classic()
ggplot() + 
  geom_point(data=dnm2, aes(PX, PY, col=JulianDate), pch=21) + 
  scale_colour_gradient() + 
  theme_classic()
# from the above plot, we can see that the left side of the plot (as graphed) was censused
# earliest; grey observations/stems don't have dates recorded
# the plot dimensions are 500m x 1000m
# notice that in dnm2,  some of the PX & PY locations are clearly wrong (dnm1 looks good)
# fix them by replacing the values outside the plot dimensions with quadrat-level mean XY values

# first, summarize PX & PY byQquadrat
dnm2_mean_XY <- dnm2 %>% group_by(Quadrat) %>% summarize(PX = mean(PX, na.rm=T),
                                                         PY = mean(PY, na.rm=T))
# first fix errors in plot X coordinates
x_correct <- filter(dnm2, PX <= 1000)
x_error <- filter(dnm2, PX > 1000 | is.na(PX))
# make sure all observations are included in either x_correct or x_error by comparing the 
# dimensions to the original dataframe
dim(dnm2); dim(x_correct)[[1]]+dim(x_error)[[1]] 
# all dataframes should be = 263911 rows
summary(x_error$PX)
# join the error dataframe (excluding the original PX and PY columns) with the 
# coordinates averaged by Quadrat
x_filled <- left_join(select(x_error,-PX), select(dnm2_mean_XY, Quadrat, PX), by = "Quadrat")
dnm2 <- bind_rows(x_filled, x_correct)

# now fix errors in plot Y coordinates
y_correct <- filter(dnm2, PY <= 500)
y_error <- filter(dnm2, PY > 500 | is.na(PY))
dim(dnm2); dim(y_correct)[[1]]+dim(y_error)[[1]]
summary(y_error$PY)
y_filled <- left_join(select(y_error,-PY), select(dnm2_mean_XY, Quadrat, PY), by = "Quadrat")
summary(y_filled$PY)
# sum values still high, set those to 500
y_filled$PY <- ifelse(y_filled$PY > 500, 500, y_filled$PY)
dnm2 <- bind_rows(y_filled, y_correct)
summary(dnm2$PX)
summary(dnm2$PY)

# plot to confirm the stem locations were fixed
ggplot() + 
  geom_point(data=dnm2, aes(PX, PY, col=JulianDate), pch=21) + 
  scale_colour_gradient() + 
  theme_classic()

# or plot so that color indicates Quadrat #
# ggplot() + 
#   geom_point(data=dnm2, aes(PX, PY, col=as.numeric(Quadrat)), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()

# summarize Julian date by quadrat
dnm1_quad_dates <- dnm1 %>% group_by(Quadrat) %>% summarize(JulianDate = mean(JulianDate, na.rm=T))
dnm2_quad_dates <- dnm2 %>% group_by(Quadrat) %>% summarize(JulianDate = mean(JulianDate, na.rm=T))

# subset dnm1 and dnm2 observations where JulianDate == NA
# apply the quadrat-level mean Julian date to stems with JulianDate == NA
dnm1_date_NAs <- filter(dnm1, is.na(JulianDate))
dnm2_date_NAs <- filter(dnm2, is.na(JulianDate))
# make sure the number of rows in the new dataframes are equivalent to the number of JulianDate NAs
dim(dnm1_date_NAs); summary(dnm1$JulianDate)
dim(dnm2_date_NAs); summary(dnm2$JulianDate)

# join the NA dataframe (excluding the original JulianDate column) with the averaged Julian dates
dnm1_date_filled <- left_join(select(dnm1_date_NAs,-JulianDate), dnm1_quad_dates, by = "Quadrat")
dnm2_date_filled <- left_join(select(dnm2_date_NAs,-JulianDate), dnm2_quad_dates, by = "Quadrat")
# notice that 166 stems in dnm2 still don't have JulianDates
# these stems are all in the same quadrat
test <- filter(dnm2_date_filled, is.na(JulianDate))
table(test$Quadrat)
# pull the mean Julian date from the neighboring Quadrat (in the original dnm2 dataframe)
mean_JD_0910 <- mean(filter(dnm2, Quadrat == "0910")$JulianDate, na.rm=T)
# assign JulianDates to this quadrat based on neighboring quadrat
dnm2_date_filled$JulianDate <- ifelse(is.na(dnm2_date_filled$JulianDate), mean_JD_0910, dnm2_date_filled$JulianDate)
summary(dnm2_date_filled)

# subset data where we already have Julian date values
dnm1_w_dates <- filter(dnm1, !is.na(JulianDate))
dnm2_w_dates <- filter(dnm2, !is.na(JulianDate))
# confirm the dataframes with stems that originally had JulianDate info are the correct dimensions
dim(dnm1_w_dates); nrow(dnm1)-nrow(dnm1_date_NAs)
dim(dnm2_w_dates); nrow(dnm2)-nrow(dnm2_date_NAs)

# combine the data back into complete dataframes
dnm1 <- bind_rows(dnm1_date_filled,dnm1_w_dates)
dnm2 <- bind_rows(dnm2_date_filled,dnm2_w_dates)

# can plot to confirm that NA 'patches' are now filled in
# ggplot() + 
#   geom_point(data=dnm2, aes(PX, PY, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
#---------------------------------------------------------------------------------------------#
# I moved the Julian date calculation before combining the DNM50 census 1 and census 2 datasets
# to enable date filling by quadrat for each census.
# The above code fills in dates where Date == NA based on sub-plots within the 50ha plot 
# identified based on PX and PY, with some additional PX & PY cleaning needed
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#TreeID, Species, Genus, Family, DBH, Status
dnm1_select <- dnm1 %>% select(Family,Genus,Species,TreeID,StemID,dbh,Status,IDlevel,Quadrat,site,
                               census,Date,JulianDate,plot)
dnm2_select <- dnm2 %>% select(Family,Genus,Species,TreeID,StemID,dbh,Status,IDlevel,Quadrat,site,
                               census,Date,JulianDate,plot)
#---------------------------------------------------------------------------------------------#
# I included Quadrat in the final dataset (subplot in ForestPlots) and I added JulianDate here
# since it's now calculated above, and I updated the colnmaes below to reflect the above 
# updates.
#---------------------------------------------------------------------------------------------#

DNM50 <- rbind(dnm1_select, dnm2_select)

colnames(DNM50) <- c("family", "genus", "species", "treeID", "stemID", "dbh", "DFstatus", "IDlevel", 
                     "quadrat", "site", "census", "Date", "JulianDate", "plot")

#stem_BA-----
DNM50$stem_BA <- 0.00007854 * DNM50$dbh^2

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
## ----------------- link to Global Wood Density Database ------------------
# https://datadryad.org/resource/doi:10.5061/dryad.234/1
# https://rdrr.io/cran/BIOMASS/man/wdData.html
library(BIOMASS)
data("wdData")

## Assign WD: species mean, genus mean, family mean, plot-level mean
# use IDlevel column == species, genus, family, unknown

# separate DNM50 into 4 dataframes: species, genus, family, unknown
fg_spp <- filter(DNM50, IDlevel == "species"); dim(fg_spp) # n = 431847 
fg_genus <- filter(DNM50, IDlevel == "genus"); dim(fg_genus) # n = 5365
fg_family <- filter(DNM50, IDlevel == "family"); dim(fg_family) # n = 0 
fg_unknown <- filter(DNM50, IDlevel == "unknown" | is.na(IDlevel)); dim(fg_unknown) # n = 83423
dim(DNM50); dim(fg_spp)[[1]]+dim(fg_genus)[[1]]+dim(fg_family)[[1]]+dim(fg_unknown)[[1]]

# summarize wdData by family, genus, species (mean, median, min, max, var, sd)
# also n_wd_obs=n(),min_wd = min(wd, na.rm=T),max_wd = max(wd, na.rm=T),var_wd = var(wd, na.rm=T),sd_wd = sd(wd, na.rm=T)
wd_spp_mean <- wdData %>% group_by(species) %>% dplyr::summarize(mean_wd = median(wd, na.rm=T))
wd_genus_mean <- wdData %>% group_by(genus) %>% dplyr::summarize(mean_wd = median(wd, na.rm=T))
wd_family_mean <- wdData %>% group_by(family) %>% dplyr::summarize(mean_wd = median(wd, na.rm=T))

#-------------------------------------------------------------------------------------------------#
### pull WD for spp, if none sample from genus, if none sample from family, ...
### if none sample from other WD vals in plot
#-------------------------------------------------------------------------------------------------#
# join summarized wdData wd with ForestGEO data by taxonomy columns

# assign WD the SPECIES level
fg_spp <- dplyr::left_join(fg_spp, wd_spp_mean, by = "species"); head(fg_spp) # 156997 NAs
# split into fg_spp and fg_spp_NA
fg_spp_NA <- filter(fg_spp, is.na(mean_wd))
fg_spp <- filter(fg_spp, !is.na(mean_wd))
# then add fg_spp_NA to fg_genus
fg_genus <- bind_rows(fg_genus, select(fg_spp_NA,-mean_wd))

# assign WD the GENUS level
fg_genus <- dplyr::left_join(fg_genus, wd_genus_mean, by = "genus"); head(fg_genus) # 3730 NAs
summary(fg_genus)
# split into fg_genus and fg_genus_NA
fg_genus_NA <- filter(fg_genus, is.na(mean_wd))
fg_genus <- filter(fg_genus, !is.na(mean_wd))
# then add fg_genus_NA to fg_family
fg_family <- bind_rows(fg_family, select(fg_genus_NA,-mean_wd))

# assign WD the FAMILY level
fg_family <- dplyr::left_join(fg_family, wd_family_mean, by = "family"); head(fg_family) # 0 NAs 

# assign WD the PLOT level
fg_unknown$mean_wd <- rep(NA, length(fg_unknown$dbh)) 

# recombine
DNM50 <- bind_rows(fg_spp, fg_genus, fg_family, fg_unknown)

# assign plot-level mean WD to remaining stems with wd == NA
DNM50$mean_wd <- ifelse(is.na(DNM50$mean_wd), mean(DNM50$mean_wd, na.rm=T),
                        DNM50$mean_wd)

ggplot(DNM50, aes(x=mean_wd, fill=census)) +
  geom_histogram(binwidth=.05, alpha=.5, col= "white") + 
  facet_grid(census~.) + 
  theme_classic()
#---------------------------------------------------------------------------------------------#
# I added the code to pull wood density (wd) for each tree by species if possible, then genus,
# then family, then plot-level mean wd. 
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#Other Plots---------------
# load ForestPlots census data, skip 1 line
# dat_sep04 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_04_PlotDump.xlsx"), skip=1)
# dat_sep05 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_05_PlotDump.xlsx"), skip=1)
# dat_sep30 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_30_PlotDump.xlsx"), skip=1)
# 
# dat_sep08 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_08_PlotDump.xlsx"), skip=1)
# dat_sep08 <- filter(dat_sep08, T1 != "-"); dim(dat_sep08); head(dat_sep08$T1) # get rid of NA rows where T1 = '-'
# summary(dat_sep08)
# 
# dat_sep09 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_09_PlotDump.xlsx"), skip=1)
# dat_sep10 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_10_PlotDump.xlsx"), skip=1)
# 
# dat_dnm01 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "DAN_01_PlotDump.xlsx"), skip=1)
# dat_dnm02 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "DAN_02_PlotDump.xlsx"), skip=1)
# dat_dnm03 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "DAN_03_PlotDump.xlsx"), skip=1)

dat_sep04 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/SEP_04_PlotDump.xlsx", sheet=2, skip=1)
dat_sep05 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/SEP_05_PlotDump.xlsx", sheet=2, skip=1)
dat_sep30 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/SEP_30_PlotDump.xlsx", sheet=2, skip=1)

dat_sep08 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/SEP_08_PlotDump.xlsx", sheet=2, skip=1)
dat_sep08 <- filter(dat_sep08, T1 != "-"); dim(dat_sep08); head(dat_sep08$T1) # get rid of NA rows where T1 = '-'
summary(dat_sep08)

dat_sep09 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/SEP_09_PlotDump.xlsx", sheet=2, skip=1)
dat_sep10 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/SEP_10_PlotDump.xlsx", sheet=2, skip=1)

dat_dnm01 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/DAN_01_PlotDump.xlsx", sheet=2, skip=1)
dat_dnm02 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/DAN_02_PlotDump.xlsx", sheet=2, skip=1)
dat_dnm03 <- read_excel("G:/My Drive/Harvard/Plot_Data/Forest_Plots/Data/DAN_03_PlotDump.xlsx", sheet=2, skip=1)
#---------------------------------------------------------------------------------------------#
# I added ', sheet=2,' when reading each file since the data is on the second excel sheet
# The code resulted in an error without this
#---------------------------------------------------------------------------------------------#

# separate first columns that apply to each census
dat_all04 <- select(dat_sep04, 'Tree ID':'WD Type')
# create separate genus & species columns
dat_all04$genus <- word(dat_all04$Species); dat_all04$species <- word(dat_all04$Species, 2)
# select columns of interest and rename
dat_all04_select <- select(dat_all04,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all04_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all04_select$quadrat <- as.factor(paste0(dat_all04_select$subplot_ID,'_sep04'))
#---------------------------------------------------------------------------------------------#
# I updated the syntax when creating subplot_ID so that "true" is no longer included and 
# I am now renaming the updated subplot_ID column to "Quadrat" for consistency with the 
# DNM50 dataset 
# I did the same for all EO EDIT locations below
#---------------------------------------------------------------------------------------------#

dat_all05 <- select(dat_sep05, 'Tree ID':'WD Type')
dat_all05$genus <- word(dat_all05$Species); dat_all05$species <- word(dat_all05$Species, 2)
dat_all05_select <- select(dat_all05,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all05_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all05_select$quadrat <- as.factor(paste0(dat_all05_select$subplot_ID,'_sep05'))
#---------------------------------------------------------------------------------------------#

dat_all30 <- select(dat_sep30, 'Tree ID':'WD Type')
dat_all30$genus <- word(dat_all30$Species); dat_all30$species <- word(dat_all30$Species, 2)
dat_all30_select <- select(dat_all30,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all30_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all30_select$quadrat <- as.factor(paste0(dat_all30_select$subplot_ID,'_sep30'))
#---------------------------------------------------------------------------------------------#

dat_all08 <- select(dat_sep08, 'Tree ID':'WD Type')
dat_all08$genus <- word(dat_all08$Species); dat_all08$species <- word(dat_all08$Species, 2)
dat_all08_select <- select(dat_all08,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all08_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all08_select$quadrat <- as.factor(paste0(dat_all08_select$subplot_ID,'_sep08'))
#---------------------------------------------------------------------------------------------#

dat_all09 <- select(dat_sep09, 'Tree ID':'WD Type')
dat_all09$genus <- word(dat_all09$Species); dat_all09$species <- word(dat_all09$Species, 2)
dat_all09_select <- select(dat_all09,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all09_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all09_select$quadrat <- as.factor(paste0(dat_all09_select$subplot_ID,'_sep09'))
#---------------------------------------------------------------------------------------------#

dat_all10 <- select(dat_sep10, 'Tree ID':'WD Type')
dat_all10$genus <- word(dat_all10$Species); dat_all10$species <- word(dat_all10$Species, 2)
dat_all10_select <- select(dat_all10,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all10_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all10_select$quadrat <- as.factor(paste0(dat_all10_select$subplot_ID,'_sep10'))
#---------------------------------------------------------------------------------------------#

dat_all01 <- select(dat_dnm01, 'Tree ID':'WD Type')
dat_all01$genus <- word(dat_all01$Species); dat_all01$species <- word(dat_all01$Species, 2)
dat_all01_select <- select(dat_all01,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all01_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all01_select$quadrat <- as.factor(paste0(dat_all01_select$subplot_ID,'dnm01'))
#---------------------------------------------------------------------------------------------#

dat_all02 <- select(dat_dnm02, 'Tree ID':'WD Type')
dat_all02$genus <- word(dat_all02$Species); dat_all02$species <- word(dat_all02$Species, 2)
dat_all02_select <- select(dat_all02,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all02_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all02_select$quadrat <- as.factor(paste0(dat_all02_select$subplot_ID,'dnm02'))
#---------------------------------------------------------------------------------------------#

dat_all03 <- select(dat_dnm03, 'Tree ID':'WD Type')
dat_all03$genus <- word(dat_all03$Species); dat_all03$species <- word(dat_all03$Species, 2)
dat_all03_select <- select(dat_all03,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
colnames(dat_all03_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all03_select$quadrat <- as.factor(paste0(dat_all03_select$subplot_ID,'dnm03'))
#---------------------------------------------------------------------------------------------#

# select DBH1 and F1 from each census
sep_04_2001 <- select(dat_sep04, 'DBH1...16','F1...23'); colnames(sep_04_2001) <- c("DBH_mm","DFstatus")
sep_04_2008 <- select(dat_sep04, 'DBH1...36','F1...43'); colnames(sep_04_2008) <- c("DBH_mm","DFstatus")
sep_04_2014 <- select(dat_sep04, 'DBH1...56','F1...63'); colnames(sep_04_2014) <- c("DBH_mm","DFstatus")
sep_05_2001 <- select(dat_sep05, 'DBH1...16','F1...23'); colnames(sep_05_2001) <- c("DBH_mm","DFstatus")
sep_05_2008 <- select(dat_sep05, 'DBH1...36','F1...43'); colnames(sep_05_2008) <- c("DBH_mm","DFstatus")
sep_05_2014 <- select(dat_sep05, 'DBH1...56','F1...63'); colnames(sep_05_2014) <- c("DBH_mm","DFstatus")
sep_30_2001 <- select(dat_sep30, 'DBH1...16','F1...23'); colnames(sep_30_2001) <- c("DBH_mm","DFstatus")
sep_30_2010 <- select(dat_sep30, 'DBH1...36','F1...43'); colnames(sep_30_2010) <- c("DBH_mm","DFstatus")
sep_30_2015 <- select(dat_sep30, 'DBH1...56','F1...63'); colnames(sep_30_2015) <- c("DBH_mm","DFstatus")

sep_08_2001 <- select(dat_sep08, 'DBH1...16','F1...23'); colnames(sep_08_2001) <- c("DBH_mm","DFstatus")
sep_08_2009 <- select(dat_sep08, 'DBH1...36','F1...43'); colnames(sep_08_2009) <- c("DBH_mm","DFstatus")
sep_08_2014 <- select(dat_sep08, 'DBH1...56','F1...63'); colnames(sep_08_2014) <- c("DBH_mm","DFstatus")

sep_09_2000 <- select(dat_sep09, 'DBH1...16','F1...23'); colnames(sep_09_2000) <- c("DBH_mm","DFstatus")
sep_09_2009 <- select(dat_sep09, 'DBH1...36','F1...43'); colnames(sep_09_2009) <- c("DBH_mm","DFstatus")
sep_09_2014 <- select(dat_sep09, 'DBH1...56','F1...63'); colnames(sep_09_2014) <- c("DBH_mm","DFstatus")
sep_10_2000 <- select(dat_sep10, 'DBH1...16','F1...23'); colnames(sep_10_2000) <- c("DBH_mm","DFstatus")
sep_10_2009 <- select(dat_sep10, 'DBH1...36','F1...43'); colnames(sep_10_2009) <- c("DBH_mm","DFstatus")
sep_10_2014 <- select(dat_sep10, 'DBH1...56','F1...63'); colnames(sep_10_2014) <- c("DBH_mm","DFstatus")

dnm_01_2006 <- select(dat_dnm01, 'DBH1...16','F1...23'); colnames(dnm_01_2006) <- c("DBH_mm","DFstatus")
dnm_01_2013 <- select(dat_dnm01, 'DBH1...36','F1...43'); colnames(dnm_01_2013) <- c("DBH_mm","DFstatus")
dnm_01_2016 <- select(dat_dnm01, 'DBH1...56','F1...63'); colnames(dnm_01_2016) <- c("DBH_mm","DFstatus")
dnm_02_2006 <- select(dat_dnm02, 'DBH1...16','F1...23'); colnames(dnm_02_2006) <- c("DBH_mm","DFstatus")
dnm_02_2013 <- select(dat_dnm02, 'DBH1...36','F1...43'); colnames(dnm_02_2013) <- c("DBH_mm","DFstatus")
dnm_02_2016 <- select(dat_dnm02, 'DBH1...56','F1...63'); colnames(dnm_02_2016) <- c("DBH_mm","DFstatus")
dnm_03_2006 <- select(dat_dnm03, 'DBH1...16','F1...23'); colnames(dnm_03_2006) <- c("DBH_mm","DFstatus")
dnm_03_2013 <- select(dat_dnm03, 'DBH1...36','F1...43'); colnames(dnm_03_2013) <- c("DBH_mm","DFstatus")
dnm_03_2016 <- select(dat_dnm03, 'DBH1...56','F1...63'); colnames(dnm_03_2016) <- c("DBH_mm","DFstatus")

# add census columns
sep_04_2001$census <- rep("04_census_2001",length(sep_04_2001$DBH_mm))
sep_04_2001$plot <- rep("SPKH_04",length(sep_04_2001$DBH_mm))
sep_04_2008$census <- rep("04_census_2008",length(sep_04_2008$DBH_mm))
sep_04_2008$plot <- rep("SPKH_04",length(sep_04_2008$DBH_mm))
sep_04_2014$census <- rep("04_census_2014",length(sep_04_2014$DBH_mm))
sep_04_2014$plot <- rep("SPKH_04",length(sep_04_2014$DBH_mm))
sep_05_2001$census <- rep("05_census_2001",length(sep_05_2001$DBH_mm))
sep_05_2001$plot <- rep("SPKH_05",length(sep_05_2001$DBH_mm))
sep_05_2008$census <- rep("05_census_2008",length(sep_05_2008$DBH_mm))
sep_05_2008$plot <- rep("SPKH_05",length(sep_05_2008$DBH_mm))
sep_05_2014$census <- rep("05_census_2014",length(sep_05_2014$DBH_mm))
sep_05_2014$plot <- rep("SPKH_05",length(sep_05_2014$DBH_mm))
sep_30_2001$census <- rep("30_census_2001",length(sep_30_2001$DBH_mm))
sep_30_2001$plot <- rep("SPKH_30",length(sep_30_2001$DBH_mm))
sep_30_2010$census <- rep("30_census_2010",length(sep_30_2010$DBH_mm))
sep_30_2010$plot <- rep("SPKH_30",length(sep_30_2010$DBH_mm))
sep_30_2015$census <- rep("30_census_2015",length(sep_30_2015$DBH_mm))
sep_30_2015$plot <- rep("SPKH_30",length(sep_30_2015$DBH_mm))

sep_08_2001$census <- rep("08_census_2001",length(sep_08_2001$DBH_mm))
sep_08_2001$plot <- rep("SPKS_08",length(sep_08_2001$DBH_mm))
sep_08_2009$census <- rep("08_census_2009",length(sep_08_2009$DBH_mm))
sep_08_2009$plot <- rep("SPKS_08",length(sep_08_2009$DBH_mm))
sep_08_2014$census <- rep("08_census_2014",length(sep_08_2014$DBH_mm))
sep_08_2014$plot <- rep("SPKS_08",length(sep_08_2014$DBH_mm))

sep_09_2000$census <- rep("09_census_2001",length(sep_09_2000$DBH_mm))
sep_09_2000$plot <- rep("SPKA_09",length(sep_09_2000$DBH_mm))
sep_09_2009$census <- rep("09_census_2009",length(sep_09_2009$DBH_mm))
sep_09_2009$plot <- rep("SPKA_09",length(sep_09_2009$DBH_mm))
sep_09_2014$census <- rep("09_census_2014",length(sep_09_2014$DBH_mm))
sep_09_2014$plot <- rep("SPKA_09",length(sep_09_2014$DBH_mm))
sep_10_2000$census <- rep("10_census_2001",length(sep_10_2000$DBH_mm))
sep_10_2000$plot <- rep("SPKA_10",length(sep_10_2000$DBH_mm))
sep_10_2009$census <- rep("10_census_2009",length(sep_10_2009$DBH_mm))
sep_10_2009$plot <- rep("SPKA_10",length(sep_10_2009$DBH_mm))
sep_10_2014$census <- rep("10_census_2014",length(sep_10_2014$DBH_mm))
sep_10_2014$plot <- rep("SPKA_10",length(sep_10_2014$DBH_mm))

dnm_01_2006$census <- rep("01_census_2006",length(dnm_01_2006$DBH_mm))
dnm_01_2006$plot <- rep("DNM1_01",length(dnm_01_2006$DBH_mm))
dnm_01_2013$census <- rep("01_census_2013",length(dnm_01_2013$DBH_mm))
dnm_01_2013$plot <- rep("DNM1_01",length(dnm_01_2013$DBH_mm))
dnm_01_2016$census <- rep("01_census_2016",length(dnm_01_2016$DBH_mm))
dnm_01_2016$plot <- rep("DNM1_01",length(dnm_01_2016$DBH_mm))
dnm_02_2006$census <- rep("02_census_2006",length(dnm_02_2006$DBH_mm))
dnm_02_2006$plot <- rep("DNM2_02",length(dnm_02_2006$DBH_mm))
dnm_02_2013$census <- rep("02_census_2013",length(dnm_02_2013$DBH_mm))
dnm_02_2013$plot <- rep("DNM2_02",length(dnm_02_2013$DBH_mm))
dnm_02_2016$census <- rep("02_census_2016",length(dnm_02_2016$DBH_mm))
dnm_02_2016$plot <- rep("DNM2_02",length(dnm_02_2016$DBH_mm))
dnm_03_2006$census <- rep("03_census_2006",length(dnm_03_2006$DBH_mm))
dnm_03_2006$plot <- rep("DNM3_03",length(dnm_03_2006$DBH_mm))
dnm_03_2013$census <- rep("03_census_2013",length(dnm_03_2013$DBH_mm))
dnm_03_2013$plot <- rep("DNM3_03",length(dnm_03_2013$DBH_mm))
dnm_03_2016$census <- rep("03_census_2016",length(dnm_03_2016$DBH_mm))
dnm_03_2016$plot <- rep("DNM3_03",length(dnm_03_2016$DBH_mm))

# bind_cols to dat_all_select
sep_04_2001 <- bind_cols(dat_all04_select,sep_04_2001)
sep_04_2008 <- bind_cols(dat_all04_select,sep_04_2008)
sep_04_2014 <- bind_cols(dat_all04_select,sep_04_2014)
sep_05_2001 <- bind_cols(dat_all05_select,sep_05_2001)
sep_05_2008 <- bind_cols(dat_all05_select,sep_05_2008)
sep_05_2014 <- bind_cols(dat_all05_select,sep_05_2014)
sep_30_2001 <- bind_cols(dat_all30_select,sep_30_2001)
sep_30_2010 <- bind_cols(dat_all30_select,sep_30_2010)
sep_30_2015 <- bind_cols(dat_all30_select,sep_30_2015)

sep_08_2001 <- bind_cols(dat_all08_select,sep_08_2001)
sep_08_2009 <- bind_cols(dat_all08_select,sep_08_2009)
sep_08_2014 <- bind_cols(dat_all08_select,sep_08_2014)

sep_09_2000 <- bind_cols(dat_all09_select,sep_09_2000)
sep_09_2009 <- bind_cols(dat_all09_select,sep_09_2009)
sep_09_2014 <- bind_cols(dat_all09_select,sep_09_2014)
sep_10_2000 <- bind_cols(dat_all10_select,sep_10_2000)
sep_10_2009 <- bind_cols(dat_all10_select,sep_10_2009)
sep_10_2014 <- bind_cols(dat_all10_select,sep_10_2014)

dnm_01_2006 <- bind_cols(dat_all01_select,dnm_01_2006)
dnm_01_2013 <- bind_cols(dat_all01_select,dnm_01_2013)
dnm_01_2016 <- bind_cols(dat_all01_select,dnm_01_2016)
dnm_02_2006 <- bind_cols(dat_all02_select,dnm_02_2006)
dnm_02_2013 <- bind_cols(dat_all02_select,dnm_02_2013)
dnm_02_2016 <- bind_cols(dat_all02_select,dnm_02_2016)
dnm_03_2006 <- bind_cols(dat_all03_select,dnm_03_2006)
dnm_03_2013 <- bind_cols(dat_all03_select,dnm_03_2013)
dnm_03_2016 <- bind_cols(dat_all03_select,dnm_03_2016)

# bind_rows of each census & add site column
SEP_H <- bind_rows(sep_04_2001,sep_04_2008,sep_04_2014,sep_05_2001,sep_05_2008,sep_05_2014,sep_30_2001,sep_30_2010,sep_30_2015) 
SEP_H$site <- rep("SPKH",length(SEP_H$DBH_mm))
SEP_H$dbh <- SEP_H$DBH_mm * .1
SEP_H$stemID <- SEP_H$treeID
SEP_H <- select(SEP_H, family:stemID) # EO EDIT - i deleted the '' around each column name here and below
SEP_H <- select(SEP_H, -DBH_mm)

SEP_S <- bind_rows(sep_08_2001,sep_08_2009,sep_08_2014) 
SEP_S$site <- rep("SPKS",length(SEP_S$DBH_mm))
SEP_S$dbh <- SEP_S$DBH_mm * .1
SEP_S$stemID <- SEP_S$treeID
SEP_S <- select(SEP_S, family:stemID)
SEP_S <- select(SEP_S, -DBH_mm)

SEP_A <- bind_rows(sep_09_2000,sep_09_2009,sep_09_2014,sep_10_2000,sep_10_2009,sep_10_2014) 
SEP_A$site <- rep("SPKA",length(SEP_A$DBH_mm))
SEP_A$dbh <- SEP_A$DBH_mm * .1
SEP_A$stemID <- SEP_A$treeID
SEP_A <- select(SEP_A, family:stemID)
SEP_A <- select(SEP_A, -DBH_mm)

DNM1 <- bind_rows(dnm_01_2006,dnm_01_2013,dnm_01_2016) 
DNM1$site <- rep("DNM1",length(DNM1$DBH_mm))
DNM1$dbh <- DNM1$DBH_mm * .1
DNM1$stemID <- DNM1$treeID
DNM1 <- select(DNM1, family:stemID)
DNM1 <- select(DNM1, -DBH_mm)

DNM2 <- bind_rows(dnm_02_2006,dnm_02_2013,dnm_02_2016) 
DNM2$site <- rep("DNM2",length(DNM2$DBH_mm))
DNM2$dbh <- DNM2$DBH_mm * .1
DNM2$stemID <- DNM2$treeID
DNM2 <- select(DNM2, family:stemID)
DNM2 <- select(DNM2, -DBH_mm)

DNM3 <- bind_rows(dnm_03_2006,dnm_03_2013,dnm_03_2016) 
DNM3$site <- rep("DNM3",length(DNM3$DBH_mm))
DNM3$dbh <- DNM3$DBH_mm * .1
DNM3$stemID <- DNM3$treeID
DNM3 <- select(DNM3, family:stemID)
DNM3 <- select(DNM3, -DBH_mm)

#Combine-----
forest_plots_clean <- bind_rows(SEP_H,SEP_S,SEP_A,DNM1,DNM2,DNM3)

#stem_BA----
forest_plots_clean$stem_BA <- 0.00007854 * forest_plots_clean$dbh^2

#Julian Date----
census <- read_csv("G:/My Drive/Harvard/Emergent_project/Data/Census_Dates.csv")
julians <- census %>% select(census, Mid_date, JulianDate)

forest_plots_clean <- left_join(forest_plots_clean, julians, by = "census")

forest_plots_clean <- rename(forest_plots_clean, Date = Mid_date)

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
# first make sure forest_plots_clean & DNM50 have the same columns in the same order
colnames(forest_plots_clean)
colnames(DNM50)
# reorder forest_plots_clean columns to match DNM50
forest_plots_final <- select(forest_plots_clean, family, genus, species, treeID, stemID, dbh, DFstatus,
                             IDlevel, quadrat, site, census, Date, JulianDate, plot, stem_BA, mean_wd)

firstcleandata <- bind_rows(forest_plots_final, DNM50)
#---------------------------------------------------------------------------------------------#

summary(firstcleandata)
ggplot(forest_plots_clean, aes(x=mean_wd)) + 
  geom_histogram(binwidth=.05, alpha=.5, col= "white") +
  facet_wrap(~site, ncol=2, scales = "free") + 
  theme_classic()

#Clean family, genus, species data---------
table(firstcleandata$family)
firstcleandata$family <- gsub("Unknown", "Indet", firstcleandata$family)

table(firstcleandata$genus)
length(firstcleandata$genus)
firstcleandata$genus <- gsub("Unidentified", "Indet", firstcleandata$genus)
length(firstcleandata$genus)

#---------------------------------------------------------------------------------------------#
# EO EDIT
#---------------------------------------------------------------------------------------------#
#Should I make "Species #"s into Indets?????????
table(firstcleandata$species)
length(firstcleandata$species)
firstcleandata$species <- gsub("Unknown.*", "Indet", firstcleandata$species)
firstcleandata$species <- gsub("indet.*", "Indet", firstcleandata$species)
firstcleandata$species <- gsub("unknown.*", "Indet", firstcleandata$species)
length(firstcleandata$species)
#---------------------------------------------------------------------------------------------#
# This looks great the way you've done it. 
#---------------------------------------------------------------------------------------------#

length(firstcleandata$species)

table(firstcleandata$census)
table(firstcleandata$site)


# write.csv(firstcleandata, here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))
write.csv(firstcleandata, "G:/My Drive/Harvard/Emergent_project/Data/data_first_clean.csv")

