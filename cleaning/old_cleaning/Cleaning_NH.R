library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)

#Dan50------
dnm1 <- read_tsv(here("Desktop", "Research", "R", "Data", "Dirty", "PlotDataReport02-14-2019_1491847679_census1.txt"))
dnm2 <- read_tsv(here("Desktop", "Research", "R", "Data", "Dirty", "PlotDataReport04-17-2020_1896600351_census2.txt"))
tax <- read_tsv(here("Desktop", "Research", "R", "Data", "Dirty", "TaxonomyReport02-14-2019_844633710.txt"))

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

#TreeID, Species, Genus, Family, DBH, Status
dnm1_select <- dnm1 %>% select(Family,Genus,Species,TreeID,StemID,dbh,Status,IDlevel,site,census,Date,plot)
dnm2_select <- dnm2 %>% select(Family,Genus,Species,TreeID,StemID,dbh,Status,IDlevel,site,census,Date,plot)

DNM50 <- rbind(dnm1_select, dnm2_select)

colnames(DNM50) <- c("family", "genus", "species", "treeID", "stemID", "dbh", "DFstatus", "IDlevel", "site", "census", "Date", "plot")

#Calculate Julian Date-------
DNM50$JulianDate <- julian.Date(DNM50$Date)

DNM50$Date <- as.character(DNM50$Date)

#stem_BA-----
DNM50$stem_BA <- 0.00007854 * DNM50$dbh^2

#Other Plots---------------
# load ForestPlots census data, skip 1 line
dat_sep04 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_04_PlotDump.xlsx"), skip=1)
dat_sep05 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_05_PlotDump.xlsx"), skip=1)
dat_sep30 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_30_PlotDump.xlsx"), skip=1)

dat_sep08 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_08_PlotDump.xlsx"), skip=1)
dat_sep08 <- filter(dat_sep08, T1 != "-"); dim(dat_sep08); head(dat_sep08$T1) # get rid of NA rows where T1 = '-'
summary(dat_sep08)

dat_sep09 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_09_PlotDump.xlsx"), skip=1)
dat_sep10 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "SEP_10_PlotDump.xlsx"), skip=1)

dat_dnm01 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "DAN_01_PlotDump.xlsx"), skip=1)
dat_dnm02 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "DAN_02_PlotDump.xlsx"), skip=1)
dat_dnm03 <- read_excel(here("Desktop", "Research", "R", "Data", "Dirty", "DAN_03_PlotDump.xlsx"), skip=1)

# separate first columns that apply to each census
dat_all04 <- select(dat_sep04, 'Tree ID':'WD Type')
# create separate genus & species columns
dat_all04$genus <- word(dat_all04$Species); dat_all04$species <- word(dat_all04$Species, 2)
# select columns of interest and rename
dat_all04_select <- select(dat_all04,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all04_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all04_select$subplot_ID <- as.factor(paste(dat_all04_select$subplot_ID,'sep04',parse=T))

dat_all05 <- select(dat_sep05, 'Tree ID':'WD Type')
dat_all05$genus <- word(dat_all05$Species); dat_all05$species <- word(dat_all05$Species, 2)
dat_all05_select <- select(dat_all05,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all05_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all05_select$subplot_ID <- as.factor(paste(dat_all05_select$subplot_ID,'sep05',parse=T))

dat_all30 <- select(dat_sep30, 'Tree ID':'WD Type')
dat_all30$genus <- word(dat_all30$Species); dat_all30$species <- word(dat_all30$Species, 2)
dat_all30_select <- select(dat_all30,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all30_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all30_select$subplot_ID <- as.factor(paste(dat_all30_select$subplot_ID,'sep30',parse=T))

dat_all08 <- select(dat_sep08, 'Tree ID':'WD Type')
dat_all08$genus <- word(dat_all08$Species); dat_all08$species <- word(dat_all08$Species, 2)
dat_all08_select <- select(dat_all08,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all08_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all08_select$subplot_ID <- as.factor(paste(dat_all08_select$subplot_ID,'sep08',parse=T))

dat_all09 <- select(dat_sep09, 'Tree ID':'WD Type')
dat_all09$genus <- word(dat_all09$Species); dat_all09$species <- word(dat_all09$Species, 2)
dat_all09_select <- select(dat_all09,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all09_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all09_select$subplot_ID <- as.factor(paste(dat_all09_select$subplot_ID,'sep09',parse=T))

dat_all10 <- select(dat_sep10, 'Tree ID':'WD Type')
dat_all10$genus <- word(dat_all10$Species); dat_all10$species <- word(dat_all10$Species, 2)
dat_all10_select <- select(dat_all10,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all10_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all10_select$subplot_ID <- as.factor(paste(dat_all10_select$subplot_ID,'sep10',parse=T))

dat_all01 <- select(dat_dnm01, 'Tree ID':'WD Type')
dat_all01$genus <- word(dat_all01$Species); dat_all01$species <- word(dat_all01$Species, 2)
dat_all01_select <- select(dat_all01,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all01_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all01_select$subplot_ID <- as.factor(paste(dat_all01_select$subplot_ID,'dnm01',parse=T))

dat_all02 <- select(dat_dnm02, 'Tree ID':'WD Type')
dat_all02$genus <- word(dat_all02$Species); dat_all02$species <- word(dat_all02$Species, 2)
dat_all02_select <- select(dat_all02,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all02_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all02_select$subplot_ID <- as.factor(paste(dat_all02_select$subplot_ID,'dnm02',parse=T))

dat_all03 <- select(dat_dnm03, 'Tree ID':'WD Type')
dat_all03$genus <- word(dat_all03$Species); dat_all03$species <- word(dat_all03$Species, 2)
dat_all03_select <- select(dat_all03,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD')
colnames(dat_all03_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd")
dat_all03_select$subplot_ID <- as.factor(paste(dat_all03_select$subplot_ID,'dnm03',parse=T))

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
SEP_H <- select(SEP_H, 'family':'stemID')
SEP_H <- select(SEP_H, -DBH_mm)

SEP_S <- bind_rows(sep_08_2001,sep_08_2009,sep_08_2014) 
SEP_S$site <- rep("SPKS",length(SEP_S$DBH_mm))
SEP_S$dbh <- SEP_S$DBH_mm * .1
SEP_S$stemID <- SEP_S$treeID
SEP_S <- select(SEP_S, 'family':'stemID')
SEP_S <- select(SEP_S, -DBH_mm)


SEP_A <- bind_rows(sep_09_2000,sep_09_2009,sep_09_2014,sep_10_2000,sep_10_2009,sep_10_2014) 
SEP_A$site <- rep("SPKA",length(SEP_A$DBH_mm))
SEP_A$dbh <- SEP_A$DBH_mm * .1
SEP_A$stemID <- SEP_A$treeID
SEP_A <- select(SEP_A, 'family':'stemID')
SEP_A <- select(SEP_A, -DBH_mm)

DNM1 <- bind_rows(dnm_01_2006,dnm_01_2013,dnm_01_2016) 
DNM1$site <- rep("DNM1",length(DNM1$DBH_mm))
DNM1$dbh <- DNM1$DBH_mm * .1
DNM1$stemID <- DNM1$treeID
DNM1 <- select(DNM1, 'family':'stemID')
DNM1 <- select(DNM1, -DBH_mm)

DNM2 <- bind_rows(dnm_02_2006,dnm_02_2013,dnm_02_2016) 
DNM2$site <- rep("DNM2",length(DNM2$DBH_mm))
DNM2$dbh <- DNM2$DBH_mm * .1
DNM2$stemID <- DNM2$treeID
DNM2 <- select(DNM2, 'family':'stemID')
DNM2 <- select(DNM2, -DBH_mm)

DNM3 <- bind_rows(dnm_03_2006,dnm_03_2013,dnm_03_2016) 
DNM3$site <- rep("DNM3",length(DNM3$DBH_mm))
DNM3$dbh <- DNM3$DBH_mm * .1
DNM3$stemID <- DNM3$treeID
DNM3 <- select(DNM3, 'family':'stemID')
DNM3 <- select(DNM3, -DBH_mm)

#Combine-----
forest_plots_clean <- bind_rows(SEP_H,SEP_S,SEP_A,DNM1,DNM2,DNM3)

#stem_BA----
forest_plots_clean$stem_BA <- 0.00007854 * forest_plots_clean$dbh^2

#Julian Date----
census <- read_csv(here("Desktop", "Research", "R", "Data", "Census_Dates.csv"))
julians <- census %>% select(census, Mid_date, JulianDate)

forest_plots_clean <- left_join(forest_plots_clean, julians, by = "census")

forest_plots_clean <- rename(forest_plots_clean, Date = Mid_date)

firstcleandata <- bind_rows(forest_plots_clean, DNM50)

summary(data)
ggplot(forest_plots_clean, aes(x=mean_wd)) + 
  geom_histogram(binwidth=.05, alpha=.5, col= "white") +
  facet_wrap(~site, ncol=2, scales = "free")

#Clean family, genus, species data---------
table(firstcleandata$family)
firstcleandata$family <- gsub("Unknown", "Indet", firstcleandata$family)

table(firstcleandata$genus)
length(firstcleandata$genus)
firstcleandata$genus <- gsub("Unidentified", "Indet", firstcleandata$genus)
length(firstcleandata$genus)

#Should I make "Species #"s into Indets?????????
table(firstcleandata$species)
length(firstcleandata$species)
firstcleandata$species <- gsub("Unknown.*", "Indet", firstcleandata$species)
firstcleandata$species <- gsub("indet.*", "Indet", firstcleandata$species)
firstcleandata$species <- gsub("unknown.*", "Indet", firstcleandata$species)
length(firstcleandata$species)

length(firstcleandata$species)

table(firstcleandata$census)
table(firstcleandata$site)



write.csv(firstcleandata, here("Desktop", "Research", "R", "Data", "data_first_clean.csv"))

