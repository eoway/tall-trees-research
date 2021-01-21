## Clean ForestGEO and ForestPlots inventory data and export 
## 8-30-2019

setwd("G:/My Drive") # Google Drive
library(dplyr); library(ggplot2); library(viridis); library(stringr)
library(ggfortify); library(cowplot); require(data.table); library(tidyr)
library(raster); library(rgdal); library(sp); library(GISTools); library(sf)
library(SDMTools); library(fasterize); library(purrr)
library(readxl); library("fgeo"); library(plyr)

# ForestGEO    :  Danum, Lambir, (Pasoh eventually)
# ForestPlots  :  Sepilok, Danum, (Allpahuayo, Cicra eventually)

# combine taxonomic info with DBH data (species, genus, family)
# keep only identified species
# unit conversions for consistency
# keep only relevant columns and rename for consistency 
# join BAAD wood density (WD) for ForestGEO using species mean, or genus mean, or family mean

# TO DO: convert stems to trees??

# Export as .csv files - use this data for analyses 

#----------------------------------- Load ForestGEO data -------------------------------# 
#--------------------------------------- DANUM data ------------------------------------# 
# single (first) census (data collected ~2011-2015)
# 50-ha plot 
# plot dims 1000 m x 500 m = 500,000 m2 = 50 ha
danum1 <- fread("Harvard/Plot_Data/CTFS_ForestGEO/Data/PlotDataReport02-14-2019_1491847679_census1.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
danum2 <- fread("Harvard/Plot_Data/CTFS_ForestGEO/Data/PlotDataReport04-17-2020_1896600351_census2.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tax_dnm <- fread("Harvard/Plot_Data/CTFS_ForestGEO/Data/TaxonomyReport02-14-2019_844633710.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)

#--------------------------------------- LAMBIR data ------------------------------------# 
# four censuses (data collected ~1991, 1997, 2003, and 2007/08)
# 52-ha plot 
# plot dims x = 1040 m X y = 500 m = 520,000 m2 = 52 ha
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem1.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem2.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem3.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem4.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/CTFSElev_lambir.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/Lambir_Soils_DatatoElsaOrdway/lambir.habs.Rdata")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/Lambir_Soils_DatatoElsaOrdway/stem4.RM3a.Rdata")

lambir.habs$soil = lambir.habs$HabType
lambir.habs$soil[lambir.habs$HabType==1]="Sandy_loam"
lambir.habs$soil[lambir.habs$HabType==2]="Clay"
lambir.habs$soil[lambir.habs$HabType==3]="Loam"
lambir.habs$soil[lambir.habs$HabType==4]="Fine_loam"

lambir1 <- lambir.stem1; lambir2 <- lambir.stem2; lambir3 <- lambir.stem3; lambir4 <- lambir.stem4

## Merge Lambir soil type indices with 4th census
## commented out example from Sabrina Russo
#stem4.RM3a = merge(stem4.RM2, lambir.habs, by.x = "index20", by.y="index", all.x=T)
lambir1$index <- as.factor(lambir1$quadrat); lambir1$index <- as.numeric(lambir1$index)
lambir2$index <- as.factor(lambir2$quadrat); lambir2$index <- as.numeric(lambir2$index)
lambir3$index <- as.factor(lambir3$quadrat); lambir3$index <- as.numeric(lambir3$index)
lambir4$index <- as.factor(lambir4$quadrat); lambir4$index <- as.numeric(lambir4$index)
lam1 = merge(lambir1, lambir.habs, by = "index", all.x=T)
lam2 = merge(lambir2, lambir.habs, by = "index", all.x=T)
lam3 = merge(lambir3, lambir.habs, by = "index", all.x=T)
lam4 = merge(lambir4, lambir.habs, by = "index", all.x=T)
#lam4 %>% group_by(soil) %>% dplyr::summarize(n=n(), m_dbh=mean(dbh, na.rm=T))

lam_elev <- CTFSElev_lambir

# dat <- subset(lam4, quadrat == "1325") # #0101, 0102
# head(dat)
# plot(dat$gy~dat$gx)

# load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.spptable.RData")
# LD = light demanding; ST = shade tolerance; NA for anything without spp designation (e.g. IDlevel = multiple, Family, or Genus)
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.spptable.ST.RData") # data from Sabrina Russo - spp categorized as light demanding or shade tolerant

# for Lambir: convert gx, gy to numeric
cols.num <- c("gx","gy")
lam1[cols.num] <- sapply(lam1[cols.num],as.numeric)
lam2[cols.num] <- sapply(lam2[cols.num],as.numeric)
lam3[cols.num] <- sapply(lam3[cols.num],as.numeric)
lam4[cols.num] <- sapply(lam4[cols.num],as.numeric)

# join species info from tax_dnm to danum and lambir.spptable to lhp1 - lhp4  
dnm1 <- dplyr::left_join(danum1, tax_dnm, by="Mnemonic")
dnm2 <- dplyr::left_join(danum2, tax_dnm, by="Mnemonic")
lhp1 <- dplyr::left_join(lam1, lambir.spptable.ST, by="sp")
lhp2 <- dplyr::left_join(lam2, lambir.spptable.ST, by="sp")
lhp3 <- dplyr::left_join(lam3, lambir.spptable.ST, by="sp")
lhp4 <- dplyr::left_join(lam4, lambir.spptable.ST, by="sp")

# Assign Danum IDlevel
# If Species ==  "sp" or includes "species", status == "genus", else status == "species"
dnm1$IDlevel <- ifelse(dnm1$Family == "Unknown","unknown",
               ifelse(str_detect(dnm1$Species, "species") | dnm1$Species == "sp", "genus", "species"))
dnm2$IDlevel <- ifelse(dnm2$Family == "Unknown","unknown",
                       ifelse(str_detect(dnm2$Species, "species") | dnm2$Species == "sp", "genus", "species"))

# Assign Lambir IDlevel
# IDlevel == "multiple" | "none" -> "family"; when Family == "Unkown", IDlevel == "unknown"; else leave as is
lhp1$IDlevel <- ifelse(lhp1$Family == "Unknown", "unknown",
                       ifelse(lhp1$IDlevel == "multiple" | lhp1$IDlevel == "none", "family", lhp1$IDlevel))
lhp2$IDlevel <- ifelse(lhp2$Family == "Unknown", "unknown",
                       ifelse(lhp2$IDlevel == "multiple" | lhp2$IDlevel == "none", "family", lhp2$IDlevel))
lhp3$IDlevel <- ifelse(lhp3$Family == "Unknown", "unknown",
                       ifelse(lhp3$IDlevel == "multiple" | lhp3$IDlevel == "none", "family", lhp3$IDlevel))
lhp4$IDlevel <- ifelse(lhp4$Family == "Unknown", "unknown",
                       ifelse(lhp4$IDlevel == "multiple" | lhp4$IDlevel == "none", "family", lhp4$IDlevel))

# not sure whether to retain all stems or aggregate
# 1 df: retaining all stems for BA calculation
# 2 df: for all other calculations - use main stem only (sum DBH?)
# for now... retaining all stems
# select "main stem"
# dnm_main <- pick_main_stem(dnm)
# lhp1_main <- pick_main_stem(lhp1)
# lhp2_main <- pick_main_stem(lhp2)
# lhp3_main <- pick_main_stem(lhp3)
# lhp4_main <- pick_main_stem(lhp4)

# DO NOT DO THIS STEP BECAUSE REMOVES DEAD TREES...
# omit all rows with no DBH data
# dnm1 <- dnm1[complete.cases(dnm1$DBH), ]; dim(dnm1)
# dnm2 <- dnm2[complete.cases(dnm2$DBH), ]; dim(dnm2)
# lhp1 <- lhp1[complete.cases(lhp1$dbh), ]; dim(lhp1)
# lhp2 <- lhp2[complete.cases(lhp2$dbh), ]; dim(lhp2)
# lhp3 <- lhp3[complete.cases(lhp3$dbh), ]; dim(lhp3)
# lhp4 <- lhp4[complete.cases(lhp4$dbh), ]; dim(lhp4)

# add site column, select columns of interest and rbind DNM with LHP4
dnm1$site <- rep("DNM50",length(dnm1$DBH))
dnm2$site <- rep("DNM50",length(dnm2$DBH))
lhp1$site <- rep("LHP",length(lhp1$dbh))
lhp2$site <- rep("LHP",length(lhp2$dbh))
lhp3$site <- rep("LHP",length(lhp3$dbh))
lhp4$site <- rep("LHP",length(lhp4$dbh))

# create column identifying census #
dnm1$census <- rep("census_2011_15",length(dnm1$DBH))
dnm2$census <- rep("census_2019",length(dnm2$DBH))
lhp1$census <- rep("census_1991",length(lhp1$dbh))
lhp2$census <- rep("census_1997",length(lhp2$dbh))
lhp3$census <- rep("census_2003",length(lhp3$dbh))
lhp4$census <- rep("census_2007_08",length(lhp4$dbh))

# rename columns in DNM data so consistent with LHP
colnames(dnm1) <- c("No..x","Latin","sp","subspecies","index","gx","gy","treeID","tag","stemID","StemTag",
                   "CensusID","mm_dbh","hom","ExactDate","codes","stem","DFstatus","No..y","Family","Genus",
                   "Species","SubSpecies","IDlevel","Authority","PriorNames","SpeciesID","site","census")
colnames(dnm2) <- c("No..x","Latin","sp","subspecies","index","gx","gy","treeID","tag","stemID","StemTag",
                   "CensusID","mm_dbh","hom","ExactDate","codes","stem","DFstatus","No..y","Family","Genus",
                   "Species","SubSpecies","IDlevel","Authority","PriorNames","SpeciesID","site","census")

# convert DBH from mm to cm, but save mm
dnm1$dbh <- dnm1$mm_dbh*0.1
dnm2$dbh <- dnm2$mm_dbh*0.1

## double trunks == 8.5% of trees in Danum (21808/256713)
# test <- dnm1 %>% group_by(treeID) %>% dplyr::summarize(n_stems=n(), 
#                                                dbh = sum(dbh))
test <- dnm2 %>% group_by(treeID) %>% dplyr::summarize(n_stems=n(), 
                                               dbh = sum(dbh))
par(mfrow=c(1,2))
boxplot(log(dnm2$dbh))
boxplot(log(test$dbh))

# Add Danum Shade.Tol column for consistency
dnm1$Shade.Tol <- rep("NA",length(dnm1$dbh))
dnm2$Shade.Tol <- rep("NA",length(dnm2$dbh))

# Add Danum soil column for consistency
dnm1$soil <- rep("NA",length(dnm1$dbh))
dnm2$soil <- rep("NA",length(dnm2$dbh))

## KEEP: TreeID, Species, Genus, Family, DBH, Status (alive, etc.)
dnm1_select <- dnm1 %>% select(gx,gy,index,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,DFstatus,IDlevel,site,census,Shade.Tol,soil)
dnm2_select <- dnm2 %>% select(gx,gy,index,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,DFstatus,IDlevel,site,census,Shade.Tol,soil)
lhp1_select <- lhp1 %>% select(gx,gy,index,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,DFstatus,IDlevel,site,census,Shade.Tol,soil)
lhp2_select <- lhp2 %>% select(gx,gy,index,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,DFstatus,IDlevel,site,census,Shade.Tol,soil)
lhp3_select <- lhp3 %>% select(gx,gy,index,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,DFstatus,IDlevel,site,census,Shade.Tol,soil)
lhp4_select <- lhp4 %>% select(gx,gy,index,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,DFstatus,IDlevel,site,census,Shade.Tol,soil)

# combine all  datasets
dnm1_select$tag <- as.character(dnm1_select$tag)
dnm2_select$tag <- as.character(dnm2_select$tag)
ForestGEO_clean <- bind_rows(dnm1_select, dnm2_select, lhp1_select, lhp2_select, lhp3_select, lhp4_select)

colnames(ForestGEO_clean) <- c("plot_x", "plot_y", "quadrat20", "tag", "sp", "family", "genus", "species", "treeID", "stemID", "dbh",
                               "date","DFstatus","IDlevel","site","census","shade.tol","soil")

# calculate stem basal area
ForestGEO_clean$stem_BA <- 0.00007854 * ForestGEO_clean$dbh^2 # using DBH in cm, calculates basal area in m2



## ----------------- link to Global Wood Density Database ------------------
# https://datadryad.org/resource/doi:10.5061/dryad.234/1
# https://rdrr.io/cran/BIOMASS/man/wdData.html
library(BIOMASS)
data("wdData")

## Assign WD: species mean, genus mean, family mean, plot-level mean
# use IDlevel column == species, genus, family, unknown

# separate ForestGEO_clean into 4 dataframes: species, genus, family, unknown
fg_spp <- filter(ForestGEO_clean, IDlevel == "species"); dim(fg_spp) # n = 1901937
fg_genus <- filter(ForestGEO_clean, IDlevel == "genus"); dim(fg_genus) # n = 142673
fg_family <- filter(ForestGEO_clean, IDlevel == "family"); dim(fg_family) # n = 25701 
fg_unknown <- filter(ForestGEO_clean, IDlevel == "unknown"); dim(fg_unknown) # n = 94251 

# summarize wdData by family, genus, species (mean, median, min, max, var, sd)
# also n_wd_obs=n(),min_wd = min(wd, na.rm=T),max_wd = max(wd, na.rm=T),var_wd = var(wd, na.rm=T),sd_wd = sd(wd, na.rm=T)
wd_spp_mean <- wdData %>% group_by(species) %>% dplyr::summarize(mean_wd = median(wd, na.rm=T))
wd_genus_mean <- wdData %>% group_by(genus) %>% dplyr::summarize(mean_wd = median(wd, na.rm=T))
wd_family_mean <- wdData %>% group_by(family) %>% dplyr::summarize(mean_wd = median(wd, na.rm=T))

#-------------------------------------------------------------------------------------------------#
### THIS NEEDS TO BE REDONE: 
### pull WD for spp, if none sample from genus, if none sample from family, ...
### if none sample from other WD vals in plot
#-------------------------------------------------------------------------------------------------#

# join summarized wdData wd with ForestGEO data by taxonomy columns
fg_spp <- dplyr::left_join(fg_spp, wd_spp_mean, by = "species"); head(fg_spp) # 455415 NAs
fg_genus <- dplyr::left_join(fg_genus, wd_genus_mean, by = "genus"); head(fg_genus) # 5494 NAs
fg_family <- dplyr::left_join(fg_family, wd_family_mean, by = "family"); head(fg_family) # 1020 NAs 
fg_unknown$mean_wd <- rep(NA, length(fg_unknown$dbh)) 

# recombine
ForestGEO_clean_wd <- bind_rows(fg_spp, fg_genus, fg_family, fg_unknown)

ggplot(ForestGEO_clean_wd, aes(x=as.factor(IDlevel), y=mean_wd)) + 
  geom_boxplot()

ggplot(ForestGEO_clean_wd, aes(x=mean_wd, fill=census)) +
  geom_histogram(binwidth=.05, alpha=.5, col= "white") + 
  facet_grid(census~.)

# # summarize plot/census level mean WD and assign to unknown and all stems with mean_wd == NA
# census_wd <- ForestGEO_clean_wd %>% group_by(census, site) %>% summarize(WD = median(mean_wd, na.rm=T)); census_wd
# ForestGEO_clean_wd$mean_wd[is.na(ForestGEO_clean_wd$mean_wd)] <- -9999
# 
# ForestGEO_clean_wd$mean_wd <- ifelse(ForestGEO_clean_wd$mean_wd == -9999 & ForestGEO_clean_wd$census == "census_1991",
#                                      census_wd[[1,3]], ForestGEO_clean_wd$mean_wd)
# ForestGEO_clean_wd$mean_wd <- ifelse(ForestGEO_clean_wd$mean_wd == -9999 & ForestGEO_clean_wd$census == "census_1997",
#                                      census_wd[[2,3]], ForestGEO_clean_wd$mean_wd)
# ForestGEO_clean_wd$mean_wd <- ifelse(ForestGEO_clean_wd$mean_wd == -9999 & ForestGEO_clean_wd$census == "census_2003",
#                                      census_wd[[3,3]], ForestGEO_clean_wd$mean_wd)
# ForestGEO_clean_wd$mean_wd <- ifelse(ForestGEO_clean_wd$mean_wd == -9999 & ForestGEO_clean_wd$census == "census_2007_08",
#                                      census_wd[[4,3]], ForestGEO_clean_wd$mean_wd)
# ForestGEO_clean_wd$mean_wd <- ifelse(ForestGEO_clean_wd$mean_wd == -9999 & ForestGEO_clean_wd$census == "census_2011_15",
#                                      census_wd[[5,3]], ForestGEO_clean_wd$mean_wd)
#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------------------------
#------------------------------------ Start Here w ForestPlots -----------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# plot_x = X
# plot_y = Y
# T1 = use to create plot_x and plot_y
# sp = NA
# family = Family
# genus = genus from Species, 1
# species = species from Species, 2
# treeID == Tree ID 
# stemID = ???
# dbh == DBH1
# date = NA
# DFstatus = F1
# IDlevel = WD Type
# site = site (CREATE)
# census = census (CREATE)
# shade.tol = NA
# stem_BA = stem_BA (CALCULATE)
# mean_wd = WD 

# T1 (subplots): For a standard 1 ha plot, T1 corresponds to the 25 20m x 20m subplot division
# T1 (subplots: n = 100; 4 ha / 100 = 0.04 ha subplots = 400 m2 = 20x20 m

# load ForestPlots census data, skip 1 line
dat_sep04 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/SEP_04_PlotDump.xlsx", sheet=2, skip=1)
dat_sep05 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/SEP_05_PlotDump.xlsx", sheet=2, skip=1)
dat_sep30 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/SEP_30_PlotDump.xlsx", sheet=2, skip=1)

dat_sep08 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/SEP_08_PlotDump.xlsx", sheet=2, skip=1)
dat_sep08 <- filter(dat_sep08, T1 != "-"); dim(dat_sep08); head(dat_sep08$T1) # get rid of NA rows where T1 = '-'

dat_sep09 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/SEP_09_PlotDump.xlsx", sheet=2, skip=1)
dat_sep10 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/SEP_10_PlotDump.xlsx", sheet=2, skip=1)

dat_dnm01 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/DAN_01_PlotDump.xlsx", sheet=2, skip=1)
dat_dnm02 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/DAN_02_PlotDump.xlsx", sheet=2, skip=1)
dat_dnm03 <- read_excel("Harvard/Plot_Data/Forest_Plots/Data/DAN_03_PlotDump.xlsx", sheet=2, skip=1)

# separate first columns that apply to each census
dat_all04 <- select(dat_sep04, 'Tree ID':'WD Type')
# create separate genus & species columns
dat_all04$genus <- word(dat_all04$Species)
dat_all04$species <- word(dat_all04$Species, 2)
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
sep_04_2008$census <- rep("04_census_2008",length(sep_04_2008$DBH_mm))
sep_04_2014$census <- rep("04_census_2014",length(sep_04_2014$DBH_mm))
sep_05_2001$census <- rep("05_census_2001",length(sep_05_2001$DBH_mm))
sep_05_2008$census <- rep("05_census_2008",length(sep_05_2008$DBH_mm))
sep_05_2014$census <- rep("05_census_2014",length(sep_05_2014$DBH_mm))
sep_30_2001$census <- rep("30_census_2001",length(sep_30_2001$DBH_mm))
sep_30_2010$census <- rep("30_census_2010",length(sep_30_2010$DBH_mm))
sep_30_2015$census <- rep("30_census_2015",length(sep_30_2015$DBH_mm))

sep_08_2001$census <- rep("08_census_2001",length(sep_08_2001$DBH_mm))
sep_08_2009$census <- rep("08_census_2009",length(sep_08_2009$DBH_mm))
sep_08_2014$census <- rep("08_census_2014",length(sep_08_2014$DBH_mm))

sep_09_2000$census <- rep("09_census_2001",length(sep_09_2000$DBH_mm))
sep_09_2009$census <- rep("09_census_2009",length(sep_09_2009$DBH_mm))
sep_09_2014$census <- rep("09_census_2014",length(sep_09_2014$DBH_mm))
sep_10_2000$census <- rep("10_census_2001",length(sep_10_2000$DBH_mm))
sep_10_2009$census <- rep("10_census_2009",length(sep_10_2009$DBH_mm))
sep_10_2014$census <- rep("10_census_2014",length(sep_10_2014$DBH_mm))

dnm_01_2006$census <- rep("01_census_2006",length(dnm_01_2006$DBH_mm))
dnm_01_2013$census <- rep("01_census_2013",length(dnm_01_2013$DBH_mm))
dnm_01_2016$census <- rep("01_census_2016",length(dnm_01_2016$DBH_mm))
dnm_02_2006$census <- rep("02_census_2006",length(dnm_02_2006$DBH_mm))
dnm_02_2013$census <- rep("02_census_2013",length(dnm_02_2013$DBH_mm))
dnm_02_2016$census <- rep("02_census_2016",length(dnm_02_2016$DBH_mm))
dnm_03_2006$census <- rep("03_census_2006",length(dnm_03_2006$DBH_mm))
dnm_03_2013$census <- rep("03_census_2013",length(dnm_03_2013$DBH_mm))
dnm_03_2016$census <- rep("03_census_2016",length(dnm_03_2016$DBH_mm))

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

SEP_S <- bind_rows(sep_08_2001,sep_08_2009,sep_08_2014) 
SEP_S$site <- rep("SPKS",length(SEP_S$DBH_mm))

SEP_A <- bind_rows(sep_09_2000,sep_09_2009,sep_09_2014,sep_10_2000,sep_10_2009,sep_10_2014) 
SEP_A$site <- rep("SPKA",length(SEP_A$DBH_mm))

DNM1 <- bind_rows(dnm_01_2006,dnm_01_2013,dnm_01_2016) 
DNM1$site <- rep("DNM1",length(DNM1$DBH_mm))

DNM2 <- bind_rows(dnm_02_2006,dnm_02_2013,dnm_02_2016) 
DNM2$site <- rep("DNM2",length(DNM2$DBH_mm))

DNM3 <- bind_rows(dnm_03_2006,dnm_03_2013,dnm_03_2016) 
DNM3$site <- rep("DNM3",length(DNM3$DBH_mm))

# COMBINE ALL SEPILOK & DANUM FOREST_PLOTS DATA & EXPORT
### ADD: DNM_01, DNM_02, DNM_03
forest_plots_clean <- bind_rows(SEP_H,SEP_S,SEP_A,DNM1,DNM2,DNM3)

# convert DBH from mm to cm and calculate stem_BA
forest_plots_clean$dbh <- forest_plots_clean$DBH_mm*0.1
forest_plots_clean$stem_BA <- 0.00007854 * forest_plots_clean$dbh^2 # using DBH in cm, calculates basal area in m2

ggplot(forest_plots_clean, aes(x=mean_wd)) + 
  geom_histogram(binwidth=.05, alpha=.5, col= "white") +
  facet_wrap(~site, ncol=2, scales = "free")

facet_dat <- select(forest_plots_clean, site, dbh)

ggplot(facet_dat, aes(mean_wd)) + 
  geom_histogram(bins=15, col="white") + 
  geom_vline(data = ddply(forest_plots_clean, "site", summarize, wavg = median(mean_wd, na.rm=T)), aes(xintercept=wavg), col="red", lty=2, lwd=1) + 
  facet_wrap(~site, scales = 'free')

ggplot(facet_dat, aes(dbh)) + 
  geom_histogram(bins=15, col="white") + 
  geom_vline(data = ddply(forest_plots_clean, "site", summarize, wavg = median(dbh, na.rm=T)), aes(xintercept=wavg), col="red", lty=2, lwd=1) + 
  facet_wrap(~site, scales = 'free')

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
# export ALL DATA (.csv)

write.csv(forest_plots_clean,"Harvard/Plot_Data/clean_inventory_data/Forest_Plots_clean.csv")
write.csv(ForestGEO_clean_wd,"Harvard/Plot_Data/clean_inventory_data/ForestGEO_clean.csv")
#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#