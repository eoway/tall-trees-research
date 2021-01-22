## Preliminary cleaning of ForestGEO and ForestPlots inventory data and export 
## 01-20-2021

#---------------------------------------------------------------------------------------------#
# ForestGEO    :  Danum, Lambir, (Pasoh eventually)
# ForestPlots  :  Sepilok, Danum, (Allpahuayo, Cicra eventually)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# combine taxonomic info with DBH data (species, genus, family)
# unit conversions for consistency
# calculate Julian date
# fill Julian dates where date == NA
# add/modify columns and factors for consistency across datasets
# calculate stem-level basal area (BA)
# join GWDD wood density (WD) for ForestGEO using species mean, or genus mean, or family mean
# convert 'unknown' spp to 'indet'
# update DFstatus
# fill in DBH for dead/broken below stems from previous census if NA or 0
# remove DNM50 outliers: remove the 0.5% stems w extreme high and low relative growth rate
# check for duplicates

# TO DO: convert stems to trees??
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
setwd("G:/My Drive") # Google Drive

library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)

#---------------------------------------------------------------------------------------------#
# Load ForestGEO data                                                                         # 
#---------------------------------------------------------------------------------------------#
#------------------------------------------ DANUM data ---------------------------------------# 
# single (first) census (data collected ~2011-2015)
# 50-ha plot 
# plot dims 1000 m x 500 m = 500,000 m2 = 50 ha
#---------------------------------------------------------------------------------------------#
dnm1 <- read_tsv("Harvard/Plot_Data/CTFS_ForestGEO/Data/PlotDataReport02-14-2019_1491847679_census1.txt")
dnm2 <- read_tsv("Harvard/Plot_Data/CTFS_ForestGEO/Data/PlotDataReport04-17-2020_1896600351_census2.txt")
tax <- read_tsv("Harvard/Plot_Data/CTFS_ForestGEO/Data/TaxonomyReport02-14-2019_844633710.txt")
#---------------------------------------------------------------------------------------------#

#----------------------------------------- LAMBIR data ---------------------------------------# 
# four censuses (data collected ~1991, 1997, 2003, and 2007/08)
# 52-ha plot 
# plot dims x = 1040 m X y = 500 m = 520,000 m2 = 52 ha
#---------------------------------------------------------------------------------------------#
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem1.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem2.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem3.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.stem4.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/CTFSElev_lambir.RData")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/Lambir_Soils_DatatoElsaOrdway/lambir.habs.Rdata")
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/Lambir_Soils_DatatoElsaOrdway/stem4.RM3a.Rdata")

# load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.spptable.RData")
# data from Sabrina Russo - spp categorized as light demanding or shade tolerant
# LD = light demanding; ST = shade tolerance; NA for anything without spp designation 
# (e.g. IDlevel = multiple, Family, or Genus)
load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.spptable.ST.RData") 
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Define Lambir soil type
#---------------------------------------------------------------------------------------------#
lambir.habs$soil = lambir.habs$HabType
lambir.habs$soil[lambir.habs$HabType==1]="Sandy_loam"
lambir.habs$soil[lambir.habs$HabType==2]="Clay"
lambir.habs$soil[lambir.habs$HabType==3]="Loam"
lambir.habs$soil[lambir.habs$HabType==4]="Fine_loam"

lambir1 <- lambir.stem1; lambir2 <- lambir.stem2; lambir3 <- lambir.stem3; lambir4 <- lambir.stem4
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
## Merge Lambir soil type indices with 4th census
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# for Lambir: convert gx, gy to numeric
#---------------------------------------------------------------------------------------------#
cols.num <- c("gx","gy")
lam1[cols.num] <- sapply(lam1[cols.num],as.numeric)
lam2[cols.num] <- sapply(lam2[cols.num],as.numeric)
lam3[cols.num] <- sapply(lam3[cols.num],as.numeric)
lam4[cols.num] <- sapply(lam4[cols.num],as.numeric)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Join with taxonomic info
#---------------------------------------------------------------------------------------------#
dnm1 <- left_join(dnm1, tax, by = "Mnemonic")
dnm2 <- left_join(dnm2, tax, by = "Mnemonic")                
lhp1 <- dplyr::left_join(lam1, lambir.spptable.ST, by="sp")
lhp2 <- dplyr::left_join(lam2, lambir.spptable.ST, by="sp")
lhp3 <- dplyr::left_join(lam3, lambir.spptable.ST, by="sp")
lhp4 <- dplyr::left_join(lam4, lambir.spptable.ST, by="sp")
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Determine IDlevel
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Fix HOM where clearly entered in wrong units (1.3) or with note-taking error (1130)
#---------------------------------------------------------------------------------------------#
dnm1$HOM <- ifelse(dnm1$HOM == 1.3 | dnm1$HOM == 1130, 130, dnm1$HOM)
dnm2$HOM <- ifelse(dnm2$HOM == 1.3 | dnm2$HOM == 1130, 130, dnm2$HOM)
dnm2$HOM <- ifelse(dnm2$HOM < 10, dnm2$HOM*100, dnm2$HOM)
dnm2$HOM <- ifelse(dnm2$HOM < 60, dnm2$HOM*10, dnm2$HOM)
lhp1$hom <- lhp1$hom*100 
lhp2$hom <- lhp2$hom*100 
lhp2$hom <- ifelse(lhp2$hom > 20000, lhp2$hom/100, lhp2$hom)
lhp3$hom <- lhp3$hom*100 
lhp4$hom <- lhp4$hom*100 
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# add site column
#---------------------------------------------------------------------------------------------#
dnm1$site <- rep("DNM50",length(dnm1$DBH))
dnm2$site <- rep("DNM50",length(dnm2$DBH))
lhp1$site <- rep("LHP",length(lhp1$dbh))
lhp2$site <- rep("LHP",length(lhp2$dbh))
lhp3$site <- rep("LHP",length(lhp3$dbh))
lhp4$site <- rep("LHP",length(lhp4$dbh))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# add census column
#---------------------------------------------------------------------------------------------#
dnm1$census <- rep("census_2011_15",length(dnm1$DBH))
dnm2$census <- rep("census_2019",length(dnm2$DBH))
lhp1$census <- rep("census_1991",length(lhp1$dbh))
lhp2$census <- rep("census_1997",length(lhp2$dbh))
lhp3$census <- rep("census_2003",length(lhp3$dbh))
lhp4$census <- rep("census_2007_08",length(lhp4$dbh))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# add plot column
#---------------------------------------------------------------------------------------------#
dnm1$plot <- rep("DNM50_FGEO",length(dnm1$DBH))
dnm2$plot <- rep("DNM50_FGEO",length(dnm2$DBH))

lhp1$plot <- ifelse(lhp1$soil == "Clay", "LH_clay", ifelse(lhp1$soil == "Sandy_loam", "LH_sandstone",
                                                           ifelse(lhp1$soil == "Loam","LH_loam",
                                                                  "LH_fineLoam")))
lhp2$plot <- ifelse(lhp2$soil == "Clay", "LH_clay", ifelse(lhp2$soil == "Sandy_loam", "LH_sandstone",
                                                           ifelse(lhp2$soil == "Loam","LH_loam",
                                                                  "LH_fineLoam")))
lhp3$plot <- ifelse(lhp3$soil == "Clay", "LH_clay", ifelse(lhp3$soil == "Sandy_loam", "LH_sandstone",
                                                           ifelse(lhp3$soil == "Loam","LH_loam",
                                                                  "LH_fineLoam")))
lhp4$plot <- ifelse(lhp4$soil == "Clay", "LH_clay", ifelse(lhp4$soil == "Sandy_loam", "LH_sandstone",
                                                           ifelse(lhp4$soil == "Loam","LH_loam",
                                                                  "LH_fineLoam")))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# convert DBH from mm to cm
#---------------------------------------------------------------------------------------------#
dnm1$dbh <- dnm1$DBH*0.1
dnm2$dbh <- dnm2$DBH*0.1
#---------------------------------------------------------------------------------------------#
# ## double trunks == 8.5% of trees in Danum (21808/256713)
# # test <- dnm1 %>% group_by(treeID) %>% dplyr::summarize(n_stems=n(), 
# #                                                dbh = sum(dbh))
# test <- dnm2 %>% group_by(treeID) %>% dplyr::summarize(n_stems=n(), 
#                                                        dbh = sum(dbh))
# par(mfrow=c(1,2))
# boxplot(log(dnm2$dbh))
# boxplot(log(test$dbh))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Calculate Julian Date
#---------------------------------------------------------------------------------------------#
dnm1$JulianDate <- julian.Date(dnm1$Date)
dnm1$Date <- as.character(dnm1$Date)

dnm2$JulianDate <- julian.Date(dnm2$Date)
dnm2$Date <- as.character(dnm2$Date)

lhp1$ExactDate <- as.Date(lhp1$ExactDate)
lhp2$ExactDate <- as.Date(lhp2$ExactDate)
lhp3$ExactDate <- as.Date(lhp3$ExactDate)
lhp4$ExactDate <- as.Date(lhp4$ExactDate)

lhp1$JulianDate <- julian.Date(lhp1$ExactDate)
lhp1$ExactDate <- as.character(lhp1$ExactDate)
lhp2$JulianDate <- julian.Date(lhp2$ExactDate)
lhp2$ExactDate <- as.character(lhp2$ExactDate)
lhp3$JulianDate <- julian.Date(lhp3$ExactDate)
lhp3$ExactDate <- as.character(lhp3$ExactDate)
lhp4$JulianDate <- julian.Date(lhp4$ExactDate)
lhp4$ExactDate <- as.character(lhp4$ExactDate)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# Fill Julian dates - DANUM
#---------------------------------------------------------------------------------------------#
## plot Julian date (as color) by plot X & plot Y locations
## ...these plots take a minute because there are so many observations...
# ggplot() + 
#   geom_point(data=dnm1, aes(PX, PY, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
# ggplot() + 
#   geom_point(data=dnm2, aes(PX, PY, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
## from the above plot, we can see that the left side of the plot (as graphed) was censused
## earliest; grey observations/stems don't have dates recorded
## the plot dimensions are 500m x 1000m
## notice that in dnm2,  some of the PX & PY locations are clearly wrong (dnm1 looks good)
## fix them by replacing the values outside the plot dimensions with quadrat-level mean XY values

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

#---------------------------------------------------------------------------------------------#
# Fill Julian dates - LAMBIR
#---------------------------------------------------------------------------------------------#
# plot Julian date (as color) by plot X & plot Y locations
# ...these plots take a minute because there are so many observations...
# ggplot() + 
#   geom_point(data=lhp1, aes(gx, gy, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
# ggplot() + 
#   geom_point(data=lhp2, aes(gx, gy, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
# ggplot() + 
#   geom_point(data=lhp3, aes(gx, gy, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
# ggplot() + 
#   geom_point(data=lhp4, aes(gx, gy, col=JulianDate), pch=21) + 
#   scale_colour_gradient() + 
#   theme_classic()
# from the above plot, we can see that the left side of the plot (as graphed) was censused
# earliest for lhp1 and the right side first for lhp3; grey observations/stems don't have 
# dates recorded the plot dimensions are 500m x 1040m == 52 ha

# summarize Julian date by quadrat
lhp1_quad_dates <- lhp1 %>% group_by(quadrat) %>% summarize(JulianDate = mean(JulianDate, na.rm=T))
lhp2_quad_dates <- lhp2 %>% group_by(quadrat) %>% summarize(JulianDate = mean(JulianDate, na.rm=T))
lhp3_quad_dates <- lhp3 %>% group_by(quadrat) %>% summarize(JulianDate = mean(JulianDate, na.rm=T))
lhp4_quad_dates <- lhp4 %>% group_by(quadrat) %>% summarize(JulianDate = mean(JulianDate, na.rm=T))

# subset dnm1 and dnm2 observations where JulianDate == NA
# apply the quadrat-level mean Julian date to stems with JulianDate == NA
lhp1_date_NAs <- filter(lhp1, is.na(JulianDate))
lhp2_date_NAs <- filter(lhp2, is.na(JulianDate))
lhp3_date_NAs <- filter(lhp3, is.na(JulianDate))
lhp4_date_NAs <- filter(lhp4, is.na(JulianDate))
# make sure the number of rows in the new dataframes are equivalent to the number of JulianDate NAs
dim(lhp1_date_NAs); summary(lhp1$JulianDate)
dim(lhp2_date_NAs); summary(lhp2$JulianDate)
dim(lhp3_date_NAs); summary(lhp3$JulianDate)
dim(lhp4_date_NAs); summary(lhp4$JulianDate)

# join the NA dataframe (excluding the original JulianDate column) with the averaged Julian dates
lhp1_date_filled <- left_join(select(lhp1_date_NAs,-JulianDate), lhp1_quad_dates, by = "quadrat")
lhp2_date_filled <- left_join(select(lhp2_date_NAs,-JulianDate), lhp2_quad_dates, by = "quadrat")
lhp3_date_filled <- left_join(select(lhp3_date_NAs,-JulianDate), lhp3_quad_dates, by = "quadrat")
lhp4_date_filled <- left_join(select(lhp4_date_NAs,-JulianDate), lhp4_quad_dates, by = "quadrat")

# subset data where we already have Julian date values
lhp1_w_dates <- filter(lhp1, !is.na(JulianDate))
lhp2_w_dates <- filter(lhp2, !is.na(JulianDate))
lhp3_w_dates <- filter(lhp3, !is.na(JulianDate))
lhp4_w_dates <- filter(lhp4, !is.na(JulianDate))

# confirm the dataframes with stems that originally had JulianDate info are the correct dimensions
dim(lhp1_w_dates); nrow(lhp1)-nrow(lhp1_date_NAs)
dim(lhp2_w_dates); nrow(lhp2)-nrow(lhp2_date_NAs)
dim(lhp3_w_dates); nrow(lhp3)-nrow(lhp3_date_NAs)
dim(lhp4_w_dates); nrow(lhp4)-nrow(lhp4_date_NAs)

# combine the data back into complete dataframes
lhp1 <- bind_rows(lhp1_date_filled,lhp1_w_dates)
lhp2 <- bind_rows(lhp2_date_filled,lhp2_w_dates)
lhp3 <- bind_rows(lhp3_date_filled,lhp3_w_dates)
lhp4 <- bind_rows(lhp4_date_filled,lhp4_w_dates)

# can plot to confirm that NA 'patches' are now filled in
# ggplot() +
#   geom_point(data=lhp1, aes(gx, gy, col=JulianDate), pch=21) +
#   scale_colour_gradient() +
#   theme_classic()
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Add Danum Shade.Tol & soil columns for consistency
#---------------------------------------------------------------------------------------------#
dnm1$Shade.Tol <- rep("NA",length(dnm1$dbh))
dnm2$Shade.Tol <- rep("NA",length(dnm2$dbh))
dnm1$soil <- rep("NA",length(dnm1$dbh))
dnm2$soil <- rep("NA",length(dnm2$dbh))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
#TreeID, Species, Genus, Family, DBH, Status
# dnm1_select <- dnm1 %>% select(Family,Genus,Species,TreeID,StemID,dbh,Status,IDlevel,Quadrat,site,
#                                census,Date,JulianDate,plot)
# dnm2_select <- dnm2 %>% select(Family,Genus,Species,TreeID,StemID,dbh,Status,IDlevel,Quadrat,site,
#                                census,Date,JulianDate,plot)

dnm1_select <- dnm1 %>% select(PX,PY,HOM,Quadrat,Tag,Mnemonic,Family,Genus,Species,TreeID,StemID,dbh,Date,JulianDate,Status,IDlevel,site,census,plot,Shade.Tol,soil)
dnm2_select <- dnm2 %>% select(PX,PY,HOM,Quadrat,Tag,Mnemonic,Family,Genus,Species,TreeID,StemID,dbh,Date,JulianDate,Status,IDlevel,site,census,plot,Shade.Tol,soil)
lhp1_select <- lhp1 %>% select(gx,gy,hom,quadrat,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,JulianDate,DFstatus,IDlevel,site,census,plot,Shade.Tol,soil)
lhp2_select <- lhp2 %>% select(gx,gy,hom,quadrat,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,JulianDate,DFstatus,IDlevel,site,census,plot,Shade.Tol,soil)
lhp3_select <- lhp3 %>% select(gx,gy,hom,quadrat,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,JulianDate,DFstatus,IDlevel,site,census,plot,Shade.Tol,soil)
lhp4_select <- lhp4 %>% select(gx,gy,hom,quadrat,tag,sp,Family,Genus,Species,treeID,stemID,dbh,ExactDate,JulianDate,DFstatus,IDlevel,site,census,plot,Shade.Tol,soil)

#---------------------------------------------------------------------------------------------#
DNM50 <- rbind(dnm1_select, dnm2_select)
colnames(DNM50) <- c("gx","gy","hom","quadrat","tag","sp","Family","Genus","Species","treeID","stemID","dbh",
                     "ExactDate","JulianDate","DFstatus","IDlevel", "site", "census", "plot",
                     "Shade.Tol","soil")

ForestGEO <- rbind(DNM50, lhp1_select, lhp2_select, lhp3_select, lhp4_select)

colnames(ForestGEO) <- c("plot_x","plot_y","hom","quadrat","tag","sp","family","genus","species","treeID","stemID",
                     "dbh","Date","JulianDate","DFstatus","IDlevel", "site", "census", "plot",
                     "Shade.Tol","soil")

#---------------------------------------------------------------------------------------------#
# calculate stem_BA
#---------------------------------------------------------------------------------------------#
ForestGEO$stem_BA <- 0.00007854 * ForestGEO$dbh^2
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
## ----------------- link to Global Wood Density Database ------------------
# https://datadryad.org/resource/doi:10.5061/dryad.234/1
# https://rdrr.io/cran/BIOMASS/man/wdData.html
library(BIOMASS)
data("wdData")

## Assign WD: species mean, genus mean, family mean, plot-level mean
# use IDlevel column == species, genus, family, unknown

# separate ForestGEO into 4 dataframes: species, genus, family, unknown
fg_spp <- filter(ForestGEO, IDlevel == "species"); dim(fg_spp) # n = 2339183 
fg_genus <- filter(ForestGEO, IDlevel == "genus"); dim(fg_genus) # n = 183785
fg_family <- filter(ForestGEO, IDlevel == "family"); dim(fg_family) # n = 28116
fg_unknown <- filter(ForestGEO, IDlevel == "unknown" | is.na(IDlevel)); dim(fg_unknown) # n = 104259
dim(ForestGEO); dim(fg_spp)[[1]]+dim(fg_genus)[[1]]+dim(fg_family)[[1]]+dim(fg_unknown)[[1]]

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
summary(fg_family)
# split into fg_family and fg_family_NA
fg_family_NA <- filter(fg_family, is.na(mean_wd))
fg_family <- filter(fg_family, !is.na(mean_wd))
# then add fg_genus_NA to fg_family
fg_unknown <- bind_rows(fg_unknown, select(fg_family_NA,-mean_wd))

# assign WD the PLOT level
fg_unknown$mean_wd <- rep(NA, length(fg_unknown$dbh)) 

# recombine
#DNM50 <- bind_rows(fg_spp, fg_genus, fg_family, fg_unknown)
ForestGEO <- bind_rows(fg_spp, fg_genus, fg_family, fg_unknown)

# assign plot-level mean WD to remaining stems with wd == NA
ForestGEO$mean_wd <- ifelse(is.na(ForestGEO$mean_wd), mean(ForestGEO$mean_wd, na.rm=T),
                            ForestGEO$mean_wd)

ggplot(ForestGEO, aes(x=mean_wd, fill=census)) +
  geom_histogram(binwidth=.05, alpha=.5, col= "white") + 
  facet_grid(census~.) + 
  theme_classic()
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# ForestPlots.net Plots
#---------------------------------------------------------------------------------------------#
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

#---------------------------------------------------------------------------------------------#
# separate first columns that apply to each census
dat_all04 <- select(dat_sep04, c('Tree ID':'WD Type','POM...22'))
# create separate genus & species columns
dat_all04$genus <- word(dat_all04$Species); dat_all04$species <- word(dat_all04$Species, 2)
# select columns of interest and rename
dat_all04_select <- select(dat_all04,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all04_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all04_select$quadrat <- as.factor(paste0(dat_all04_select$subplot_ID,'_sep04'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all05 <- select(dat_sep05, c('Tree ID':'WD Type','POM...22'))
dat_all05$genus <- word(dat_all05$Species); dat_all05$species <- word(dat_all05$Species, 2)
dat_all05_select <- select(dat_all05,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all05_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all05_select$quadrat <- as.factor(paste0(dat_all05_select$subplot_ID,'_sep05'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all30 <- select(dat_sep30, c('Tree ID':'WD Type','POM...22'))
dat_all30$genus <- word(dat_all30$Species); dat_all30$species <- word(dat_all30$Species, 2)
dat_all30_select <- select(dat_all30,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all30_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all30_select$quadrat <- as.factor(paste0(dat_all30_select$subplot_ID,'_sep30'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all08 <- select(dat_sep08, c('Tree ID':'WD Type','POM...22'))
dat_all08$genus <- word(dat_all08$Species); dat_all08$species <- word(dat_all08$Species, 2)
dat_all08_select <- select(dat_all08,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all08_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all08_select$quadrat <- as.factor(paste0(dat_all08_select$subplot_ID,'_sep08'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all09 <- select(dat_sep09, c('Tree ID':'WD Type','POM...22'))
dat_all09$genus <- word(dat_all09$Species); dat_all09$species <- word(dat_all09$Species, 2)
dat_all09_select <- select(dat_all09,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all09_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all09_select$quadrat <- as.factor(paste0(dat_all09_select$subplot_ID,'_sep09'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all10 <- select(dat_sep10, c('Tree ID':'WD Type','POM...22'))
dat_all10$genus <- word(dat_all10$Species); dat_all10$species <- word(dat_all10$Species, 2)
dat_all10_select <- select(dat_all10,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all10_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all10_select$quadrat <- as.factor(paste0(dat_all10_select$subplot_ID,'_sep10'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all01 <- select(dat_dnm01, c('Tree ID':'WD Type','POM...22'))
dat_all01$genus <- word(dat_all01$Species); dat_all01$species <- word(dat_all01$Species, 2)
dat_all01_select <- select(dat_all01,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all01_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all01_select$quadrat <- as.factor(paste0(dat_all01_select$subplot_ID,'dnm01'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all02 <- select(dat_dnm02, c('Tree ID':'WD Type','POM...22'))
dat_all02$genus <- word(dat_all02$Species); dat_all02$species <- word(dat_all02$Species, 2)
dat_all02_select <- select(dat_all02,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all02_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all02_select$quadrat <- as.factor(paste0(dat_all02_select$subplot_ID,'dnm02'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dat_all03 <- select(dat_dnm03, c('Tree ID':'WD Type','POM...22'))
dat_all03$genus <- word(dat_all03$Species); dat_all03$species <- word(dat_all03$Species, 2)
dat_all03_select <- select(dat_all03,'T1',X,Y,Family,genus,species,'Tree ID','WD Type','WD',"POM...22")
colnames(dat_all03_select) <- c("subplot_ID","plot_x","plot_y","family","genus","species","treeID","IDlevel","mean_wd","hom")
dat_all03_select$quadrat <- as.factor(paste0(dat_all03_select$subplot_ID,'dnm03'))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# select DBH1 and F1 from each census
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# add census columns
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# bind_cols to dat_all_select
#---------------------------------------------------------------------------------------------#
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
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# bind_rows of each census & add site column
#---------------------------------------------------------------------------------------------#
SEP_H <- bind_rows(sep_04_2001,sep_04_2008,sep_04_2014,sep_05_2001,sep_05_2008,sep_05_2014,sep_30_2001,sep_30_2010,sep_30_2015) 
SEP_H$site <- rep("SPKH",length(SEP_H$DBH_mm))
SEP_H$dbh <- SEP_H$DBH_mm * .1
SEP_H$stemID <- SEP_H$treeID
SEP_H <- select(SEP_H, -subplot_ID)
SEP_H <- select(SEP_H, -DBH_mm)

SEP_S <- bind_rows(sep_08_2001,sep_08_2009,sep_08_2014) 
SEP_S$site <- rep("SPKS",length(SEP_S$DBH_mm))
SEP_S$dbh <- SEP_S$DBH_mm * .1
SEP_S$stemID <- SEP_S$treeID
SEP_S <- select(SEP_S, -subplot_ID)
SEP_S <- select(SEP_S, -DBH_mm)

SEP_A <- bind_rows(sep_09_2000,sep_09_2009,sep_09_2014,sep_10_2000,sep_10_2009,sep_10_2014) 
SEP_A$site <- rep("SPKA",length(SEP_A$DBH_mm))
SEP_A$dbh <- SEP_A$DBH_mm * .1
SEP_A$stemID <- SEP_A$treeID
SEP_A <- select(SEP_A, -subplot_ID)
SEP_A <- select(SEP_A, -DBH_mm)

DNM1 <- bind_rows(dnm_01_2006,dnm_01_2013,dnm_01_2016) 
DNM1$site <- rep("DNM1",length(DNM1$DBH_mm))
DNM1$dbh <- DNM1$DBH_mm * .1
DNM1$stemID <- DNM1$treeID
DNM1 <- select(DNM1, -subplot_ID)
DNM1 <- select(DNM1, -DBH_mm)

DNM2 <- bind_rows(dnm_02_2006,dnm_02_2013,dnm_02_2016) 
DNM2$site <- rep("DNM2",length(DNM2$DBH_mm))
DNM2$dbh <- DNM2$DBH_mm * .1
DNM2$stemID <- DNM2$treeID
DNM2 <- select(DNM2, -subplot_ID)
DNM2 <- select(DNM2, -DBH_mm)

DNM3 <- bind_rows(dnm_03_2006,dnm_03_2013,dnm_03_2016) 
DNM3$site <- rep("DNM3",length(DNM3$DBH_mm))
DNM3$dbh <- DNM3$DBH_mm * .1
DNM3$stemID <- DNM3$treeID
DNM3 <- select(DNM3, -subplot_ID)
DNM3 <- select(DNM3, -DBH_mm)
#---------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------#
# Combine 
#---------------------------------------------------------------------------------------------#
forest_plots_clean <- bind_rows(SEP_H,SEP_S,SEP_A,DNM1,DNM2,DNM3)

#---------------------------------------------------------------------------------------------#
# Convert hom units to cm
#---------------------------------------------------------------------------------------------#
forest_plots_clean$hom <- forest_plots_clean$hom/10

#---------------------------------------------------------------------------------------------#
# Calculate stem_BA
#---------------------------------------------------------------------------------------------#
forest_plots_clean$stem_BA <- 0.00007854 * forest_plots_clean$dbh^2

#---------------------------------------------------------------------------------------------#
# Calculate Julian Date
#---------------------------------------------------------------------------------------------#
census <- read_csv("G:/My Drive/Harvard/Tall_trees_Borneo_project/Data/Census_Dates.csv")
julians <- census %>% select(census, Mid_date, JulianDate)

forest_plots_clean <- left_join(forest_plots_clean, julians, by = "census")

forest_plots_clean <- rename(forest_plots_clean, Date = Mid_date)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Add forest_plots Shade.Tol & soil columns for consistency
#---------------------------------------------------------------------------------------------#
forest_plots_clean$Shade.Tol <- rep("NA",length(forest_plots_clean$dbh))
forest_plots_clean$soil <- rep("NA",length(forest_plots_clean$dbh))
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# make sure forest_plots_clean & DNM50 have the same columns in the same order
colnames(forest_plots_clean)
#colnames(DNM50)
colnames(ForestGEO)
ForestGEO_final <- select(ForestGEO, -c(sp, tag))

colnames(ForestGEO_final)
# reorder forest_plots_clean columns to match DNM50
forest_plots_final <- select(forest_plots_clean, plot_x, plot_y, hom, quadrat, family, genus, species, 
                             treeID, stemID, dbh, Date, JulianDate, DFstatus, IDlevel, site, census, 
                             plot, Shade.Tol, soil, stem_BA, mean_wd)

firstcleandata <- bind_rows(forest_plots_final, ForestGEO_final)
#---------------------------------------------------------------------------------------------#

summary(firstcleandata)
ggplot(firstcleandata, aes(x=mean_wd)) + 
  geom_histogram(binwidth=.05, alpha=.5, col= "white") +
  facet_wrap(~site, ncol=2, scales = "free") + 
  theme_classic()

#---------------------------------------------------------------------------------------------#
# Convert hom == NA | hom == 0 to 130-------
#---------------------------------------------------------------------------------------------#
firstcleandata$hom <- ifelse(firstcleandata$hom == 0 | is.na(firstcleandata$hom), 130, firstcleandata$hom)
summary(firstcleandata$hom)

#---------------------------------------------------------------------------------------------#
# Clean family, genus, species data---------
#---------------------------------------------------------------------------------------------#
table(firstcleandata$family)
firstcleandata$family <- gsub("Unknown", "Indet", firstcleandata$family)

table(firstcleandata$genus)
length(firstcleandata$genus)
firstcleandata$genus <- gsub("Unidentified", "Indet", firstcleandata$genus)
length(firstcleandata$genus)

#---------------------------------------------------------------------------------------------#
# Convert all "unknown" species to "Indets"
#---------------------------------------------------------------------------------------------#
table(firstcleandata$species)
length(firstcleandata$species)
firstcleandata$species <- gsub("Unknown.*", "Indet", firstcleandata$species)
firstcleandata$species <- gsub("indet.*", "Indet", firstcleandata$species)
firstcleandata$species <- gsub("unknown.*", "Indet", firstcleandata$species)
length(firstcleandata$species)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Update DFstatus
#---------------------------------------------------------------------------------------------#
table(firstcleandata$DFstatus)

firstcleandata$DFstatus <- gsub("0", "y", firstcleandata$DFstatus)
firstcleandata$DFstatus <- gsub("dead", "y", firstcleandata$DFstatus)
firstcleandata$DFstatus<- gsub("broken below", "yy", firstcleandata$DFstatus)
firstcleandata$DFstatus <- gsub("^b.*", "yy", firstcleandata$DFstatus)
#---------------------------------------------------------------------------------------------#
# Prior = a stem that is not in the current census, but will appear in a future census. 
#---------------------------------------------------------------------------------------------#
firstcleandata$DFstatus <- gsub("prior", "yyy", firstcleandata$DFstatus) # EO EDIT - for lambir
firstcleandata$DFstatus <- gsub("stem_gone", "yyy", firstcleandata$DFstatus) # EO EDIT - for lambir
firstcleandata$DFstatus <- gsub("missing", "yyy", firstcleandata$DFstatus)

firstcleandata$DFstatus<- gsub("[^y]+", "A", firstcleandata$DFstatus)
firstcleandata$DFstatus <- gsub("yyy","missing", firstcleandata$DFstatus)
firstcleandata$DFstatus<- gsub("yy", "B", firstcleandata$DFstatus)
firstcleandata$DFstatus <- gsub("y", "D", firstcleandata$DFstatus)

table(firstcleandata$DFstatus)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# Fill in DBH for dead/broken below stems from previous census if NA or 0
#---------------------------------------------------------------------------------------------#
table(firstcleandata$DFstatus)

SPKA <- subset(firstcleandata, site == "SPKA"); table(SPKA$DFstatus)
SPKS <- subset(firstcleandata, site == "SPKS"); table(SPKS$DFstatus)
SPKH <- subset(firstcleandata, site == "SPKH"); table(SPKH$DFstatus)
LHC  <- subset(firstcleandata, plot == "LH_clay"); table(LHC$DFstatus)
LHS  <- subset(firstcleandata, plot == "LH_sandstone"); table(LHS$DFstatus)
LHl  <- subset(firstcleandata, plot == "LH_loam"); table(LHl$DFstatus)
LHfl <- subset(firstcleandata, plot == "LH_fineLoam"); table(LHfl$DFstatus)
DNM1 <- subset(firstcleandata, site == "DNM1"); table(DNM1$DFstatus)
DNM2 <- subset(firstcleandata, site == "DNM2"); table(DNM2$DFstatus)
DNM3 <- subset(firstcleandata, site == "DNM3"); table(DNM3$DFstatus)
DNM  <- subset(firstcleandata, site == "DNM50"); table(DNM$DFstatus)

# convert all DBH to NA for all ForestPlots where DFstatus == D & DBH == 0
dat_D <- subset(SPKA, DFstatus == "D"); table(dat_D$DFstatus)
dat_notD <- subset(SPKA, DFstatus != "D" | is.na(DFstatus)); table(dat_notD$DFstatus)
dat_D$dbh <- ifelse(dat_D$dbh == 0, NA, dat_D$dbh)
SPKA <- rbind(dat_D, dat_notD)

dat_D <- subset(SPKS, DFstatus == "D"); table(dat_D$DFstatus)
dat_notD <- subset(SPKS, DFstatus != "D" | is.na(DFstatus)); table(dat_notD$DFstatus)
dat_D$dbh <- ifelse(dat_D$dbh == 0, NA, dat_D$dbh)
SPKS <- rbind(dat_D, dat_notD)

dat_D <- subset(SPKH, DFstatus == "D"); table(dat_D$DFstatus)
dat_notD <- subset(SPKH, DFstatus != "D" | is.na(DFstatus)); table(dat_notD$DFstatus)
dat_D$dbh <- ifelse(dat_D$dbh == 0, NA, dat_D$dbh)
SPKH <- rbind(dat_D, dat_notD)

dat_D <- subset(DNM1, DFstatus == "D"); table(dat_D$DFstatus)
dat_notD <- subset(DNM1, DFstatus != "D" | is.na(DFstatus)); table(dat_notD$DFstatus)
dat_D$dbh <- ifelse(dat_D$dbh == 0, NA, dat_D$dbh)
DNM1 <- rbind(dat_D, dat_notD)

dat_D <- subset(DNM2, DFstatus == "D"); table(dat_D$DFstatus)
dat_notD <- subset(DNM2, DFstatus != "D" | is.na(DFstatus)); table(dat_notD$DFstatus)
dat_D$dbh <- ifelse(dat_D$dbh == 0, NA, dat_D$dbh)
DNM2 <- rbind(dat_D, dat_notD)

dat_D <- subset(DNM3, DFstatus == "D"); table(dat_D$DFstatus)
dat_notD <- subset(DNM3, DFstatus != "D" | is.na(DFstatus)); table(dat_notD$DFstatus)
dat_D$dbh <- ifelse(dat_D$dbh == 0, NA, dat_D$dbh)
DNM3 <- rbind(dat_D, dat_notD)

# test <- subset(DNM, DFstatus != "A")
# test <- subset(DNM, DFstatus == "A")
# test <- subset(DNM, DFstatus == "D")
# table(test$DFstatus)
# summary(test$dbh)

#---------------------------------------------------------------------------------------------#
# ForestGEO dead trees are mostly == NA DBH
#---------------------------------------------------------------------------------------------#
dnm_census2 <- subset(DNM, census == "census_2019"); dim(dnm_census2) #263911
dnm_census1 <- subset(DNM, census == "census_2011_15"); dim(dnm_census1) #256724

t1dbh <- select(dnm_census1, stemID, dbh)

# add previous census dbh to next census
temp_dat <- left_join(dnm_census2, t1dbh, by = "stemID"); dim(temp_dat)
summary(temp_dat$dbh.x)
temp_dat$dbh.x <- ifelse(is.na(temp_dat$dbh.x), temp_dat$dbh.y, temp_dat$dbh.x)
summary(temp_dat$dbh.x); dim(temp_dat)

# reassign to original dat
dnm_census2$dbh <- temp_dat$dbh.x

# rbind into single site again
DNM50 <- rbind(dnm_census1, dnm_census2)

DNM_update <- subset(DNM50, DFstatus != "A" & census == "census_2019")
table(DNM_update$DFstatus)
table(DNM_update$census)
summary(DNM_update$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# do for each set of censuses (Dead at census 2 & census 3 & census 4)
#---------------------------------------------------------------------------------------------#
lhc_census1 <- subset(LHC, plot == "LH_clay" & census == "census_1991"); dim(lhc_census1) #28894
lhc_census2 <- subset(LHC, plot == "LH_clay" & census == "census_1997"); dim(lhc_census2) #28894
lhc_census3 <- subset(LHC, plot == "LH_clay" & census == "census_2003"); dim(lhc_census3) #28894
lhc_census4 <- subset(LHC, plot == "LH_clay" & census == "census_2007_08"); dim(lhc_census4) #28894

t1dbh <- select(lhc_census1, stemID, dbh)

# add previous census dbh to next census
temp_dat1 <- left_join(lhc_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
lhc_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(lhc_census2, stemID, dbh)

# add previous census dbh to next census
temp_dat2 <- left_join(lhc_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
lhc_census3$dbh <- temp_dat2$dbh.x

t3dbh <- select(lhc_census3, stemID, dbh)

# add previous census dbh to next census
temp_dat3 <- left_join(lhc_census4, t3dbh, by = "stemID"); dim(temp_dat3)
summary(temp_dat3$dbh.x)
temp_dat3$dbh.x <- ifelse(is.na(temp_dat3$dbh.x), temp_dat3$dbh.y, temp_dat3$dbh.x)
summary(temp_dat3$dbh.x); dim(temp_dat3)
# reassign to original dat
lhc_census4$dbh <- temp_dat3$dbh.x

# rbind into single site again
dim(LHC)
LHC <- rbind(lhc_census1, lhc_census2, lhc_census3, lhc_census4)
dim(LHC)

LHC_update <- subset(LHC, DFstatus == "D")
table(LHC_update$DFstatus)
summary(LHC_update$dbh)

#---------------------------------------------------------------------------------------------#
# do for each set of censuses (Dead at census 2 & census 3 & census 4)
#---------------------------------------------------------------------------------------------#
lhs_census1 <- subset(LHS, plot == "LH_sandstone" & census == "census_1991"); dim(lhs_census1) #316402
lhs_census2 <- subset(LHS, plot == "LH_sandstone" & census == "census_1997"); dim(lhs_census2) #316402
lhs_census3 <- subset(LHS, plot == "LH_sandstone" & census == "census_2003"); dim(lhs_census3) #316402
lhs_census4 <- subset(LHS, plot == "LH_sandstone" & census == "census_2007_08"); dim(lhs_census4) #316402

t1dbh <- select(lhs_census1, stemID, dbh)

# add previous census dbh to next census
temp_dat1 <- left_join(lhs_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
lhs_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(lhs_census2, stemID, dbh)

# add previous census dbh to next census
temp_dat2 <- left_join(lhs_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
lhs_census3$dbh <- temp_dat2$dbh.x

t3dbh <- select(lhs_census3, stemID, dbh)

# add previous census dbh to next census
temp_dat3 <- left_join(lhs_census4, t3dbh, by = "stemID"); dim(temp_dat3)
summary(temp_dat3$dbh.x)
temp_dat3$dbh.x <- ifelse(is.na(temp_dat3$dbh.x), temp_dat3$dbh.y, temp_dat3$dbh.x)
summary(temp_dat3$dbh.x); dim(temp_dat3)
# reassign to original dat
lhs_census4$dbh <- temp_dat3$dbh.x

# rbind into single site again
dim(LHS)
LHS <- rbind(lhs_census1, lhs_census2, lhs_census3, lhs_census4)
dim(LHS)

LHS_update <- subset(LHS, DFstatus == "D")
table(LHS_update$DFstatus)
summary(LHS_update$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# do for each set of censuses (Dead at census 2 & census 3 & census 4)
#---------------------------------------------------------------------------------------------#
lhl_census1 <- subset(LHl, census == "census_1991"); dim(lhl_census1) #82168
lhl_census2 <- subset(LHl, census == "census_1997"); dim(lhl_census2) #82168
lhl_census3 <- subset(LHl, census == "census_2003"); dim(lhl_census3) #82168
lhl_census4 <- subset(LHl, census == "census_2007_08"); dim(lhl_census4) #82168

t1dbh <- select(lhl_census1, stemID, dbh)

# add previous census dbh to next census
temp_dat1 <- left_join(lhl_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
lhl_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(lhl_census2, stemID, dbh)

# add previous census dbh to next census
temp_dat2 <- left_join(lhl_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
lhl_census3$dbh <- temp_dat2$dbh.x

t3dbh <- select(lhl_census3, stemID, dbh)

# add previous census dbh to next census
temp_dat3 <- left_join(lhl_census4, t3dbh, by = "stemID"); dim(temp_dat3)
summary(temp_dat3$dbh.x)
temp_dat3$dbh.x <- ifelse(is.na(temp_dat3$dbh.x), temp_dat3$dbh.y, temp_dat3$dbh.x)
summary(temp_dat3$dbh.x); dim(temp_dat3)
# reassign to original dat
lhl_census4$dbh <- temp_dat3$dbh.x

# rbind into single site again
dim(LHl)
LHl <- rbind(lhl_census1, lhl_census2, lhl_census3, lhl_census4)
dim(LHl)

LHl_update <- subset(LHl, DFstatus == "D")
table(LHl_update$DFstatus)
summary(LHl_update$dbh)
#---------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------#
# do for each set of censuses (Dead at census 2 & census 3 & census 4)
#---------------------------------------------------------------------------------------------#
lhfl_census1 <- subset(LHfl, census == "census_1991"); dim(lhfl_census1) #106213
lhfl_census2 <- subset(LHfl, census == "census_1997"); dim(lhfl_census2) #106213
lhfl_census3 <- subset(LHfl, census == "census_2003"); dim(lhfl_census3) #106213
lhfl_census4 <- subset(LHfl, census == "census_2007_08"); dim(lhfl_census4) #106213

t1dbh <- select(lhfl_census1, stemID, dbh)

# add previous census dbh to next census
temp_dat1 <- left_join(lhfl_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
lhfl_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(lhfl_census2, stemID, dbh)

# add previous census dbh to next census
temp_dat2 <- left_join(lhfl_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
lhfl_census3$dbh <- temp_dat2$dbh.x

t3dbh <- select(lhfl_census3, stemID, dbh)

# add previous census dbh to next census
temp_dat3 <- left_join(lhfl_census4, t3dbh, by = "stemID"); dim(temp_dat3)
summary(temp_dat3$dbh.x)
temp_dat3$dbh.x <- ifelse(is.na(temp_dat3$dbh.x), temp_dat3$dbh.y, temp_dat3$dbh.x)
summary(temp_dat3$dbh.x); dim(temp_dat3)
# reassign to original dat
lhfl_census4$dbh <- temp_dat3$dbh.x

# rbind into single site again
dim(LHfl)
LHfl <- rbind(lhfl_census1, lhfl_census2, lhfl_census3, lhfl_census4)
dim(LHfl)

LHfl_update <- subset(LHfl, DFstatus == "D")
table(LHfl_update$DFstatus)
summary(LHfl_update$dbh)
#---------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------#
# ForestPlots.net dead trees are mostly == 0 DBH
#---------------------------------------------------------------------------------------------#
# do for each set of censuses (Dead at census 2 & census 3)
#---------------------------------------------------------------------------------------------#
spka9_census1 <- subset(SPKA, census == "09_census_2001"); dim(spka9_census1) #4503
spka9_census2 <- subset(SPKA, census == "09_census_2009"); dim(spka9_census2) #4503
spka9_census3 <- subset(SPKA, census == "09_census_2014"); dim(spka9_census3) #4503

spka10_census1 <- subset(SPKA, census == "10_census_2001"); dim(spka10_census1) #4341
spka10_census2 <- subset(SPKA, census == "10_census_2009"); dim(spka10_census2) #4341
spka10_census3 <- subset(SPKA, census == "10_census_2014"); dim(spka10_census3) #4341

t1dbh <- select(spka9_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(spka9_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
spka9_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(spka9_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(spka9_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
spka9_census3$dbh <- temp_dat2$dbh.x


t1dbh <- select(spka10_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(spka10_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
spka10_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(spka10_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(spka10_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
spka10_census3$dbh <- temp_dat2$dbh.x

# rbind into single site again
SPKA <- rbind(spka9_census1, spka9_census2, spka9_census3, spka10_census1, spka10_census2, spka10_census3)

SPKA_update <- subset(SPKA, DFstatus != "A")
table(SPKA_update$DFstatus)
summary(SPKA_update$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
spks8_census1 <- subset(SPKS, census == "08_census_2001"); dim(spks8_census1) #6274
spks8_census2 <- subset(SPKS, census == "08_census_2009"); dim(spks8_census2) #6274
spks8_census3 <- subset(SPKS, census == "08_census_2014"); dim(spks8_census3) #6274

t1dbh <- select(spks8_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(spks8_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
spks8_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(spks8_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(spks8_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
spks8_census3$dbh <- temp_dat2$dbh.x

# rbind into single site again
SPKS <- rbind(spks8_census1, spks8_census2, spks8_census3)

SPKS_update <- subset(SPKS, DFstatus != "A")
table(SPKS_update$DFstatus)
summary(SPKS_update$dbh)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
spkh4_census1 <- subset(SPKH, census == "04_census_2001"); dim(spkh4_census1) #6532
spkh4_census2 <- subset(SPKH, census == "04_census_2008"); dim(spkh4_census2) #6532
spkh4_census3 <- subset(SPKH, census == "04_census_2014"); dim(spkh4_census3) #6532

spkh5_census1 <- subset(SPKH, census == "05_census_2001"); dim(spkh5_census1) #8413
spkh5_census2 <- subset(SPKH, census == "05_census_2008"); dim(spkh5_census2) #8413
spkh5_census3 <- subset(SPKH, census == "05_census_2014"); dim(spkh5_census3) #8413

spkh30_census1 <- subset(SPKH, census == "30_census_2001"); dim(spkh30_census1) #9420
spkh30_census2 <- subset(SPKH, census == "30_census_2010"); dim(spkh30_census2) #9420
spkh30_census3 <- subset(SPKH, census == "30_census_2015"); dim(spkh30_census3) #9420

t1dbh <- select(spkh4_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(spkh4_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
spkh4_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(spkh4_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(spkh4_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
spkh4_census3$dbh <- temp_dat2$dbh.x


t1dbh <- select(spkh5_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(spkh5_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
spkh5_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(spkh5_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(spkh5_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
spkh5_census3$dbh <- temp_dat2$dbh.x


t1dbh <- select(spkh30_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(spkh30_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
spkh30_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(spkh30_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(spkh30_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
spkh30_census3$dbh <- temp_dat2$dbh.x

# rbind into single site again
SPKH <- rbind(spkh4_census1, spkh4_census2, spkh4_census3, 
              spkh5_census1, spkh5_census2, spkh5_census3,
              spkh30_census1, spkh30_census2, spkh30_census3)

SPKH_update <- subset(SPKH, DFstatus != "A")
table(SPKH_update$DFstatus)
summary(SPKH_update$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dnm1_census1 <- subset(DNM1, census == "01_census_2006"); dim(dnm1_census1) #453
dnm1_census2 <- subset(DNM1, census == "01_census_2013"); dim(dnm1_census2) #453
dnm1_census3 <- subset(DNM1, census == "01_census_2016"); dim(dnm1_census3) #453

t1dbh <- select(dnm1_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(dnm1_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
dnm1_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(dnm1_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(dnm1_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
dnm1_census3$dbh <- temp_dat2$dbh.x

# rbind into single site again
DNM1 <- rbind(dnm1_census1, dnm1_census2, dnm1_census3)

DNM1_update <- subset(DNM1, DFstatus != "A")
table(DNM1_update$DFstatus)
summary(DNM1_update$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dnm2_census1 <- subset(DNM2, census == "02_census_2006"); dim(dnm2_census1) #529
dnm2_census2 <- subset(DNM2, census == "02_census_2013"); dim(dnm2_census2) #529
dnm2_census3 <- subset(DNM2, census == "02_census_2016"); dim(dnm2_census3) #529

t1dbh <- select(dnm2_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(dnm2_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
dnm2_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(dnm2_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(dnm2_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
dnm2_census3$dbh <- temp_dat2$dbh.x

# rbind into single site again
DNM2 <- rbind(dnm2_census1, dnm2_census2, dnm2_census3)

DNM2_update <- subset(DNM2, DFstatus != "A")
table(DNM2_update$DFstatus)
summary(DNM2_update$dbh)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
dnm3_census1 <- subset(DNM3, census == "03_census_2006"); dim(dnm3_census1) #536
dnm3_census2 <- subset(DNM3, census == "03_census_2013"); dim(dnm3_census2) #536
dnm3_census3 <- subset(DNM3, census == "03_census_2016"); dim(dnm3_census3) #536

t1dbh <- select(dnm3_census1, stemID, dbh)
# add previous census dbh to next census
temp_dat1 <- left_join(dnm3_census2, t1dbh, by = "stemID"); dim(temp_dat1)
summary(temp_dat1$dbh.x)
temp_dat1$dbh.x <- ifelse(is.na(temp_dat1$dbh.x), temp_dat1$dbh.y, temp_dat1$dbh.x)
summary(temp_dat1$dbh.x); dim(temp_dat1)
# reassign to original dat
dnm3_census2$dbh <- temp_dat1$dbh.x

t2dbh <- select(dnm3_census2, stemID, dbh)
# add previous census dbh to next census
temp_dat2 <- left_join(dnm3_census3, t2dbh, by = "stemID"); dim(temp_dat2)
summary(temp_dat2$dbh.x)
temp_dat2$dbh.x <- ifelse(is.na(temp_dat2$dbh.x), temp_dat2$dbh.y, temp_dat2$dbh.x)
summary(temp_dat2$dbh.x); dim(temp_dat2)
# reassign to original dat
dnm3_census3$dbh <- temp_dat2$dbh.x

# rbind into single site again
DNM3 <- rbind(dnm3_census1, dnm3_census2, dnm3_census3)

DNM3_update <- subset(DNM3, DFstatus != "A")
table(DNM3_update$DFstatus)
summary(DNM3_update$dbh)
#---------------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# DNM50 - remove outliers based on extreme growth rates (relative growth rate)
#---------------------------------------------------------------------------------------------#
# order based on annual increment and exclude top and bottom 0.5% 
# plot and check, use that to bind to others for full dataset
#---------------------------------------------------------------------------------------------#
table(DNM50$census)
table(DNM50$DFstatus)

census1 <- filter(DNM50, census == "census_2011_15")
census2 <- filter(DNM50, census == "census_2019")

# check the number of unique stems in each dataset and compare between datasets
length(unique(census1$stemID)); dim(census1)
length(unique(census2$stemID)); dim(census2)

# restrict each census to stems that are included in both datasets using inner_join(), a dplyr function
DNM50_2011_2019 <- inner_join(census1, census2, by="stemID")
dim(DNM50_2011_2019) 

time <- (DNM50_2011_2019$JulianDate.y-DNM50_2011_2019$JulianDate.x)/365
size2 <- DNM50_2011_2019$dbh.y
size1 <- DNM50_2011_2019$dbh.x

# calculate growth rates: 
DNM50_2011_2019$annual_increment <- (size2 - size1)/time
DNM50_2011_2019$relative_gr      <- (log(size2) - log(size1))/time

summary(DNM50_2011_2019$annual_increment)
summary(DNM50_2011_2019$relative_gr)
quantile(DNM50_2011_2019$relative_gr, 0.9999, na.rm=T)

#---------------------------------------------------------------------------------------------#
p99 <- quantile(DNM50_2011_2019$relative_gr, prob=0.995, na.rm=T); p99
p01 <- quantile(DNM50_2011_2019$relative_gr, prob=0.015, na.rm=T); p01
# set aside stems to remove: 
DNM50_cleaned <- filter(DNM50_2011_2019, relative_gr < p99 & relative_gr > p01)

dim(DNM50_2011_2019) 
dim(DNM50_cleaned)
dim(DNM50_2011_2019)[[1]] - dim(DNM50_cleaned)[[1]]
dim(DNM50_cleaned)[[1]]/dim(DNM50_2011_2019)[[1]]*100
#---------------------------------------------------------------------------------------------#
# 5146 stems removed (98.0% stems retained) 
#---------------------------------------------------------------------------------------------#

# take a look at the values
par(mfrow=c(1,2))
hist(DNM50_2011_2019$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM50_2011_2019$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

hist(DNM50_cleaned$relative_gr, xlab="Relative growth rate (% yr-1)", col="grey", main="")
hist(DNM50_cleaned$annual_increment, xlab="Annual increment (cm)", col="grey", main="")

#DNM50 Plot----------
# look at the change in DBH from census 1 to census 2
par(mfrow=c(1,2))
plot(DNM50_2011_2019$dbh.x,DNM50_2011_2019$dbh.y, pch=19,
     xlab="DBH DNM50 2011 (cm)", ylab="DBH DNM50 2019 (cm)")
abline(0,1, col="red", lwd=2)

plot(DNM50_cleaned$dbh.x,DNM50_cleaned$dbh.y, pch=19, 
     xlab="DBH DNM50 2011 (cm)", ylab="DBH DNM50 2019 (cm)")
#add a 1:1 line to see more obviously how different DBH values are from census 1 to census 2
abline(0,1, col="red", lwd=2) 
#---------------------------------------------------------------------------------------------#
DNM50_census1 <- DNM50_cleaned[,c(1:21)] 
DNM50_census2 <- DNM50_cleaned[,c(22:29,9,30:41)]
colnames(DNM50_census1) <- colnames(DNM50)
colnames(DNM50_census2) <- colnames(DNM50)

DNM50_v2 <- rbind(DNM50_census1, DNM50_census2)
head(DNM50_v2) 
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#









#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# CHECK FOR DUPLICATES
#---------------------------------------------------------------------------------------------#

library(here)
library(skimr)
library(janitor)
library(stringr)

#---------------------------------------------------------------------------------------------#
# LAMBIR
#---------------------------------------------------------------------------------------------#
table(LHC$census)
LHP91 <- filter(LHC, census == "census_1991")
LHP91$stemID[duplicated(LHP91$stemID)] 
LHP97 <- filter(LHC, census == "census_1997")
LHP97$stemID[duplicated(LHP97$stemID)] 
LHP03 <- filter(LHC, census == "census_2003")
LHP03$stemID[duplicated(LHP03$stemID)] 
LHP08 <- filter(LHC, census == "census_2007_08")
LHP08$stemID[duplicated(LHP08$stemID)] 

LHP91 <- filter(LHS, census == "census_1991")
LHP91$stemID[duplicated(LHP91$stemID)] 
LHP97 <- filter(LHS, census == "census_1997")
LHP97$stemID[duplicated(LHP97$stemID)] 
LHP03 <- filter(LHS, census == "census_2003")
LHP03$stemID[duplicated(LHP03$stemID)] 
LHP08 <- filter(LHS, census == "census_2007_08")
LHP08$stemID[duplicated(LHP08$stemID)] 

LHP91 <- filter(LHl, census == "census_1991")
LHP91$stemID[duplicated(LHP91$stemID)] 
LHP97 <- filter(LHl, census == "census_1997")
LHP97$stemID[duplicated(LHP97$stemID)] 
LHP03 <- filter(LHl, census == "census_2003")
LHP03$stemID[duplicated(LHP03$stemID)] 
LHP08 <- filter(LHl, census == "census_2007_08")
LHP08$stemID[duplicated(LHP08$stemID)] 

LHP91 <- filter(LHfl, census == "census_1991")
LHP91$stemID[duplicated(LHP91$stemID)] 
LHP97 <- filter(LHfl, census == "census_1997")
LHP97$stemID[duplicated(LHP97$stemID)] 
LHP03 <- filter(LHfl, census == "census_2003")
LHP03$stemID[duplicated(LHP03$stemID)] 
LHP08 <- filter(LHfl, census == "census_2007_08")
LHP08$stemID[duplicated(LHP08$stemID)] 

# NO DUPLICATES #

#LHP52: Check for Strange Taxonomic Entries--------
table(LHC$family)
table(LHC$genus)
table(LHC$species)
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# DNM50
#---------------------------------------------------------------------------------------------#
table(DNM50_v2$census)
DNM5011 <- filter(DNM50_v2, census == "census_2011_15")
DNM5011$stemID[duplicated(DNM5011$stemID)] 
DNM5019 <- filter(DNM50_v2, census == "census_2019")
DNM5019$stemID[duplicated(DNM5019$stemID)] 
#---------------------------------------------------------------------------------------------#
# Use stemID here instead of treeID, although I left the treeID check in case we do want to 
# filter stems where there are more than N duplicates (e.g. 4 in the example below)
#---------------------------------------------------------------------------------------------#
#DNM50: Check for Strange Taxonomic Entries--------
table(DNM50_v2$family)
table(DNM50_v2$genus)
table(DNM50_v2$species)

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
# DNM1
#---------------------------------------------------------------------------------------------#
table(DNM1$census)
DNM106 <- filter(DNM1, census == "01_census_2006")
DNM106$stemID[duplicated(DNM106$stemID)] # EO EDIT: you're using treeID AND stemID here, not sure
DNM113 <- filter(DNM1, census == "01_census_2013")
DNM113$stemID[duplicated(DNM113$stemID)]
DNM116 <- filter(DNM1, census == "01_census_2016")
DNM116$stemID[duplicated(DNM116$stemID)]

#DNM1: Check for Strange Taxonomic Entries--------
table(DNM1$family)
table(DNM1$genus)
table(DNM1$species)

#---------------------------------------------------------------------------------------------#
# DNM2
#---------------------------------------------------------------------------------------------#
table(DNM2$census)
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

#---------------------------------------------------------------------------------------------#
# DNM3
#---------------------------------------------------------------------------------------------#
table(DNM3$census)
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

#---------------------------------------------------------------------------------------------#
# SPKA
#---------------------------------------------------------------------------------------------#
table(SPKA$census)

SPKA901 <- filter(SPKA, census == "09_census_2001")
SPKA901$stemID[duplicated(SPKA901$stemID)]
SPKA909 <- filter(SPKA, census == "09_census_2009")
SPKA909$stemID[duplicated(SPKA909$stemID)]
SPKA914 <- filter(SPKA, census == "09_census_2014")
SPKA914$stemID[duplicated(SPKA914$stemID)]

SPKA1001 <- filter(SPKA, census == "10_census_2001")
SPKA1001$stemID[duplicated(SPKA1001$stemID)]
SPKA1009 <- filter(SPKA, census == "10_census_2009")
SPKA1009$stemID[duplicated(SPKA1009$stemID)]
SPKA1014 <- filter(SPKA, census == "10_census_2014")
SPKA1014$stemID[duplicated(SPKA1014$stemID)]

#Check for Strange Taxonomic Entries--------
table(SPKA$family)
table(SPKA$genus)
table(SPKA$species)

#---------------------------------------------------------------------------------------------#
# SPKH
#---------------------------------------------------------------------------------------------#
table(SPKH$census)

SPKH401 <- filter(SPKH, census == "04_census_2001")
SPKH401$stemID[duplicated(SPKH401$stemID)]
SPKH408 <- filter(SPKH, census == "04_census_2008")
SPKH408$stemID[duplicated(SPKH408$stemID)]
SPKH414 <- filter(SPKH, census == "04_census_2014")
SPKH414$stemID[duplicated(SPKH414$stemID)]

SPKH501 <- filter(SPKH, census == "05_census_2001")
SPKH501$stemID[duplicated(SPKH501$stemID)]
SPKH508 <- filter(SPKH, census == "05_census_2008")
SPKH508$stemID[duplicated(SPKH508$stemID)]
SPKH514 <- filter(SPKH, census == "05_census_2014")
SPKH514$stemID[duplicated(SPKH514$stemID)]

SPKH3001 <- filter(SPKH, census == "30_census_2001")
SPKH3001$stemID[duplicated(SPKH3001$stemID)]
SPKH3010 <- filter(SPKH, census == "30_census_2010")
SPKH3010$stemID[duplicated(SPKH3010$stemID)]
SPKH3015 <- filter(SPKH, census == "30_census_2015")
SPKH3015$stemID[duplicated(SPKH3015$stemID)]

#SPKH: Check for Strange Taxonomic Entries--------
table(SPKH$family)
table(SPKH$genus)
table(SPKH$species)

#---------------------------------------------------------------------------------------------#
# SPKS8
#---------------------------------------------------------------------------------------------#
table(SPKS$census)
SPKS801 <- filter(SPKS, census == "08_census_2001")
SPKS801$stemID[duplicated(SPKS801$stemID)]
SPKS809 <- filter(SPKS, census == "08_census_2009")
SPKS809$stemID[duplicated(SPKS809$stemID)]
SPKS814 <- filter(SPKS, census == "08_census_2014")
SPKS814$stemID[duplicated(SPKS814$stemID)]

#Check for Strange Taxonomic Entries--------
table(SPKS$family)
table(SPKS$genus)
table(SPKS$species)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------------#
# Combine all plots
#---------------------------------------------------------------------------------------------#
second_clean_dat <- rbind(DNM1, DNM2, DNM3, DNM50_v2, LHC, LHS, LHl, LHfl, SPKA, SPKS, SPKH)
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
length(firstcleandata$species)
length(second_clean_dat$species)

table(firstcleandata$plot)
table(second_clean_dat$plot)

table(firstcleandata$site)
table(second_clean_dat$site)
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
#write.csv(firstcleandata, "G:/My Drive/Harvard/Emergent_project/Data/data_first_clean.csv")
#write.csv(firstcleandata, "G:/My Drive/Harvard/Plot_Data/clean_inventory_data/data_first_clean.csv")
write.csv(second_clean_dat, "G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv")
#---------------------------------------------------------------------------------------------#
