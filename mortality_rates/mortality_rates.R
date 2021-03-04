#----------------------------------------------------------------------------------------#
# Read [clean] inventory plot data and calculate mortality rates
#----------------------------------------------------------------------------------------#
setwd("G:/My Drive") # Google Drive

library(dplyr); library(ggplot2); library(viridis); library(stringr)
library(ggfortify); library(cowplot); require(data.table); 
library(tidyverse)
library(raster); library(rgdal); library(sp); library(GISTools); library(sf)
library(vegan); library(RColorBrewer); library(splus2R); library(fgeo)
# install.packages("devtools")
# devtools::install_github("forestgeo/ctfs")
# library(ctfs)
# edit(mortality.calculation)

pal <- brewer.pal(3, "Dark2")
pal2 <- brewer.pal(5, "BrBG")

#---------------------------------------------------------------------------------------------#
# Load data                                                                                   # 
#---------------------------------------------------------------------------------------------#
#data <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv")
clean_dat <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/mort_dat.csv")

colnames(clean_dat)
#clean_dat <-rename (data, censusID = census, dbh = dbh, date = JulianDate, status = DFstatus)
#---------------------------------------------------------------------------------------------#
table(clean_dat$status)
#clean_dat_dead <- filter(clean_dat, status == "D" | status == "B") # excludes "missing"
#clean_dat <- filter(clean_dat, status == "A")

length(unique(clean_dat$treeID))

table(clean_dat$site)
table(clean_dat$plot)
#----------------------------------------------------------------------------------------#
# update site column so that it differentiates between lambir soil types
#----------------------------------------------------------------------------------------#
clean_dat$site <- substr(clean_dat$plot, 1, 4)
table(clean_dat$site)
clean_dat$site <- factor(clean_dat$site, 
                         levels = c("LH_c","LH_l","LH_f","LH_s","SPKA","DNM5","DNM3","SPKS","DNM2","DNM1","SPKH"),
                         labels = c("LHPc","LHPl","LHPf","LHPs","SPKA","DNM50","DNM3","SPKS","DNM2","DNM1","SPKH"))
table(clean_dat$site)
#----------------------------------------------------------------------------------------#

# reorganize data to define status based on treeID, not stemID 
# if ALL stems == Dead, tree is dead
# if only some stems == Dead, tree is alive
# edit(pick_main_stem)
# pick_main_stem()

# create clean_mort_dat.R file 
# select the largest stem (see code from Naomi)
# how many stems? just largest?


#----------------------------------------------------------------------------------------#
# http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/packages/CTFS.pdf
#----------------------------------------------------------------------------------------#
# search for "mortality.calculation'
# N  = number of trees at the first census
# S  = number of trees that survived to the second census, N - D
# meantime = interval between censuses in years for trees in the first census
#----------------------------------------------------------------------------------------#
# also see: 
# fgeo::recruitment_ctfs, mortality_ctfs, growth_ctfs
# ?mortality_ctfs
# edit(mortality_ctfs)
#----------------------------------------------------------------------------------------#


mortality_rate <- function (N, S, meantime) 
{
  lower.ci = find.climits(N, (N - S), kind = "lower")
  upper.ci = find.climits(N, (N - S), kind = "upper")
  mort.rate = (log(N) - log(S))/meantime
  upper.rate = (log(N) - log(N - upper.ci))/meantime
  lower.rate = (log(N) - log(N - lower.ci))/meantime
  mort.rate[S == 0] = upper.rate[S == 0] = Inf
  upper.rate[upper.ci == N] = Inf
  lower.rate[lower.ci == N] = 0
  mort.rate[N == 0] = lower.rate[N == 0] = upper.rate[N == 
                                                        0] = NA
  if (is.null(dim(N))) 
    return(data.frame(N = N, S = S, D = N - S, rate = mort.rate, 
                      lowerCI = lower.rate, upperCI = upper.rate, time = meantime))
  else return(list(N = N, S = S, D = N - S, rate = mort.rate, 
                   lowerCI = lower.rate, upperCI = upper.rate, time = meantime))
}
#----------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
# see: https://www.forestplots.net/upload/ManualsEnglish/RAINFOR_data_codes_EN.pdf
table(clean_dat$status)

dat <- clean_dat
#dat <- subset(clean_dat, dbh >= 2 | is.na(dbh))
# dat <- subset(clean_dat, dbh >= 4 | is.na(dbh)) #dbh >= 5
# dat <- subset(clean_dat, dbh >= 10 | is.na(dbh))
# dat <- subset(clean_dat, dbh >= 30 | is.na(dbh))
table(dat$status)

# test <- subset(clean_dat, site == "DNM50" & status == "D")
# test <- subset(clean_dat, site == "LHPc" & status == "D")
# table(test$status)
# summary(test$dbh)

dem_dat <- dat
#dem_dat <- filter(dat, status != "missing" & status != "B")
#dem_dat <- filter(dat, status != "missing" & status != "prior")
#dem_dat <- filter(dat, status != "missing" & status != "prior" & status != "stem_gone")
#dem_dat <- filter(dat, status != "missing" & status != "prior" & status != "stem_gone" & status != "B")
#dem_dat <- dat

table(dem_dat$status)
#dem_dat$status <- gsub("B", "A", dem_dat$status)
#dem_dat$status <- gsub("stem_gone", "D", dem_dat$status)
#dem_dat$status <- gsub("B", "D", dem_dat$status)
#dem_dat$status <- gsub("missing", "A", dem_dat$status)
table(dem_dat$status)

# test <- filter(dem_dat, DFstatus == "prior"); tail(test)
# table(dem_dat$DFstatus)
# test <- filter(dem_dat, DFstatus == "D"); dim(test); summary(test$dbh)

colnames(dem_dat)
# Change JulianDate & DFstatus to date & status
#colnames(dem_dat)[13:14] <- c("date","status")
#dem_dat$hom <- rep(130, length(dem_dat$dbh))

table(dem_dat$status)

(table(dem_dat$status)[[2]]/table(dem_dat$status)[[1]])*100 # 11.94


# # DANUM CLEAN STEMS TO USE - from Lucie data cleaning based on growth rates
# cleaned_dat <- read.csv("G:/My Drive/Harvard/Lucie_Internship/Data/Census_2_dnm50_clean_main_stem.csv")
# clean_stems <- cleaned_dat$stemID
#dnm_dat <- subset(uncleaned_dat, stemID %in% clean_stems)

# dnm_dat <- subset(ForestGEO_dat, site == "DNM50_NA")
# dnm_fgeo <- fgeo.tool::pick_main_stem(dnm_dat)
dnm_fgeo <- subset(dem_dat, site == "DNM50")
table(dnm_fgeo$census)
table(dnm_fgeo$status)
(table(dnm_fgeo$status)[[2]]/table(dnm_fgeo$status)[[1]])*100 # 5.06

# table(dnm_fgeo$census)
# DNM_c1 <- subset(dnm_fgeo, census == "census_2011_15")
# DNM_c2 <- subset(dnm_fgeo, census == "census_2019")
# table(DNM_c1$status); table(DNM_c2$status)

# entire lambir 52 ha plot
lh_fgeo <- subset(dem_dat, plot == "LH_clay" | plot == "LH_fineLoam" | plot == "LH_loam" | plot == "LH_sandstone")
table(lh_fgeo$census)
table(lh_fgeo$status)
(table(lh_fgeo$status)[[2]]/table(lh_fgeo$status)[[1]])*100 # 14.31

# lhc_dat <- subset(ForestGEO_dat, site == "LHPc")
# lhc_fgeo <- fgeo.tool::pick_main_stem(lhc_dat)
lhc_fgeo <- subset(dem_dat, plot == "LH_clay")
table(lhc_fgeo$census)
table(lhc_fgeo$status)
(table(lhc_fgeo$status)[[2]]/table(lhc_fgeo$status)[[1]])*100 # 20.52

# lhs_dat <- subset(ForestGEO_dat, site == "LHPs")
# lhs_fgeo <- fgeo.tool::pick_main_stem(lhs_dat)
lhs_fgeo <- subset(dem_dat, plot == "LH_sandstone")
table(lhs_fgeo$census)
table(lhs_fgeo$status)
(table(lhs_fgeo$status)[[2]]/table(lhs_fgeo$status)[[1]])*100 # 11.14

# FOREST PLOTS MORTALITY 
table(dem_dat$status)
# see: https://www.forestplots.net/upload/ManualsEnglish/RAINFOR_data_codes_EN.pdf
# ForestPlots_dat$status <- ifelse(ForestPlots_dat$DFstatus == '0', "D","A")
# table(ForestPlots_dat$status)

table(dem_dat$site)
SPKA <- subset(dem_dat, site == "SPKA"); table(SPKA$status)
SPKS <- subset(dem_dat, site == "SPKS"); table(SPKS$status)
SPKH <- subset(dem_dat, site == "SPKH"); table(SPKH$status)
#----------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------#
# separate sites by census
#----------------------------------------------------------------------------------------#
table(dnm_fgeo$census)
DNM_c1 <- subset(dnm_fgeo, census == "census_2011_15")
DNM_c2 <- subset(dnm_fgeo, census == "census_2019")

table(lh_fgeo$census)
LH_c1 <- subset(lh_fgeo, census == "census_1991")
LH_c2 <- subset(lh_fgeo, census == "census_1997")
LH_c3 <- subset(lh_fgeo, census == "census_2003")
LH_c4 <- subset(lh_fgeo, census == "census_2007_08")

table(lhc_fgeo$census)
LHC_c1 <- subset(lhc_fgeo, census == "census_1991")
LHC_c2 <- subset(lhc_fgeo, census == "census_1997")
LHC_c3 <- subset(lhc_fgeo, census == "census_2003")
LHC_c4 <- subset(lhc_fgeo, census == "census_2007_08")

table(lhs_fgeo$census)
LHS_c1 <- subset(lhs_fgeo, census == "census_1991")
LHS_c2 <- subset(lhs_fgeo, census == "census_1997")
LHS_c3 <- subset(lhs_fgeo, census == "census_2003")
LHS_c4 <- subset(lhs_fgeo, census == "census_2007_08")

table(SPKA$census)
SPKA_09_c1 <- subset(SPKA, census == "09_census_2001")
SPKA_09_c2 <- subset(SPKA, census == "09_census_2009")
SPKA_09_c3 <- subset(SPKA, census == "09_census_2014")

SPKA_10_c1 <- subset(SPKA, census == "10_census_2001") 
SPKA_10_c2 <- subset(SPKA, census == "10_census_2009")
SPKA_10_c3 <- subset(SPKA, census == "10_census_2014") 

table(SPKS$census)
SPKS_08_c1 <- subset(SPKS, census == "08_census_2001")
SPKS_08_c2 <- subset(SPKS, census == "08_census_2009")
SPKS_08_c3 <- subset(SPKS, census == "08_census_2014")

table(SPKH$census)
SPKH_04_c1 <- subset(SPKH, census == "04_census_2001")
SPKH_04_c2 <- subset(SPKH, census == "04_census_2008")
SPKH_04_c3 <- subset(SPKH, census == "04_census_2014")

SPKH_05_c1 <- subset(SPKH, census == "05_census_2001")
SPKH_05_c2 <- subset(SPKH, census == "05_census_2008")
SPKH_05_c3 <- subset(SPKH, census == "05_census_2014")

SPKH_30_c1 <- subset(SPKH, census == "30_census_2001") 
SPKH_30_c2 <- subset(SPKH, census == "30_census_2010")
SPKH_30_c3 <- subset(SPKH, census == "30_census_2015") 

#--------------------------------------------------------------------------------------------------#
# calculate by hand
#--------------------------------------------------------------------------------------------------#
# N  = number of trees at the first census
# S  = number of trees that survived to the second census, N - D
# meantime = interval between censuses in years for trees in the first census
#----------------------------------------------------------------------------------------#
census_dat1 <- DNM_c1 # LH_c3
census_dat2 <- DNM_c2 # LH_c4

table(census_dat1$status); table(census_dat2$status)

census_dat1_alive <- filter(census_dat1, status == "A")
join_dat <- inner_join(census_dat1_alive, census_dat2, by="treeID")
N <- table(join_dat$status.x)[1]
S <- table(join_dat$status.y)[1]
mean_time <- mean(join_dat$date.y-join_dat$date.x)

(log(N) - log(S))/(mean_time/365)

join_dat <- semi_join(census_dat2, census_dat1_alive, by="treeID")
N <- table(census_dat1_alive$status)[1]
S <- table(join_dat$status)[1]
mean_time <- mean(join_dat$date-census_dat1_alive$date)

(log(N) - log(S))/(mean_time/365)

#test <- mortality_rate(table(join_dat$status.x)[1], table(join_dat$status.y)[1], meantime=mean_time) 
## can't find function 'find.climits'
#----------------------------------------------------------------------------------------#
  


#--------------------------------------------------------------------------------------------------#
# calculate using mortality_ctfs() function
#--------------------------------------------------------------------------------------------------#
DNM_mort1 <- mortality_ctfs(DNM_c1, DNM_c2) # mort rate = 0.0038 (95CI: 0.0009, 0.0213)
DNM_mort1$rate*100; DNM_mort1$lower*100; DNM_mort1$upper*100

# Lambir - entire plot
LH_mort1 <- mortality_ctfs(LH_c1, LH_c2) # mort rate = )
LH_mort1$rate*100; LH_mort1$lower*100; LH_mort1$upper*100
LH_mort2 <- mortality_ctfs(LH_c2, LH_c3) # mort rate =  (95CI: )
LH_mort2$rate*100; LH_mort2$lower*100; LH_mort2$upper*100
LH_mort3 <- mortality_ctfs(LH_c3, LH_c4) # mort rate =  (95CI: )
LH_mort3$rate*100; LH_mort3$lower*100; LH_mort3$upper*100

LH_mort1$rate; LH_mort2$rate; LH_mort3$rate

LHC_mort1 <- mortality_ctfs(LHC_c1, LHC_c2) # mort rate = 0.0093 (95CI: 0.0029, 0.0335)
LHC_mort1$rate*100; LHC_mort1$lower*100; LHC_mort1$upper*100
LHC_mort2 <- mortality_ctfs(LHC_c2, LHC_c3) # mort rate =  (95CI: )
LHC_mort2$rate*100; LHC_mort2$lower*100; LHC_mort2$upper*100
LHC_mort3 <- mortality_ctfs(LHC_c3, LHC_c4) # mort rate =  (95CI: )
LHC_mort3$rate*100; LHC_mort3$lower*100; LHC_mort3$upper*100

LHS_mort1 <- mortality_ctfs(LHS_c1, LHS_c2) # mort rate =  (95CI: )
LHS_mort1$rate*100; LHS_mort1$lower*100; LHS_mort1$upper*100
LHS_mort2 <- mortality_ctfs(LHS_c2, LHS_c3) # mort rate =  (95CI: )
LHS_mort2$rate*100; LHS_mort2$lower*100; LHS_mort2$upper*100
LHS_mort3 <- mortality_ctfs(LHS_c3, LHS_c4) # mort rate =  (95CI: )
LHS_mort3$rate*100; LHS_mort3$lower*100; LHS_mort3$upper*100


# SPKA_09_c1$date <- rep(14933,length(SPKA_09_c1$dbh)) #"2000-11-19"
# SPKA_09_c2$date <- rep(18229,length(SPKA_09_c2$dbh)) #"2009-11-28"
# SPKA_09_c3$date <- rep(19920,length(SPKA_09_c3$dbh)) #"2014-07-16"
# 
# SPKA_10_c1$date <- rep(14924,length(SPKA_10_c1$dbh)) #"2000-11-10"
# SPKA_10_c2$date <- rep(18112,length(SPKA_10_c2$dbh)) #"2009-08-03"
# SPKA_10_c3$date <- rep(19883,length(SPKA_10_c3$dbh)) #"2014-06-09"

SPKA_09_mort1 <- mortality_ctfs(SPKA_09_c1, SPKA_09_c2) # mort rate = 0.0210 (95CI: 0.0196, 0.0225)
SPKA_09_mort1$rate; SPKA_09_mort1$lower; SPKA_09_mort1$upper
SPKA_09_mort2 <- mortality_ctfs(SPKA_09_c2, SPKA_09_c3) # mort rate = 0.0211 (95CI: 0.0189, 0.0234)
SPKA_09_mort2$rate; SPKA_09_mort2$lower; SPKA_09_mort2$upper
SPKA_10_mort1 <- mortality_ctfs(SPKA_10_c1, SPKA_10_c2) # mort rate = 0.0212 (95CI: 0.0196, 0.0228)
SPKA_10_mort1$rate; SPKA_10_mort1$lower; SPKA_10_mort1$upper
SPKA_10_mort2 <- mortality_ctfs(SPKA_10_c2, SPKA_10_c3) # mort rate = 0.0216 (95CI: 0.0194, 0.0241)
SPKA_10_mort2$rate; SPKA_10_mort2$lower; SPKA_10_mort2$upper

# SPKS_08_c1$date <- rep(14994,length(SPKS_08_c1$dbh)) #"2001-01-19"
# SPKS_08_c2$date <- rep(18034,length(SPKS_08_c2$dbh)) #"2009-05-17"
# SPKS_08_c3$date <- rep(19899,length(SPKS_08_c3$dbh)) #"2014-06-25"

SPKS_08_mort1 <- mortality_ctfs(SPKS_08_c1, SPKS_08_c2) # mort rate = 0.0111 (95CI: 0.0102, 0.0121)
SPKS_08_mort1$rate; SPKS_08_mort1$lower; SPKS_08_mort1$upper
SPKS_08_mort2 <- mortality_ctfs(SPKS_08_c2, SPKS_08_c3) # mort rate = 0.0142 (95CI: 0.0128, 0.0157)
SPKS_08_mort2$rate; SPKS_08_mort2$lower; SPKS_08_mort2$upper

# SPKH_04_c1$date <- rep(15125,length(SPKH_04_c1$dbh)) #"2001-05-30"
# SPKH_04_c2$date <- rep(17754,length(SPKH_04_c2$dbh)) #"2008-08-10"
# SPKH_04_c3$date <- rep(20060,length(SPKH_04_c3$dbh)) #"2014-12-03"
# 
# SPKH_05_c1$date <- rep(15124,length(SPKH_05_c1$dbh)) #"2001-05-29"
# SPKH_05_c2$date <- rep(17772,length(SPKH_05_c2$dbh)) #"2008-08-28"
# SPKH_05_c3$date <- rep(20004,length(SPKH_05_c3$dbh)) #"2014-10-08"
# 
# SPKH_30_c1$date <- rep(15097,length(SPKH_30_c1$dbh)) #"2001-05-02"
# SPKH_30_c2$date <- rep(18433,length(SPKH_30_c2$dbh)) #"2010-06-20"
# SPKH_30_c3$date <- rep(20127,length(SPKH_30_c3$dbh)) #"2015-02-08"

SPKH_04_mort1 <- mortality_ctfs(SPKH_04_c1, SPKH_04_c2) # mort rate = 0.0212 (95CI: 0.0198, 0.0227)
SPKH_04_mort1$rate; SPKH_04_mort1$lower; SPKH_04_mort1$upper
SPKH_04_mort2 <- mortality_ctfs(SPKH_04_c2, SPKH_04_c3) # mort rate = 0.0226 (95CI: 0.0210, 0.0242)
SPKH_04_mort2$rate; SPKH_04_mort2$lower; SPKH_04_mort2$upper
SPKH_05_mort1 <- mortality_ctfs(SPKH_05_c1, SPKH_05_c2) # mort rate = 0.0180 (95CI: 0.0169, 0.0191)
SPKH_05_mort1$rate; SPKH_05_mort1$lower; SPKH_05_mort1$upper
SPKH_05_mort2 <- mortality_ctfs(SPKH_05_c2, SPKH_05_c3) # mort rate = 0.0221 (95CI: 0.0208, 0.0236)
SPKH_05_mort2$rate; SPKH_05_mort2$lower; SPKH_05_mort2$upper
SPKH_30_mort1 <- mortality_ctfs(SPKH_30_c1, SPKH_30_c2) # mort rate = 0.0156 (95CI: 0.0147, 0.0166)
SPKH_30_mort1$rate; SPKH_30_mort1$lower; SPKH_30_mort1$upper
SPKH_30_mort2 <- mortality_ctfs(SPKH_30_c2, SPKH_30_c3) # mort rate = 0.0140 (95CI: 0.0128, 0.0154)
SPKH_30_mort2$rate; SPKH_30_mort2$lower; SPKH_30_mort2$upper
#--------------------------------------------------------------------------------------------------#

DNM_mort <- as.data.frame(rbind(c(DNM_mort1$rate, DNM_mort1$lower, DNM_mort1$upper,"DNM")))

LHC_mort <- as.data.frame(rbind(c(LHC_mort1$rate, LHC_mort1$lower, LHC_mort1$upper,"LHC"),
                                c(LHC_mort2$rate, LHC_mort2$lower, LHC_mort2$upper,"LHC"),
                                c(LHC_mort3$rate, LHC_mort3$lower, LHC_mort3$upper,"LHC")))

LHS_mort <- as.data.frame(rbind(c(LHS_mort1$rate, LHS_mort1$lower, LHS_mort1$upper,"LHS"),
                                c(LHS_mort2$rate, LHS_mort2$lower, LHS_mort2$upper,"LHS"),
                                c(LHS_mort3$rate, LHS_mort3$lower, LHS_mort3$upper,"LHS")))


SPKA_mort <- as.data.frame(rbind(c(SPKA_09_mort1$rate, SPKA_09_mort1$lower, SPKA_09_mort1$upper,"SPKA"),
                                 c(SPKA_09_mort2$rate, SPKA_09_mort2$lower, SPKA_09_mort2$upper,"SPKA"),
                                 c(SPKA_10_mort1$rate, SPKA_10_mort1$lower, SPKA_10_mort1$upper,"SPKA"),
                                 c(SPKA_10_mort2$rate, SPKA_10_mort2$lower, SPKA_10_mort2$upper,"SPKA")))
SPKS_mort <- as.data.frame(rbind(c(SPKS_08_mort1$rate, SPKS_08_mort1$lower, SPKS_08_mort1$upper,"SPKS"),
                                 c(SPKS_08_mort1$rate, SPKS_08_mort1$lower, SPKS_08_mort1$upper,"SPKS")))
SPKH_mort <- as.data.frame(rbind(c(SPKH_04_mort1$rate, SPKH_04_mort1$lower, SPKH_04_mort1$upper,"SPKH"),
                                 c(SPKH_04_mort2$rate, SPKH_04_mort2$lower, SPKH_04_mort2$upper,"SPKH"),
                                 c(SPKH_05_mort1$rate, SPKH_05_mort1$lower, SPKH_05_mort1$upper,"SPKH"),
                                 c(SPKH_05_mort2$rate, SPKH_05_mort2$lower, SPKH_05_mort2$upper,"SPKH"),
                                 c(SPKH_30_mort1$rate, SPKH_30_mort1$lower, SPKH_30_mort1$upper,"SPKH"),
                                 c(SPKH_30_mort2$rate, SPKH_30_mort2$lower, SPKH_30_mort2$upper,"SPKH")))

SPK_mort <- rbind(DNM_mort, LHC_mort, LHS_mort, SPKA_mort, SPKS_mort, SPKH_mort)
colnames(SPK_mort) <- c("mort", "lower", "upper", "site")
SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.character)
SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.numeric)


# SPKA_mort <- as.data.frame(cbind(mort=c(0.0210,0.0211,0.0212,0.0216), lower=c(0.0196,0.0189,0.0196,0.0194), upper=c(0.0225,0.0234,0.0228,0.0241), site=rep("SPKA",4)))
# SPKS_mort <- as.data.frame(cbind(mort=c(0.0111,0.0142), lower=c(0.0102,0.0128), upper=c(0.0121,0.0157), site=rep("SPKS",2)))
# SPKH_mort <- as.data.frame(cbind(mort=c(0.0212,0.0226,0.0180,0.0221,0.0156,0.0140), lower=c(0.0198,0.0210,0.0169,0.0208,0.0147,0.0128), upper=c(0.0227,0.0242,0.0191,0.0236,0.0166,0.0154), site=rep("SPKH",6)))
# 
# SPK_mort <- rbind(SPKA_mort, SPKS_mort, SPKH_mort)
# SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.character)
# SPK_mort[1:3] <- sapply(SPK_mort[1:3],as.numeric)

#SPK_mort$site <- factor(SPK_mort$site, levels = c("SPKA", "SPKS", "SPKH"))
SPK_mort$site <- factor(SPK_mort$site, levels = c("SPKA", "DNM", "LHC", "SPKS", "LHS", "SPKH"))

ggplot(SPK_mort, aes(x=site,y=mort, fill=site)) + geom_point(size=6, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4])) +
  #  scale_fill_manual(values=pal) +
  labs(x="", y=expression(Mortality~rate~(yr^{-1}))) +
  #scale_x_discrete(labels = c("SPK_alluvial","SPK_sandstone","SPK_heath")) +
  theme(legend.position = "none")
# 5x5 ED2_sites_mort_rates_by_census_5cm / ED2_sites_mort_rates_by_census_10cm / ED2_sites_mort_rates_by_census_30cm

plot_dat <- SPK_mort %>% group_by(site) %>% summarize(mmort = median(mort),
                                                      mlow = median(lower),
                                                      mup = median(upper))

ggplot(plot_dat, aes(x=site,y=mmort, fill=site)) + 
  geom_errorbar(aes(ymin=mlow, ymax=mup), width=.2, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=5, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4])) +
  labs(x="", y=expression(Mortality~rate~(yr^{-1}))) +
  scale_x_discrete(labels = c("SPKa","DNM50","LHc","SPKs","LHs","SPKh")) +
  theme(legend.position = "none") #+ 
#  ylim(0,0.05)
# 5x5 ED2_sites_mort_rates_5cm / ED2_sites_mort_rates_10cm / ED2_sites_mort_rates_30cm

# JUST SEPILOK
plot_dat <- subset(plot_dat, site == "SPKA" | site == "SPKS" | site == "SPKH")
ggplot(plot_dat, aes(x=site,y=mmort, fill=site)) + 
  geom_errorbar(aes(ymin=mlow, ymax=mup), width=.2, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=5, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[2],rev(pal2)[4])) +
  labs(x="", y="Mean annual mortality rate") +
  scale_x_discrete(labels = c("SPKa","SPKs","SPKh")) +
  theme(legend.position = "none")
# 5x5 SPK_sites_mort_rates


#------------------------------------------------------------------------#
# plot mortality BINNED by DBH
#------------------------------------------------------------------------#
library(Hmisc)

# constants #
FGEO_breaks   = c(seq(0,9,1),seq(10,30,10),seq(40,100,20),300)
FPlots_breaks = c(0,seq(6,9,1),seq(10,30,10),seq(40,100,20),181)
ylower        = 0.001
yupper        = 0.15

binned_mort <- function(census1_dat, census2_dat, bin_breaks=bin_breaks, g=50){
  listofdfs <- list()
  
  # omit DBH == NA from dat_c1_prelim
  dat_c1 <- census1_dat[complete.cases(census1_dat$dbh),]
  
  ## create DBH bins
  dat_c1$dbh_bin <- as.factor(cut(dat_c1$dbh, breaks = bin_breaks)); table(dat_c1$dbh_bin)

  # old version where g = # intervals and intervals have roughly same # obs
  #dat_c1$dbh_bin <- as.factor(cut2(dat_c1$dbh, g=g)); table(dat_c1$dbh_bin)
  
  dat_c1$dbh_bin_start <- gsub("(.*?),.*", "\\1",  dat_c1$dbh_bin); table(dat_c1$dbh_bin_start)
  dat_c1$dbh_bin_start <- gsub("[()]", "",dat_c1$dbh_bin_start); table(dat_c1$dbh_bin_start)
  dat_c1$dbh_bin_start <- gsub("\\[", "",dat_c1$dbh_bin_start); table(dat_c1$dbh_bin_start)
  dat_c1$dbh_bin_start <- as.numeric(dat_c1$dbh_bin_start); table(dat_c1$dbh_bin_start)
  
  dat_c1$dbh_bin_end <- gsub(".*,(.*?)", "\\1",  dat_c1$dbh_bin); table(dat_c1$dbh_bin_end)
  dat_c1$dbh_bin_end <- gsub("[()]", "",dat_c1$dbh_bin_end); table(dat_c1$dbh_bin_end)
  dat_c1$dbh_bin_end <- gsub("\\]", "",dat_c1$dbh_bin_end); table(dat_c1$dbh_bin_end)
  dat_c1$dbh_bin_end <- as.numeric(dat_c1$dbh_bin_end); table(dat_c1$dbh_bin_end)
  
  dat_c1$dbh_bin_val <- rowMeans(dat_c1[,c('dbh_bin_start', 'dbh_bin_end')], na.rm=TRUE); table(dat_c1$dbh_bin_val)
  dat_c1$dbh_bin_ID <- as.factor(dat_c1$dbh_bin_val); table(dat_c1$dbh_bin_ID)
  dat_c1 <- subset(dat_c1, dbh_bin_ID != "NaN"); table(dat_c1$dbh_bin_ID)
  dat_c1$dbh_bin_ID <- as.numeric(dat_c1$dbh_bin_ID); table(dat_c1$dbh_bin_ID)
  
  for(i in 1:max(dat_c1$dbh_bin_ID, na.rm=T)){
    print(i)
    dat1    <- subset(dat_c1, dbh_bin_ID == i)
    dat2    <- inner_join(select(dat1, treeID), census2_dat, by="treeID")
    if(dim(dat1)[[1]] > 0){
      mort_dat  <- mortality_ctfs(dat1, dat2)
      final_dat <- cbind(i, mort_dat$rate, mort_dat$lower, mort_dat$upper)
    }
    else {
      final_dat <- cbind(i, NA, NA, NA)
    }
    listofdfs[[i]] <- final_dat # save dataframes into the list
  }
  clean_list <- listofdfs[!sapply(listofdfs, is.null)] # remove NULL list elements (B)
  # COMBINE all lists into single dataframe
  output_dat <- do.call(rbind, clean_list) # less efficient way of concatenating list elements into df, but works with spatial df's
  output_dat <- as.data.frame(output_dat)
  bin_names <- as.data.frame(table(dat_c1$dbh_bin_val))
  output_dat$dbh_bin <- as.character(bin_names$Var1)
  output_dat$dbh_bin <- as.numeric(output_dat$dbh_bin)
  
  colnames(output_dat) <- c("bin", "mort_rate","lower","upper", "dbh_bin")
  return(output_dat)
}


lambir_cp1 <- binned_mort(LH_c1, LH_c2, bin_breaks=FGEO_breaks)
# test2 <- na.omit(test); head(test2)
# bin_names <- as.data.frame(table(dat_c1$dbh_bin_val))
# test2$dbh_bin <- as.character(bin_names$Var1); head(test2)
# #test2$dbh_bin <- c(as.character(bin_names$Var1),NA); head(test2)
# test2$dbh_bin <- as.numeric(test2$dbh_bin); head(test2)

p1 <- ggplot(lambir_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
#  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  #scale_y_continuous(trans='log10', limits=c(ylower,yupper)) + 
#  ylim(0.002,0.1) + 
#  scale_y_continuous(breaks=c(0.002, 1.00)) + 
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Lambir (c1-c2)") +
  theme(legend.position = "none")

lambir_cp2 <- binned_mort(LH_c2, LH_c3, bin_breaks=FGEO_breaks)
p2 <- ggplot(lambir_cp2, aes(x=dbh_bin,y=mort_rate)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Lambir (c2-c3)") +
  theme(legend.position = "none")

lambir_cp3 <- binned_mort(LH_c3, LH_c4, bin_breaks=FGEO_breaks)
p3 <- ggplot(lambir_cp3, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Lambir (c3-c4)") +
  theme(legend.position = "none")

danum <- binned_mort(DNM_c1, DNM_c2, bin_breaks=FGEO_breaks)
p4 <- ggplot(danum, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.002,0.10),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="Danum (c1-c2)") +
  theme(legend.position = "none")

plot_grid(p1,p2,p3,p4, ncol=2)


# LAMBIR CLAY & SANDSTONE 
lambirC_cp1 <- binned_mort(LHC_c1, LHC_c2, bin_breaks=FGEO_breaks)
p1 <- ggplot(lambirC_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Clay (c1-c2)") +
  theme(legend.position = "none")

lambirC_cp2 <- binned_mort(LHC_c2, LHC_c3, bin_breaks=FGEO_breaks)
p2 <- ggplot(lambirC_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Clay (c2-c3)") +
  theme(legend.position = "none")

lambirC_cp3 <- binned_mort(LHC_c3, LHC_c4, bin_breaks=FGEO_breaks)
p3 <- ggplot(lambirC_cp3, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Clay (c3-c4)") +
  theme(legend.position = "none")


lambirS_cp1 <- binned_mort(LHS_c1, LHS_c2, bin_breaks=FGEO_breaks)
p4 <- ggplot(lambirS_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Sandstone (c1-c2)") +
  theme(legend.position = "none")

lambirS_cp2 <- binned_mort(LHS_c2, LHS_c3, bin_breaks=FGEO_breaks)
p5 <- ggplot(lambirS_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Sandstone (c2-c3)") +
  theme(legend.position = "none")

lambirS_cp3 <- binned_mort(LHS_c3, LHS_c4, bin_breaks=FGEO_breaks)
p6 <- ggplot(lambirS_cp3, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  #  scale_y_continuous(trans='log10', limits=c(0.008,0.10)) + 
  scale_y_continuous(limits=c(0.00,0.13),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="LH Sandstone (c3-c4)") +
  theme(legend.position = "none")

plot_grid(p1,p2,p3,p4,p5,p6,ncol=3)


# SEPILOK ALLUVIAL 
# triple check that treeID's are unique for different plots
summary(SPKA_09_c1$treeID); summary(SPKA_10_c1$treeID)
SPKA_c1 <- rbind(SPKA_09_c1, SPKA_10_c1)
SPKA_c2 <- rbind(SPKA_09_c2, SPKA_10_c2)
SPKA_c3 <- rbind(SPKA_09_c3, SPKA_10_c3)

spka_cp1 <- binned_mort(SPKA_c1, SPKA_c2, bin_breaks=FPlots_breaks)
spka_cp1$upper <- ifelse(spka_cp1$upper > 0.1, 0.1, spka_cp1$upper)
sp1 <- ggplot(spka_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Alluvial (c1-c2)") +
  theme(legend.position = "none")

spka_cp2 <- binned_mort(SPKA_c2, SPKA_c3, bin_breaks=FPlots_breaks)
spka_cp2$upper <- ifelse(spka_cp2$upper > 0.1, 0.1, spka_cp2$upper)
sp2 <- ggplot(spka_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Alluvial (c2-c3)") +
  theme(legend.position = "none")


# SEPILOK SANDSTONE
spks_cp1 <- binned_mort(SPKS_08_c1, SPKS_08_c2, bin_breaks=FPlots_breaks)
spks_cp1$upper <- ifelse(spks_cp1$upper > 0.1, 0.1, spks_cp1$upper)
sp3 <- ggplot(spks_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Sandstone (c1-c2)") +
  theme(legend.position = "none")

spks_cp2 <- binned_mort(SPKS_08_c2, SPKS_08_c3, bin_breaks=FPlots_breaks)
spks_cp2$upper <- ifelse(spks_cp2$upper > 0.1, 0.1, spks_cp2$upper)
sp4 <- ggplot(spks_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Sandstone (c2-c3)") +
  theme(legend.position = "none")


# SEPILOK HEATH 
# triple check that treeID's are unique for different plots
summary(SPKH_04_c1$treeID); summary(SPKH_05_c1$treeID); summary(SPKH_30_c1$treeID) 
SPKH_c1 <- rbind(SPKH_04_c1, SPKH_05_c1, SPKH_30_c1)
SPKH_c2 <- rbind(SPKH_04_c2, SPKH_05_c2, SPKH_30_c2)
SPKH_c3 <- rbind(SPKH_04_c3, SPKH_05_c3, SPKH_30_c3)

spkh_cp1 <- binned_mort(SPKH_c1, SPKH_c2, bin_breaks=FPlots_breaks)
spkh_cp1$upper <- ifelse(spkh_cp1$upper > 0.1, 0.1, spkh_cp1$upper)
sp5 <- ggplot(spkh_cp1, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Heath (c1-c2)") +
  theme(legend.position = "none")

spkh_cp2 <- binned_mort(SPKH_c2, SPKH_c3, bin_breaks=FPlots_breaks)
spkh_cp2$upper <- ifelse(spkh_cp2$upper > 0.1, 0.1, spkh_cp2$upper)
sp6 <- ggplot(spkh_cp2, aes(x=dbh_bin,y=mort_rate)) +  
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.02, position=position_dodge(0.05), lwd=0.5) +
  geom_point(size=3, pch=19) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(limits=c(0.00,0.11),breaks=c(0.002, 0.01, 0.02, 0.05, 0.10)) + 
  labs(x="DBH (cm)", y=expression(Mortality~rate~(yr^{-1})), title="SPK Heath (c2-c3)") +
  theme(legend.position = "none")

plot_grid(sp1,sp3,sp5,sp2,sp4,sp6,ncol=3)
#------------------------------------------------------------------------#


#----------------------------- CANOPY GAP SIZE FREQUENCY DISTRIBUTION ---------------------------------
# Zeta distribution (lambda) ia a metric to quantify and compare the negative relationship
# between canopy gap frequency and size across sites
# code from Asner et al. 2013 PLOSone
## see Gap_size_freq_dist_ForestGapR.R 

# combine lambdas & boostrapped SE

#DNM50 & ForestPlots - 2 m
plot_dat <- as.data.frame(cbind(lambda=c(1.512,1.422,1.533,1.501), lower=c(1.494947,1.406294,1.532976,1.472568), upper=c(1.532976,1.437786,1.532976,1.528418), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#DNM50 & ForestPlots - 5 m
plot_dat <- as.data.frame(cbind(lambda=c(1.452,1.386,1.513,1.488), lower=c(1.426651,1.374719,1.494947,1.466253), upper=c(1.480466,1.400118,1.532976,1.514177), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#DNM50 & ForestPlots - 10 m
plot_dat <- as.data.frame(cbind(lambda=c(1.378,1.352,1.437,1.435), lower=c(1.360905,1.343113,1.398154,1.420566), upper=c(1.398311,1.360533,1.464974,1.450949), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#DNM50 & ForestPlots - 20 m
plot_dat <- as.data.frame(cbind(lambda=c(1.3602,1.357,1.41303,1.36009), lower=c(1.343011,1.347129,1.384092,1.347281), upper=c(1.377477,1.366647,1.444553,1.372631), site=c("SPKA","DNM50","SPKS","SPKH")))
plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#RS data - 2 m
# plot_dat <- as.data.frame(cbind(lambda=c(1.445,1.441,1.42), lower=c(1.426242,1.431351,1.395217), upper=c(1.461195,1.451024,1.438839), site=c("SPKA","SPKS","SPKH")))
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#RS data - 5 m
# plot_dat <- as.data.frame(cbind(lambda=c(1.414,1.412,1.425), lower=c(1.405226,1.405435,1.412781), upper=c(1.422847,1.419298,1.439280), site=c("SPKA","SPKS","SPKH")))
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

#RS data - 10 m
# plot_dat <- as.data.frame(cbind(lambda=c(1.388,1.396,1.414), lower=c(1.381752,1.391436,1.407099), upper=c(1.393949,1.400844,1.421779), site=c("SPKA","SPKS","SPKH")))
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.character)
# plot_dat[1:3] <- sapply(plot_dat[1:3],as.numeric)

plot_dat$site <- factor(plot_dat$site, levels = c("SPKA", "DNM50", "SPKS", "SPKH"))

#scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],pal[1],rev(pal2)[2],rev(pal2)[5],rev(pal2)[4])) +

ggplot(plot_dat, aes(x=site,y=lambda, fill=site)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(0.05), lwd=1) +
  geom_point(size=6, pch=21) +
  scale_fill_manual("", values=c( "cornflowerblue",rev(pal2)[1],rev(pal2)[2],rev(pal2)[4])) +
  #  scale_fill_manual("",values=pal) +
  #  ylim(1.34,1.55) + 
  labs(x="", y=expression(lambda)) +
  scale_x_discrete(labels = c("SPKa","DNM50","SPKs","SPKh")) +
  theme(legend.position = "none")
# jpg 450x400 SPK_lambda_5m; SPK_ForPlots_lambda_5m





#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#
