#==========================================================================================#
# Reformat inventory plot data for initialization  
#==========================================================================================#

# must be in csv file format

#==========================================================================================#
#==========================================================================================#
# script expects following variables
#   (names are case sensitive and must appear in first line of csv file)
#   (order does not matter)
#==========================================================================================#
# plots (10x10m): unique plot identification. these will be pixels or subplots of a larger (e.g. 50 ha) plot
# tag: unique identifier for tree. do not repeat tags for trees in same plot
# scientific: scientific name. currently not used by the script
# wood.dens: wood density (g cm-3). in case the wood density is not known, use some WD database and the scientific names to provide estimates
# dbh: stem diameter at breast height (cm)
#==========================================================================================#
#==========================================================================================#

#==========================================================================================#
setwd("G:/My Drive/Harvard/") # Google Drive
library(dplyr); library(ggplot2); library(viridis); library(stringr)
library(ggfortify); library(cowplot); require(data.table); library(forcats)
library(tidyr); library(purrr); library(data.table); library(scales)
library(stringr)
#==========================================================================================#
# ForestGEO    :  Danum, Lambir, (Pasoh eventually)
# ForestPlots  :  Sepilok, Danum, (Allpahuayo, Cicra eventually)
#==========================================================================================#
ForestGEO <- read.csv("Plot_Data/clean_inventory_data/ForestGEO_clean.csv")
#==========================================================================================#
ForestPlots <- read.csv("Plot_Data/clean_inventory_data/Forest_Plots_clean.csv")
#==========================================================================================#



#==========================================================================================#
# LAMBIR HILLS
#==========================================================================================#
Site_dat <- subset(ForestGEO, site == "LHP" & census == "census_2007_08")
table(Site_dat$soil)

## Lambir Hills Sandy Laom (LHS)
Site_dat <- subset(ForestGEO, site == "LHP" & census == "census_2007_08" & soil == "Sandy_loam"); dim(Site_dat)
table(Site_dat$soil)

## Lambir Hills Clay (LHC)
Site_dat <- subset(ForestGEO, site == "LHP" & census == "census_2007_08" & soil == "Clay"); dim(Site_dat)
table(Site_dat$soil)

# load("Plot_Data/CTFS_ForestGEO/Data/Lambir_Soils_DatatoElsaOrdway/lambir.habs.Rdata")
# load("Plot_Data/CTFS_ForestGEO/Data/Lambir_Soils_DatatoElsaOrdway/stem4.RM3a.Rdata")
# 
# LHP_habs <- lambir.habs
# LHP_stem_habs <- stem4.RM3a
# dim(LHP_stem_habs); dim(LHP_dat)
#==========================================================================================#


#==========================================================================================#
# DANUM VALLEY
#==========================================================================================#
Site_dat <- subset(ForestGEO, site == "DNM50" & census == "census_2011_15") #census_2011_15 | census_2019
dim(Site_dat) # DNM census 1 = 256713; DNM census 2 = 226971
length(unique(Site_dat$tag)) # DNM census 1 = 234905; DNM census 2 = 214838

Site_dat1 <- subset(ForestGEO, site == "DNM50" & census == "census_2011_15") #census_2011_15 | census_2019
# use DNM 2019 census data "cleaned" by Lucie - outliers removed - to subset census_2019 dat
# clean_dat <- read.csv("G:/My Drive/Harvard/Plot_Data/CTFS_ForestGEO/Data/Census_2_dnm50_clean_main_stem.csv")
# clean_stems <- clean_dat$stemID
dat_2019 <- subset(ForestGEO, site == "DNM50" & census == "census_2019") #census_2011_15 | census_2019
# Site_dat2 <- subset(dat_2019, stemID %in% clean_stems)
# dim(Site_dat2); dim(clean_dat)

combined_dat <- left_join(Site_dat1, dat_2019, by = "stemID")
plot(combined_dat$dbh.x, combined_dat$dbh.y)

clean_stems <- 

Site_dat2 <- updated



# plot_x and plot_y wrong in census 2
# join census 1 & 2 by tag to use plot_x and plot_y from census 1
Site_dat_join <- inner_join(Site_dat1, Site_dat2, by="tag") 
# unique tags overlapping in census 1 & 2 = 209582
# ggplot(Site_dat_join, aes(plot_x.x, plot_y.x, col=dbh.x)) +
#   geom_point(size=3)
# ggplot(Site_dat_join, aes(plot_x.x, plot_y.x, col=dbh.y)) +
#   geom_point(size=3)
Site_dat <- select(Site_dat_join, X.y, plot_x.x, plot_y.x, tag, sp.y, family.y, genus.y, species.y, treeID.y, stemID.y, dbh.y, date.y, DFstatus.y, IDlevel.y, site.y, census.y, shade.tol.y, stem_BA.y, mean_wd.y)
colnames(Site_dat) <-c("X", "plot_x", "plot_y", "tag", "sp", "family", "genus", "species", "treeID", "stemID", "dbh", "date", "DFstatus", "IDlevel", "site", "census", "shade.tol", "stem_BA", "mean_wd")
# ggplot(Site_dat, aes(plot_x, plot_y, col=dbh)) +
#   geom_point(size=3)
#==========================================================================================#


#==========================================================================================#
# FORESTPLOTS
#==========================================================================================#
ForestPlots$census_year <- substr(ForestPlots$census, 11, 14)
Site_dat_1 <- subset(ForestPlots, site == "SPKA") # n = 2 plots (2*4 = 8 ha)
#Site_dat_1 <- subset(ForestPlots, site == "SPKS") # n = 1 plot (4 ha = 16 50x50m patches)
#Site_dat_1 <- subset(ForestPlots, site == "SPKH") # n = 3 plots (3*4 = 12 ha)
Site_dat <- subset(Site_dat_1, census_year == "2014") # n = 2 plots (4 ha)
# create 'plot'column that does not repeat across multiple plots using subplot_ID
# e.g. SPKA should have 200 plots (2 4-ha plots * 100 subplots each = 200)
# somehow = 201 for Heath - even though 3 4-ha plots
Site_dat$plot <- as.integer(Site_dat$subplot_ID)
length(table(Site_dat$plot))
#==========================================================================================#
#==========================================================================================#




#=================================================================================================#
# CREATE DATA FRAME
#=================================================================================================#
#-------------------------------------------------------------------------------------------------#
# Only needed for ForestGEO / plots with plot_x and plot_y
#-------------------------------------------------------------------------------------------------#
Site_dat$plot_20 <- Site_dat$quadrat20

#----------------------------------------------------------------------#
# # break into patches/plots (20x20m) based on plot_x and plot_y
#----------------------------------------------------------------------#
# Site_dat$bin_x <- cut(Site_dat$plot_x, breaks=c(seq(0,1000,by=20)))
# Site_dat$bin_y <- cut(Site_dat$plot_y, breaks=c(seq(0,500,by=20)))
# Site_dat$bin <- with(Site_dat, paste0(bin_x, bin_y))
# 
ggplot(Site_dat, aes(plot_x, plot_y, col=plot_20)) +
  geom_point(size=3)
ggplot(Site_dat, aes(plot_x, plot_y, col=dbh)) +
  geom_point(size=3)
# 
# # assign plot number based on 20x20m sub-plots using plot_x and plot_y
# Site_dat$plot_20 <- as.factor(Site_dat$bin); head(Site_dat)
# Site_dat$plot_20 <- as.numeric(Site_dat$plot_20); head(Site_dat)
# 4999 unique plots * (10*10) = 499,000 m2
length(unique(Site_dat$plot_20))

test <- Site_dat %>% group_by(plot_20) %>% summarize(n=n()); summary(test)

#ggplot(Site_dat, aes(plot_x, plot_y, col=plot)) + geom_point()
#----------------------------------------------------------------------#



#----------------------------------------------------------------------#
# break into patches/plots (10x10m) based on plot_x and plot_y
#----------------------------------------------------------------------#
Site_dat$bin_x <- cut(Site_dat$plot_x, breaks=c(seq(0,1000,by=10)))
Site_dat$bin_y <- cut(Site_dat$plot_y, breaks=c(seq(0,500,by=10)))
Site_dat$bin <- with(Site_dat, paste0(bin_x, bin_y))

ggplot(Site_dat, aes(plot_x, plot_y, col=dbh)) +
  geom_point(size=3)

# assign plot number based on 10x10m sub-plots using plot_x and plot_y
Site_dat$plot_10 <- as.factor(Site_dat$bin); head(Site_dat)
Site_dat$plot_10 <- as.numeric(Site_dat$plot_10); head(Site_dat)
# 4999 unique plots * (10*10) = 499,000 m2
length(unique(Site_dat$plot_10))
#ggplot(Site_dat, aes(plot_x, plot_y, col=plot)) + geom_point()
#----------------------------------------------------------------------#

#----------------------------------------------------------------------#
# break into patches/plots (15x15m) based on plot_x and plot_y
#----------------------------------------------------------------------#
# Site_dat$bin_x <- cut(Site_dat$plot_x, breaks=c(seq(0,1000,by=15)))
# Site_dat$bin_y <- cut(Site_dat$plot_y, breaks=c(seq(0,500,by=15)))
# Site_dat$bin <- with(Site_dat, paste0(bin_x, bin_y))
# 
# ggplot(Site_dat, aes(plot_x, plot_y, col=dbh)) +
#   geom_point(size=3)
# 
# # assign plot number based on 10x10m sub-plots using plot_x and plot_y
# Site_dat$plot_15 <- as.factor(Site_dat$bin); head(Site_dat)
# Site_dat$plot_15 <- as.numeric(Site_dat$plot_15); head(Site_dat)
# # 2278 unique plots * (15*15) = 499,000 m2
# length(unique(Site_dat$plot_15))
#----------------------------------------------------------------------#



#----------------------------------------------------------------------#
# # break into patches/plots (50x50m) based on plot_x and plot_y
#----------------------------------------------------------------------#
# Site_dat$bin_x <- cut(Site_dat$plot_x, breaks=c(seq(0,1000,by=50))) 
# Site_dat$bin_y <- cut(Site_dat$plot_y, breaks=c(seq(0,500,by=50))) 
# Site_dat$bin <- with(Site_dat, paste0(bin_x, bin_y))
# 
# # assign plot number based on 10x10m sub-plots using plot_x and plot_y
# Site_dat$plot_50 <- as.factor(Site_dat$bin); head(Site_dat)
# Site_dat$plot_50 <- as.numeric(Site_dat$plot_50); head(Site_dat)
# # 201 unique plots * (50*50) = 502,500 m2
# length(unique(Site_dat$plot_50))
# #ggplot(Site_dat, aes(plot_x, plot_y, col=plot_50)) + geom_point()
#----------------------------------------------------------------------#
 
#----------------------------------------------------------------------#
# # break into patches/plots (100x100m) based on plot_x and plot_y
#----------------------------------------------------------------------#
# Site_dat$bin_x <- cut(Site_dat$plot_x, breaks=c(seq(0,1000,by=100))) 
# Site_dat$bin_y <- cut(Site_dat$plot_y, breaks=c(seq(0,500,by=100))) 
# Site_dat$bin <- with(Site_dat, paste0(bin_x, bin_y))
# 
# # assign plot number based on 100x100m sub-plots using plot_x and plot_y
# Site_dat$plot_100 <- as.factor(Site_dat$bin); head(Site_dat)
# Site_dat$plot_100 <- as.numeric(Site_dat$plot_100); head(Site_dat)
# # 51 unique plots * (100*100) = 510,000 m2
# length(unique(Site_dat$plot_100))
# #ggplot(Site_dat, aes(plot_x, plot_y, col=plot_100)) + geom_point()
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# START HERE FOR ALL
#-------------------------------------------------------------------------------------------------#
# assign sub-plot level unique tree id "tag" for each tree 

# ForestGEO
cleanData <- Site_dat %>% group_by(plot_10) %>% mutate(tag_10 = 1:n())
# cleanData <- Site_dat %>% group_by(plot_15) %>% mutate(tag_15 = 1:n())
# cleanData <- Site_dat %>% group_by(plot_20) %>% mutate(tag_20 = 1:n())

# cleanData1 <- Site_dat %>% group_by(plot_50) %>% mutate(tag_50 = 1:n())
# summary(cleanData1$tag_50) # 1572 trees max in 50x50 m plot | 126 trees max in 10x10 m plot
# cleanData <- cleanData1 %>% group_by(plot_100) %>% mutate(tag_100 = 1:n())
#summary(cleanData$tag_20) # 641 trees max in 20x20 plot; 5631 trees max in 100x100 m plot
#summary(cleanData$tag_15) # 641 trees max in 20x20 plot; 5631 trees max in 100x100 m plot
summary(cleanData$tag_10) # 641 trees max in 20x20 plot; 5631 trees max in 100x100 m plot


# ForestPlots
cleanData <- Site_dat %>% group_by(plot) %>% mutate(tag_20 = 1:n())
summary(cleanData$tag_20) 


# number of unique species in Danum plot
sum(ifelse(table(cleanData$species) > 0,1,0)) 
# 865 spp for LHS ForestGEO sub 52 ha plot - most spp
# 714 spp for LHC ForestGEO sub 52 ha plot 
# 420 spp for Danum ForestGEO 50 ha plot
# 302 spp for SPKA ForestPlots 4 ha plot - most spp
# 216 spp for SPKS ForestPlots 4 ha plot
# 153 spp for SPKH ForestPlots 4 ha plot
#-------------------------------------------------------------------------------------------------#

#==========================================================================================#
# Pull traits: WD, LMA, SLA, N_m, P_m, N_a, P_a, Vcmax, Lignin, Phenols
#==========================================================================================#
# for CSS & PSS, need new wood.dens column - for every single individual
# use Marcos' approach: don't use mean WD, instead sample random number from another spp in genus, then family, 
# and then fully random sample for unknowns 

### pull WD for spp, if none sample from genus, if none sample from family, ...
### if none sample from other WD vals in plot

#==========================================================================================#
#-------------------------------- LOAD DATA ---------------------------------------
#==========================================================================================#
## ----------------- link to Global Wood Density Database ------------------
# https://datadryad.org/resource/doi:10.5061/dryad.234/1
# https://rdrr.io/cran/BIOMASS/man/wdData.html
library(BIOMASS)
data("wdData")

# Marcos' TRY version
TRY <- read.csv("Trait_Data/TRYplus_DataBase/TRY_Tropical/TRY_Tropical.csv"); dim(TRY)
table(TRY$family)
table(TRY$habitat)
# restrict to tropics
TRY <- filter(TRY, habitat == "tropical forest"); dim(TRY)

# Elsa's TRY version - already restricted to tropics
# more recent, but only public databases
TRYv5 <- read.csv("Trait_Data/TRY_v5_2019/TRY5_Tropics_select.csv"); dim(TRYv5)
# most extensive - includes private databases
TRYv4 <- read.csv("Trait_Data/TRY_v4_2017/TRY_Tropics_select.csv"); dim(TRYv4)

# library(devtools)
# devtools::install_github("richfitz/datastorr")
# devtools::install_github("traitecoevo/baad.data")
BAAD <- baad.data::baad_data()
#BAAD$dictionary
#BAAD$dictionary$description 

summary(BAAD$data)
str(BAAD$data)
# restrict to tropics
baad <- filter(BAAD$data, latitude <= 25, latitude >= -25)

#==========================================================================================#
#-------------------------------- CONVERT UNITS ---------------------------------------
#==========================================================================================#

#=============================================================================================================#
# TRAIT              |   ### TRY ###  |  ## TRYv4 ##  |  ## TRYv5 ##  |  ## BAAD ##  |## wdData ##| ## GAO ## |
#=============================================================================================================#
# wd                 |     g cm-3     |    mg mm-3    |    mg mm-3    |    kg m-3    |   g cm-3   |     --    |
# SLA                |    m2 kgC-1    |   mm2 mg-1?   |   mm2 mg-1?   |      --      |     --     |     --    |
# LMA                |       --       |      --       |      --       |    kg m-2    |     --     |    g m-2  |
# n.mass / p.mass    |       --       |    mg g-1     |      --       |    kg kg-1   |     --     | % (g g-1?)| (or mg g-1?)
# n.area / p.area    |  kg(N/P) m-2   |     g m-2     |      --       |      --      |     --     |     --    |
# a.vcmax            |  mmol m-2 s-1  |  mmol m-2 s-1 |  mmol m-2 s-1 |      --      |     --     |     --    |
# m.vcmax            | mmol kgC-1 s-1 |  mmol g-1 s-1 |  mmol m-2 s-1 |      --      |     --     |     --    |
#=============================================================================================================#

# SLA = ((1/LMA)*1000)*2 (if going from CAO LMA (g m-2) to ED2 SLA (m2 kgC-1))
# (above: SLA should be in m2 kgC-1, in which case, multiply LMA *2)
# LMA = 1/((SLA/2)/1000)
# (above: LMA in biomass (not C??))
# C -> mass : /2
# mass -> C : *2

#---Desired Units----------
# wd:          g  cm-3
# SLA:         m2 kg-1 & m2 kgC-1
# LMA:         g  m-2
# N_m, P_m:    mg g-1
# N_a, P_a:    g  m-2
# a.vcmax:   mmol m-2 s-1
# m.vcmax:   mmol g-1 s-1
#--------------------------

#------------------------------- WD --------------------------------#
# convert Wood denisty (r.st) to 10^3 kg/m3 (standard units)
baad$wd <- baad$r.st/1000; summary(baad$wd) 
#-------------------------------------------------------------------#

#------------------------------- SLA -------------------------------#
# calculate SLA from LMA (in m2 kg-1) (LMA = 1/SLA) 
baad$SLA <- 1/baad$ma.ilf; summary(baad$SLA)
baad$SLA_C <- (1/baad$ma.ilf)*2; summary(baad$SLA_C)
#same as: baad$SLA_C <- ((1/baad$LMA)*1000)*2; summary(baad$SLA_C)
# BAAD SLA generally higher than other databases...

# change 'SLA' to 'sla' and rename SLA.PR.excl to 'SLA'
TRYv4$sla <- TRYv4$SLA; TRYv5$sla <- TRYv5$SLA
TRYv4$SLA <- TRYv4$SLA.PR.excl; summary(TRYv4$sla); summary(TRYv4$SLA)
TRYv5$SLA <- TRYv5$SLA.PR.excl; summary(TRYv5$sla); summary(TRYv5$SLA)
# convert SLA from m2 g-1 (same as mm2 mg-1) to m2 kgC-1 for TRYv4 & TRYv5
# although SLA already appears to be in m2 kg-1, not m2 g-1 for TRYv4 & TRYv5
TRYv4$SLA_C <- TRYv4$SLA*2; summary(TRYv4$SLA_C) 
TRYv5$SLA_C <- TRYv5$SLA*2; summary(TRYv5$SLA_C) 

# rename TRY SLA to specify in units of kg C & create SLA in units of biomass
TRY$SLA_C <- TRY$SLA; summary(TRY)
TRY$SLA <- TRY$SLA_C/2

par(mfrow=c(4,1))
hist(baad$SLA); hist(TRY$SLA); hist(TRYv4$SLA); hist(TRYv5$SLA)
hist(baad$SLA_C); hist(TRY$SLA_C); hist(TRYv4$SLA_C); hist(TRYv5$SLA_C)
#-------------------------------------------------------------------#

#------------------------------- LMA -------------------------------#
# convert LMA (ma.ilf) from kg m-2 to g m-2 (mass not C)
baad$LMA <- (baad$ma.ilf*1000); summary(baad$LMA)
# convert SLA (m2 kg-1) to LMA and convert units to (g m-2)
TRY$LMA <- ((1/TRY$SLA)*1000); summary(TRY$LMA)

# Calculate LMA from SLA for TRYv4 & TRYv5
# LMA in mg mm-2 equivalent to g m-2
TRYv4$LMA <- (1/TRYv4$SLA)*1000; summary(TRYv4$LMA)
TRYv5$LMA <- (1/TRYv5$SLA)*1000; summary(TRYv5$LMA)

hist(baad$LMA); hist(TRY$LMA); hist(TRYv4$LMA); hist(TRYv5$LMA)
#-------------------------------------------------------------------#

#------------------------------- N&P -------------------------------#
# convert BAAD leaf N (n.lf) from kg kg-1 to mg g-1
baad$n.mass <- baad$n.lf*1000; summary(baad$n.mass) 
summary(TRYv4$n.mass)
# convert BAAD leaf n mass to g g-1 and then calculate n.area
baad$n.area <- (baad$n.mass*0.001)*baad$LMA; summary(baad$n.area) 

# convert TRY area-based foliar N & P from kg m-2 to g m-2 
TRY$n.area <- TRY$n.area*1000; summary(TRY$n.area) # gN m-2
TRY$p.area <- TRY$p.area*1000; summary(TRY$p.area) # gP m-2

# convert n.area / p.area (g m-2) to n.mass and p.mass (mg g-1)
TRY$n.mass <- (TRY$n.area/TRY$LMA)/0.001; summary(TRY$n.mass)
TRY$p.mass <- (TRY$p.area/TRY$LMA)/0.001; summary(TRY$p.mass)

## HOW TO CONVERT TO PERCENT CONCENTRATION ???
# leaf N in kg/kg -> XX/XX * 100 (%)
# leaf n.mass should be in 
# XXX$n.conc <- 

par(mfrow=c(3,1))
hist(baad$n.mass, xlim=c(5,50)); hist(TRY$n.mass, xlim=c(5,50)); hist(TRYv4$n.mass, xlim=c(5,50))
hist(baad$n.area, xlim=c(0,5)); hist(TRY$n.area, xlim=c(0,5)); hist(TRYv4$n.area, xlim=c(0,5))

par(mfrow=c(2,1))
hist(TRY$p.mass); hist(TRYv4$p.mass)
hist(TRY$p.area); hist(TRYv4$p.area)
#-------------------------------------------------------------------#

#----------------------------- Vcmax -------------------------------#
par(mfrow=c(3,1))
hist(TRY$vcmax); hist(TRYv4$a.vcmax); hist(TRYv5$a.vcmax)

# convert TRY mass-based Vcmax from mmol kgC-1 s-1 to mmol g-1 s-1 
TRY$m.vcmax <- (TRY$m.vcmax/2)*0.001; summary(TRY$m.vcmax)

hist(TRY$m.vcmax); hist(TRYv4$m.vcmax); hist(TRYv5$m.vcmax)
par(mfrow=c(1,1))
#-------------------------------------------------------------------#

#==========================================================================================#
# clean genera & family names
#==========================================================================================#
# first, separate species and genus in baad, TRYv4, and TRYv5 
baad$genus <- word(baad$species)
baad$spp <- word(baad$species, 2)
TRY$species <- word(TRY$scientific, 2)
TRYv4$genus <- word(TRYv4$scientific)
TRYv4$species <- word(TRYv4$scientific, 2)
TRYv5$genus <- word(TRYv5$scientific)
TRYv5$species <- word(TRYv5$scientific, 2)

length(table(baad$family))
baad$family <- gsub("Anacardaceae","Anacardiaceae",baad$family)
baad$family <- gsub("Bignonaceae","Bignoniaceae",baad$family)
baad$family <- gsub("Caesalpiniaceae ","Caesalpiniaceae",baad$family)
baad$family <- gsub("Laureaceae","Lauraceae",baad$family)
baad$family <- gsub("Myritaceae.","Myritaceae",baad$family)
baad$family <- gsub("Sterculaceae","Sterculiaceae",baad$family)
baad$family <- gsub("Malvaceae - Bombacoideae","Malvaceae",baad$family)
baad$family <- gsub("Malvaceae - Tilioideae","Malvaceae",baad$family)
baad$family <- gsub("Fabaceae - Caesalinioideae","Fabaceae-caesalpinioideae",baad$family)
baad$family <- gsub("Fabaceae - Caesalpinioideae","Fabaceae-caesalpinioideae",baad$family)
baad$family <- gsub("Fabaceae - Mimosoideae","Fabaceae-mimosoideae",baad$family)
baad$family <- gsub("Fabaceae - Papilionoideae","Fabaceae-papilionoideae",baad$family)
baad$family <- gsub("Leguminosae","Fabaceae",baad$family)
baad$family <- gsub("Papilionoidae","Fabaceae-papilionoideae",baad$family)
baad$family <- gsub("Myritaceae","Myrtaceae",baad$family)
baad$family <- gsub("Ebanaceae","Ebenaceae",baad$family)
baad$family <- gsub("Combretacea","Combretaceae",baad$family)
baad$family <- gsub("Combretaceaee","Combretaceae",baad$family)
baad$family <- gsub("Nyctanginaceae","Nyctaginaceae",baad$family)
baad$family <- gsub("Aracaceae","Arecaceae",baad$family)
baad$family <- gsub("Poeceae","Poaceae",baad$family)
length(table(baad$family))

length(table(TRYv4$genus))
TRYv4$genus <- gsub("Lechytis","Lecythis",TRYv4$genus)
TRYv4$genus <- gsub("Dipterix","Dipteryx",TRYv4$genus)
TRYv4$genus <- gsub("Prionostema","Prionostemma",TRYv4$genus)
TRYv4$genus <- gsub("Sclerobium","Sclerolobium",TRYv4$genus)
TRYv4$genus <- gsub("Vochysiacea","Vochysia",TRYv4$genus)
TRYv4$genus <- gsub("INGA","Inga",TRYv4$genus)
TRYv4$genus <- gsub("MATA","Mata",TRYv4$genus)
TRYv4$genus <- gsub("Hyeronima","Hieronyma",TRYv4$genus)
TRYv4$genus <- gsub("Dendropax","Dendropanax",TRYv4$genus)
TRYv4$genus <- gsub("Tomovita","Tovomita",TRYv4$genus)
TRYv4$genus <- gsub("Pharenea","Faramea",TRYv4$genus)
TRYv4$genus <- gsub("Visnea","Vismia",TRYv4$genus)
TRYv4$genus <- gsub("Zizyphus","Ziziphus",TRYv4$genus)
TRYv4$genus <- gsub("Adenthera","Anadenanthera",TRYv4$genus)
TRYv4$genus <- gsub("Beureria","Bourreria",TRYv4$genus)
TRYv4$genus <- gsub("Matricaria","Margaritaria",TRYv4$genus)
TRYv4$genus <- gsub("Julb.","Julbernardia",TRYv4$genus)
TRYv4$genus <- gsub("Swarzia","Swartzia",TRYv4$genus)
TRYv4$genus <- gsub("Crataeva","Crateva",TRYv4$genus)
TRYv4$genus <- gsub("Boboa","Bocoa",TRYv4$genus)
TRYv4$genus <- gsub("Pseudomeldia","Pseudolmedia",TRYv4$genus)
TRYv4$genus <- gsub("Poeceae","Poaceae",TRYv4$genus)
TRYv4$genus <- gsub("Leguminosae","Fabaceae",TRYv4$genus)
TRYv4$genus <- gsub("Papilionoidae","Fabaceae-papilionoideae",TRYv4$genus)
#TRYv4$genus <- gsub("Copa","Copaifera",TRYv4$genus)
length(table(TRYv4$genus))

length(table(TRYv5$genus))
TRYv5$genus <- gsub("Lechytis","Lecythis",TRYv5$genus)
TRYv5$genus <- gsub("Dipterix","Dipteryx",TRYv5$genus)
TRYv5$genus <- gsub("Prionostema","Prionostemma",TRYv5$genus)
TRYv5$genus <- gsub("Sclerobium","Sclerolobium",TRYv5$genus)
TRYv5$genus <- gsub("Vochysiacea","Vochysia",TRYv5$genus)
TRYv5$genus <- gsub("INGA","Inga",TRYv5$genus)
TRYv5$genus <- gsub("MATA","Mata",TRYv5$genus)
TRYv5$genus <- gsub("Hyeronima","Hieronyma",TRYv5$genus)
TRYv5$genus <- gsub("Dendropax","Dendropanax",TRYv5$genus)
TRYv5$genus <- gsub("Tomovita","Tovomita",TRYv5$genus)
TRYv5$genus <- gsub("Pharenea","Faramea",TRYv5$genus)
TRYv5$genus <- gsub("Visnea","Vismia",TRYv5$genus)
TRYv5$genus <- gsub("Zizyphus","Ziziphus",TRYv5$genus)
TRYv5$genus <- gsub("Adenthera","Anadenanthera",TRYv5$genus)
TRYv5$genus <- gsub("Beureria","Bourreria",TRYv5$genus)
TRYv5$genus <- gsub("Matricaria","Margaritaria",TRYv5$genus)
TRYv5$genus <- gsub("Julb.","Julbernardia",TRYv5$genus)
TRYv5$genus <- gsub("Swarzia","Swartzia",TRYv5$genus)
TRYv5$genus <- gsub("Crataeva","Crateva",TRYv5$genus)
TRYv5$genus <- gsub("Boboa","Bocoa",TRYv5$genus)
TRYv5$genus <- gsub("Pseudomeldia","Pseudolmedia",TRYv5$genus)
TRYv5$genus <- gsub("Poeceae","Poaceae",TRYv5$genus)
TRYv5$genus <- gsub("Leguminosae","Fabaceae",TRYv5$genus)
TRYv5$genus <- gsub("Papilionoidae","Fabaceae-papilionoideae",TRYv5$genus)
length(table(TRYv5$genus))

# create family column for TRYv4 & TRYv5 from TRY, baad, and wdData
TRY_fam <- select(TRY, family, genus); dim(TRY_fam)
baad_fam <- select(baad, family, genus); dim(baad_fam)
wdDat_fam <- select(wdData, family, genus); dim(wdDat_fam)
fam_combine <- bind_rows(TRY_fam, baad_fam, wdDat_fam); dim(fam_combine)

fam_distinct <- distinct(fam_combine); dim(fam_distinct); head(fam_distinct)
fam_distinct <- filter(fam_distinct, family != "Leguminosae"); dim(fam_distinct)
fam_distinct <- filter(fam_distinct, !(family == "Asteraceae" & genus == "Albizia")); dim(fam_distinct)
fam_distinct[which(fam_distinct$genus == "Chaetocarpus"), "family"] <- "Peraceae" # should be family Peraceae not Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Ximenia"), "family"] <- "Olacaceae" # should be family Olacaceae not Ximeniaceae
fam_distinct[which(fam_distinct$genus == "Baccharis"), "family"] <- "Asteraceae" # should be family Asteraceae not Compositae
fam_distinct[which(fam_distinct$genus == "Pera"), "family"] <- "Peraceae" # should be family Peraceae not Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Vernonanthura"), "family"] <- "Asteraceae" # should be family Asteraceae not Compositae
fam_distinct[which(fam_distinct$genus == "Vernonia"), "family"] <- "Asteraceae" # should be family Asteraceae not Compositae
fam_distinct[which(fam_distinct$genus == "Pogonophora"), "family"] <- "Peraceae" # should be family Peraceae not Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Piptocarpha"), "family"] <- "Asteraceae" # should be family Asteraceae not Compositae
fam_distinct[which(fam_distinct$genus == "Piptocarpha"), "family"] <- "Asteraceae" # should be family Asteraceae not Compositae
fam_distinct[which(fam_distinct$genus == "Calophyllum"), "family"] <- "Calophyllaceae" # should be family Calophyllaceae not Clusiaceae
fam_distinct[which(fam_distinct$genus == "Calophyllum"), "family"] <- "Calophyllaceae" # should be family Calophyllaceae not Clusiaceae
fam_distinct[which(fam_distinct$genus == "Cecropia"), "family"] <- "Urticaceae" # should be family Urticaceae not Cecropiaceae
fam_distinct[which(fam_distinct$genus == "Sclerolobium"), "family"] <- "Fabaceae-caesalpinioideae"
fam_distinct[which(fam_distinct$genus == "Theobroma"), "family"] <- "Malvaceae" # should be family Malvaceae not Sterculiaceae
fam_distinct[which(fam_distinct$genus == "Casearia"), "family"] <- "Flacourtiaceae" # should be family Flacourtiaceae not Salicaceae
fam_distinct[which(fam_distinct$genus == "Dicorynia"), "family"] <- "Fabaceae" # should be family Fabaceae not Caesalpiniaceae
fam_distinct[which(fam_distinct$genus == "Eperua"), "family"] <- "Fabaceae" # should be family Fabaceae not Caesalpiniaceae
fam_distinct[which(fam_distinct$genus == "Stryphnodendron"), "family"] <- "Fabaceae" # should be family Fabaceae not Caesalpiniaceae
fam_distinct[which(fam_distinct$genus == "Albizia"), "family"] <- "Fabaceae" # should be family Fabaceae not Dipterocarpaceae
fam_distinct[which(fam_distinct$genus == "Eriotheca"), "family"] <- "Malvaceae" # should be family Malvaceae not Bombacaceae
fam_distinct[which(fam_distinct$genus == "Acosmium"), "family"] <- "Fabaceae-papilionoideae"
fam_distinct[which(fam_distinct$genus == "Recordoxylon"), "family"] <- "Caesalpiniaceae" # should be family Caesalpiniaceae not Fabaceae
fam_distinct[which(fam_distinct$genus == "Luehea"), "family"] <- "Malvaceae" # should be family Malvaceae not Tiliaceae
fam_distinct[which(fam_distinct$genus == "Pterodon"), "family"] <- "Fabaceae-papilionoideae"
fam_distinct[which(fam_distinct$genus == "Grewia"), "family"] <- "Malvaceae" # should be family Malvaceae not Tiliaceae
fam_distinct[which(fam_distinct$genus == "Aporosa"), "family"] <- "Phyllanthaceae" # should be family Phyllanthaceae not Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Cleistanthus"), "family"] <- "Phyllanthaceae" # should be family Phyllanthaceae not Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Adinandra"), "family"] <- "Pentaphylacaceae" # should be family Pentaphylacaceae not Theaceae
fam_distinct[which(fam_distinct$genus == "Baccaurea"), "family"] <- "Phyllanthaceae" # should be family Phyllanthaceae not Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Trema"), "family"] <- "Cannabaceae" # should be family Cannabaceae not Ulmaceae
fam_distinct[which(fam_distinct$genus == "Drypetes"), "family"] <- "Euphorbiaceae" # should be family Euphorbiaceae
fam_distinct[which(fam_distinct$genus == "Vouacapoua"), "family"] <- "Fabaceae" # should be family Fabaceae not Caesalpiniaceae
fam_distinct[which(fam_distinct$genus == "Lacistema"), "family"] <- "Lacistemataceae" # should be family Lacistemataceae
fam_distinct[which(fam_distinct$genus == "Cochlospermum"), "family"] <- "Bixaceae" # should be family Bixaceae
fam_distinct[which(fam_distinct$genus == "Herrania"), "family"] <- "Malvaceae" # should be family Malvaceae
fam_distinct[which(fam_distinct$genus == "Stylogyne"), "family"] <- "Primulaceae" # should be family Primulaceae
fam_distinct[which(fam_distinct$genus == "Fagraea"), "family"] <- "Loganiaceae" # should be family Loganiaceae
fam_distinct[which(fam_distinct$genus == "Callicarpa"), "family"] <- "Lamiaceae" # should be family Lamiaceae
fam_distinct[which(fam_distinct$genus == "Glochidion"), "family"] <- "Phyllanthaceae" # should be family Phyllanthaceae
fam_distinct[which(fam_distinct$genus == "Durio"), "family"] <- "Malvaceae" # should be family Malvaceae
fam_distinct[which(fam_distinct$genus == "Plathymenia"), "family"] <- "Fabaceae-mimosoideae"
fam_distinct[which(fam_distinct$genus == "Forchhammeria"), "family"] <- "Resedaceae" # should be family Resedaceae
fam_distinct[which(fam_distinct$genus == "Trigonopleura"), "family"] <- "Peraceae" # should be family Peraceae
fam_distinct[which(fam_distinct$genus == "Crypteronia"), "family"] <- "Crypteroniaceae" # should be family Peraceae
fam_distinct[which(fam_distinct$genus == "Machaerium"), "family"] <- "Fabaceae-papilionoideae" 
fam_distinct[which(fam_distinct$genus == "Hymenaea"), "family"] <- "Fabaceae-caesalpinioideae" 
fam_distinct[which(fam_distinct$genus == "Copaifera"), "family"] <- "Fabaceae-caesalpinioideae" 
fam_distinct[which(fam_distinct$genus == "Pterogyne"), "family"] <- "Fabaceae-caesalpinioideae" 
fam_distinct[which(fam_distinct$genus == "Schizolobium"), "family"] <- "Fabaceae-caesalpinioideae" 
fam_distinct[which(fam_distinct$genus == "Anadenanthera"), "family"] <- "Fabaceae-mimosoideae" 
fam_distinct[which(fam_distinct$genus == "Centrolobium"), "family"] <- "Fabaceae-papilionoideae" 
fam_distinct[which(fam_distinct$genus == "Caesalpinia"), "family"] <- "Fabaceae-caesalpinioideae" 
fam_distinct[which(fam_distinct$genus == "Sweetia"), "family"] <- "Fabaceae-papilionoideae" 

fam_distinct2 <- distinct(fam_distinct); dim(fam_distinct2); head(fam_distinct2)
fam_distinct2 <- na.omit(fam_distinct2); dim(fam_distinct2)

# get_dat <- filter(fam_distinct, genus == "Sweetia"); get_dat
# 
# test <- dplyr::left_join(TRYv5, fam_distinct2, by="genus")
# dim(TRYv4); dim(test) # 129443 | 130042
# dim(TRYv5); dim(test) # 76541 | 76550
# # identify the problem rows if they exist
# sum(ifelse(table(test$X)> 1, 1, 0))
# prob_rows <- table(test$X)>1
# prob_obs <- test[prob_rows,]; prob_obs

TRYv4 <- dplyr::left_join(TRYv4, fam_distinct2, by="genus"); dim(TRYv4) # 129443 x 70
summary(TRYv4) 
TRYv5 <- dplyr::left_join(TRYv5, fam_distinct2, by="genus"); dim(TRYv5) # 76541 x 38
summary(TRYv5)

# convert all the below genus names to family names
TRYv4[which(TRYv4$genus == "Rubiaceae"), "family"] <- "Rubiaceae"
TRYv4[which(TRYv4$genus == "Poaceae"), "family"] <- "Poaceae"
TRYv4[which(TRYv4$genus == "Fabaceae"), "family"] <- "Fabaceae"
TRYv4[which(TRYv4$genus == "Verbenaceae"), "family"] <- "Verbenaceae"
TRYv4[which(TRYv4$genus == "Euphorbiaceae"), "family"] <- "Euphorbiaceae"
TRYv4[which(TRYv4$genus == "Melastomataceae"), "family"] <- "Melastomataceae"
TRYv4[which(TRYv4$genus == "Papilionoideae"), "family"] <- "Papilionoideae"
TRYv4[which(TRYv4$genus == "Myrtaceae"), "family"] <- "Myrtaceae"
TRYv4[which(TRYv4$genus == "Lauraceae"), "family"] <- "Lauraceae"
TRYv4[which(TRYv4$genus == "Malpighiaceae"), "family"] <- "Malpighiaceae"
TRYv4[which(TRYv4$genus == "Hippocrateoideae"), "family"] <- "Hippocrateoideae"
TRYv4[which(TRYv4$genus == "Lecythidaceae"), "family"] <- "Lecythidaceae"

TRYv5[which(TRYv5$genus == "Rubiaceae"), "family"] <- "Rubiaceae"
TRYv5[which(TRYv5$genus == "Poaceae"), "family"] <- "Poaceae"
TRYv5[which(TRYv5$genus == "Fabaceae"), "family"] <- "Fabaceae"
TRYv5[which(TRYv5$genus == "Verbenaceae"), "family"] <- "Verbenaceae"
TRYv5[which(TRYv5$genus == "Euphorbiaceae"), "family"] <- "Euphorbiaceae"
TRYv5[which(TRYv5$genus == "Melastomataceae"), "family"] <- "Melastomataceae"
TRYv5[which(TRYv5$genus == "Papilionoideae"), "family"] <- "Papilionoideae"
TRYv5[which(TRYv5$genus == "Myrtaceae"), "family"] <- "Myrtaceae"
TRYv5[which(TRYv5$genus == "Lauraceae"), "family"] <- "Lauraceae"
TRYv5[which(TRYv5$genus == "Malpighiaceae"), "family"] <- "Malpighiaceae"
TRYv5[which(TRYv5$genus == "Hippocrateoideae"), "family"] <- "Hippocrateoideae"
TRYv5[which(TRYv4$genus == "Lecythidaceae"), "family"] <- "Lecythidaceae"

summary(as.factor(TRYv5$genus))
summary(as.factor(TRYv4$family))
#==========================================================================================#


#==========================================================================================#
#-------------------------------- COMBINE DATA --------------------------------------
#==========================================================================================#
# BAAD, wdData, TRY, TRYv4, TRYv5
#==========================================================================================#
#wdData: wd
#BAAD: wd, SLA, LMA, n.mass, n.area
#TRY: wood.dens, SLA, LMA, n.mass, p.mass, vcmax, m.vcmax 
#TRYv4: wood.dens, SLA, LMA, n.mass, p.mass, a.vcmax, m.vcmax
#TRYv5: wood.dens, SLA, LMA, a.vcmax, m.vcmax, 
#----------TRYv6: lignin, phenols (TBD) - no lignin & phenols in tropics in public TRY 

# make sure all of above in same units
summary(wdData$wd); summary(baad$wd); summary(TRYv5$wood.dens); summary(TRYv4$wood.dens); summary(TRY$wood.dens)
summary(baad$SLA); summary(TRY$SLA); summary(TRYv4$SLA); summary(TRYv5$SLA)
summary(baad$LMA); summary(TRY$LMA); summary(TRYv4$LMA); summary(TRYv5$LMA)

summary(baad$n.mass); summary(TRY$n.mass); summary(TRYv4$n.mass) # TRY looks low
summary(TRY$p.mass); summary(TRYv4$p.mass)
summary(TRY$m.vcmax); summary(TRYv4$m.vcmax); summary(TRYv5$m.vcmax)
summary(TRY$vcmax); summary(TRYv4$a.vcmax); summary(TRYv5$a.vcmax)

# select (family, genus, species, trait(s)) and rename columns
wdData_select <- wdData %>% select(family, genus, species, wd)
baad_select <- baad %>% select(family, genus, spp, wd, SLA, SLA_C, LMA, n.mass, n.area)
colnames(baad_select) <- c("family", "genus", "species", "wd", "SLA", "SLA_C", "LMA", "n.mass", "n.area")
TRY_select <- TRY %>% select(family, genus, species, wood.dens, SLA, SLA_C, LMA, n.mass, n.area, p.mass, p.area, m.vcmax, vcmax)
colnames(TRY_select) <- c("family", "genus", "species", "wd", "SLA", "SLA_C", "LMA", "n.mass", "n.area", "p.mass", "p.area", "m.vcmax", "a.vcmax")
TRYv4_select <- TRYv4 %>% select(family, genus, species, wood.dens, SLA, SLA_C, LMA, n.mass, n.area, p.mass, p.area, m.vcmax, a.vcmax)
colnames(TRYv4_select) <- c("family", "genus", "species", "wd", "SLA", "SLA_C", "LMA", "n.mass", "n.area", "p.mass", "p.area", "m.vcmax", "a.vcmax")
TRYv5_select <- TRYv5 %>% select(family, genus, species, wood.dens, SLA, SLA_C, LMA, m.vcmax, a.vcmax)
colnames(TRYv5_select) <- c("family", "genus", "species", "wd", "SLA", "SLA_C", "LMA", "m.vcmax", "a.vcmax")

# bind rows of above before pulling
traits_combined <- bind_rows(wdData_select, baad_select, TRY_select, TRYv4_select, TRYv5_select); dim(traits_combined)
head(traits_combined)

# summarize to mean/median per species 
traits_spp <- traits_combined %>% group_by(species) %>% summarize(n=n(), 
                                                                  mn_wd = mean(wd, na.rm=T),
                                                                  md_wd = median(wd, na.rm=T),
                                                                  sd_wd = sd(wd, na.rm=T),
                                                                  cv_wd = (sd_wd/mn_wd)*100,
                                                                  mn_SLA = mean(SLA, na.rm=T),
                                                                  md_SLA = median(SLA, na.rm=T),
                                                                  sd_SLA = sd(SLA, na.rm=T),
                                                                  cv_SLA = (sd_SLA/mn_SLA)*100,
                                                                  mn_LMA = mean(LMA, na.rm=T),
                                                                  md_LMA = median(LMA, na.rm=T),
                                                                  sd_LMA = sd(LMA, na.rm=T),
                                                                  cv_LMA = (sd_LMA/mn_LMA)*100,
                                                                  mn_Nm = mean(n.mass, na.rm=T),
                                                                  md_Nm = median(n.mass, na.rm=T),
                                                                  sd_Nm = sd(n.mass, na.rm=T),
                                                                  cv_Nm = (sd_Nm/mn_Nm)*100,
                                                                  mn_Pm = mean(p.mass, na.rm=T),
                                                                  md_Pm = median(p.mass, na.rm=T),
                                                                  sd_Pm = sd(p.mass, na.rm=T),
                                                                  cv_Pm = (sd_Pm/mn_Pm)*100,
                                                                  mn_mVcmax = mean(m.vcmax, na.rm=T),
                                                                  md_mVcmax = median(m.vcmax, na.rm=T),
                                                                  sd_mVcmax = sd(m.vcmax, na.rm=T),
                                                                  cv_mVcmax = (sd_mVcmax/mn_mVcmax)*100)

# n obs ranges from 1 to 10,960
# look at species-level coefficient of variation per trait 
par(mfrow=c(3,2))
hist(traits_spp$cv_wd); hist(traits_spp$cv_SLA); hist(traits_spp$cv_LMA); hist(traits_spp$cv_Nm); hist(traits_spp$cv_Pm); hist(traits_spp$cv_mVcmax)
# look at species-level median and mean values per trait 
par(mfrow=c(4,3))
hist(traits_spp$md_wd); hist(traits_spp$md_SLA); hist(traits_spp$md_LMA); hist(traits_spp$md_Nm); hist(traits_spp$md_Pm); hist(traits_spp$md_mVcmax)
hist(traits_spp$mn_wd); hist(traits_spp$mn_SLA); hist(traits_spp$mn_LMA); hist(traits_spp$mn_Nm); hist(traits_spp$mn_Pm); hist(traits_spp$mn_mVcmax)
# using mean vs median yield very similar results, could use either

traits_spp <- traits_combined %>% group_by(species) %>% summarize(n=n(), 
                                                                  md_wd = median(wd, na.rm=T),
                                                                  md_SLA = median(SLA, na.rm=T),
                                                                  md_LMA = median(LMA, na.rm=T),
                                                                  md_Nm = median(n.mass, na.rm=T),
                                                                  md_Pm = median(p.mass, na.rm=T),
                                                                  md_mVcmax = median(m.vcmax, na.rm=T))
# select just taxonomy and remove NAs for each trait
traits_combined_wd <- na.omit(select(traits_combined, family, genus, species, wd), cols="wd"); dim(traits_combined_wd)
traits_combined_SLA <- na.omit(select(traits_combined, family, genus, species, SLA), cols="SLA"); dim(traits_combined_SLA)
traits_combined_LMA <- na.omit(select(traits_combined, family, genus, species, LMA), cols="LMA"); dim(traits_combined_LMA)
traits_combined_Nm <- na.omit(select(traits_combined, family, genus, species, n.mass), cols="n.mass"); dim(traits_combined_Nm)
traits_combined_Pm <- na.omit(select(traits_combined, family, genus, species, p.mass), cols="p.mass"); dim(traits_combined_Pm)
traits_combined_Vcmax <- na.omit(select(traits_combined, family, genus, species, m.vcmax), cols="m.vcmax"); dim(traits_combined_Vcmax)

# summarize to mean/median per species 
# traits_genus <- traits_combined %>% group_by(genus) %>% summarize(n=n(), 
#                                                                   md_wd = median(wd, na.rm=T),
#                                                                   md_SLA = median(SLA, na.rm=T),
#                                                                   md_LMA = median(LMA, na.rm=T),
#                                                                   md_Nm = median(n.mass, na.rm=T),
#                                                                   md_Pm = median(p.mass, na.rm=T),
#                                                                   md_mVcmax = median(m.vcmax, na.rm=T))
# summary(traits_genus)

# summarize to mean/median per species 
# traits_fam <- traits_combined %>% group_by(family) %>% summarize(n=n(), 
#                                                                   md_wd = median(wd, na.rm=T),
#                                                                   md_SLA = median(SLA, na.rm=T),
#                                                                   md_LMA = median(LMA, na.rm=T),
#                                                                   md_Nm = median(n.mass, na.rm=T),
#                                                                   md_Pm = median(p.mass, na.rm=T),
#                                                                   md_mVcmax = median(m.vcmax, na.rm=T))
# summary(traits_fam)

#==========================================================================================#
#------------------------------ ASSIGN VALUES TO STEMS -------------------------------------
#==========================================================================================#
# pull for spp/stems in inventory plot data (as above with WD) - do it all here or do each trait separately?
# separate cleanData into 4 dataframes: species, genus, family, unknown


## ForestGEO
fg_spp <- filter(cleanData, IDlevel == "species"); dim(fg_spp)
fg_genus <- filter(cleanData, IDlevel == "genus"); dim(fg_genus)
fg_family <- filter(cleanData, IDlevel == "family"); dim(fg_family)
fg_unknown <- filter(cleanData, IDlevel == "unknown"); dim(fg_unknown)


## ForestPlots
fg_spp <- filter(cleanData, IDlevel == "WDSpecies"); dim(fg_spp)
fg_genus <- filter(cleanData, IDlevel == "WDGenus"); dim(fg_genus)
fg_family <- filter(cleanData, IDlevel == "WDFamily"); dim(fg_family)
fg_unknown <- filter(cleanData, IDlevel == "WDPlot"); dim(fg_unknown)

#=============================================================================================================#
# TRAIT (n) | DNM_FGEO | SPKA_14 | SPKS_14 | SPKH_14 |  LHP  |   PSO  |
#=============================================================================================================#
# spp       |  189016  |   4917  |   3518  |   7423  |       |        |
# genus     |  2236    |   3227  |   2570  |   5533  |       |        |
# family    |  0       |   251   |   166   |   778   |       |        |
# unknown   |  43653   |   449   |   20    |   1211  |       |        | 
#=============================================================================================================#


#-------------------------------------------------------------------------------------------------#
# species
#-------------------------------------------------------------------------------------------------#
# # assign species-level mean/median WD based on wdData
# wd_spp_mean <- wdData %>% group_by(species) %>% summarize(wood.dens = median(wd, na.rm=T))
# join summarized wdData wd with ForestGEO data by taxonomy columns
fg_spp <- dplyr::left_join(fg_spp, traits_spp, by = "species"); head(fg_spp)
summary(fg_spp)

# fg_spp$X is unique
sum(ifelse(table(fg_spp$X)> 1, 1, 0))
# prob_rows <- table(fg_spp$X)>1
# prob_obs <- fg_spp[prob_rows,]; prob_obs
# separate by trait (pull X, family, genus, species), retain X column to use for join

fg_sppWD <- select(fg_spp, X, family, genus, species, md_wd)
fg_sppSLA <- select(fg_spp, X, family, genus, species, md_SLA)
fg_sppLMA <- select(fg_spp, X, family, genus, species, md_LMA)
fg_sppNm <- select(fg_spp, X, family, genus, species, md_Nm)
fg_sppPm <- select(fg_spp, X, family, genus, species, md_Pm)
fg_sppVC <- select(fg_spp, X, family, genus, species, md_mVcmax)

fg_spp_wd <- subset(fg_sppWD, md_wd != "NA"); dim(fg_spp_wd) # n = 120208
colnames(fg_spp_wd)[6] <- "wd"
fg_spp_wd_na <- fg_sppWD[!fg_sppWD$X%in%fg_spp_wd$X,]; dim(fg_spp_wd_na) # n = 68808
fg_spp_SLA <- subset(fg_sppSLA, md_SLA != "NA"); dim(fg_spp_SLA) # n = 62677
colnames(fg_spp_SLA)[6] <- "SLA"
fg_spp_SLA_na <- fg_sppSLA[!fg_sppSLA$X%in%fg_spp_SLA$X,]; dim(fg_spp_SLA_na) # n = 126339
fg_spp_LMA <- subset(fg_sppLMA, md_LMA != "NA"); dim(fg_spp_LMA) # n = 62677
colnames(fg_spp_LMA)[6] <- "LMA"
fg_spp_LMA_na <- fg_sppLMA[!fg_sppLMA$X%in%fg_spp_LMA$X,]; dim(fg_spp_LMA_na) # n = 126339
fg_spp_Nm <- subset(fg_sppNm, md_Nm != "NA"); dim(fg_spp_Nm) # n = 76868
colnames(fg_spp_Nm)[6] <- "n.mass"
fg_spp_Nm_na <- fg_sppNm[!fg_sppNm$X%in%fg_spp_Nm$X,]; dim(fg_spp_Nm_na) # n = 112148
fg_spp_Pm <- subset(fg_sppPm, md_Pm != "NA"); dim(fg_spp_Pm) # n = 64963
colnames(fg_spp_Pm)[6] <- "p.mass"
fg_spp_Pm_na <- fg_sppPm[!fg_sppPm$X%in%fg_spp_Pm$X,]; dim(fg_spp_Pm_na) # n = 124053
fg_spp_Vcmax <- subset(fg_sppVC, md_mVcmax != "NA"); dim(fg_spp_Vcmax) # n = 37799
colnames(fg_spp_Vcmax)[6] <- "m.vcmax"
fg_spp_Vcmax_na <- fg_sppVC[!fg_sppVC$X%in%fg_spp_Vcmax$X,]; dim(fg_spp_Vcmax_na) # n = 151217

#-------------------------------------------------------------------------------------------------#
# genus
#-------------------------------------------------------------------------------------------------#

#----------------------------- WD -----------------------------#
# assign genus-level WD based random sample from 'traits_genus' by="genus"
fg_genus_combine_wd <- bind_rows(select(fg_genus, X, family, genus, species), select(fg_spp_wd_na, -md_wd)) # n = 71044
genus_df <- fg_genus_combine_wd %>% group_by(genus) %>% nest()
set.seed(87)
wdData_genus_samp <- select(traits_combined_wd, -family, -species) %>% group_by(genus) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_wd$genus)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)
fg_genus_combine_wd <- dplyr::left_join(genus_df, wdData_genus_samp, by = "genus") %>% unnest()
dim(fg_genus); head(fg_genus_combine_wd)
summary(fg_genus_combine_wd$wd)
#columns_to_use <- c("plot_100", "X","family","genus","species")
#columns_to_use <- c("plot_20", "X","family","genus","species")
#columns_to_use <- c("plot_15", "X","family","genus","species")
columns_to_use <- c("plot_10", "X","family","genus","species")
fg_genus_combine_wd <- select(fg_genus_combine_wd, c(columns_to_use,"wd"))

fg_genus_wd <- subset(fg_genus_combine_wd, wd != "NA"); dim(fg_genus_wd) # n = 69301
fg_genus_wd_na <- fg_genus_combine_wd[!fg_genus_combine_wd$X%in%fg_genus_wd$X,]; dim(fg_genus_wd_na) # n = 1743

#----------------------------- SLA -----------------------------#
fg_genus_combine_SLA <- bind_rows(select(fg_genus, X, family, genus, species), select(fg_spp_SLA_na, -md_SLA)) # n = 128575
genus_df <- fg_genus_combine_SLA %>% group_by(genus) %>% nest()
set.seed(87)
wdData_genus_samp <- select(traits_combined_SLA, -family, -species) %>% group_by(genus) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_SLA$genus)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_genus_combine_SLA <- dplyr::left_join(genus_df, wdData_genus_samp, by = "genus") %>% unnest(); 
dim(fg_genus); head(fg_genus_combine_SLA)
summary(fg_genus_combine_SLA$SLA)
fg_genus_combine_SLA <- select(fg_genus_combine_SLA, c(columns_to_use,"SLA"))

fg_genus_SLA <- subset(fg_genus_combine_SLA, SLA != "NA"); dim(fg_genus_SLA) # n = 91256
fg_genus_SLA_na <- fg_genus_combine_SLA[!fg_genus_combine_SLA$X%in%fg_genus_SLA$X,]; dim(fg_genus_SLA_na) # n = 37319

#----------------------------- LMA -----------------------------#
fg_genus_combine_LMA <- bind_rows(select(fg_genus, X, family, genus, species), select(fg_spp_LMA_na, -md_LMA)) # n = 128575
dim(fg_genus_combine_LMA)
sum(ifelse(table(fg_genus_combine_LMA$X)> 1, 1, 0))

genus_df <- fg_genus_combine_LMA %>% group_by(genus) %>% nest()
set.seed(87)
wdData_genus_samp <- select(traits_combined_LMA, -family, -species) %>% group_by(genus) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_LMA$genus)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)
fg_genus_combine_LMA <- dplyr::left_join(genus_df, wdData_genus_samp, by = "genus") %>% unnest(); 
dim(fg_genus); head(fg_genus_combine_LMA)
summary(fg_genus_combine_LMA$LMA)
fg_genus_combine_LMA <- select(fg_genus_combine_LMA, c(columns_to_use,"LMA"))

fg_genus_LMA <- subset(fg_genus_combine_LMA, LMA != "NA"); dim(fg_genus_LMA) # n = 91256
fg_genus_LMA_na <- fg_genus_combine_LMA[!fg_genus_combine_LMA$X%in%fg_genus_LMA$X,]; dim(fg_genus_LMA_na) # n = 37319

#----------------------------- N.mass -----------------------------#
fg_genus_combine_N <- bind_rows(select(fg_genus, X, family, genus, species), select(fg_spp_Nm_na, -md_Nm)) # n = 114384
dim(fg_genus_combine_N)
genus_df <- fg_genus_combine_N %>% group_by(genus) %>% nest()
set.seed(87)
wdData_genus_samp <- select(traits_combined_Nm, -family, -species) %>% group_by(genus) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_Nm$genus)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)
fg_genus_combine_N <- dplyr::left_join(genus_df, wdData_genus_samp, by = "genus") %>% unnest(); 
dim(fg_genus); head(fg_genus_combine_N)
summary(fg_genus_combine_N$n.mass)
fg_genus_combine_N <- select(fg_genus_combine_N, c(columns_to_use,"n.mass"))

fg_genus_Nm <- subset(fg_genus_combine_N, n.mass != "NA"); dim(fg_genus_Nm) # n = 89010
fg_genus_Nm_na <- fg_genus_combine_N[!fg_genus_combine_N$X%in%fg_genus_Nm$X,]; dim(fg_genus_Nm_na) # n = 25374

#----------------------------- P.mass -----------------------------#
fg_genus_combine_P <- bind_rows(select(fg_genus, X, family, genus, species), select(fg_spp_Pm_na, -md_Pm)) # n = 126289
dim(fg_genus_combine_P)
genus_df <- fg_genus_combine_P %>% group_by(genus) %>% nest()
set.seed(87)
wdData_genus_samp <- select(traits_combined_Pm, -family, -species) %>% group_by(genus) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_Pm$genus)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)
fg_genus_combine_P <- dplyr::left_join(genus_df, wdData_genus_samp, by = "genus") %>% unnest(); 
dim(fg_genus); head(fg_genus_combine_P)
summary(fg_genus_combine_P$p.mass)
fg_genus_combine_P <- select(fg_genus_combine_P, c(columns_to_use,"p.mass"))

fg_genus_Pm <- subset(fg_genus_combine_P, p.mass != "NA"); dim(fg_genus_Pm) # n = 83121
fg_genus_Pm_na <- fg_genus_combine_P[!fg_genus_combine_P$X%in%fg_genus_Pm$X,]; dim(fg_genus_Pm_na) # n = 43168

#----------------------------- Vcmax -----------------------------#
fg_genus_combine_Vc <- bind_rows(select(fg_genus, X, family, genus, species), select(fg_spp_Vcmax_na, -md_mVcmax)) # n = 126289
dim(fg_genus_combine_Vc)
#sum(ifelse(table(fg_genus_combine_Vc$X)> 1, 1, 0))
genus_df <- fg_genus_combine_Vc %>% group_by(genus) %>% nest()
set.seed(87)
wdData_genus_samp <- select(traits_combined_Vcmax, -family, -species) %>% group_by(genus) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_Vcmax$genus)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)
fg_genus_combine_Vc <- dplyr::left_join(genus_df, wdData_genus_samp, by = "genus") %>% unnest(); 
dim(fg_genus); head(fg_genus_combine_Vc)
summary(fg_genus_combine_Vc$m.vcmax)
fg_genus_combine_Vc <- select(fg_genus_combine_Vc, c(columns_to_use,"m.vcmax"))

fg_genus_Vc <- subset(fg_genus_combine_Vc, m.vcmax != "NA"); dim(fg_genus_Vc) # n = 76749
fg_genus_Vc_na <- fg_genus_combine_Vc[!fg_genus_combine_Vc$X%in%fg_genus_Vc$X,]; dim(fg_genus_Vc_na) # n = 76704

#-------------------------------------------------------------------------------------------------#
# family
#-------------------------------------------------------------------------------------------------#
# [1] nrow(fg_family) = 0, so just pull family level info for each trait - no need to combine
# [2] assign family-level trait based random sample from 'traits_combined' by="family"

#----------------------------- WD -----------------------------#
#fg_genus_na <- dplyr::left_join(select(fg_genus_wd_na, -c(family,plot)), select(cleanData, X, family), by = "X")
# combine fg_genus_na + fg_family here if fg_family has any obs
fg_family_combine_wd <- bind_rows(select(fg_family, X, family, genus, species), select(fg_genus_wd_na, -wd))
dim(fg_family_combine_wd)
#--------------------------------------------------------------#
#family_df <- select(fg_genus_wd_na, -wd) %>% group_by(family) %>% nest() #ForestGEO
family_df <- fg_family_combine_wd %>% group_by(family) %>% nest() #SPKA ForestPlots
set.seed(87)
wdData_family_samp <- select(traits_combined_wd, -genus, -species) %>% group_by(family) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_wd$family)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_family_combine_wd <- dplyr::left_join(family_df, wdData_family_samp, by = "family") %>% unnest()
dim(fg_family_combine_wd); head(fg_family_combine_wd); summary(fg_family_combine_wd$wd)
fg_family_combine_wd <- select(fg_family_combine_wd, c(columns_to_use,"wd"))

fg_family_wd <- subset(fg_family_combine_wd, wd != "NA"); dim(fg_family_wd) # n = 36863
fg_family_wd_na <- fg_family_combine_wd[!fg_family_combine_wd$X%in%fg_family_wd$X,]; dim(fg_family_wd_na) # n = 456
#--------------------------------------------------------------#

#----------------------------- SLA -----------------------------#
fg_family_combine_SLA <- bind_rows(select(fg_family, X, family, genus, species), select(fg_genus_SLA_na, -SLA)) 
dim(fg_family_combine_SLA)
#--------------------------------------------------------------#
#family_df <- select(fg_genus_SLA_na, -SLA) %>% group_by(family) %>% nest() #ForestGEO
family_df <- fg_family_combine_SLA %>% group_by(family) %>% nest() #SPKA ForestPlots
set.seed(87)
wdData_family_samp <- select(traits_combined_SLA, -genus, -species) %>% group_by(family) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_SLA$family)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_family_combine_SLA <- dplyr::left_join(family_df, wdData_family_samp, by = "family") %>% unnest()
dim(fg_family_combine_SLA); head(fg_family_combine_SLA); summary(fg_family_combine_SLA$SLA)
fg_family_combine_SLA <- select(fg_family_combine_SLA, c(columns_to_use,"SLA"))

fg_family_SLA <- subset(fg_family_combine_SLA, SLA != "NA"); dim(fg_family_SLA) # n = 36863
fg_family_SLA_na <- fg_family_combine_SLA[!fg_family_combine_SLA$X%in%fg_family_SLA$X,]; dim(fg_family_SLA_na) # n = 456
#--------------------------------------------------------------#

#----------------------------- LMA -----------------------------#
fg_family_combine_LMA <- bind_rows(select(fg_family, X, family, genus, species), select(fg_genus_LMA_na, -LMA)) 
dim(fg_family_combine_LMA); dim(fg_genus_LMA_na); dim(fg_family)
#--------------------------------------------------------------#
#family_df <- select(fg_genus_LMA_na, -LMA) %>% group_by(family) %>% nest() #ForestGEO
family_df <- fg_family_combine_LMA %>% group_by(family) %>% nest() #SPKA ForestPlots
set.seed(87)
wdData_family_samp <- select(traits_combined_LMA, -genus, -species) %>% group_by(family) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_LMA$family)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_family_combine_LMA <- dplyr::left_join(family_df, wdData_family_samp, by = "family") %>% unnest()
dim(fg_family_combine_LMA); head(fg_family_combine_LMA); summary(fg_family_combine_LMA$LMA)
fg_family_combine_LMA <- select(fg_family_combine_LMA, c(columns_to_use,"LMA"))

fg_family_LMA <- subset(fg_family_combine_LMA, LMA != "NA"); dim(fg_family_LMA) # n = 36863
fg_family_LMA_na <- fg_family_combine_LMA[!fg_family_combine_LMA$X%in%fg_family_LMA$X,]; dim(fg_family_LMA_na) # n = 456

#----------------------------- N.mass -----------------------------#
fg_family_combine_Nm <- bind_rows(select(fg_family, X, family, genus, species), select(fg_genus_Nm_na, -n.mass)) 
dim(fg_family_combine_Nm); dim(fg_genus_Nm_na); dim(fg_family)
#--------------------------------------------------------------#
#family_df <- select(fg_genus_Nm_na, -n.mass) %>% group_by(family) %>% nest() #ForestGEO
family_df <- fg_family_combine_Nm %>% group_by(family) %>% nest() #SPKA ForestPlots
set.seed(87)
wdData_family_samp <- select(traits_combined_Nm, -genus, -species) %>% group_by(family) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_Nm$family)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_family_combine_Nm <- dplyr::left_join(family_df, wdData_family_samp, by = "family") %>% unnest()
dim(fg_family_combine_Nm); head(fg_family_combine_Nm); summary(fg_family_combine_Nm$n.mass)
fg_family_combine_Nm <- select(fg_family_combine_Nm, c(columns_to_use,"n.mass"))

fg_family_Nm <- subset(fg_family_combine_Nm, n.mass != "NA"); dim(fg_family_Nm) # n = 24799
fg_family_Nm_na <- fg_family_combine_Nm[!fg_family_combine_Nm$X%in%fg_family_Nm$X,]; dim(fg_family_Nm_na) # n = 575

#----------------------------- P.mass -----------------------------#
fg_family_combine_Pm <- bind_rows(select(fg_family, X, family, genus, species), select(fg_genus_Pm_na, -p.mass)) 
dim(fg_family_combine_Pm); dim(fg_genus_Pm_na); dim(fg_family)
#--------------------------------------------------------------#
#family_df <- select(fg_genus_Pm_na, -p.mass) %>% group_by(family) %>% nest() #ForestGEO
family_df <- fg_family_combine_Pm %>% group_by(family) %>% nest() #SPKA ForestPlots
set.seed(87)
wdData_family_samp <- select(traits_combined_Pm, -genus, -species) %>% group_by(family) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_Pm$family)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_family_combine_Pm <- dplyr::left_join(family_df, wdData_family_samp, by = "family") %>% unnest()
dim(fg_family_combine_Pm); head(fg_family_combine_Pm); summary(fg_family_combine_Pm$p.mass)
fg_family_combine_Pm <- select(fg_family_combine_Pm, c(columns_to_use,"p.mass"))

fg_family_Pm <- subset(fg_family_combine_Pm, p.mass != "NA"); dim(fg_family_Pm) # n = 42299
fg_family_Pm_na <- fg_family_combine_Pm[!fg_family_combine_Pm$X%in%fg_family_Pm$X,]; dim(fg_family_Pm_na) # n = 869

#----------------------------- Vcmax -----------------------------#
fg_family_combine_Vc <- bind_rows(select(fg_family, X, family, genus, species), select(fg_genus_Vc_na, -m.vcmax)) 
dim(fg_family_combine_Vc); dim(fg_genus_Vc_na); dim(fg_family)
#--------------------------------------------------------------#
#family_df <- select(fg_genus_Vc_na, -m.vcmax) %>% group_by(family) %>% nest() #ForestGEO
family_df <- fg_family_combine_Vc %>% group_by(family) %>% nest() #SPKA ForestPlots
set.seed(87)
wdData_family_samp <- select(traits_combined_Vcmax, -genus, -species) %>% group_by(family) %>% nest() %>% ungroup() %>% 
  mutate(n = rep(1,length(unique(traits_combined_Vcmax$family)))) %>% mutate(samp = map2(data, n, sample_n)) %>% 
  select(-data) %>% unnest(samp)

fg_family_combine_Vc <- dplyr::left_join(family_df, wdData_family_samp, by = "family") %>% unnest()
dim(fg_family_combine_Vc); head(fg_family_combine_Vc); summary(fg_family_combine_Vc$m.vcmax)
fg_family_combine_Vc <- select(fg_family_combine_Vc, c(columns_to_use,"m.vcmax"))

fg_family_Vc <- subset(fg_family_combine_Vc, m.vcmax != "NA"); dim(fg_family_Vc) # n = 61244
fg_family_Vc_na <- fg_family_combine_Vc[!fg_family_combine_Vc$X%in%fg_family_Vc$X,]; dim(fg_family_Vc_na) # n = 15460

#-------------------------------------------------------------------------------------------------#
# recombine spp/genus/family by trait
#-------------------------------------------------------------------------------------------------#
# stitch back together by rows for each trait first and then recombine columns
# [1] recombine rows for each trait
# [2] sample remaining NA's from that column and bind all rows for trait (n = 234905; nrow(cleanData))
#-------------------------------------------------------------------------------------------------#

#----------------------------- WD -----------------------------#
almost_dat_wd <- bind_rows(fg_spp_wd,fg_genus_wd,fg_family_wd)
dim(cleanData); dim(almost_dat_wd)[[1]]+dim(fg_unknown)[[1]] + nrow(fg_family_wd_na)
par(mfrow=c(1,1))
hist(almost_dat_wd$wd); summary(almost_dat_wd$wd)

#fg_unknown_WD <- bind_rows(select(fg_unknown, plot_20, X, family, genus, species), select(fg_family_wd_na, -wd))
#fg_unknown_WD <- bind_rows(select(fg_unknown, plot_15, X, family, genus, species), select(fg_family_wd_na, -wd))
fg_unknown_WD <- bind_rows(select(fg_unknown, plot_10, X, family, genus, species), select(fg_family_wd_na, -wd))
fg_unknown_WD$wd <- sample(almost_dat_wd$wd, length(fg_unknown_WD$X), replace=T)
summary(fg_unknown_WD$wd)
#combined_dat_wd <- bind_rows(almost_dat_wd, select(fg_unknown, plot_100, X, family, genus, species, wd)); dim(combined_dat_wd)
#combined_dat_wd <- bind_rows(almost_dat_wd, select(fg_unknown_WD, plot_20, X, family, genus, species, wd)); dim(combined_dat_wd)
#combined_dat_wd <- bind_rows(almost_dat_wd, select(fg_unknown_WD, plot_15, X, family, genus, species, wd)); dim(combined_dat_wd)
combined_dat_wd <- bind_rows(almost_dat_wd, select(fg_unknown_WD, plot_10, X, family, genus, species, wd)); dim(combined_dat_wd)


#----------------------------- SLA -----------------------------#
almost_dat_SLA <- bind_rows(fg_spp_SLA,fg_genus_SLA,fg_family_SLA)
dim(cleanData); nrow(almost_dat_SLA)+nrow(fg_unknown) + nrow(fg_family_SLA_na)
hist(almost_dat_SLA$SLA); summary(almost_dat_SLA$SLA)

#fg_unknown_SLA <- bind_rows(select(fg_unknown, plot_100, X, family, genus, species), select(fg_family_SLA_na, -SLA))
#fg_unknown_SLA <- bind_rows(select(fg_unknown, plot_20, X, family, genus, species), select(fg_family_SLA_na, -SLA))
#fg_unknown_SLA <- bind_rows(select(fg_unknown, plot_15, X, family, genus, species), select(fg_family_SLA_na, -SLA))
fg_unknown_SLA <- bind_rows(select(fg_unknown, plot_10, X, family, genus, species), select(fg_family_SLA_na, -SLA))
fg_unknown_SLA$SLA <- sample(almost_dat_SLA$SLA, length(fg_unknown_SLA$X), replace=T)
summary(fg_unknown_SLA$SLA)
#combined_dat_SLA <- bind_rows(almost_dat_SLA, select(fg_unknown_SLA, plot_100, X, family, genus, species, SLA)); dim(combined_dat_SLA)
#combined_dat_SLA <- bind_rows(almost_dat_SLA, select(fg_unknown_SLA, plot_20, X, family, genus, species, SLA)); dim(combined_dat_SLA)
#combined_dat_SLA <- bind_rows(almost_dat_SLA, select(fg_unknown_SLA, plot_15, X, family, genus, species, SLA)); dim(combined_dat_SLA)
combined_dat_SLA <- bind_rows(almost_dat_SLA, select(fg_unknown_SLA, plot_10, X, family, genus, species, SLA)); dim(combined_dat_SLA)

#----------------------------- LMA -----------------------------#
almost_dat_LMA <- bind_rows(fg_spp_LMA,fg_genus_LMA,fg_family_LMA)
dim(cleanData); nrow(almost_dat_LMA)+nrow(fg_unknown) + nrow(fg_family_LMA_na)
hist(almost_dat_LMA$LMA); summary(almost_dat_LMA$LMA)

#fg_unknown_LMA <- bind_rows(select(fg_unknown, plot_100, X, family, genus, species), select(fg_family_LMA_na, -LMA))
#fg_unknown_LMA <- bind_rows(select(fg_unknown, plot_20, X, family, genus, species), select(fg_family_LMA_na, -LMA))
#fg_unknown_LMA <- bind_rows(select(fg_unknown, plot_15, X, family, genus, species), select(fg_family_LMA_na, -LMA))
fg_unknown_LMA <- bind_rows(select(fg_unknown, plot_10, X, family, genus, species), select(fg_family_LMA_na, -LMA))
fg_unknown_LMA$LMA <- sample(almost_dat_LMA$LMA, length(fg_unknown_LMA$X), replace=T)
summary(fg_unknown_LMA$LMA)
#combined_dat_LMA <- bind_rows(almost_dat_LMA, select(fg_unknown_LMA, plot_100, X, family, genus, species, LMA)); dim(combined_dat_LMA)
#combined_dat_LMA <- bind_rows(almost_dat_LMA, select(fg_unknown_LMA, plot_20, X, family, genus, species, LMA)); dim(combined_dat_LMA)
#combined_dat_LMA <- bind_rows(almost_dat_LMA, select(fg_unknown_LMA, plot_15, X, family, genus, species, LMA)); dim(combined_dat_LMA)
combined_dat_LMA <- bind_rows(almost_dat_LMA, select(fg_unknown_LMA, plot_10, X, family, genus, species, LMA)); dim(combined_dat_LMA)

#----------------------------- N.mass -----------------------------#
almost_dat_Nm <- bind_rows(fg_spp_Nm,fg_genus_Nm,fg_family_Nm)
dim(cleanData); nrow(almost_dat_Nm)+nrow(fg_unknown) + nrow(fg_family_Nm_na)
hist(almost_dat_Nm$n.mass); summary(almost_dat_Nm$n.mass)

#fg_unknown_Nm <- bind_rows(select(fg_unknown, plot_100, X, family, genus, species), select(fg_family_Nm_na, -n.mass))
#fg_unknown_Nm <- bind_rows(select(fg_unknown, plot_20, X, family, genus, species), select(fg_family_Nm_na, -n.mass))
#fg_unknown_Nm <- bind_rows(select(fg_unknown, plot_15, X, family, genus, species), select(fg_family_Nm_na, -n.mass))
fg_unknown_Nm <- bind_rows(select(fg_unknown, plot_10, X, family, genus, species), select(fg_family_Nm_na, -n.mass))
fg_unknown_Nm$n.mass <- sample(almost_dat_Nm$n.mass, length(fg_unknown_Nm$X), replace=T)
summary(fg_unknown_Nm$n.mass)
#combined_dat_Nm <- bind_rows(almost_dat_Nm, select(fg_unknown_Nm, plot_100, X, family, genus, species, n.mass)); dim(combined_dat_Nm)
#combined_dat_Nm <- bind_rows(almost_dat_Nm, select(fg_unknown_Nm, plot_20, X, family, genus, species, n.mass)); dim(combined_dat_Nm)
#combined_dat_Nm <- bind_rows(almost_dat_Nm, select(fg_unknown_Nm, plot_15, X, family, genus, species, n.mass)); dim(combined_dat_Nm)
combined_dat_Nm <- bind_rows(almost_dat_Nm, select(fg_unknown_Nm, plot_10, X, family, genus, species, n.mass)); dim(combined_dat_Nm)

#----------------------------- P.mass -----------------------------#
almost_dat_Pm <- bind_rows(fg_spp_Pm,fg_genus_Pm,fg_family_Pm)
dim(cleanData); nrow(almost_dat_Pm)+nrow(fg_unknown) + nrow(fg_family_Pm_na)
hist(almost_dat_Pm$p.mass); summary(almost_dat_Pm$p.mass)

#fg_unknown_Pm <- bind_rows(select(fg_unknown, plot_100, X, family, genus, species), select(fg_family_Pm_na, -p.mass))
#fg_unknown_Pm <- bind_rows(select(fg_unknown, plot_20, X, family, genus, species), select(fg_family_Pm_na, -p.mass))
#fg_unknown_Pm <- bind_rows(select(fg_unknown, plot_15, X, family, genus, species), select(fg_family_Pm_na, -p.mass))
fg_unknown_Pm <- bind_rows(select(fg_unknown, plot_10, X, family, genus, species), select(fg_family_Pm_na, -p.mass))
fg_unknown_Pm$p.mass <- sample(almost_dat_Pm$p.mass, length(fg_unknown_Pm$X), replace=T)
summary(fg_unknown_Pm$p.mass)
#combined_dat_Pm <- bind_rows(almost_dat_Pm, select(fg_unknown_Pm, plot_100, X, family, genus, species, p.mass)); dim(combined_dat_Pm)
#combined_dat_Pm <- bind_rows(almost_dat_Pm, select(fg_unknown_Pm, plot_20, X, family, genus, species, p.mass)); dim(combined_dat_Pm)
#combined_dat_Pm <- bind_rows(almost_dat_Pm, select(fg_unknown_Pm, plot_15, X, family, genus, species, p.mass)); dim(combined_dat_Pm)
combined_dat_Pm <- bind_rows(almost_dat_Pm, select(fg_unknown_Pm, plot_10, X, family, genus, species, p.mass)); dim(combined_dat_Pm)

#----------------------------- Vcmax -----------------------------#
almost_dat_Vc <- bind_rows(fg_spp_Vcmax,fg_genus_Vc,fg_family_Vc)
dim(cleanData); nrow(almost_dat_Vc)+nrow(fg_unknown) + nrow(fg_family_Vc_na)
hist(almost_dat_Vc$m.vcmax); summary(almost_dat_Vc$m.vcmax)

#fg_unknown_Vc <- bind_rows(select(fg_unknown, plot_100, X, family, genus, species), select(fg_family_Vc_na, -m.vcmax))
#fg_unknown_Vc <- bind_rows(select(fg_unknown, plot_20, X, family, genus, species), select(fg_family_Vc_na, -m.vcmax))
#fg_unknown_Vc <- bind_rows(select(fg_unknown, plot_15, X, family, genus, species), select(fg_family_Vc_na, -m.vcmax))
fg_unknown_Vc <- bind_rows(select(fg_unknown, plot_10, X, family, genus, species), select(fg_family_Vc_na, -m.vcmax))
fg_unknown_Vc$m.vcmax <- sample(almost_dat_Vc$m.vcmax, length(fg_unknown_Vc$X), replace=T)
summary(fg_unknown_Vc$m.vcmax)
#combined_dat_Vc <- bind_rows(almost_dat_Vc, select(fg_unknown_Vc, plot_100, X, family, genus, species, m.vcmax)); dim(combined_dat_Vc)
#combined_dat_Vc <- bind_rows(almost_dat_Vc, select(fg_unknown_Vc, plot_20, X, family, genus, species, m.vcmax)); dim(combined_dat_Vc)
#combined_dat_Vc <- bind_rows(almost_dat_Vc, select(fg_unknown_Vc, plot_15, X, family, genus, species, m.vcmax)); dim(combined_dat_Vc)
combined_dat_Vc <- bind_rows(almost_dat_Vc, select(fg_unknown_Vc, plot_10, X, family, genus, species, m.vcmax)); dim(combined_dat_Vc)
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# [3] then recombine columns/traits
#-------------------------------------------------------------------------------------------------#
# first order all by $X
cleanData <- cleanData[order(cleanData$X),]
combined_dat_wd <- combined_dat_wd[order(combined_dat_wd$X),]
combined_dat_SLA <- combined_dat_SLA[order(combined_dat_SLA$X),]
combined_dat_LMA <- combined_dat_LMA[order(combined_dat_LMA$X),]
combined_dat_Nm <- combined_dat_Nm[order(combined_dat_Nm$X),]
combined_dat_Pm <- combined_dat_Pm[order(combined_dat_Pm$X),]
combined_dat_Vc <- combined_dat_Vc[order(combined_dat_Vc$X),]

# combined_dat <- bind_cols(cleanData, select(combined_dat_wd, -family, -species, -genus, -plot_100),
#                           select(combined_dat_SLA, -family, -species, -genus, -plot_100),
#                           select(combined_dat_LMA, -family, -species, -genus, -plot_100),
#                           select(combined_dat_Nm, -family, -species, -genus, -plot_100),
#                           select(combined_dat_Pm, -family, -species, -genus, -plot_100),
#                           select(combined_dat_Vc, -family, -species, -genus, -plot_100))
# combined_dat <- bind_cols(cleanData, select(combined_dat_wd, -family, -species, -genus, -plot_20),
#                           select(combined_dat_SLA, -family, -species, -genus, -plot_20),
#                           select(combined_dat_LMA, -family, -species, -genus, -plot_20),
#                           select(combined_dat_Nm, -family, -species, -genus, -plot_20),
#                           select(combined_dat_Pm, -family, -species, -genus, -plot_20),
#                           select(combined_dat_Vc, -family, -species, -genus, -plot_20))
# combined_dat <- bind_cols(cleanData, select(combined_dat_wd, -family, -species, -genus, -plot_15),
#                           select(combined_dat_SLA, -family, -species, -genus, -plot_15),
#                           select(combined_dat_LMA, -family, -species, -genus, -plot_15),
#                           select(combined_dat_Nm, -family, -species, -genus, -plot_15),
#                           select(combined_dat_Pm, -family, -species, -genus, -plot_15),
#                           select(combined_dat_Vc, -family, -species, -genus, -plot_15))
combined_dat <- bind_cols(cleanData, select(combined_dat_wd, -family, -species, -genus, -plot_10),
                          select(combined_dat_SLA, -family, -species, -genus, -plot_10),
                          select(combined_dat_LMA, -family, -species, -genus, -plot_10),
                          select(combined_dat_Nm, -family, -species, -genus, -plot_10),
                          select(combined_dat_Pm, -family, -species, -genus, -plot_10),
                          select(combined_dat_Vc, -family, -species, -genus, -plot_10))
head(combined_dat$X); head(combined_dat$X1); head(combined_dat$X2); head(combined_dat$X3); head(combined_dat$X4); head(combined_dat$X5); head(combined_dat$X6)

dim(combined_dat)
sum(ifelse(table(combined_dat$X)> 1, 1, 0))
summary(combined_dat)
head(combined_dat); head(cleanData)

par(mfrow=c(3,2))
hist(combined_dat$wd); hist(combined_dat$SLA); hist(combined_dat$LMA); hist(combined_dat$n.mass); hist(combined_dat$p.mass); hist(combined_dat$m.vcmax)

#-------------------------------------------------------------------------------------------------#
# Assign PFTs
#-------------------------------------------------------------------------------------------------#
# exclude stems < 1 cm DBH (relevant to ForestPlots)
combined_dat <- filter(combined_dat, dbh >= 1); dim(combined_dat)

plot_area <- 50 #4, 8, 12, 50, 52
combined_dat$stem_BA <- 0.00007854 * combined_dat$dbh^2 # using DBH in cm, calculates basal area in m2
stems_gt_10 <- subset(combined_dat, dbh >= 10)
sum(combined_dat$stem_BA)/plot_area
sum(stems_gt_10$stem_BA)/plot_area

combined_dat$pft <- ifelse(combined_dat$wd < 0.5, "low", ifelse(combined_dat$wd > 0.7, "high", "med"))
(table(combined_dat$pft)/dim(combined_dat)[[1]])*100

#-----------------------------------------#
# Site  |   high   |    low    |    med 
#-----------------------------------------#
# DNM   | 16.37702 |  16.93548 |  66.68749
# SPKA  | 16.14103 |  17.97084 |  65.88813 
# SPKS  | 12.07533 |  21.93501 |  65.98966 
# SPKH  | 20.42118 |  15.70255 |  63.87627
#-----------------------------------------#
#-------------------------------------------------------------------------------------------------#

## How many species fall into each category?
sum(ifelse(table(fg_spp_wd$species) > 0,1,0)) # 270/425 = 63.53% spp (63)
sum(ifelse(table(fg_genus_wd$species) > 0,1,0)) # 110/425 = 25.88% spp (25)
sum(ifelse(table(fg_family$species) > 0,1,0)) # 0 spp
sum(ifelse(table(fg_unknown$species) > 0,1,0)) # 53/425 = 12.47% spp (12)
#-------------------------------------------------------------------------------------------------#
# recombine
#ForestGEO_clean_wd <- bind_rows(fg_spp, fg_genus, fg_family, fg_unknown)
Complete_Dat <- combined_dat

ggplot(ForestGEO, aes(x=mean_wd, fill=census)) +
  geom_histogram(binwidth=.05, alpha=.5, col= "white") + 
  facet_grid(census~.)

ggplot(Complete_Dat, aes(x=wood.dens, fill=census)) +
  geom_histogram(binwidth=.05, alpha=.5, col= "white") + 
  facet_grid(census~.)

#==========================================================================================#
# select columns of interest and rename (e.g. "tag2" -> "tag")
Complete_Dat$scientific <- with(Complete_Dat, paste(family, genus, species, sep = " "))

#final_dat <- select(Complete_Dat, plot_100, tag_100, scientific, wd, dbh)
#final_dat <- select(Complete_Dat, plot_20, tag_20, scientific, wd, dbh); dim(final_dat); dim(Complete_Dat)
#final_dat <- select(Complete_Dat, plot_15, tag_15, scientific, wd, dbh); dim(final_dat); dim(Complete_Dat)
final_dat <- select(Complete_Dat, plot_10, tag_10, scientific, wd, dbh); dim(final_dat); dim(Complete_Dat)
colnames(final_dat) <- c("plot", "tag", "scientific", "wood.dens", "dbh")
#final_dat_traits <- select(Complete_Dat, plot_x, plot_y, plot_10, plot_50, plot_100, tag_50, tag_100, family, genus, species, wd, dbh, SLA, LMA, n.mass, p.mass, m.vcmax)
#final_dat_traits <- select(Complete_Dat, plot_x, plot_y, plot_20, tag_20, family, genus, species, wd, dbh, SLA, LMA, n.mass, p.mass, m.vcmax); dim(final_dat_traits)
#final_dat_traits <- select(Complete_Dat, plot_x, plot_y, plot_15, tag_15, family, genus, species, wd, dbh, SLA, LMA, n.mass, p.mass, m.vcmax); dim(final_dat_traits)
final_dat_traits <- select(Complete_Dat, plot_x, plot_y, plot_10, tag_10, family, genus, species, wd, dbh, SLA, LMA, n.mass, p.mass, m.vcmax); dim(final_dat_traits)

site <- "DNM" #SPKH, SPKS, SPKA, DNM, LHS, LHC
year <- "2019" #2008, 2014, 2015, 2019 
res <- "10m"   #10m, 15m, 20m, 1ha

#write.csv(final_dat,"Harvard/Analysis/ED2_Initialization/DNM_2015_census.csv")
write.csv(final_dat,paste('Analysis/ED2_Initialization/',site,'_',year,'_census_',res,'.csv',sep=""))
write.csv(final_dat_traits,paste('Analysis/ED2_Initialization/',site,'_',year,'_census_traits.csv',sep=""))

#dat <- read.csv("G:/My Drive/Harvard/Analysis/ED2_Initialization/DNM_2015_census.csv")
#dat <- read.csv("G:/My Drive/Harvard/Analysis/ED2_Initialization/SPKA_2014_census_20m.csv")

#==========================================================================================#
#==========================================================================================#
