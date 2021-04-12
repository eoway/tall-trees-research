#---------------------------------------------------------------------------------------------#
# calculate Lambir topographic metrics by soil type
#---------------------------------------------------------------------------------------------#

# see: https://rspatial.org/raster/spatial/6-crs.html

library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(fgeo)

#----- PLOT COLOR PALETTE -----
library(colorRamps)
#colr = inferno(100, direction=-1)
#colr = viridis(100, direction=-1)
r2 <- matlab.like2(200)
r3 <- matlab.like2(4)
pal <- bpy.colors(99) 
#------------------------------

#----------------------------------------- LAMBIR data ---------------------------------------# 
# four censuses (data collected ~1991, 1997, 2003, and 2007/08)
# 52-ha plot 
# plot dims x = 1040 m X y = 500 m = 520,000 m2 = 52 ha
#---------------------------------------------------------------------------------------------#
load("lambir.stem4.RData")
load("lambir.elev.RData")
load("CTFSElev_lambir.RData")
load("lambir.habs.Rdata")
#load("stem4.RM3a.Rdata")


# load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.spptable.RData")
# data from Sabrina Russo - spp categorized as light demanding or shade tolerant
# LD = light demanding; ST = shade tolerance; NA for anything without spp designation 
# (e.g. IDlevel = multiple, Family, or Genus)
#load("Harvard/Plot_Data/CTFS_ForestGEO/Data/lambir.spptable.ST.RData") 
#---------------------------------------------------------------------------------------------#
#                                        Lambir Soils!
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# Define Lambir soil type
#---------------------------------------------------------------------------------------------#
lambir.habs$soil = lambir.habs$HabType
lambir.habs$soil[lambir.habs$HabType==1]="Sandy_loam"
lambir.habs$soil[lambir.habs$HabType==2]="Clay"
lambir.habs$soil[lambir.habs$HabType==3]="Loam"
lambir.habs$soil[lambir.habs$HabType==4]="Fine_loam"

lambir4 <- lambir.stem4
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
## Merge Lambir soil type indices with 4th census
#---------------------------------------------------------------------------------------------#
lambir4$index <- as.factor(lambir4$quadrat); lambir4$index <- as.numeric(lambir4$index)

length(unique(lambir4$index))

lam4 = merge(lambir4, lambir.habs, by = "index", all.x=T)
#---------------------------------------------------------------------------------------------#

#ggplot(lam4, aes(gx, gy, fill=soil)) + 
  #geom_point()


#---------------------------------------------------------------------------------------------#
# LAMBIR ELEVATION DATA
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
lam_elev <- lambir.elev
names(lam_elev) #xdim = 1040m; ydim = 500m
#---------------------------------------------------------------------------------------------#
lhp_elev_df <- lam_elev
head(lhp_elev_df)
dim(lhp_elev_df)
lhp_elev_df$ID <- seq(1:length(lhp_elev_df$elev))

ggplot() + 
  geom_point(data=lhp_elev_df, aes(x,y, col=elev))

#---------------------------------------------------------------------------------------------#
#---------------------------------Add Height Info----------------------------------------------
#---------------------------------------------------------------------------------------------#
#Equation
dbh2h_01 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){ 
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh)),
              exp(b1Ht + b2Ht * log(dbh_crit)))
  return(h)
}

# Parameters
b1Ht_SEA    = 0.5279284 * log(10) # Use for dbh2h_01
# SAME AS: b1Ht_SEA = 1.2156
b2Ht_SEA    = 0.5782 #"coefficient of ln(D)" # Use for dbh2h_01
hgt_ref_SEA = -0.0114
hgt_max_SEA = 100

#Calculate
lam4$height <- dbh2h_01(lam4$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(lam4$height)

#convert gx and gy to numeric values
lam4$gx <- as.numeric(lam4$gx)
lam4$gy <- as.numeric(lam4$gy)

#I think this is actually what I want in the end
test <- lam4 %>% group_by(quadrat, HabType, soil) %>% summarize(x = mean(gx, na.rm=T),
                                                                y = mean(gy, na.rm=T), 
                                                                dbh = mean(dbh, na.rm=T),
                                                                heightmean = mean(height, na.rm=T),
                                                                heightmedian = median(height, na.rm=T),
                                                                height99 = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                heightmax = max(height,na.rm=T))


ggplot() + 
  geom_point(data=test, aes(x,y, col=soil), size=5) + 
  theme_classic() 

#---------------------------------------------------------------------------------------------#
## add geographic (lat/lon) coordinates
#---------------------------------------------------------------------------------------------#
lhp_elev_df$x <- 168797+lhp_elev_df$x
lhp_elev_df$y <- 463372+lhp_elev_df$y


# convert lhp_elev_df to a raster
lhp_elev_df_raster <- lhp_elev_df
# create spatial points data frame
coordinates(lhp_elev_df_raster) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(lhp_elev_df_raster) <- TRUE
# coerce to raster
elev_rast <- raster(lhp_elev_df_raster)
elev_rast
res(elev_rast)
plot(elev_rast)

# project to WGS
#What is this????????---------------------------------
crs(elev_rast) <- CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#crs(elev_rast) <- CRS("+proj=longlat +datum=WGS84")
projection(elev_rast)

Lambir_slope_aspect_TPI <- terrain(elev_rast, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
summary(Lambir_slope_aspect_TPI)

# writeRaster(elev_rast, "G:/My Drive/Harvard/CAO_data/GIS/Lambir_elevation.tif", overwrite=T)
# writeRaster(Lambir_slope_aspect_TPI, "G:/My Drive/Harvard/CAO_data/GIS/Lambir_slope_aspect_TPI.tif", overwrite=T)
#---------------------------------------------------------------------------------------------#
# use lambir.habs data where soil type is summarized into 5x5m sub-quadrats (n=1300)
# Lambir (n = 1300 20x20m quadrats; n = 21109 5x5m quadrats)
elev_rast_20m <- aggregate(elev_rast, fact=4.5)
topo_20m <- aggregate(Lambir_slope_aspect_TPI, fact=4.5)
res(elev_rast_20m); res(topo_20m)
plot(elev_rast_20m)
plot(topo_20m)

# assign 20m quadrat IDs vertically to match Lambir quadrats....
#elev_rast_20m$index <- 1:ncell(elev_rast_20m)

#Every 25th entry is NA for soils stuff?????-----------------------------ELSA---------------------------
elev_rast_20m$index <- c(seq(26,1378,by=26),seq(25,1377,by=26),seq(24,1376,by=26),
                         seq(23,1375,by=26),seq(22,1374,by=26),seq(21,1373,by=26),
                         seq(20,1372,by=26),seq(19,1371,by=26),seq(18,1370,by=26),
                         seq(17,1369,by=26),seq(16,1368,by=26),seq(15,1367,by=26),
                         seq(14,1366,by=26),seq(13,1365,by=26),seq(12,1364,by=26),
                         seq(11,1363,by=26),seq(10,1362,by=26),seq(9,1361,by=26),
                         seq(8,1360,by=26),seq(7,1359,by=26),seq(6,1358,by=26),
                         seq(5,1357,by=26),seq(4,1356,by=26),seq(3,1355,by=26),
                         seq(2,1354,by=26),seq(1,1353,by=26))

plot(elev_rast_20m)
elev_rast_20m
summary(elev_rast)
summary(elev_rast_20m_df)
plot(elev_rast_20m$index)
length(unique(elev_rast_20m$index))
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# combine 20m elev data with 20m quadrat soil data 
#---------------------------------------------------------------------------------------------#
dim(lambir.habs)
length(unique(lam4$index))
#dim(lhp_elev_df)
dim(elev_rast_20m)

elev_topo_20m <- stack(elev_rast_20m, topo_20m)
plot(elev_topo_20m)

par(mfrow=c(2,1))
plot(elev_topo_20m$elev, col=r2, main="Elevation (m)")
plot(elev_topo_20m$slope, col=r2, main="Slope (degrees)")

# assign soil labels to quadrats and separate into LHC & LHS
elev_rast_20m_df <- as.data.frame(elev_topo_20m, xy=T)
elev_soil = merge(elev_rast_20m_df, lambir.habs, by = "index", all.x=T)

ggplot() + 
  geom_point(data=elev_soil, aes(x,y, col=elev), size=8) + 
  scale_color_gradientn(colours=r2) +
  theme_classic()

plot(elev_rast_20m$elev, col=r2)

#test <- na.omit(elev_soil)
#ggplot() + 
  #geom_point(data=test, aes(x,y, col=soil), size=6) + 
  #theme_classic() + 
  #theme(legend.position="bottom")
#---------------------------------------------------------------------------------------------#
#The Great Merge-------------
#---------------------------------------------------------------------------------------------#
lam4$quadrat <- as.character(lam4$index)
lambir <- inner_join(lam4, elev_soil, by="index")
lambir <- subset(lambir, select = -c(HabType.y, soil.y))
lambir <- rename(lambir, HabType = HabType.x, soil = soil.x)

heightmetrics <- lam4 %>% group_by(quadrat, HabType, soil) %>% summarize( 
                                                                dbhmean = mean(dbh, na.rm=T),
                                                                heightmean = mean(height, na.rm=T),
                                                                heightmedian = median(height, na.rm=T),
                                                                height99 = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                heightmax = max(height,na.rm=T))
lambir_all <- inner_join(lambir, heightmetrics, by= "quadrat")
lambir_all <- subset(lambir_all, select = -c(HabType.y, soil.y))
lambir_all <- rename(lambir_all, HabType = HabType.x, soil = soil.x)



#---------------------------------------------------------------------------------------------#
# ALL LAMBIR
#---------------------------------------------------------------------------------------------#
#cellStats(elev_rast, mean); cellStats(elev_rast, sd)

#cellStats(Lambir_slope_aspect_TPI$slope, mean)
#cellStats(Lambir_slope_aspect_TPI$slope, sd)

#cellStats(Lambir_slope_aspect_TPI$aspect, mean)
#cellStats(Lambir_slope_aspect_TPI$aspect, sd)

#cellStats(Lambir_slope_aspect_TPI$aspect, mean)
#cellStats(Lambir_slope_aspect_TPI$aspect, sd)
#---------------------------------------------------------------------------------------------#
# calculated TWI in ArcMAP
#---------------------------------------------------------------------------------------------#
# download TauDEM as ADMINISTATOR
# (in start, right click CMD -> as administrator -> type: net user administrator /active:yes)
# restart, login as admin, download, restart
# see section C
# https://hydrology.usu.edu/taudem/taudem5/support.html

# https://www.srbc.net/pennsylvania-lidar-working-group/docs/twi-srbc.pdf
# TWI = ln(CA/slope)
# [1] Use D-Infinity Flow Directions tool to calculate slope & flow direction
# [2] Use D-Infinity Contributing Area tool calc flow accumulation from flow dir
# where flow dir method = D8 (DEFAULT) - Assigns flow direction to the steepest downslope neighbor.
# [3] Use Raster Calculator to calculate TWI = Ln(flow accumulation / slope)
#Danum_TWI = raster("Harvard/CAO_data/GIS/Danum_TWI.tif"); plot(Danum_TWI)
#Sepilok_TWI = raster("Harvard/CAO_data/GIS/Sepilok_TWI.tif"); plot(Sepilok_TWI)
library(rgdal)
Lambir_TWI = raster("Lambir_TWI.tif"); plot(Lambir_TWI)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
cellStats(Lambir_TWI, mean); cellStats(Lambir_TWI, sd)

TWI_20m <- aggregate(Lambir_TWI, fact=4)
res(TWI_20m)
plot(TWI_20m)

# assign 20m quadrat IDs vertically to match Lambir quadrats....
#elev_rast_20m$index <- 1:ncell(elev_rast_20m)
TWI_20m$index <- c(seq(26,1378,by=26),seq(25,1377,by=26),seq(24,1376,by=26),
                   seq(23,1375,by=26),seq(22,1374,by=26),seq(21,1373,by=26),
                   seq(20,1372,by=26),seq(19,1371,by=26),seq(18,1370,by=26),
                   seq(17,1369,by=26),seq(16,1368,by=26),seq(15,1367,by=26),
                   seq(14,1366,by=26),seq(13,1365,by=26),seq(12,1364,by=26),
                   seq(11,1363,by=26),seq(10,1362,by=26),seq(9,1361,by=26),
                   seq(8,1360,by=26),seq(7,1359,by=26),seq(6,1358,by=26),
                   seq(5,1357,by=26),seq(4,1356,by=26),seq(3,1355,by=26),
                   seq(2,1354,by=26),seq(1,1353,by=26))

#plot(elev_rast_20m$elev)
plot(TWI_20m$index)
length(unique(TWI_20m$index))

#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
# combine 20m elev data with 20m quadrat soil data 
#---------------------------------------------------------------------------------------------#
# assign soil labels to quadrats and separate into LHC & LHS
TWI_20m_df <- as.data.frame(TWI_20m, xy=T)
twi_soil = merge(TWI_20m_df, lambir.habs, by = "index", all.x=T)

ggplot() + 
  geom_point(data=twi_soil, aes(x,y, col=Lambir_TWI), size=8) + 
  scale_color_gradientn(colours=r2) +
  theme_classic()

plot(TWI_20m$Lambir_TWI, col=r2)

ggplot() + 
  geom_point(data=twi_soil, aes(x,y, col=soil), size=6) + 
  theme_classic()

lambir_topo <- inner_join(lambir_all, twi_soil, by="index")
lambir_topo <- subset(lambir_topo, select = -c(HabType.y, soil.y,x.y,y.y))
lambir_topo <- rename(lambir_topo, HabType = HabType.x, soil = soil.x, x=x.x, y=y.x)
write.csv(lambir_topo, here("Desktop","Research","HCRP","Lambir Data", "lam_topo.csv"))
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#

#DNM_50_TWI <- mask(Danum_TWI, DNM_50); DNM_50_TWI <- crop(DNM_50_TWI, DNM_50, snap="out"); plot(DNM_50_TWI, col=rev(r2))
#cellStats(DNM_50_TWI, mean); cellStats(DNM_50_TWI, sd)

#SPKA_fp_TWI <- mask(Sepilok_TWI, spka); SPKA_fp_TWI <- crop(SPKA_fp_TWI, spka, snap="out"); plot(SPKA_fp_TWI, col=rev(r2))
#SPKS_fp_TWI <- mask(Sepilok_TWI, spks); SPKS_fp_TWI <- crop(SPKS_fp_TWI, spks, snap="out"); plot(SPKS_fp_TWI, col=rev(r2))
#SPKH_fp_TWI <- mask(Sepilok_TWI, spkh); SPKH_fp_TWI <- crop(SPKH_fp_TWI, spkh, snap="out"); plot(SPKH_fp_TWI, col=rev(r2))

#cellStats(SPKA_fp_TWI, mean); cellStats(SPKA_fp_TWI, sd)
#cellStats(SPKS_fp_TWI, mean); cellStats(SPKS_fp_TWI, sd)
#cellStats(SPKH_fp_TWI, mean); cellStats(SPKH_fp_TWI, sd)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
