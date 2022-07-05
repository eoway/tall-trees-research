library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(fgeo)
library(plyr)
setwd("~/Desktop/Research/HCRP")

#----- PLOT COLOR PALETTE -----
library(colorRamps)
r2 <- matlab.like2(200)
r3 <- matlab.like2(4)
pal <- bpy.colors(99) 
#------------------------------

#---------------------------------------------------------------------------------------------#
#----------------------------------------- LAMBIR data ---------------------------------------# 
#---------------------------------------------------------------------------------------------#
# 4th census data
load("lambir.stem4.RData")
lambir4 <- lambir.stem4
# elevation
load("lambir.elev.RData")
# elevation?
load("CTFSElev_lambir.RData")
# soils
load("lambir.habs.Rdata")


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

#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
## Merge Lambir soil type indices with 4th census
#---------------------------------------------------------------------------------------------#
lambir4$index <- as.factor(lambir4$quadrat); lambir4$index <- as.numeric(lambir4$index)

length(unique(lambir4$index))

lam4 = merge(lambir4, lambir.habs, by = "index", all.x=T)

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

# create an ID column
lhp_elev_df$ID <- seq(1:length(lhp_elev_df$elev))

# Plot elevation
ggplot() + 
  geom_point(data=lhp_elev_df, aes(x,y, col=elev))



#---------------------------------------------------------------------------------------------#
# Test Summarizing
#---------------------------------------------------------------------------------------------#
test <- lam4 %>% group_by(quadrat, HabType, soil) %>% dplyr::summarize(x = mean(gx, na.rm=T),
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
## add geographic (lat/lon) coordinates for each tree and each quadrat
#---------------------------------------------------------------------------------------------#
lam4$treex <- 168797+lam4$gx
lam4$treey <- 463372+lam4$gy

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
crs(elev_rast) <- CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#crs(elev_rast) <- CRS("+proj=longlat +datum=WGS84")
projection(elev_rast)
extent(elev_rast)

# Calculate slope, aspect, and TPI
Lambir_slope_aspect_TPI <- terrain(elev_rast, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
summary(Lambir_slope_aspect_TPI)

#---------------------------------------------------------------------------------------------#
# use lambir.habs data where soil type is summarized into 5x5m sub-quadrats (n=1300)
# Lambir (n = 1300 20x20m quadrats; n = 21109 5x5m quadrats)
elev_rast_20m <- aggregate(elev_rast, fact=4.5)
topo_20m <- aggregate(Lambir_slope_aspect_TPI, fact=4.5)
res(elev_rast_20m); res(topo_20m)
plot(elev_rast_20m)
plot(topo_20m)

# assign 20m quadrat IDs vertically to match Lambir quadrats....
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
summary(elev_rast_20m)
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

#stack elevation and topography metrics
elev_topo_20m <- stack(elev_rast_20m, topo_20m)
plot(elev_topo_20m)

#plot
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

#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
#The Great Merge-------------
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#

lam4$quadrat <- as.character(lam4$index)
#filter data to only include stems greater than 10cm
lam4 <- filter(lam4, dbh>=10)

#combine last census data with topo and soil metrics
lambir <- inner_join(lam4, elev_soil, by="index")

#Remove duplicate columns
lambir <- subset(lambir, select = -c(HabType.y, soil.y))
lambir <- rename(lambir, replace= c("HabType.x" = "HabType", "soil.x" = "soil"))

# Calculate height metrics to be added to full dataset (allows for dataset to be by individual instead of by quadrat while still having height values)
heightmetrics <- lam4 %>% group_by(quadrat, index, soil, HabType) %>% dplyr::summarize(
  dbhmean = mean(dbh, na.rm=T),
  heightmean = mean(height, na.rm=T),
  heightmedian = median(height, na.rm=T),
  height99 = quantile(height, probs = 0.99, na.rm = TRUE),
  heightmax = max(height,na.rm=T))
# Join full dataset with height metrics
lambir_all <- inner_join(lambir, heightmetrics, by= "quadrat")
#Remove duplicate columns
lambir_all <- subset(lambir_all, select = -c(HabType.y, soil.y))
lambir_all <- rename(lambir_all, replace= c("HabType.x" = "HabType", "soil.x" = "soil"))



#---------------------------------------------------------------------------------------------#
# ALL LAMBIR
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
# TWI Data
#---------------------------------------------------------------------------------------------#
library(rgdal)
Lambir_TWI = raster("Lambir_TWI.tif"); plot(Lambir_TWI)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
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

#plot
ggplot() + 
  geom_point(data=twi_soil, aes(x,y, col=Lambir_TWI), size=8) + 
  scale_color_gradientn(colours=r2) +
  theme_classic()

plot(TWI_20m$Lambir_TWI, col=r2)

ggplot() + 
  geom_point(data=twi_soil, aes(x,y, col=soil), size=6) + 
  theme_classic()

#join full dataset with TWI and Soil metrics
lambir_all <- rename(lambir_all, replace = c("index.x" = "index"))
lambir_topo <- inner_join(lambir_all, twi_soil, by="index")
# Remove duplicate columns
lambir_topo <- subset(lambir_topo, select = -c(HabType.y, soil.y,x.x,y.x))
lambir_topo <- rename(lambir_topo, replace = c("HabType.x" = "HabType", "soil.x" = "soil", "x.y"="x", "y.y"="y", "sp"="species"))
summary(lambir_topo)

# Export data file
write.csv(lambir_topo, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_2022.csv")



#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#----------------------------Surrounding Tree Analysis Dataset--------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#-1
coords<- lambir_topo[,c("treex","treey")]
lam_proj <- crs(elev_rast)
lam_analysis_spdf <- SpatialPointsDataFrame(coords=coords,
                                            data=lambir_topo,
                                            proj4string=lam_proj)
plot(lam_analysis_spdf)

#0
lam_analysis_spdf$tree_type <- ifelse(lam_analysis_spdf$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")

lamdatemerg <- subset(lambir_topo, dbh >= quantile99dbh)
lamdatsamp <- subset(lambir_topo, dbh < quantile99dbh)
lamdatsamp <- sample_n(lamdatsamp, 10400)
summary(lamdatsamp)
lamdatemerg <- rbind(lamdatsamp, lamdatemerg)
dim(lamdatemerg)
coords<- lamdatemerg[,c("treex","treey")]
lamdat_emerg <- SpatialPointsDataFrame(coords=coords,
                                       data=lamdatemerg,
                                       proj4string=lam_proj)


#dandat_emerg <- subset(dandat_analysis_spdf, dbh >= quantile99dbh)
dim(lamdat_emerg); dim(lamdat_emerg)
#lamdat_emerg$ID <- 1:nrow(lamdat_emerg)
# coords<- dandat_emerg[,c("x_utm","y_utm")]
# emdan <- SpatialPointsDataFrame(coords=coords,
#                                 data=dandat_emerg,
#                                 proj4string=dan_proj)

plot(lamdat_emerg)

#3
library(rgeos)
#dandat_emerg <- buffer(dandat_emerg, width=5)
lamdat_emerg_buff <- gBuffer(lamdat_emerg, byid=T, width=5) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(lamdat_emerg_buff)
dim(lamdat_emerg_buff); dim(lamdat_emerg)
#EO: You should still have a separate row for each buffer polygon, equal to the number of points in dandat_emerg 
class(lamdat_emerg_buff)
#EO: notice that this is still a spatial dataframe (SpatialPolygonsDataFrame)

#4 & 5
#EO: Using the raster extract() function automatically integrates the two datasets below
lam_spdf <- raster::extract(lamdat_emerg_buff, lam_analysis_spdf)
dim(lam_spdf); dim(lam_analysis_spdf)
# EO: I think dan_spdf has more observations because there is redundancy in trees that are within multiple emergent tree  buffer zones
# EO: we can check this using the following three lines of code

unique_trees <- lam_spdf %>% group_by(point.ID) %>% dplyr::summarize(n=n())
summary(unique_trees)
subset(unique_trees, n > 1)
# we see that several trees IDs (point.ID) are repeated twice, indicating they exist within two overlapping emergent tree buffers

summary(lam_spdf)
summary(lam_spdf$dbh); summary(lam_analysis_spdf$dbh)

#EO: to count the number of trees within each emergent buffer (poly.ID) run the code below
trees_within_buff <- lam_spdf %>% group_by(poly.ID) %>% dplyr::summarize(n=n())
summary(trees_within_buff)
#EO: the minimum number of trees within a 5m buffer is 9, the max is definitely not 249,518 trees
#EO: that max number suggestes one of the polygons in dan_spdf is the larger plot boundary excluding the buffer polygons
#EO: remove that one using na.omit to get back to 270 polygons instead of 271
trees_within_buff <- na.omit(trees_within_buff)
summary(trees_within_buff)
#EO: now the max number of trees within an emergent buffer is 75
hist(trees_within_buff$n)

#6. Summarize neighboring trees
#EO: enter what you want to summarize in the code below
buff_summaries <- lam_spdf %>% group_by(poly.ID, treeID) %>% dplyr::summarize(heightmean=mean(height),
                                                                              dbhmean=mean(dbh),
                                                                              height99 = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                              n_trees = n())


#7. add summary variable to original dataset
lamdat_analysis_surr <- merge(lambir_topo,buff_summaries, by="treeID")
summary(lamdat_analysis_surr)

# Export data file
write.csv(lamdat_analysis_surr, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_surrounding_tree_data.csv")


#EO: add summary variables to original dataset based on the poly.ID from buff_sumaries and ...
#EO: ...the X1 or treeID value from dandat_emerg_buff, where the rows correspond to poly.ID 1:270