library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(rgdal)
library(sp)
library(here)
library(plyr)
require("maptools")

dat <- read_csv("~/Desktop/Research/HCRP/Elsa Clean/main_dat.csv")
#dat <- read_csv("G:/My Drive/Research/cleandat/main_dat.csv")
#dat <- read_csv("G:/My Drive/Harvard/Plot_Data/clean_inventory_data/main_dat.csv")


#elev------ 
Danum_elev <- raster("~/Desktop/Research/HCRP/dan_dat/ASU_GAO_Danum_50HaPlot_GroundElev.tif"); plot(Danum_elev)
#Danum_elev <- raster("G:/My Drive/Harvard/Tall_trees_Borneo_project/Data/Ordway-Harvard-Danum-50HaPlotChems-20200324/ASU_GAO_Danum_50HaPlot_GroundElev.tif"); plot(Danum_elev)
Danum_slope_aspect_TPI <- terrain(Danum_elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
#topo_dat <-stack(Danum_elev) LOOK AT SCREENSHOT
#dan_elev_rast_20m <- aggregate(Danum_elev, fact=10)
#dan_topo_20m <- aggregate(Danum_slope_aspect_TPI, fact=10)
#res(dan_elev_rast_20m); res(dan_topo_20m)
plot(Danum_slope_aspect_TPI)

#twi--------
Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif"); plot(Danum_TWI)
#Danum_TWI <- raster("G:/My Drive/Harvard/CAO_data/GIS/Danum_TWI.tif"); plot(Danum_TWI)
cellStats(Danum_TWI, mean); cellStats(Danum_TWI, sd)

#Main dat--------
dandat <- filter(dat, site == "DNM50")
dandat <- filter(dandat, dbh >= 10)
summary(dandat)
table(dandat$census)
dandat <- filter(dat,census == "census_2019")
colnames(dandat)

## add geographic coordinates
# 0,0 corner = 4.95144, 117.79219
# lat (4.95144) = UTM northing (547348.96)
# lon (117.79219) = UTM easting (587826.92)
# Sabah UTM Zone = 50 N
# the 500m PY actually runs from E to W (== x_utm) and the 1000m PX actually runs from S to N (== y_utm)
#x -> easting, y-> northing

dandat$x_utm <- (587826.92-dandat$plot_y)+500 # mirror x coords and shift back ... appear to align much better with RS data
dandat$y_utm <- (547348.96+dandat$plot_x)

plot(dandat$x_utm,dandat$y_utm)

dandat_analysis <- dandat
coords<- dandat_analysis[,c("x_utm","y_utm")]
dan_proj <- crs(Danum_TWI)

spatialdan <- SpatialPointsDataFrame(coords=coords,
                                     data=dandat_analysis,
                                     proj4string=dan_proj)
class(spatialdan)
spplot(spatialdan, "dbh")
colnames(dandat)


#soils-----
dantest <- filter(dandat, dbh=="92")
shape_dat <- readOGR(dsn="~/Desktop/Research/HCRP/dan_dat", layer="soil_association_utm50n") 
#shape_dat <- readOGR(dsn="G:/My Drive/GIS_Data/SE_Asia/Soils_Topo", layer="soil_association_utm50n") 
# filter soils shapefile to only SO_ASSOCIA [5] or SOIL_CLASS [10]
colnames(shape_dat@data)
soil_type = shape_dat[10]

test <- raster::extract(soil_type,spatialdan)
head(test)
length(test)
dim(test)
# unfortunately, this dataset indicates only one soil type in the 50-ha plot
#table(test$SO_ASSOCIA) 
table(test$SOIL_CLASS)
dim(spatialdan)
# It turns out the raster extract function works after all!!
dandat_analysis$soil <- raster::extract(shape_dat,spatialdan)

# datapol <- data.frame(shape_dat)
# points <- data.frame(x=dantest$x_utm, y=dantest$y_utm)
# coordinates(points) <- ~ x + y 
# proj4string(points) <- crs(shape_dat) #shape_dat; spatialdan

# #function over from package sp
# test <- data.frame(xx=over(shape_dat, points))
# combine <- cbind(test, datapol)
# combine <- na.omit(combine) #only one point left


test <- raster::extract(Danum_TWI,spatialdan)
head(test)
length(test)
dim(spatialdan)
dandat_analysis$twi <- raster::extract(Danum_TWI,spatialdan)

temp <- raster::extract(Danum_slope_aspect_TPI$slope,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
dandat_analysis$slope <- raster::extract(Danum_slope_aspect_TPI$slope,spatialdan)

temp <- raster::extract(Danum_slope_aspect_TPI$aspect,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
dandat_analysis$aspect <- raster::extract(Danum_slope_aspect_TPI$aspect,spatialdan)

temp <- raster::extract(Danum_slope_aspect_TPI$tpi,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
dandat_analysis$tpi <- raster::extract(Danum_slope_aspect_TPI$tpi,spatialdan)

temp <- raster::extract(Danum_elev,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
dandat_analysis$elev <- raster::extract(Danum_elev,spatialdan)

#temp <- raster::extract(rastersoil,spatialdan)
#head(temp)
#length(temp)
#dim(spatialdan)
#dandat_analysis$soil <- raster::extract(shape_dat,spatialdan)

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
dandat_analysis$height <- dbh2h_01(dandat_analysis$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(dandat_analysis$height)

#the merge
#EO: I added dply:: before summarize because it was using some other summarize function that
# ...resulted in 1 observation instead of 1250 (the number of unique quadrats in dandat_analysis)
analysismetrics <- dandat_analysis %>% group_by(quadrat) %>% dplyr::summarize(
  dbhmean = mean(dbh, na.rm=T),
  heightmean = mean(height, na.rm=T),
  heightmedian = median(height, na.rm=T),
  height99 = quantile(height, probs = 0.99, na.rm = TRUE),
  heightmax = max(height,na.rm=T),
  quad_x = mean(x_utm),
  quad_y = mean(y_utm))

dim(analysismetrics)


summary(dandat_analysis)
dandat_analysis <- filter(dandat_analysis, dbh >=10)

#---------------------------------------------------------------------------------------------#
#----------------------------Surrounding Tree Analysis Dataset--------------------------------
#---------------------------------------------------------------------------------------------#
#Elsa Help
#Workflow
#-1. Create a spatial points dataframe from entire Danum dataset
#0. Create a column to label emergents and nonemergents
#1. Restrict dataset to emergents
#2. Give each emergent a unique ID
#3. Create buffer around emergent individuals
#4. Intersect with original dataset
#5. add corresponding ID to nonemergent trees within each buffer (e.g. if a tree is in the buffer of emergent tree with an ID of 96, label the tree with an ID of 96 also)
#6. Summarize neighboring trees
#7. add summary variable to original dataset

#---------------------------------------------------------------------------------------------#
#----------------------------Surrounding Tree Analysis Dataset--------------------------------
#-------------------------------------sampling >=10 cm----------------------------------------#

#-1 
coords<- dandat_analysis[,c("x_utm","y_utm")]
dandat_analysis_spdf <- SpatialPointsDataFrame(coords=coords,
                                data=dandat_analysis,
                                proj4string=dan_proj)

#0
dandat_analysis_spdf$tree_type <- ifelse(dandat_analysis_spdf$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")

#1 & 2
dandatemerg <- subset(dandat_analysis, dbh >= quantile99dbh)

dandatsamp <- subset(dandat_analysis, dbh < quantile99dbh)
dandatsamp <- sample_n(dandatsamp, 325)

dandat_emerg <- rbind(dandatsamp, dandatemerg)

coords<- dandat_emerg[,c("x_utm","y_utm")]
dandat_emerg <- SpatialPointsDataFrame(coords=coords,
                                               data=dandat_emerg,
                                               proj4string=dan_proj)


#dandat_emerg <- subset(dandat_analysis_spdf, dbh >= quantile99dbh)
dim(dandat_analysis_spdf); dim(dandat_emerg)
dandat_emerg$ID <- 1:nrow(dandat_emerg)
# coords<- dandat_emerg[,c("x_utm","y_utm")]
# emdan <- SpatialPointsDataFrame(coords=coords,
#                                 data=dandat_emerg,
#                                 proj4string=dan_proj)

plot(dandat_emerg)

#3
library(rgeos)
#dandat_emerg <- buffer(dandat_emerg, width=5)
dandat_emerg_buff <- gBuffer(dandat_emerg, byid=T, width=5) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(dandat_emerg_buff)
dim(dandat_emerg_buff); dim(dandat_emerg)
#EO: You should still have a separate row for each buffer polygon, equal to the number of points in dandat_emerg 
class(dandat_emerg_buff)
#EO: notice that this is still a spatial dataframe (SpatialPolygonsDataFrame)

#4 & 5
#EO: Using the raster extract() function automatically integrates the two datasets below
dan_spdf <- raster::extract(dandat_emerg_buff, dandat_analysis_spdf)
dim(dan_spdf); dim(dandat_analysis_spdf)
# EO: I think dan_spdf has more observations because there is redundancy in trees that are within multiple emergent tree  buffer zones
# EO: we can check this using the following three lines of code

unique_trees <- dan_spdf %>% group_by(point.ID) %>% dplyr::summarize(n=n())
summary(unique_trees)
subset(unique_trees, n > 1)
# we see that several trees IDs (point.ID) are repeated twice, indicating they exist within two overlapping emergent tree buffers

summary(dan_spdf)
summary(dan_spdf$dbh); summary(dandat_analysis_spdf$dbh)

#EO: to count the number of trees within each emergent buffer (poly.ID) run the code below
trees_within_buff <- dan_spdf %>% group_by(poly.ID) %>% dplyr::summarize(n=n())
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
buff_summaries <- dan_spdf %>% group_by(poly.ID, X1) %>% dplyr::summarize(heightmean=mean(height),
                                                                      dbhmean=mean(dbh),
                                                                      n_trees = n())
                                                                      

#7. add summary variable to original dataset
dandat_analysis_surr <- merge(dandat_analysis,buff_summaries, by="X1")

#EO: add summary variables to original dataset based on the poly.ID from buff_sumaries and ...
#EO: ...the X1 or treeID value from dandat_emerg_buff, where the rows correspond to poly.ID 1:270
#EO: let me know if you run into more questions here

#---------------------------------------------------------------------------------------------#
#----------------------------Surrounding Tree Analysis Dataset--------------------------------
#-------------------------------------sampling >=60 cm----------------------------------------#
#-1 
coords<- dandat_analysis[,c("x_utm","y_utm")]
dandat_analysis_spdf <- SpatialPointsDataFrame(coords=coords,
                                               data=dandat_analysis,
                                               proj4string=dan_proj)

#0
dandat_analysis_spdf$tree_type <- ifelse(dandat_analysis_spdf$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")

#1 & 2
dandatemerg <- subset(dandat_analysis, dbh >= quantile99dbh)

dandatsamp <- subset(dandat_analysis, dbh < quantile99dbh & dbh >= 60)
summary(dandatsamp)
dandatsamp <- sample_n(dandatsamp, 325)

dandat_emerg <- rbind(dandatsamp, dandatemerg)

coords<- dandat_emerg[,c("x_utm","y_utm")]
dandat_emerg <- SpatialPointsDataFrame(coords=coords,
                                       data=dandat_emerg,
                                       proj4string=dan_proj)


#dandat_emerg <- subset(dandat_analysis_spdf, dbh >= quantile99dbh)
dim(dandat_analysis_spdf); dim(dandat_emerg)
dandat_emerg$ID <- 1:nrow(dandat_emerg)
# coords<- dandat_emerg[,c("x_utm","y_utm")]
# emdan <- SpatialPointsDataFrame(coords=coords,
#                                 data=dandat_emerg,
#                                 proj4string=dan_proj)

plot(dandat_emerg)

#3
library(rgeos)
#dandat_emerg <- buffer(dandat_emerg, width=5)
dandat_emerg_buff <- gBuffer(dandat_emerg, byid=T, width=5) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(dandat_emerg_buff)
dim(dandat_emerg_buff); dim(dandat_emerg)
#EO: You should still have a separate row for each buffer polygon, equal to the number of points in dandat_emerg 
class(dandat_emerg_buff)
#EO: notice that this is still a spatial dataframe (SpatialPolygonsDataFrame)

#4 & 5
#EO: Using the raster extract() function automatically integrates the two datasets below
dan_spdf <- raster::extract(dandat_emerg_buff, dandat_analysis_spdf)
dim(dan_spdf); dim(dandat_analysis_spdf)
# EO: I think dan_spdf has more observations because there is redundancy in trees that are within multiple emergent tree  buffer zones
# EO: we can check this using the following three lines of code

unique_trees <- dan_spdf %>% group_by(point.ID) %>% dplyr::summarize(n=n())
summary(unique_trees)
subset(unique_trees, n > 1)
# we see that several trees IDs (point.ID) are repeated twice, indicating they exist within two overlapping emergent tree buffers

summary(dan_spdf)
summary(dan_spdf$dbh); summary(dandat_analysis_spdf$dbh)

#EO: to count the number of trees within each emergent buffer (poly.ID) run the code below
trees_within_buff <- dan_spdf %>% group_by(poly.ID) %>% dplyr::summarize(n=n())
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
buff_summaries <- dan_spdf %>% group_by(poly.ID, X1) %>% dplyr::summarize(heightmean=mean(height),
                                                                          dbhmean=mean(dbh),
                                                                          n_trees = n())


#7. add summary variable to original dataset
dandat_analysis_surr_60 <- merge(dandat_analysis,buff_summaries, by="X1")

#EO: add summary variables to original dataset based on the poly.ID from buff_sumaries and ...
#EO: ...the X1 or treeID value from dandat_emerg_buff, where the rows correspond to poly.ID 1:270
#EO: let me know if you run into more questions here




#Export file
write.csv(dandat_analysis, ("~/Desktop/Research/HCRP/dan_dat/dan_topo.csv"))

write.csv(dandat_analysis_surr, ("~/Desktop/Research/HCRP/dan_dat/dan_surr.csv"))

write.csv(dandat_analysis_surr_60, ("~/Desktop/Research/HCRP/dan_dat/dan_surr60.csv"))
