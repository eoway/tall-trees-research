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

spatialdan <- readOGR(dsn="~/Desktop/Research/GEDI/Danum", layer="danum_latlong") 
plot(spatialdan)
#elev------ 
Danum_elev <- raster("~/Google Drive/My Drive/Research/elevdat/elevdat.tif"); 
 
e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
crs(e) <- crs(Danum_elev)
delev <- crop(Danum_elev, e)
crs(spatialdan) = crs(Danum_elev)
plot(delev)

Danum_slope_aspect_TPI <- terrain(delev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
plot(Danum_slope_aspect_TPI)


# Water
#waterdat <- readOGR("~/Google Drive/My Drive/Research/river_data/rivers_asia_37331","rivers_asia_37331")
#plot(waterdat)
#projection(waterdat)
#crs(samp)
#crs(waterdat)
##calculate distance to nearest river
#library(rgeos)
#spatialdan$distance <- apply(gDistance(spatialdan, waterdat,byid=TRUE),2,min)
#summary(spatialdan$distance)

#extract data
#elev
test <- raster::extract(delev,spatialdan)
head(test)
length(test)
dim(spatialdan)
analysis <- as(spatialdan, "data.frame")
analysis$elev <- raster::extract(delev,spatialdan)

#topo
temp <- raster::extract(Danum_slope_aspect_TPI$slope,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$slope <- raster::extract(Danum_slope_aspect_TPI$slope,spatialdan)

temp <- raster::extract(Danum_slope_aspect_TPI$aspect,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$aspect <- raster::extract(Danum_slope_aspect_TPI$aspect,spatialdan)

temp <- raster::extract(Danum_slope_aspect_TPI$tpi,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$tpi <- raster::extract(Danum_slope_aspect_TPI$tpi,spatialdan)

soilbd <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_BD_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilbd)
#print(crs(e))
#print(crs(soilbd))
#soilbd <- crop(soilbd, e)
#soilbd <- projectRaster(soilbd, crs=crs(delev))

soilSOC <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_SOC_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilSOC)
#soilSOC <- crop(soilSOC, e)
#soilSOC <- projectRaster(soilSOC, crs=crs(delev))

soilSand <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_Sand_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilSand)
#soilSand <- crop(soilSand, e)
#soilSand <- projectRaster(soilSand, crs=crs(delev))

soilpH <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_pH_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilpH)
#delev <- crop(soilpH, e)
#soilpH <- projectRaster(soilpH, crs=crs(delev))

soilnit <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_N_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilnit)
#delev <- crop(soilnit, e)
#soilnit <- projectRaster(soilnit, crs=crs(delev))

soilclay <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_Clay_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilclay)
#delev <- crop(soilclay, e)
#soilclay <- projectRaster(soilclay, crs=crs(delev))

soilcec <- raster("~/Google Drive/My Drive/Research/soils/SoilGrids_CEC_SEA-0000000000-0000000000.tif")
#e <- as(extent(117.492871, 117.820664, 4.849471, 5.092841), 'SpatialPolygons')
#crs(e) <- crs(soilcec)
#delev <- crop(soilcec, e)
#soilcec <- projectRaster(soilcec, crs=crs(delev))

#bulk density
temp <- raster::extract(soilbd, spatialdan)
head(temp)
summary(temp)
length(temp)
dim(spatialdan)
analysis$bulkdensity <- raster::extract(soilbd,spatialdan)

#soc
temp <- raster::extract(soilSOC,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$soc <- raster::extract(soilSOC,spatialdan)

#sand
temp <- raster::extract(soilSand,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$sand <- raster::extract(soilSand,spatialdan)

#ph
temp <- raster::extract(soilpH,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$ph <- raster::extract(soilpH,spatialdan)

#nitrogen
temp <- raster::extract(soilnit,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$nitrogen <- raster::extract(soilnit,spatialdan)

#clay
temp <- raster::extract(soilclay,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$clay <- raster::extract(soilclay,spatialdan)

#cec
temp <- raster::extract(soilcec,spatialdan)
head(temp)
length(temp)
dim(spatialdan)
analysis$cec <- raster::extract(soilcec,spatialdan)

#heights
heightdat <- raster("~/Google Drive/My Drive/Research/SEA_Regional_Datasets/Forest_height_2019_SASIA_subset.tif")
temp <- raster::extract(heightdat,spatialdan)
summary(temp)
head(temp)
length(temp)
dim(heightsample)
analysis$height <- raster::extract(heightdat,spatialdan)

#Export file

write.csv(analysis, ("~/Desktop/Research/GEDI/Danum/dan_topo.csv"))
#dat <- read_csv("~/Desktop/Research/HCRP/dan_dat/dan_topo.csv")
#summary(dat)