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
library(rgeos)
require("maptools")

dat <- read_csv("~/Desktop/Research/Danum_GEDIdata/dan_GEDIdat.csv")
summary(dat$Longitude)
dat$id <- seq.int(nrow(dat)) 

Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif")
dan_proj <- crs(Danum_TWI)

#-1 
coords<- dat[,c("Latitude","Longitude")]
dandat <- SpatialPointsDataFrame(coords=coords,
                                               data=dat,
                                               proj4string=dan_proj)

plot(dandat)
dandat_buff <- gBuffer(dandat, width=.0025, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(dandat_buff)

spatialdan <- dandat_buff
#elev------ 
Danum_elev <- raster("~/Google Drive/My Drive/Research/elevdat/elevdat.tif"); 

e <- as(extent(116.492871, 117.820664, 3.849471, 6.094), 'SpatialPolygons')
crs(e) <- crs(Danum_elev)
delev <- crop(Danum_elev, e)
crs(spatialdan) = crs(Danum_elev)
plot(delev)

Danum_slope_aspect_TPI <- terrain(delev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
plot(Danum_slope_aspect_TPI)
plot(spatialdan)

#extract data
#elev
test <- raster::extract(Danum_elev,spatialdan)
head(test)
summary(test)
length(test)
dim(spatialdan)
analysis <- as(spatialdan, "data.frame")
analysis$elev <- raster::extract(delev,spatialdan)

#topo
temp <- raster::extract(Danum_slope_aspect_TPI$slope,spatialdan, )
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
