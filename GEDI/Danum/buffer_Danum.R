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

# load data
# gedi
dat <- read_csv("~/Desktop/Research/Danum_GEDIdata/dan_GEDIdat.csv")
summary(dat$Longitude)
dat$id <- seq.int(nrow(dat)) 
# elev
Danum_elev <- raster("~/Google Drive/My Drive/Research/elevdat/elevdat.tif")
NAvalue(Danum_elev) <- -9999
plot(Danum_elev)
# plot(Danum_elev) 
# twi
Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif")

# Make spatial points dataframe with gedi data
dan_proj = crs(Danum_elev)
dan_proj
coords<- dat[,c("Longitude","Latitude")]
dandat <- SpatialPointsDataFrame(coords=coords,
                                        data=dat,
                                        proj4string=dan_proj)
crs(dandat) = crs(Danum_TWI)
plot(dandat)

#test extracting data
test <- raster::extract(Danum_elev, dandat)
head(test)
summary(test)
length(test)
dim(spatialdan)

# make buffer
dandat_buff <- gBuffer(dandat, width=.0025, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(dandat_buff)

spatialdan <- dandat_buff

# code to crop danum elev but isn't working?
#e <- as(extent(116.492871, 118.820664, 3.849471, 6.094), 'SpatialPolygons')
#crs(e) <- crs(Danum_elev)

#delev <- crop(Danum_elev, e)
crs(spatialdan) = crs(Danum_elev)
plot(delev)

Danum_slope_aspect_TPI <- terrain(delev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
plot(Danum_slope_aspect_TPI)
plot(spatialdan)

#extract data
#elev
crs(Danum_elev)
crs(spatialdan)
test <- raster::extract(Danum_elev,dandat)
plot(Danum_elev)
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

# TWI
crs(Danum_TWI)
crs(spatialdan)

test <- raster::extract(Danum_TWI,spatialdan, method = "bilinear")
head(test)
summary(test)
length(test)
dim(spatialdan)
analysis <- as(spatialdan, "data.frame")
analysis$elev <- raster::extract(Danum_TWI,spatialdan, method = "bilinear")

