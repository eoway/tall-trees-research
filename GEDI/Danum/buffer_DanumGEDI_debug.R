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

# load data----------
# gedi
dat <- read_csv("~/Desktop/Research/Danum_GEDIdata/dan_GEDIdat.csv")

# elevation
# small file -> just danum
Danum_elev <- raster("~/Desktop/Research/HCRP/dan_dat/ASU_GAO_Danum_50HaPlot_GroundElev.tif")
plot(Danum_elev)
slope_aspect_TPI <- terrain(Danum_elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
summary(slope_aspect_TPI)
# big file -> all of SE asia
delev <- raster("~/Google Drive/My Drive/Research/elevdat/elevdat.tif")
#summary(delev)

## Calculate slope, aspect, and tpi
Danum_slope_aspect_TPI <- terrain(Danum_elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')

#twi--------
Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif")
plot(Danum_TWI)

# check crs
# identify which file has long/lat
crs(Danum_TWI)
crs(Danum_elev)
crs(delev)

# convert GEDI data to spatial points dataframe
coords<- dat[,c("Longitude","Latitude")]
dan_proj <- crs(delev)
spdf_dan <- SpatialPointsDataFrame(coords=coords,
                                       data=dat,
                                       proj4string=dan_proj)
plot(spdf_dan)
spdf_dan

# convert crs to utm
spdf_dan <- spTransform(spdf_dan, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"))
spdf_dan

# test data extraction
temp <- raster::extract(Danum_TWI,spdf_dan)
head(temp)
summary(temp)

# create buffer spdf
dandat_buff <- gBuffer(spdf_dan, width= 25, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(dandat_buff)

# test data extraction, method = bilinear
temp <- raster::extract(Danum_elev, dandat_buff, method = "bilinear")
head(temp)
summary(temp)

# plot data together to see overlap
plot(Danum_TWI)
plot(spdf_dan, add =TRUE)

# try another way
temp <- raster::extract(Danum_TWI, spdf_dan, small = TRUE, buffer = 25)
head(temp)
tail(temp)
summary(temp)

# compare extents
extent(dandat_buff)
extent(Danum_TWI)
extent(Danum_elev)
