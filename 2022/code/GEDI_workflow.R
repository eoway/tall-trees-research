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
#dat$id <- seq.int(nrow(dat)) 

# small file -> just danum
Danum_elev <- raster("~/Desktop/Research/HCRP/dan_dat/ASU_GAO_Danum_50HaPlot_GroundElev.tif")
plot(Danum_elev)
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

# convert crs to utm
spdf_dan <- spTransform(spdf_dan, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"))
extent(spdf_dan)

# test data extraction
temp <- raster::extract(Danum_TWI,spdf_dan)
head(temp)
summary(temp)

# create buffer spdf
dandat_buff <- gBuffer(spdf_dan, width= 25, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(dandat_buff)

# plot data together to see overlap
plot(Danum_TWI)
plot(spdf_dan, add =TRUE)

plot(Danum_elev)
plot(Danum_TWI, add =TRUE)
plot(spdf_dan, add =TRUE)

# ok so for some reason elev doesnt overlap, I think somewhere I cropped it badly

# convert spdf to a dataframe to add data to
analysis <- as(spdf_dan, "data.frame")

# elev data extraction, method = bilinear
analysis$elev <- raster::extract(Danum_elev,dandat_buff, method = "bilinear")

# twi data extraction, method = bilinear
analysis$twi <- raster::extract(Danum_TWI, dandat_buff, method = "bilinear")

# Extract slope, aspect, tpi, and twi
analysis$slope <- raster::extract(Danum_slope_aspect_TPI$slope, dandat_buff, method = "bilinear")
analysis$aspect <- raster::extract(Danum_slope_aspect_TPI$aspect, dandat_buff, method = "bilinear")
analysis$tpi <- raster::extract(Danum_slope_aspect_TPI$tpi, dandat_buff, method = "bilinear")

# export dataframe for analyses
write.csv(analysis, "~/Desktop/Research_2022/Data/GEDI/workflow_gedi_data.csv")