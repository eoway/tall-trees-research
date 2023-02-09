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

# GEDI
#dat <- read_csv("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_ALL.csv")
dat <- read_csv("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_GEDI/SEAsia_trunc_processed/SEAsia00300.csv")

# elevation
elev <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SRTM/elevdat.tif")

## Calculate slope, aspect, and tpi
slope_aspect_TPI <- terrain(elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')

# water
water <- readOGR("/n/holyscratch01/moorcroft_lab/nhegwood/Water/rivers_asia","rivers_asia_37331")

# soil
clay05 <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/clay_0-5.tif")

nitro05 <-raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/nitro_0-5.tif")

sand05 <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/sand_0-5.tif")

SOC05 <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/SOC_0-5.tif")

  
# convert GEDI data to spatial points dataframe
coords<- dat[,c("Longitude","Latitude")]
proj <- crs(elev)
spdf_dat <- SpatialPointsDataFrame(coords=coords,
                                   data=dat,
                                   proj4string=proj)

# convert crs to utm
#spdf_dan <- spTransform(spdf_dan, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"))
#extent(spdf_dan)

# create buffer spdf
dat_buff <- gBuffer(spdf_dat, width= 11, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T

# convert spdf to a dataframe to add data to
analysis <- as(spdf_dat, "data.frame")

# elev 
analysis$elev <- raster::extract(elev,dat_buff, method = "bilinear")

# slope 
analysis$slope <- raster::extract(slope_aspect_TPI$slope,dat_buff, method = "bilinear")

# aspect 
analysis$aspect <- raster::extract(slope_aspect_TPI$aspect,dat_buff, method = "bilinear")

# tpi 
analysis$tpi <- raster::extract(slope_aspect_TPI$tpi,dat_buff, method = "bilinear")

# clay
analysis$clay <- raster::extract(clay05,dat_buff, method = "bilinear")

# nitro05
analysis$nitro <- raster::extract(nitro05,dat_buff, method = "bilinear")

# sand05
analysis$sand <- raster::extract(sand05,dat_buff, method = "bilinear")

# SOC05
analysis$SOC05 <- raster::extract(SOC05,dat_buff, method = "bilinear")

# distance to water
analysis$distance <- apply(gDistance(spdf_dat, water,byid=TRUE),2,min)



# export dataframe for analyses
write.csv(analysis, "/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_test.csv")