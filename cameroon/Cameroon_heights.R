library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(raster)
library(rgdal)
library(sp)
library(plyr)


# load data----------
# Cameroon lidar
cameroon <- raster("~/Desktop/Research/Cameroon/CHM_1m.tif")
#plot(cameroon)


# Cameroon elev
camelev <- raster("~/Desktop/Research_2022/Data/GEDI/DTM_1m.tif")
#plot(camelev)


# gedi
dat <- read_csv("~/Desktop/Research_2022/Data/GEDI/cameroon_filtered_with_distances_all_algs.csv")
dat <- subset(dat, distance_to_camp <= 4)

# reserve- for crs
reserve <- readOGR('~/Desktop/Research/Cameroon/DjaFaunalReserve',"DjaFaunalReserveOSM")
#plot(reserve)
crs(reserve)

# convert GEDI data to spatial points dataframe
coords<- dat[,c("Longitude","Latitude")]
cam_proj <- crs(reserve)
spdf_cam <- SpatialPointsDataFrame(coords=coords,
                                   data=dat,
                                   proj4string=cam_proj)
#plot(spdf_cam)

# convert crs to utm
spdf_cam <- spTransform(spdf_cam, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs "))
extent(spdf_cam)
extent(cameroon)

# create buffer spdf
cam_buff <- gBuffer(spdf_cam, width= 11, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T

# export to python
writeOGR(cam_buff, dsn = "~/Desktop/Research_2022/Data", layer = "cam_buff_all",
         driver = "ESRI Shapefile" )



# extract
heights <- as(cam_buff, "data.frame")

# use mask & then crop
# crop(DNM_LAD_LAI, peakLAI, snap="out")
# SPK_LAD_LAI <- mask(stk, SPK)
# SPK_LAD_LAI <- crop(SPK_LAD_LAI, SPK, snap="out")
# convert list to df
# add x,y during extract function

heights$height <- raster::extract(cameroon, cam_buff, method = "simple")
heights$lidar_elev <- raster::extract(camelev, cam_buff, method = "simple", fun = mean, na.rm=T)

heights$height100 <- sapply(heights$height, function(x) quantile(x, 1, na.rm =T))
heights$height99 <- sapply(heights$height, function(x) quantile(x, .99, na.rm =T))
heights$height98 <- sapply(heights$height, function(x) quantile(x, .98, na.rm =T))
heights$height95 <- sapply(heights$height, function(x) quantile(x, .95, na.rm =T))
heights$height90 <- sapply(heights$height, function(x) quantile(x, .90, na.rm =T))
heights$height85 <- sapply(heights$height, function(x) quantile(x, .85, na.rm =T))
heights$height80 <- sapply(heights$height, function(x) quantile(x, .80, na.rm =T))
heights$height75 <- sapply(heights$height, function(x) quantile(x, .75, na.rm =T))
heights$height70 <- sapply(heights$height, function(x) quantile(x, .70, na.rm =T))
heights$height65 <- sapply(heights$height, function(x) quantile(x, .65, na.rm =T))
#-----------------------------------------------------------------------------------#
#------ Export heights Dataframe so you can stop running that code dear god----------
#-----------------------------------------------------------------------------------#
heights <- subset(heights, select = -c(height))
write.csv(heights, "~/Desktop/Research_2022/Data/GEDI/cameroon_height_data_all.csv")
heights <- read_csv("~/Desktop/Research_2022/Data/GEDI/cameroon_height_data_all.csv")
summary(heights)
#-----------------------------------------------------------------------------------#
#----------------- -----Add elevation from copernicus--------------------------------
#-----------------------------------------------------------------------------------#
# try to run on lab desktop 
# try function rm to remove everything you aren't using anymore
# read in cam_buff
# export to python
cam_buff <- readOGR("~/Desktop/Research_2022/Data", layer = "cam_buff_all")
# do extraction
elevation <- raster("~/Desktop/Research_2022/Data/elev_cameroon/elevation_cameroon.img")
plot(elevation)
crs(elevation)
summary(elevation$elevation_cameroon)
elevation <- projectRaster(elevation, crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs ")
crs(elevation)
heights$coper_elev <- raster::extract(elevation, cam_buff, method = "simple", fun = mean, na.rm=T)
summary(heights$coper_elev)
write.csv(heights, "~/Desktop/Research_2022/Data/GEDI/cameroon_height_data_all_coper.csv")
#-----------------------------------------------------------------------------------#
#----------------- -----Add field data set to heights--------------------------------
#-----------------------------------------------------------------------------------#
# load field data

fielddat <- read_csv("~/Desktop/Research_2022/Data/Field/cameroon_field_data.csv")
fielddat$Height <- na_if(fielddat$Height, "center")
fielddat$Height <- as.numeric(fielddat$Height)

# summarize field data using dplyr summarise function
field_sum <- fielddat %>% group_by(`Shot Number`) %>% dplyr::summarise(field_height100 = quantile(Height, probs = 1, na.rm=TRUE),
                                                                       field_height99 = quantile(Height, probs = .99, na.rm=TRUE),
                                                                       field_height98 = quantile(Height, probs = .98, na.rm=TRUE),
                                                                       field_height95 = quantile(Height, probs = .95, na.rm=TRUE),
                                                                       field_height90 = quantile(Height, probs = .90, na.rm=TRUE),
                                                                       field_height85 = quantile(Height, probs = .85, na.rm=TRUE),
                                                                       field_elevation = mean(`Elevation (m)`, na.rm = TRUE),
                                                                       )

# aggregate function
 
field_sum <- dplyr::rename(field_sum, Shot_Number = `Shot Number`) 
write.csv(field_sum, "~/Desktop/Research_2022/Data/GEDI/cameroon_fielddata_w_quantiles_all.csv")


gedifielddat <- merge(dat,field_sum,by="Shot_Number")
write.csv(gedifielddat, "~/Desktop/Research_2022/Data/GEDI/cameroon_gedi_field_all.csv")

full_dat <- merge(heights,field_sum,by="Shot_Number")
write.csv(full_dat, "~/Desktop/Research_2022/Data/GEDI/cameroon_field_lidar_all.csv")
#-----------------------------------------------------------------------------------#
#------------------------------Calculate Statistics----------------------------------
#-----------------------------------------------------------------------------------#

#---------------------------------Gedi to Lidar--------------------------------------
# remove nas
heights <- heights[!is.na(heights$height100), ] 
write.csv(heights, "~/Desktop/Research_2022/Data/GEDI/cam_field_gedi_lidar_all.csv")

library(Metrics)
# Calculate R squared and RMSE 
rmse(heights$rh100,heights$lidar_height)
rmse(heights$rh98,heights$lidar_height)

rmse(heights$rh100_a2,heights$lidar_height)
rmse(heights$rh98_a3,heights$lidar_height)

rmse(heights$rh100_a2,heights$lidar_height)
rmse(heights$rh98_a2,heights$lidar_height)


#-----------------------------------------------------------------------------------#
#----------------- Make a map of ground data with buffers----------------------------
#-----------------------------------------------------------------------------------#
fielddat <- read_csv("~/Desktop/Research_2022/Data/Field/cameroon_field_data.csv")
centerdat <- filter(fielddat, fielddat$Height == "center")
centerdat <-centerdat %>% filter(!is.na(Longitude))
fielddat <-fielddat %>% filter(!is.na(Longitude))
# convert all ground data to spatial points dataframe
coords<- fielddat[,c("Longitude","Latitude")]
cam_proj <- crs(reserve)
spdf_ground <- SpatialPointsDataFrame(coords=coords,
                                   data=fielddat,
                                   proj4string=cam_proj)
plot(spdf_ground)

# convert center points to spatial points dataframe
coords<- centerdat[,c("Longitude","Latitude")]
cam_proj <- crs(reserve)
spdf_center <- SpatialPointsDataFrame(coords=coords,
                                      data=centerdat,
                                      proj4string=cam_proj)
plot(spdf_center)


# Create a spdf for gedi shots
coords<- full_dat[,c("Longitude","Latitude")]
cam_proj <- crs(reserve)
spdf_gedi <- SpatialPointsDataFrame(coords=coords,
                                      data=full_dat,
                                      proj4string=cam_proj)
plot(spdf_gedi)


# convert crs to utm
spdf_ground <- spTransform(spdf_ground, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs "))
extent(spdf_ground)

# convert crs to utm
spdf_center <- spTransform(spdf_center, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs "))
extent(spdf_center)

# convert crs to utm
spdf_gedi <- spTransform(spdf_gedi, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs "))
extent(spdf_gedi)

# create buffer spdf
cam_buff <- gBuffer(spdf_gedi, width= 22, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(cam_buff)

center_buff <- gBuffer(spdf_center, width= 22, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(center_buff)

# plot
plot(cam_buff, col = "blue")
plot(center_buff, add = TRUE, col = "red")

# export and plot in python
