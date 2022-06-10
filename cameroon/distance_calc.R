library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(sp)
library(raster)
require(XML)
require(rgdal)
require(raster)
library(rgeos)

# Load data to obtain CRS from
irdplot <- readOGR('~/Desktop/Research/Cameroon/IRDplot',"Bouamir_IRDPlot")
plot(irdplot)
projection(irdplot)
crs(irdplot)

reserve <- readOGR('~/Desktop/Research/Cameroon/DjaFaunalReserve',"DjaFaunalReserveOSM")
plot(reserve)
projection(reserve)
crs(reserve)

# Load Road Data
roads <- readOGR('~/Desktop/Research/Cameroon/roads',"Bouamir_trails_roads")
plot(roads)
extent(roads)
crs(roads) = crs(reserve)
crs(roads)

# Load Bouamir location
camp <- readOGR('~/Desktop/Research/Cameroon/Bouamir_camp',"Bouamir_camp")
plot(camp)
extent(camp)
crs(camp)

# Load GEDI data
gedi_dat <- read_csv("~/Desktop/Research/Cameroon/cameroon_filtered_cov_95")
summary(gedi_dat)

# Convert it to a spatial points dataframe
coords<- gedi_dat[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_cam <- SpatialPointsDataFrame(coords=coords,
                                   data=gedi_dat,
                                   proj4string=proj)
plot(spdf_cam)
crs(spdf_cam)
extent(spdf_cam)

# Convert crs to UTM to calculate distance in meters
# rather than lat long degrees
roads_utm <- spTransform(roads, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
cam_utm <- spTransform(spdf_cam, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
bouamir_utm <- spTransform(camp, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
irdplot_utm <- spTransform(irdplot, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
crs(bouamir_utm)

#calculate distance to nearest road
gedi_dat$distance <- apply(gDistance(cam_utm, roads_utm,byid=TRUE),2,min)/1000
summary(gedi_dat$distance)

#calculate distance to camp
gedi_dat$distance_to_camp <- apply(gDistance(cam_utm, bouamir_utm,byid=TRUE),2,min)/1000
summary(gedi_dat$distance_to_camp)

#calculate distance to IRD Plots
gedi_dat$distance_to_IRD <- apply(gDistance(cam_utm, irdplot,byid=TRUE),2,min)/1000
summary(gedi_dat$distance_to_IRD)

# filter out distances larger than half a kilometer
#gedi_low_distance <- filter(gedi_dat, distance <= .5)
#summary(gedi_low_distance$distance)
#summary(gedi_low_distance$rh100)

# export data to be plotted in python
write.csv(gedi_dat, ("~/Desktop/Research/Cameroon/gedi_dist_cov_95"))
