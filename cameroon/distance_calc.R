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


#ignore
gedi_dat <- read_csv("~/Desktop/SEAsia_test00100.csv")

# Load data to obtain CRS from
#irdplot <- readOGR('~/Desktop/Research/Cameroon/IRDplot',"Bouamir_IRDPlot")
#plot(irdplot)
#projection(irdplot)
#crs(irdplot)

reserve <- readOGR('~/Desktop/Research/Cameroon/DjaFaunalReserve',"DjaFaunalReserveOSM")
plot(reserve)
projection(reserve)
crs(reserve)

# Load Road Data
#roads <- readOGR('~/Desktop/Research/Cameroon/roads',"Bouamir_trails_roads")
#plot(roads)
#extent(roads)
#crs(roads) = crs(reserve)
#crs(roads)

# Load Bouamir location
camp <- readOGR('~/Desktop/Research/Cameroon/Bouamir_camp',"Bouamir_camp")
plot(camp)
extent(camp)
crs(camp)

# Load GEDI data
gedi_dat <- read_csv("~/Desktop/Research_2022/Data/cam_filtered_all_algs.csv")
gedi_dat$Shot_Number
#summary(gedi_dat)

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
#roads_utm <- spTransform(roads, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
cam_utm <- spTransform(spdf_cam, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
bouamir_utm <- spTransform(camp, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
#irdplot_utm <- spTransform(irdplot, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
#crs(bouamir_utm)

cam_utm1 <- as(cam_utm, "data.frame")

#calculate distance to nearest road
#gedi_dat$distance <- apply(gDistance(cam_utm, roads_utm,byid=TRUE),2,min)/1000
#summary(gedi_dat$distance)

#calculate distance to camp
gedi_dat$distance_to_camp <- apply(gDistance(cam_utm, bouamir_utm,byid=TRUE),2,min)/1000
summary(gedi_dat$distance_to_camp)

#calculate distance to IRD Plots
#gedi_dat$distance_to_IRD <- apply(gDistance(cam_utm, irdplot,byid=TRUE),2,min)/1000
#summary(gedi_dat$distance_to_IRD)

#-----------------------------------------------------#
#------------------------Export!----------------------
#-----------------------------------------------------#
write.csv(gedi_dat, "~/Desktop/Research_2022/Data/GEDI/cameroon_filtered_with_distances_all_algs.csv")

# filter out distances larger than half a kilometer
#gedi_low_distance <- filter(gedi_dat, distance <= .5)
#summary(gedi_low_distance$distance)
#summary(gedi_low_distance$rh100)

# export data to be plotted in python
#write.csv(gedi_dat, ("~/Desktop/Research/Cameroon/gedi_dist_full_final"))

#-----------------------------------------------------#
#-----------Make shapefiles for field work!!----------
#-----------------------------------------------------#
#---------All Points-------------
# Load data
dat <- read_csv("~/Desktop/Research/Cameroon/cameroon_all_points_for_shp")
summary(dat)
dat <- subset (dat, select = -c(...1, ...3))


coords<- dat[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_all <- SpatialPointsDataFrame(coords=coords,
                                   data=dat,
                                   proj4string=proj)
plot(spdf_all)
crs(spdf_all)
extent(spdf_all)

writeOGR(spdf_all, dsn = "~/Desktop/Research/Cameroon/", layer = "all_shots_cameroon",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "all_shots_cameroon")
check1 <- as(check, "data.frame")

# Make buffer!
spdf_all_buff <- spTransform(spdf_all, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
crs(spdf_all_buff)
extent(spdf_all_buff)

spdf_all_buff <- gBuffer(spdf_all_buff, width= 11, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(spdf_all_buff)

# export
writeOGR(spdf_all_buff, dsn = "~/Desktop/Research/Cameroon/", layer = "all_shots_cameroon_buffer",
         driver = "ESRI Shapefile" )

# Check again
check <- readOGR("~/Desktop/Research/Cameroon/", "all_shots_cameroon_buffer")
check1 <- as(check, "data.frame")


#---------Points of interest near camp-------------
shot_dat <- read_csv("~/Desktop/Research/Cameroon/cameroon_interest_points_for_shp")
summary(shot_dat)
shot_dat <- subset (shot_dat, select = -c(...1, ...3))


coords<- shot_dat[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_shots <- SpatialPointsDataFrame(coords=coords,
                                   data=shot_dat,
                                   proj4string=proj)
plot(spdf_shots)
crs(spdf_shots)
extent(spdf_shots)

writeOGR(spdf_shots, dsn = "~/Desktop/Research/Cameroon/", layer = "shots_near_camp_cameroon",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "shots_near_camp_cameroon")
check1 <- as(check, "data.frame")

# Make buffer!
spdf_shots_buff <- spTransform(spdf_shots, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
crs(spdf_shots_buff)
extent(spdf_shots_buff)

spdf_shots_buff <- gBuffer(spdf_shots_buff, width= 11, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(spdf_shots_buff)

# export
writeOGR(spdf_shots_buff, dsn = "~/Desktop/Research/Cameroon/", layer = "shots_near_camp_cameroon_buffer",
         driver = "ESRI Shapefile" )

# Check again
check <- readOGR("~/Desktop/Research/Cameroon/", "shots_near_camp_cameroon_buffer")
check1 <- as(check, "data.frame")

#---------Points within 300m of any trail-------------
#---------All Points-------------
# Load data
dat300 <- read_csv("~/Desktop/Research/Cameroon/cameroon_all_300m")
summary(dat300)
dat300 <- subset (dat300, select = -c(...1, ...3))


coords<- dat300[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_300 <- SpatialPointsDataFrame(coords=coords,
                                   data=dat300,
                                   proj4string=proj)
plot(spdf_300)
crs(spdf_300)
extent(spdf_300)

writeOGR(spdf_300, dsn = "~/Desktop/Research/Cameroon/", layer = "300m_shots_cameroon",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "300m_shots_cameroon")
check1 <- as(check, "data.frame")

# Make buffer!
spdf_300_buff <- spTransform(spdf_300, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
spdf_300_buff

spdf_300_buff <- gBuffer(spdf_300_buff, width= 11, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T
plot(spdf_300_buff)

spdf_300_buff <- spTransform(spdf_300, CRS("+proj=longlat +datum=WGS84 +no_defs"))
spdf_300_buff

# export
writeOGR(spdf_300_buff, dsn = "~/Desktop/Research/Cameroon/", layer = "300m_shots_cameroon_buffer",
         driver = "ESRI Shapefile" )

# Check again
check <- readOGR("~/Desktop/Research/Cameroon/", "300m_shots_cameroon_buffer")
check1 <- as(check, "data.frame")

# Shape files with algoritm 3 filtering
#---------close to camp-------------
# Load data
camp_a3 <- read_csv("~/Desktop/Research/Cameroon/shots_near_camp_alg3")
summary(camp_a3)
camp_a3 <- subset (camp_a3, select = -c(...1, ...3))


coords<- camp_a3[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_camp_a3 <- SpatialPointsDataFrame(coords=coords,
                                   data=camp_a3,
                                   proj4string=proj)
plot(spdf_camp_a3)
crs(spdf_camp_a3)
extent(spdf_camp_a3)

writeOGR(spdf_camp_a3, dsn = "~/Desktop/Research/Cameroon/", layer = "cam_camp_a3",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "cam_camp_a3")
check1 <- as(check, "data.frame")

#---------close to trail-------------
# Load data
trail_a3 <- read_csv("~/Desktop/Research/Cameroon/full_near_any_trail_a3_filter")
summary(trail_a3)
trail_a3 <- subset (trail_a3, select = -c(...1, ...3))


coords<- trail_a3[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_trail_a3 <- SpatialPointsDataFrame(coords=coords,
                                   data=trail_a3,
                                   proj4string=proj)
plot(spdf_trail_a3)
crs(spdf_trail_a3)
extent(spdf_trail_a3)

writeOGR(spdf_trail_a3, dsn = "~/Desktop/Research/Cameroon/", layer = "cam_trails_a3",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "cam_trails_a3")
check1 <- as(check, "data.frame")


#--------->60 trees-------------
# Load data
tall <- read_csv("~/Desktop/Research/Cameroon/tall_shots")
summary(tall)
tall <- subset (tall, select = -c(...1, ...3))


coords<- tall[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_tall <- SpatialPointsDataFrame(coords=coords,
                                        data=tall,
                                        proj4string=proj)
plot(spdf_tall)
crs(spdf_tall)
extent(spdf_tall)

writeOGR(spdf_tall, dsn = "~/Desktop/Research/Cameroon/", layer = "tall_shots",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "tall_shots")
check1 <- as(check, "data.frame")

#--------->60 trees-------------
# Load data
tall_75 <- read_csv("~/Desktop/Research/Cameroon/tall_shots_75")
summary(tall_75)
tall_75 <- subset (tall_75, select = -c(...1, ...3))


coords<- tall_75[,c("Longitude","Latitude")]
proj <- crs(reserve)
spdf_tall_75 <- SpatialPointsDataFrame(coords=coords,
                                    data=tall_75,
                                    proj4string=proj)
plot(spdf_tall_75)
crs(spdf_tall_75)
extent(spdf_tall_75)

writeOGR(spdf_tall_75, dsn = "~/Desktop/Research/Cameroon/", layer = "tall_shots_75",
         driver = "ESRI Shapefile" )

# Check
check <- readOGR("~/Desktop/Research/Cameroon/", "tall_shots_75")
check1 <- as(check, "data.frame")

