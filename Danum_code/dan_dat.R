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

#elev------ 
Danum_elev <- raster("~/Desktop/Research/HCRP/dan_dat/ASU_GAO_Danum_50HaPlot_GroundElev.tif"); plot(Danum_elev)
Danum_slope_aspect_TPI <- terrain(Danum_elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
#topo_dat <-stack(Danum_elev) LOOK AT SCREENSHOT
#dan_elev_rast_20m <- aggregate(Danum_elev, fact=10)
#dan_topo_20m <- aggregate(Danum_slope_aspect_TPI, fact=10)
#res(dan_elev_rast_20m); res(dan_topo_20m)
plot(Danum_slope_aspect_TPI)
plot(Danum_slope_aspect_TPI)

#twi--------
Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif"); plot(Danum_TWI)
cellStats(Danum_TWI, mean); cellStats(Danum_TWI, sd)

#Main dat--------
dandat <- filter(dat,site == "DNM50")
dandat <- filter(dandat, dbh >= 10)
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

#Elsa Help----------------------------
#soils-----
dantest <- filter(dandat, dbh=="92")
shape_dat <- readOGR(dsn="~/Desktop/Research/HCRP/dan_dat", layer="soil_association_utm50n") 

datapol <- data.frame(shape_dat)
points <- data.frame(x=dantest$x_utm, y=dantest$y_utm)
coordinates(points) <- ~ x + y 
proj4string(points) <- crs(spatialdan)
#
#function over from package sp
test <- data.frame(over(xx=shape_dat, points))
combine <- cbind(test, datapol)
combine <- na.omit(combine) #only one point left







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
analysismetrics <- dandat_analysis %>% group_by(quadrat) %>% summarize( 
  dbhmean = mean(dbh, na.rm=T),
  heightmean = mean(height, na.rm=T),
  heightmedian = median(height, na.rm=T),
  height99 = quantile(height, probs = 0.99, na.rm = TRUE),
  heightmax = max(height,na.rm=T),
  quad_x = mean(x_utm),
  quad_y = mean(y_utm))

dan_all <- inner_join(lambir, analysismetrics, by= "quadrat")


write.csv(dandat_analysis, here("Desktop","Research","HCRP","dan_dat", "dan_topo.csv"))
