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

#elevation------ 
Danum_elev <- raster("~/Desktop/Research/HCRP/dan_dat/ASU_GAO_Danum_50HaPlot_GroundElev.tif")
plot(Danum_elev)

Danum_slope_aspect_TPI <- terrain(Danum_elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
plot(Danum_slope_aspect_TPI)

#twi--------
Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif")
plot(Danum_TWI)

# census data-------
dat <- read_csv("~/Desktop/Research/HCRP/Elsa Clean/main_dat.csv")

# filter to only Danum 50 hectare plot
dandat <- filter(dat, site == "DNM50")

# exclude stems less than 10cm
dandat <- filter(dandat, dbh >= 10)
summary(dandat$dbh)

#filter to latest census
dandat <- filter(dandat,census == "census_2019")

## add geographic coordinates
# 0,0 corner = 4.95144, 117.79219
# lat (4.95144) = UTM northing (547348.96)
# lon (117.79219) = UTM easting (587826.92)
# Sabah UTM Zone = 50 N
# the 500m PY actually runs from E to W (== x_utm) and the 1000m PX actually runs from S to N (== y_utm)
# x -> easting, y-> northing

dandat$x_utm <- (587826.92-dandat$plot_y)+500 # mirror x coords and shift back ... appear to align much better with RS data
dandat$y_utm <- (547348.96+dandat$plot_x)

#Create a spatial points dataframe
dandat_analysis <- dandat
coords<- dandat_analysis[,c("x_utm","y_utm")]
dan_proj <- crs(Danum_TWI)
spatialdan <- SpatialPointsDataFrame(coords=coords,
                                     data=dandat_analysis,
                                     proj4string=dan_proj)

#soils: removed from code - dataset we have indicates that it is just one soil type

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

#Height Equation---------
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

heig <- dbh2h_01(95,hgt_max_SEA,hgt_ref_SEA,b1Ht_SEA,b2Ht_SEA)

#Calculate Heights---------------
dandat_analysis$height <- dbh2h_01(dandat_analysis$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(dandat_analysis$height)

analysismetrics <- dandat_analysis %>% group_by(quadrat) %>% dplyr::summarize(
  dbhmean = mean(dbh, na.rm=T),
  heightmean = mean(height, na.rm=T),
  heightmedian = median(height, na.rm=T),
  height99 = quantile(height, probs = 0.99, na.rm = TRUE),
  heightmax = max(height,na.rm=T),
  quad_x = mean(x_utm),
  quad_y = mean(y_utm))

dim(analysismetrics)


summary(dandat_analysis)
dandat_analysis <- filter(dandat_analysis, dbh >=10)
write_csv(dandat_analysis, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_2022.csv")
