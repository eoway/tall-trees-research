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

cameroon <- raster("~/Desktop/Research/Cameroon/Large_CHM02.tif")
plot(cameroon)
crs(cameroon)

summary(cameroon)


shape_dat <- readOGR(dsn="~/Desktop/Research/Cameroon/IRDplot", layer="Bouamir_IRDPlot") 

crs(shape_dat)
shape_dat_longlat <- spTransform(shape_dat, CRS("+proj=longlat"))
writeOGR(shape_dat_longlat, dsn = "~/Desktop/Research/Cameroon/IRDplot", layer = "longlat",
         driver = "ESRI Shapefile" )
