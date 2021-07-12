library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(raster)
require(XML)
require(rgdal)
require(raster)


setwd("G:/My Drive/Research")
heightdat <- raster("height_data/Forest_height_2019_SASIA.tif"); plot(heightdat)

#Elsa Help
heightdat[heightdat > 100] <- NA

heightdat
table(heightdat)

plot(heightdat)

#save a new version of the raster


#water data

waterdat <- readOGR(dsn="G:/My Drive/Research/river_data", layer="._rivers_asia_37331") 
