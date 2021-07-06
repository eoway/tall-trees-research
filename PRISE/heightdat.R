library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(raster)
require(XML)
require(rgdal)
require(raster)


setwd("/Volumes/LaCie/Research")
heightdat <- raster("Forest_height_2019_SASIA.tif"); plot(heightdat)

heightdat[heightdat > 100] <- NA

heightdat
table(heightdat)

plot(heightdat)

#save a new version of the raster


#soils
wcs <- "https://maps.isric.org/mapserv?map=/map/nitrogen.map&
  SERVICE=WCS&
  VERSION=2.0.0&
  REQUEST=GetCapabilities"
