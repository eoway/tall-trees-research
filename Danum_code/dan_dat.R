library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(fgeo)

dat <- read_csv("~/Desktop/Research/HCRP/Elsa Clean/main_dat.csv")

dandat <- filter(dat,site == "DNM50")
table(dandat$census)
dandat <- filter(dat,census == "census_2019")
colnames(dandat)

## add geographic coordinates
# 0,0 corner = 4.95144, 117.79219
# lat (4.95144) = UTM northing (547348.96)
# lon (117.79219) = UTM easting (587826.92)
# Sabah UTM Zone = 50 N
# the 500m PY actually runs from E to W (== x_utm) and the 1000m PX actually runs from S to N (== y_utm)
dandat$x_utm <- (587826.92-dandat$plot_y)+500 # mirror x coords and shift back ... appear to align much better with RS data
dandat$y_utm <- (547348.96+dandat$plot_x)

summary(dandat$x_utm)
summary(dandat$y_utm)

coords <- cbind(x=)

#Make polygon file
polydf <- Polygon(coords, hole=as.logical(NA))
