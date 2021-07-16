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

#---------------------------------------------------------------------------------------------#
#----------------------------Surrounding Tree Analysis Dataset--------------------------------
#-------------------------------------sampling >=10 cm----------------------------------------#

surdat <- read_csv("~/Desktop/Research/HCRP/dan_dat/dan_surr.csv")

summary(surdat)

lm.helev <- lm(height~heightmean, data=surdat)
summary(lm.helev)
