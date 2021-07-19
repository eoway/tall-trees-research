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

plot(surdat$plot_x,surdat$plot_y)
emergsurdat <- filter(surdat, dbh >= 95)
plot(emergsurdat$plot_x,emergsurdat$plot_y)
nonsurdat <- filter(surdat, dbh < 95)
plot(nonsurdat$plot_x,nonsurdat$plot_y)

summary(nonsurdat)
summary(emergsurdat)

t.test(nonsurdat$heightmean,emergsurdat$heightmean)

t.test(nonsurdat$n_trees,emergsurdat$n_trees)

t.test(nonsurdat$heightmean,emergsurdat$heightmean)


#---------------------------------------------------------------------------------------------#
#----------------------------Surrounding Tree Analysis Dataset--------------------------------
#-------------------------------------sampling >=60 cm----------------------------------------#

surdat60 <- read_csv("~/Desktop/Research/HCRP/dan_dat/dan_surr60.csv")
plot(surdat60$plot_x,surdat60$plot_y)

emergsurdat60 <- filter(surdat60, dbh >= 95)
nonsurdat60 <- filter(surdat60, dbh < 95)


summary(nonsurdat60)
summary(emergsurdat60)

t.test(nonsurdat60$heightmean,emergsurdat60$heightmean)

t.test(nonsurdat60$n_trees,emergsurdat60$n_trees)

t.test(nonsurdat60$heightmean,emergsurdat60$heightmean)
