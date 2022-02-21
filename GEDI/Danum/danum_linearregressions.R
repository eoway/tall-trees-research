library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(rgdal)
library(sp)
library(plyr)


dat <- read_csv("~/Desktop/Research/GEDI/Danum/danum_GEDI_withtopo.csv")
summary(dat$rh100_a3)
plot_dat = subset(dat, select = -c(Shot_number, geometry, coords.x1, coords.x2, Latitude_y, Longitude_y, 
                                   rh100, rh98, rh100_a4, rh98_a4, rh100_a5, rh98_a5,
                                   Quality_Flag, Degrade_Flag, Sensitivity, ))

#calculate 99th percentile dbh
dbh99 <- quantile(dat$rh100_a3, probs = 0.99, na.rm = TRUE)

hist(dat$rh100_a3, xlab = "rh100_a3", main = "Histogram of Tree rh100_a3s")
abline(v = median(dat$rh100_a3), col="red", lwd=3)

#ggpairs
library(ggplot2)
library(GGally)
ggpairs(plot_dat)

#check for correlation
#soc
lm.soccec <- lm(soc~cec, data=dat)
summary(lm.soccec)

lm.socelev <- lm(soc~elev, data=dat)
summary(lm.socelev)

lm.socdistance <- lm(soc~distance, data=dat)
summary(lm.socdistance)

lm.socslope <- lm(soc~slope, data=dat)
summary(lm.socslope)

lm.socaspect <- lm(soc~aspect, data=dat)
summary(lm.socaspect)

lm.soctpi <- lm(soc~tpi, data=dat)
summary(lm.soctpi)

lm.socbd <- lm(soc~bd, data=dat)
summary(lm.socbd)

lm.socnit <- lm(soc~nitrogen, data=dat)
summary(lm.socnit)

lm.socsand <- lm(soc~sand, data=dat)
summary(lm.socsand)

lm.socph <- lm(soc~ph, data=dat)
summary(lm.socph)

lm.socclay <- lm(soc~clay, data=dat)
summary(lm.socclay)

#elev
lm.elevcec <- lm(elev~cec, data=dat)
summary(lm.elevcec)

lm.elevdistance <- lm(elev~distance, data=dat)
summary(lm.elevdistance)

lm.elevslope <- lm(elev~slope, data=dat)
summary(lm.elevslope)

lm.elevaspect <- lm(elev~aspect, data=dat)
summary(lm.elevaspect)

lm.elevtpi <- lm(elev~tpi, data=dat)
summary(lm.elevtpi)

lm.elevbd <- lm(elev~bd, data=dat)
summary(lm.elevbd)

lm.elevsand <- lm(elev~sand, data=dat)
summary(lm.elevsand)

lm.elevph <- lm(elev~ph, data=dat)
summary(lm.elevph)

lm.elevclay <- lm(elev~clay, data=dat)
summary(lm.elevclay)

#distance
lm.distancecec <- lm(distance~cec, data=dat)
summary(lm.distancecec)

lm.distanceslope <- lm(distance~slope, data=dat)
summary(lm.distanceslope)

lm.distanceaspect <- lm(distance~aspect, data=dat)
summary(lm.distanceaspect)

lm.distancetpi <- lm(distance~tpi, data=dat)
summary(lm.distancetpi)

lm.distancebd <- lm(distance~bd, data=dat)
summary(lm.distancebd)

lm.distancesand <- lm(distance~sand, data=dat)
summary(lm.distancesand)

lm.distanceph <- lm(distance~ph, data=dat)
summary(lm.distanceph)

lm.distanceclay <- lm(distance~clay, data=dat)
summary(lm.distanceclay)

#slope
lm.slopecec <- lm(slope~cec, data=dat)
summary(lm.slopecec)

lm.slopeaspect <- lm(slope~aspect, data=dat)
summary(lm.slopeaspect)

lm.slopetpi <- lm(slope~tpi, data=dat)
summary(lm.slopetpi)

lm.slopebd <- lm(slope~bd, data=dat)
summary(lm.slopebd)

lm.slopesand <- lm(slope~sand, data=dat)
summary(lm.slopesand)

lm.slopeph <- lm(slope~ph, data=dat)
summary(lm.slopeph)

lm.slopeclay <- lm(slope~clay, data=dat)
summary(lm.slopeclay)

#aspect
lm.aspectcec <- lm(aspect~cec, data=dat)
summary(lm.aspectcec)

lm.aspectslope <- lm(aspect~slope, data=dat)
summary(lm.aspectslope)

lm.aspecttpi <- lm(aspect~tpi, data=dat)
summary(lm.aspecttpi)

lm.aspectbd <- lm(aspect~bd, data=dat)
summary(lm.aspectbd)

lm.aspectsand <- lm(aspect~sand, data=dat)
summary(lm.aspectsand)

lm.aspectph <- lm(aspect~ph, data=dat)
summary(lm.aspectph)

lm.aspectclay <- lm(aspect~clay, data=dat)
summary(lm.aspectclay)

#tpi
lm.tpicec <- lm(tpi~cec, data=dat)
summary(lm.tpicec)

lm.tpislope <- lm(tpi~slope, data=dat)
summary(lm.tpislope)

lm.tpiaspect <- lm(tpi~aspect, data=dat)
summary(lm.tpiaspect)

lm.tpibd <- lm(tpi~bd, data=dat)
summary(lm.tpibd)

lm.tpisand <- lm(tpi~sand, data=dat)
summary(lm.tpisand)

lm.tpiph <- lm(tpi~ph, data=dat)
summary(lm.tpiph)

lm.tpiclay <- lm(tpi~clay, data=dat)
summary(lm.tpiclay)

#bd
lm.bdcec <- lm(bd~cec, data=dat)
summary(lm.bdcec)

lm.bdslope <- lm(bd~slope, data=dat)
summary(lm.bdslope)

lm.bdaspect <- lm(bd~aspect, data=dat)
summary(lm.bdaspect)

lm.bdsand <- lm(bd~sand, data=dat)
summary(lm.bdsand)

lm.bdph <- lm(bd~ph, data=dat)
summary(lm.bdph)

lm.bdclay <- lm(bd~clay, data=dat)
summary(lm.bdclay)

#sand
lm.sandcec <- lm(sand~cec, data=dat)
summary(lm.sandcec)

lm.sandslope <- lm(sand~slope, data=dat)
summary(lm.sandslope)

lm.sandaspect <- lm(sand~aspect, data=dat)
summary(lm.sandaspect)

lm.sandbd <- lm(sand~bd, data=dat)
summary(lm.sandbd)

lm.sandph <- lm(sand~ph, data=dat)
summary(lm.sandph)

lm.sandclay <- lm(sand~clay, data=dat)
summary(lm.sandclay)

#ph
lm.phcec <- lm(ph~cec, data=dat)
summary(lm.phcec)

lm.phslope <- lm(ph~slope, data=dat)
summary(lm.phslope)

lm.phaspect <- lm(ph~aspect, data=dat)
summary(lm.phaspect)

lm.phbd <- lm(ph~bd, data=dat)
summary(lm.phbd)

lm.phclay <- lm(ph~clay, data=dat)
summary(lm.phclay)

#clay
lm.claycec <- lm(clay~cec, data=dat)
summary(lm.claycec)

lm.clayslope <- lm(clay~slope, data=dat)
summary(lm.clayslope)

lm.clayaspect <- lm(clay~aspect, data=dat)
summary(lm.clayaspect)

lm.claybd <- lm(clay~bd, data=dat)
summary(lm.claybd)

lm.clayph <- lm(clay~ph, data=dat)
summary(lm.clayph)

#cec
lm.cecslope <- lm(cec~slope, data=dat)
summary(lm.cecslope)

lm.cecaspect <- lm(cec~aspect, data=dat)
summary(lm.cecaspect)

lm.cecbd <- lm(cec~bd, data=dat)
summary(lm.cecbd)

lm.cecph <- lm(cec~ph, data=dat)
summary(lm.cecph)

lm.cecclay <- lm(cec~clay, data=dat)
summary(lm.cecclay)

#linear regression of all (minus bd due to high correlation between it and ph)
lm.all <- lm(rh100_a3~elev+distance+slope+aspect+tpi+ph+soc+sand+clay+cec+nitrogen, data=dat)
summary(lm.all)

lm.all <- lm(rh100_a3~slope+distance+aspect+tpi+ph+soc+clay+nitrogen, data=dat)
summary(lm.all)

#Linear Regression
lm.helev <- lm(rh100_a3~elev, data=dat)
summary(lm.helev)
plot(dat$elev,dat$rh100_a3)

lm.hnitrogen <- lm(rh100_a3~nitrogen, data=dat)
summary(lm.hnitrogen)
plot(dat$nitrogen,dat$rh100_a3)

lm.hdistance <- lm(rh100_a3~distance, data=dat)
summary(lm.hdistance)
plot(dat$distance,dat$rh100_a3)

lm.hslope <- lm(rh100_a3~slope, data=dat)
summary(lm.hslope)
plot(dat$slope,dat$rh100_a3, xlab="Slope", ylab="rh100_a3")

lm.haspect <- lm(rh100_a3~aspect, data=dat)
summary(lm.haspect)
plot(dat$aspect,dat$rh100_a3)

lm.htpi <- lm(rh100_a3~tpi, data=dat)
summary(lm.htpi)
plot(dat$tpi,dat$rh100_a3)

summary(dat$bd)
lm.hbulkdensity <- lm(rh100_a3~bulkdensity, data=dat)
summary(lm.hbulkdensity)
plot(dat$bulkdensity,dat$rh100_a3)

lm.hsoc <- lm(rh100_a3~soc, data=dat)
summary(lm.hsoc)
plot(dat$soc,dat$rh100_a3)

lm.hsand <- lm(rh100_a3~sand, data=dat)
summary(lm.hsand)
plot(dat$sand,dat$rh100_a3)

lm.hph <- lm(rh100_a3~ph, data=dat)
summary(lm.hph)
plot(dat$ph,dat$rh100_a3)

lm.hclay <- lm(rh100_a3~clay, data=dat)
summary(lm.hclay)
plot(dat$clay,dat$rh100_a3)

lm.hcec <- lm(rh100_a3~cec, data=dat)
summary(lm.hcec)
plot(dat$cec,dat$rh100_a3, xlab="Cation Exchange Capacity", ylab="rh100_a3")

#Linear Regression
lm.helev <- lm(rh100_a3~elev, data=dat)
summary(lm.helev)
plot(dat$elev,dat$rh100_a3)

lm.hdistance <- lm(rh100_a3~distance, data=dat)
summary(lm.hdistance)
plot(dat$distance,dat$rh100_a3)

lm.hslope <- lm(rh100_a3~slope, data=dat)
summary(lm.hslope)
plot(dat$slope,dat$rh100_a3)

lm.haspect <- lm(rh100_a3~aspect, data=dat)
summary(lm.haspect)
plot(dat$aspect,dat$rh100_a3)

lm.htpi <- lm(rh100_a3~tpi, data=dat)
summary(lm.htpi)
plot(dat$tpi,dat$rh100_a3)

summary(dat$bd)
lm.hbulkdensity <- lm(rh100_a3~bd, data=dat)
summary(lm.hbulkdensity)
plot(dat$bulkdensity,dat$rh100_a3)

lm.hsoc <- lm(rh100_a3~soc, data=dat)
summary(lm.hsoc)
plot(dat$soc,dat$rh100_a3)

lm.hsand <- lm(rh100_a3~sand, data=dat)
summary(lm.hsand)
plot(dat$sand,dat$rh100_a3)

lm.hph <- lm(rh100_a3~ph, data=dat)
summary(lm.hph)
plot(dat$ph,dat$rh100_a3)

lm.hclay <- lm(rh100_a3~clay, data=dat)
summary(lm.hclay)
plot(dat$clay,dat$rh100_a3)

lm.hcec <- lm(rh100_a3~cec, data=dat)
summary(lm.hcec)
plot(dat$cec,dat$rh100_a3)

lm.hnitrogen <- lm(rh100_a3~nitrogen, data=dat)
summary(lm.hnitrogen)
plot(dat$nitrogen,dat$rh100_a3)