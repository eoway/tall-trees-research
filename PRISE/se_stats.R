library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(rgdal)
library(sp)
library(plyr)


dat <- read_csv("~/Google Drive/My Drive/Research/PRISE_data/biganalysisdat")
summary(dat$height)

#calculate 99th percentile dbh
dbh99 <- quantile(dat$height, probs = 0.99, na.rm = TRUE)

hist(dat$height, xlab = "height", main = "Histogram of Tree Heights")
abline(v = median(dat$height), col="red", lwd=3)

#concatonate columns
dat$bulkdensity[is.na(dat$bulkdensity)] <- ""
dat$bulkdensity2[is.na(dat$bulkdensity2)] <- ""
dat$bulkdensity3[is.na(dat$bulkdensity3)] <- ""
dat$bulkdensity4[is.na(dat$bulkdensity4)] <- ""

dat$soc[is.na(dat$soc)] <- ""
dat$soc2[is.na(dat$soc2)] <- ""
dat$soc3[is.na(dat$soc3)] <- ""
dat$soc4[is.na(dat$soc4)] <- ""

dat$sand[is.na(dat$sand)] <- ""
dat$sand2[is.na(dat$sand2)] <- ""
dat$sand3[is.na(dat$sand3)] <- ""
dat$sand4[is.na(dat$sand4)] <- ""

dat$ph[is.na(dat$ph)] <- ""
dat$ph2[is.na(dat$ph2)] <- ""
dat$ph3[is.na(dat$ph3)] <- ""
dat$ph4[is.na(dat$ph4)] <- ""

dat$nitrogen[is.na(dat$nitrogen)] <- ""
dat$nitrogen2[is.na(dat$nitrogen2)] <- ""
dat$nitrogen3[is.na(dat$nitrogen3)] <- ""
dat$nitrogen4[is.na(dat$nitrogen4)] <- ""

dat$clay[is.na(dat$clay)] <- ""
dat$clay2[is.na(dat$clay2)] <- ""
dat$clay3[is.na(dat$clay3)] <- ""
dat$clay4[is.na(dat$clay4)] <- ""

dat$cec[is.na(dat$cec)] <- ""
dat$cec2[is.na(dat$cec2)] <- ""
dat$cec3[is.na(dat$cec3)] <- ""
dat$cec4[is.na(dat$cec4)] <- ""

# Paste columns together
dat$bd <- paste(dat$bulkdensity, dat$bulkdensity2, dat$bulkdensity3, dat$bulkdensity4)
dat$soc <- paste(dat$soc, dat$soc2, dat$soc3, dat$soc4)
dat$sand <- paste(dat$sand, dat$sand2, dat$sand3, dat$sand4)
dat$ph <- paste(dat$ph, dat$ph2, dat$ph3, dat$ph4)
dat$nitrogen <- paste(dat$nitrogen, dat$nitrogen2, dat$nitrogen3, dat$nitrogen4)
dat$clay <- paste(dat$clay, dat$clay2, dat$clay3, dat$clay4)
dat$cec <- paste(dat$cec, dat$cec2, dat$cec3, dat$cec4)

dat = select(dat, -c(bulkdensity, cec2, cec3, cec4, bulkdensity2, bulkdensity3, bulkdensity4, 
                     soc2, soc3, soc4, sand2, sand3, sand4, ph2, ph3, ph4, nitrogen2,
                     nitrogen3, nitrogen4, cec2, cec3, cec4, clay2, clay3, clay4))

dat$bd <- as.numeric(as.character(dat$bd))
dat$soc <- as.numeric(as.character(dat$soc))
dat$sand <- as.numeric(as.character(dat$sand))
dat$ph <- as.numeric(as.character(dat$ph))
dat$nitrogen <- as.numeric(as.character(dat$nitrogen))
dat$clay <- as.numeric(as.character(dat$clay))
dat$cec <- as.numeric(as.character(dat$cec))

summary(dat$sand)
summary(dat$ph)
summary(dat$nitrogen)
summary(dat$clay)
summary(dat$cec)

#emergent labelling
dat$tree_type <- ifelse(dat$height>=dbh99, "emrgnt", "nonemrgnt")
dat$bitree_type <- ifelse(dat$height>=dbh99, 1, 0)
table(dat$tree_type)
table(dat$bitree_type)

#ggpairs
library(ggplot2)
library(GGally)
ggpairs(dat)

#plot of SOC and height for final presentation
library(ggplot2)
library(jtools)
fit1 <- lm(height~soc, data=dat)
summary(fit1)
effect_plot(fit1, pred = soc, plot.points = TRUE, colors="red", x.label="Soil Organic Carbon Stock")

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
lm.all <- lm(height~elev+distance+slope+aspect+tpi+ph+soc+sand+clay+cec+nitrogen, data=dat)
summary(lm.all)

lm.all <- lm(height~slope+distance+aspect+tpi+ph+soc+clay+nitrogen, data=dat)
summary(lm.all)

#Linear Regression
lm.helev <- lm(height~elev, data=dat)
summary(lm.helev)
plot(dat$elev,dat$height)

lm.hnitrogen <- lm(height~nitrogen, data=dat)
summary(lm.hnitrogen)
plot(dat$nitrogen,dat$height)

lm.hdistance <- lm(height~distance, data=dat)
summary(lm.hdistance)
plot(dat$distance,dat$height)

lm.hslope <- lm(height~slope, data=dat)
summary(lm.hslope)
plot(dat$slope,dat$height, xlab="Slope", ylab="Height")

lm.haspect <- lm(height~aspect, data=dat)
summary(lm.haspect)
plot(dat$aspect,dat$height)

lm.htpi <- lm(height~tpi, data=dat)
summary(lm.htpi)
plot(dat$tpi,dat$height)

summary(dat$bd)
lm.hbulkdensity <- lm(height~bd, data=dat)
summary(lm.hbulkdensity)
plot(dat$bulkdensity,dat$height)

lm.hsoc <- lm(height~soc, data=dat)
summary(lm.hsoc)
plot(dat$soc,dat$height)

lm.hsand <- lm(height~sand, data=dat)
summary(lm.hsand)
plot(dat$sand,dat$height)

lm.hph <- lm(height~ph, data=dat)
summary(lm.hph)
plot(dat$ph,dat$height)

lm.hclay <- lm(height~clay, data=dat)
summary(lm.hclay)
plot(dat$clay,dat$height)

lm.hcec <- lm(height~cec, data=dat)
summary(lm.hcec)
plot(dat$cec,dat$height, xlab="Cation Exchange Capacity", ylab="Height")

#Linear Regression
lm.helev <- lm(height~elev, data=dat)
summary(lm.helev)
plot(dat$elev,dat$height)

lm.hdistance <- lm(height~distance, data=dat)
summary(lm.hdistance)
plot(dat$distance,dat$height)

lm.hslope <- lm(height~slope, data=dat)
summary(lm.hslope)
plot(dat$slope,dat$height)

lm.haspect <- lm(height~aspect, data=dat)
summary(lm.haspect)
plot(dat$aspect,dat$height)

lm.htpi <- lm(height~tpi, data=dat)
summary(lm.htpi)
plot(dat$tpi,dat$height)

summary(dat$bd)
lm.hbulkdensity <- lm(height~bd, data=dat)
summary(lm.hbulkdensity)
plot(dat$bulkdensity,dat$height)

lm.hsoc <- lm(height~soc, data=dat)
summary(lm.hsoc)
plot(dat$soc,dat$height)

lm.hsand <- lm(height~sand, data=dat)
summary(lm.hsand)
plot(dat$sand,dat$height)

lm.hph <- lm(height~ph, data=dat)
summary(lm.hph)
plot(dat$ph,dat$height)

lm.hclay <- lm(height~clay, data=dat)
summary(lm.hclay)
plot(dat$clay,dat$height)

lm.hcec <- lm(height~cec, data=dat)
summary(lm.hcec)
plot(dat$cec,dat$height)

lm.hnitrogen <- lm(height~nitrogen, data=dat)
summary(lm.hnitrogen)
plot(dat$nitrogen,dat$height)


#Logistic Regression
bielev3 <- glm(bitree_type~elev, data=dat, family="binomial")
summary(bielev3)
plot(dat$elev, dat$bitree_type)

bidistance3 <- glm(bitree_type~distance, data=dat, family="binomial")
summary(bidistance3)
plot(dat$distance, dat$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dat, family="binomial")
summary(bislope3)
plot(dat$slope, dat$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dat, family="binomial")
summary(biaspect3)
plot(dat$aspect, dat$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dat, family="binomial")
summary(bitpi3)
plot(dat$tpi, dat$bitree_type)

bibulkdensity3 <- glm(bitree_type~bulkdensity, data=dat, family="binomial")
summary(bibulkdensity3)
plot(dat$bulkdensity, dat$bitree_type)

bisoc3 <- glm(bitree_type~soc, data=dat, family="binomial")
summary(bisoc3)
plot(dat$soc, dat$bitree_type)

bisand3 <- glm(bitree_type~sand, data=dat, family="binomial")
summary(bisand3)
plot(dat$sand, dat$bitree_type)

biph3 <- glm(bitree_type~ph, data=dat, family="binomial")
summary(biph3)
plot(dat$ph, dat$bitree_type)

biclay3 <- glm(bitree_type~clay, data=dat, family="binomial")
summary(biclay3)
plot(dat$clay, dat$bitree_type)

bicec3 <- glm(bitree_type~cec, data=dat, family="binomial")
summary(bicec3)
plot(dat$cec, dat$bitree_type)
