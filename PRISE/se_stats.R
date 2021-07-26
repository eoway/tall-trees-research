library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(rgdal)
library(sp)
library(plyr)

dat <- read_csv("G:/My Drive/Research/PRISE_data/analysisdat")

dbh99 <- quantile(dat$height, probs = 0.99, na.rm = TRUE)

#emergent labelling
dat$tree_type <- ifelse(dat$height>=dbh99, "emrgnt", "nonemrgnt")
dat$bitree_type <- ifelse(dat$height>=dbh99, 1, 0)
table(dat$tree_type)
table(dat$bitree_type)


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

lm.hbulkdensity <- lm(height~bulkdensity, data=dat)
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