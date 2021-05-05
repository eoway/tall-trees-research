library(MASS)
library(ISLR)

dan_data <- read_csv(here("Desktop","Research","HCRP","dan_dat", "dan_topo.csv"))

#Add quadrat level emergent labeling
source("~/Documents/GitHub/tall-trees-research/heights.r")
dbh99 <- quantile99dbh #from heights.r
dan_data$tree_type <- ifelse(dan_data$dbh>=dbh99, "emrgnt", "nonemrgnt")
table(dan_data$tree_type)
dan_label <- dan_data %>% group_by(quadrat,tree_type)  %>%  summarise()
table(dan_label$tree_type)
emergents <- filter(dan_label, tree_type=="emrgnt")
emergentquad <- unique(emergents$quadrat)
table(emergentquad)

dan_data$quad_type <- ifelse(dan_data$quadrat %in% emergentquad, "emrgnt", "nonemrgnt")
table(dan_data$quad_type)

dan_stat <- dan_data
#dan_stat <- dan_data %>% group_by(quadrat,quad_type)  %>%  summarise(quad_x = mean(x_utm),
#                                                                             quad_y = mean(y_utm),
#                                                                             dbhmean = mean(dbh, na.rm=T),
#                                                                             heightmean = mean(height, na.rm=T),
#                                                                             heightmedian = median(height, na.rm=T),
#                                                                             height99 = quantile(height, probs = 0.99, na.rm = TRUE),
#                                                                             heightmax = max(height,na.rm=T),
#                                                                             slope = mean(slope, na.rm=T),
#                                                                             aspect = mean(aspect, na.rm=T),
#                                                                     twi = mean(twi, na.rm=T),
#                                                                     tpi = mean(tpi, na.rm=T),
#                                                                     elev = mean(elev, na.rm=T))
summary(dan_stat$quad_x)
dan_stat$soil <-as.factor(dan_stat$soil)
table(dan_stat$quad_type)
dan_stat$bitype <- ifelse(dan_stat$quad_type=="emrgnt", 1,0)
table(dan_stat$bitype)

#Multiple Logistic Regression---------
twi3 <- glm(bitype~twi+I(twi^3), data=dan_stat, family="binomial")
summary(twi3)


bielev <- glm(bitype~elev, data=dan_stat, family="binomial")
summary(bielev)
plot(dan_stat$elev, dan_stat$bitype)

bitpi <- glm(bitype~tpi, data=dan_stat, family="binomial")
summary(bitpi)
plot(dan_stat$tpi, dan_stat$bitype)

biaspect <- glm(bitype~aspect, data=dan_stat, family="binomial")
summary(biaspect)
plot(dan_stat$aspect, dan_stat$bitype)

bislope <- glm(bitype~slope, data=dan_stat, family="binomial")
summary(bislope)
plot(dan_stat$slope, dan_stat$bitype)

bitwi <- glm(bitype~twi, data=dan_stat, family="binomial")
summary(bitwi)
plot(dan_stat$twi, dan_stat$bitype)

bisoil <- glm(bitype~soil, data=dan_stat, family="binomial")
summary(bisoil)


#with x and y coords
bielevxy <- glm(bitype~elev+quad_x+quad_y, data=dan_stat, family="binomial")
summary(bielevxy)
plot(dan_stat$elev, dan_stat$bitype)

bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=dan_stat, family="binomial")
summary(bitpixy)
plot(dan_stat$tpi, dan_stat$bitype)

biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=dan_stat, family="binomial")
summary(biaspectxy)
plot(dan_stat$aspect, dan_stat$bitype)

bislopexy <- glm(bitype~slope+quad_x+quad_y, data=dan_stat, family="binomial")
summary(bislopexy)
plot(dan_stat$slope, dan_stat$bitype)

bitwixy <- glm(bitype~twi+quad_x+quad_y, data=dan_stat, family="binomial")
summary(bitwixy)
plot(dan_stat$twi, dan_stat$bitype)

bisoilxy <- glm(bitype~soil+quad_x+quad_y, data=dan_stat, family="binomial")
summary(bisoilxy)

#Random Effects
#Just Ints--------
library(lme4)
#Height 99~Elevation: 
lmer.h99elev <- lmer(height~elev+(1|soil), data=dan_stat)
tval <- 
summary(lmer.h99elev)
AIC(lmer.h99elev)
names(lmer.h99elev)

2*pt(-abs(t),df=n-1)


names(lmer.h99elev)
confint(lmer.h99elev)
predict(lmer.h99elev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev)
plot(dan_stat$elev,dan_stat$height99)

#Height 99~TPI: 
lmer.h99tpi <- lmer(height99~tpi+(1|soil), data=dan_stat)
summary(lmer.h99tpi)
AIC(lmer.h99tpi)
confint(lmer.h99tpi)
predict(lmer.h99tpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi)
plot(dan_stat$tpi,dan_stat$height99)

#Height 99~aspect: 
lmer.h99aspect <- lmer(height99~aspect+(1|soil), data=dan_stat)
summary(lmer.h99aspect)
AIC(lmer.h99aspect)
confint(lmer.h99aspect)
predict(lmer.h99aspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect)
plot(dan_stat$aspect,dan_stat$height99)

#Height 99~slope: 
lmer.h99slope <- lmer(height99~slope+(1|soil), data=dan_stat)
summary(lmer.h99slope)
AIC(lmer.h99slope)
confint(lmer.h99slope)
predict(lmer.h99slope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope)
plot(dan_stat$slope,dan_stat$height99)

#Height 99~TWI: 
lmer.h99twi <- lmer(height99~twi+(1|soil), data=dan_stat)
summary(lmer.h99twi)
AIC(lmer.h99twi)
confint(lmer.h99twi)
predict(lmer.h99twi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi)
plot(dan_stat$twi,dan_stat$height99)


#Height 99~TWI+TPI: 
lmer.h99twitpi <- lmer(height99~twi+tpi+(1|soil), data=dan_stat)
summary(lmer.h99twitpi)
AIC(lmer.h99twitpi)
confint(lmer.h99twitpi)
predict(lmer.h99twitpi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twitpi)
plot(dan_stat$twi,dan_stat$height99)

#Height 99~TWI*TPI: 
lmer.h99twitpi1 <- lmer(height99~twi*tpi+(1|soil), data=dan_stat)
summary(lmer.h99twitpi1)
AIC(lmer.h99twitpi1)
confint(lmer.h99twitpi1)
predict(lmer.h99twitpi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twitpi1)
plot(dan_stat$twi,dan_stat$height99)

#Height 99~slope+aspect: 
lmer.h99slopeaspect <- lmer(height99~slope+aspect+(1|soil), data=dan_stat)
summary(lmer.h99slopeaspect)
AIC(lmer.h99slopeaspect)
confint(lmer.h99slopeaspect)
predict(lmer.h99slopeaspect, data.frame(slope=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99slopeaspect)
plot(dan_stat$danbir_slope,dan_stat$height99)

#Ints and Only--------
#Height 99~Elevation: 
lmer.h99elev1 <- lmer(height99~elev +(1|soil)+ (0 + elev|soil), data=dan_stat)
summary(lmer.h99elev1)
AIC(lmer.h99elev1)
confint(lmer.h99elev1)
predict(lmer.h99elev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev1)
plot(dan_stat$elev,dan_stat$height99)

#Height 99~TPI: 
lmer.h99tpi1 <- lmer(height99~tpi+(1|soil)+ (0 + tpi|soil), data=dan_stat)
summary(lmer.h99tpi1)
AIC(lmer.h99tpi1)
confint(lmer.h99tpi1)
predict(lmer.h99tpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi1)
plot(dan_stat$tpi,dan_stat$height99)

#Height 99~aspect: 
lmer.h99aspect1 <- lmer(height99~aspect+(1|soil)+ (0 + aspect|soil), data=dan_stat)
summary(lmer.h99aspect1)
AIC(lmer.h99aspect1)
confint(lmer.h99aspect1)
predict(lmer.h99aspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect1)
plot(dan_stat$aspect,dan_stat$height99)

#Height 99~slope: 
lmer.h99slope1 <- lmer(height99~slope+(1|soil)+ (0 + slope|soil), data=dan_stat)
summary(lmer.h99slope1)
AIC(lmer.h99slope1)
confint(lmer.h99slope1)
predict(lmer.h99slope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope1)
plot(dan_stat$slope,dan_stat$height99)

#Height 99~TWI: 
lmer.h99twi1 <- lmer(height99~twi+(1|soil)+ (0 + twi|soil), data=dan_stat)
summary(lmer.h99twi1)
AIC(lmer.h99twi1)
confint(lmer.h99twi1)
predict(lmer.h99twi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi1)
plot(dan_stat$twi,dan_stat$height99)

#Random Effect Model
library(nlme)
heightelev <- lme(height99~elev, random=~1|soil, data=dan_stat, method="REML")
summary(heightelev)
plot(heightelev) %>%
  abline()
hist(dan_data$elev)
library(lme4)
colnames(dan_data)
dan_data$quadrat_group <- as.factor(dan_data$quadrat)
heightelev <- lmer(height99~elev+(1|quadrat_group), data=dan_data)
#if hist(dan_dat) or hist(danstat) are skewed- log transform them
hist(dan_data$height99)
hist(dan_stat$height99)
summary(heightelev)
str(dan_data$quadrat)
confint(heightelev)

ranef(heightelev)$soil %>% head(4)
library(merTools)
predictInterval(heightelev)
REsim(heightelev)
plotREsim(REsim(heightelev))
#Surrounding Trees Data




dim(dan_stat)
library("GGally")
ggpairs(dan_stat)
install.packages("GGally")
structure(dan_stat)
str(dan_stat)
#Linear Regression------------
#Soils
#Height 99~soil: 
lm.h99soil <- lm(height99~soil, data=dan_stat)
summary(lm.h99soil)
confint(lm.h99soil)
predict(lm.h99soil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99soil) %>%
  abline()
plot(dan_stat$soil,dan_stat$height99)




#Height 99~Elevation: 
lm.h99elev <- lm(height99~elev, data=dan_stat)
summary(lm.h99elev)
names(lm.h99elev)
confint(lm.h99elev)
predict(lm.h99elev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99elev) %>%
  abline()
plot(dan_stat$elev,dan_stat$height99)

#Height Mean~Elevation: 
lm.hmeanelev <- lm(heightmean~elev, data=dan_stat)
summary(lm.hmeanelev)
confint(lm.hmeanelev)
predict(lm.hmeanelev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanelev) %>%
  abline ()

plot(dan_stat$elev,dan_stat$heightmean)

#Height Max~Elevation: 
lm.hmaxelev <- lm(heightmax~elev, data=dan_stat)
summary(lm.hmaxelev)
names(lm.hmaxelev)
confint(lm.hmaxelev)
predict(lm.hmaxelev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxelev) %>%
  abline ()
plot(dan_stat$elev,dan_stat$heightmax)





#Height 99~TPI: 
lm.h99tpi <- lm(height99~tpi, data=dan_stat)
summary(lm.h99tpi)
names(lm.h99tpi)
confint(lm.h99tpi)
predict(lm.h99tpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.h99tpi) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height99)

#Height Mean~TPI: 
lm.hmeantpi <- lm(heightmean~tpi, data=dan_stat)
summary(lm.hmeantpi)
names(lm.hmeantpi)
confint(lm.hmeantpi)
predict(lm.hmeantpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeantpi) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$heightmean)

#Height Max~tpi: 
lm.hmaxtpi <- lm(heightmax~tpi, data=dan_stat)
summary(lm.hmaxtpi)
names(lm.hmaxtpi)
confint(lm.hmaxtpi)
predict(lm.hmaxtpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxtpi) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$heightmax)



#Height 99~aspect: 
lm.h99aspect <- lm(height99~aspect, data=dan_stat)
summary(lm.h99aspect)
names(lm.h99aspect)
confint(lm.h99aspect)
predict(lm.h99aspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.h99aspect) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height99)

#Height Mean~aspect: 
lm.hmeanaspect <- lm(heightmean~aspect, data=dan_stat)
summary(lm.hmeanaspect)
names(lm.hmeanaspect)
confint(lm.hmeanaspect)
predict(lm.hmeanaspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanaspect) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$heightmean)

#Height Max~aspect: 
lm.hmaxaspect <- lm(heightmax~tpi, data=dan_stat)
summary(lm.hmaxaspect)
names(lm.hmaxaspect)
confint(lm.hmaxaspect)
predict(lm.hmaxaspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxaspect) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$heightmax)



#Height 99~slope: 
lm.h99slope <- lm(height99~slope, data=dan_stat)
summary(lm.h99slope)
names(lm.h99slope)
confint(lm.h99slope)
predict(lm.h99slope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.h99slope) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height99)

#Height Mean~slope: 
lm.hmeanslope <- lm(heightmean~slope, data=dan_stat)
summary(lm.hmeanslope)
names(lm.hmeanslope)
confint(lm.hmeanslope)
predict(lm.hmeanslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanslope) %>%
  abline ()
plot(dan_stat$slope,dan_stat$heightmean)

#Height Max~slope: 
lm.hmaxslope <- lm(heightmax~slope, data=dan_stat)
summary(lm.hmaxslope)
names(lm.hmaxslope)
confint(lm.hmaxslope)
predict(lm.hmaxslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxslope) %>%
  abline ()
plot(dan_stat$slope,dan_stat$heightmax)



#Height 99~TWI: 
lm.h99twi <- lm(height99~twi, data=dan_stat)
summary(lm.h99twi)
names(lm.h99twi)
confint(lm.h99twi)
predict(lm.h99twi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99twi) %>%
  abline()
plot(dan_stat$twi,dan_stat$height99)

#Height Mean~TWI: 
lm.hmeantwi <- lm(heightmean~twi, data=dan_stat)
summary(lm.hmeantwi)
confint(lm.hmeantwi)
predict(lm.hmeantwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeantwi) %>%
  abline ()

plot(dan_stat$twi,dan_stat$heightmean)

#Height Max~TWI: 
lm.hmaxtwi <- lm(heightmax~twi, data=dan_stat)
summary(lm.hmaxtwi)
names(lm.hmaxtwi)
confint(lm.hmaxtwi)
predict(lm.hmaxtwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxtwi) %>%
  abline ()
plot(dan_stat$twi,dan_stat$heightmax)

#Height 99~Soil: 
lm.h99soil <- lm(height99~soil, data=dan_stat)
summary(lm.h99soil)
names(lm.h99soil)
confint(lm.h99soil)
predict(lm.h99soil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99soil) %>%
  abline()
plot(dan_stat$soil,dan_stat$height99)



#Height 99~HabType: 
lm.h99HabType <- lm(height99~HabType, data=dan_stat)
summary(lm.h99HabType)
names(lm.h99HabType)
confint(lm.h99HabType)
predict(lm.h99HabType, data.frame(HabType=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99HabType) %>%
  abline()
plot(dan_stat$HabType,dan_stat$height99)

#Height Mean~HabType: 
lm.hmeanHabType <- lm(heightmean~HabType, data=dan_stat)
summary(lm.hmeanHabType)
confint(lm.hmeanHabType)
predict(lm.hmeanHabType, data.frame(HabType=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanHabType) %>%
  abline ()

plot(dan_stat$HabType,dan_stat$heightmean)

#Height Max~HabType: 
lm.hmaxHabType <- lm(heightmax~HabType, data=dan_stat)
summary(lm.hmaxHabType)
names(lm.hmaxHabType)
confint(lm.hmaxHabType)
predict(lm.hmaxHabType, data.frame(HabType=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxHabType) %>%
  abline ()
plot(dan_stat$HabType,dan_stat$heightmax)

#Multiple Linear Regresion------------
#with soil
#Height99~ elev+slope+aspect+tpi+twi+soil
lm.h99eastts <- lm(height99~elev+aspect+slope+tpi+twi+soil, data=dan_stat)
summary(lm.h99eastts)
library(car)
vif(lm.h99eastts)
plot(lm.h99eastts) %>%
  abline ()

#Height99~ elev+slope+aspect+twi+soil
lm.h99easts <- lm(height99~elev+aspect+slope+twi+soil, data=dan_stat)
summary(lm.h99easts)
vif(lm.h99easts)
plot(lm.h99easts) %>%
  abline ()

#Height99~ elev+twi+soil
lm.h99estwi <- lm(height99~elev+twi+soil, data=dan_stat)
summary(lm.h99estwi)
vif(lm.h99estwi)
plot(lm.h99estwi) %>%
  abline ()

#Height99~ elev+soil
lm.h99esoil <- lm(height99~elev+soil, data=dan_stat)
summary(lm.h99esoil)
vif(lm.h99esoil)
plot(lm.h99esoil) %>%
  abline ()

#Height99~ twi+soil
lm.h99stwi <- lm(height99~twi+soil, data=dan_stat)
summary(lm.h99stwi)
vif(lm.h99stwi)
plot(lm.h99stwi) %>%
  abline ()

#Height99~ elev*slope*aspect*tpi*twi*soil
lm.h99eastts1 <- lm(height99~elev*aspect*slope*tpi*twi*soil, data=dan_stat)
summary(lm.h99eastts1)
library(car)
vif(lm.h99eastts1)
plot(lm.h99eastts1) %>%
  abline ()

#Height99~ elev*slope*aspect*twi*soil
lm.h99easts1 <- lm(height99~elev*aspect*slope*twi*soil, data=dan_stat)
summary(lm.h99easts1)
vif(lm.h99easts1)
plot(lm.h99easts1) %>%
  abline ()

#Height99~ elev*twi*soil
lm.h99estwi1 <- lm(height99~elev*twi*soil, data=dan_stat)
summary(lm.h99estwi1)
vif(lm.h99estwi1)
plot(lm.h99estwi1) %>%
  abline ()

#Height99~ elev*soil
lm.h99esoil1 <- lm(height99~elev*soil, data=dan_stat)
summary(lm.h99esoil1)
vif(lm.h99esoil1)
plot(lm.h99esoil1) %>%
  abline ()

#Height99~ twi*soil
lm.h99stwi1 <- lm(height99~twi*soil, data=dan_stat)
summary(lm.h99stwi1)
vif(lm.h99stwi1)
plot(lm.h99stwi1) %>%
  abline ()



#without soil
#Height99~ elev+slope+aspect+tpi+twi
lm.h99eastt <- lm(height99~elev+aspect+slope+tpi+twi, data=dan_stat)
summary(lm.h99eastt)
library(car)
vif(lm.h99eastt)
plot(lm.h99eastt) %>%
  abline ()

#Height99~ elev+slope+aspect+tpi
lm.h99east <- lm(height99~elev+aspect+slope+tpi, data=dan_stat)
summary(lm.h99east)
vif(lm.h99east)
plot(lm.h99east) %>%
  abline ()

#Height99~ elev+slope+aspect+twi
lm.h99eastwi <- lm(height99~elev+aspect+slope+twi, data=dan_stat)
summary(lm.h99eastwi)
vif(lm.h99eastwi)
plot(lm.h99eastwi) %>%
  abline ()

#Height99~ elev+slope+twi
lm.h99estwi <- lm(height99~elev+slope+twi, data=dan_stat)
summary(lm.h99estwi)
vif(lm.h99estwi)
plot(lm.h99estwi) %>%
  abline ()

#Height99~ elev+aspect+twi
lm.h99eatwi <- lm(height99~elev+aspect+twi, data=dan_stat)
summary(lm.h99eatwi)
vif(lm.h99eatwi)
plot(lm.h99eatwi) %>%
  abline ()

#Height99~ elev+slope+aspect
lm.h99eas <- lm(height99~elev+aspect+slope, data=dan_stat)
summary(lm.h99eas)
vif(lm.h99eas)
plot(lm.h99eas) %>%
  abline ()

#Height99~elev+slope
lm.h99es <- lm(height99~elev+slope, data=dan_stat)
summary(lm.h99es)
vif(lm.h99es)
plot(lm.h99es) %>%
  abline ()

#Height99~elev+TWI
lm.h99etwi <- lm(height99~elev+twi, data=dan_stat)
summary(lm.h99etwi)
vif(lm.h99etwi)
plot(lm.h99etwi) %>%
  abline ()

#Height99~elev+aspect
lm.h99ea <- lm(height99~elev+aspect, data=dan_stat)
summary(lm.h99ea)
vif(lm.h99ea)
plot(lm.h99ea) %>%
  abline ()

#Height99~aspect+slope
lm.h99as <- lm(height99~aspect+slope, data=dan_stat)
summary(lm.h99as)
library(car)
vif(lm.h99as)
plot(lm.h99as) %>%
  abline ()


#Height99~ elev*slope*aspect*twi*tpi
lm.h99eastt1 <- lm(height99~elev*aspect*slope*tpi*twi, data=dan_stat)
summary(lm.h99eastt1)
library(car)
vif(lm.h99eastt1)
plot(lm.h99eastt1) %>%
  abline ()

#Height99~ elev*slope*aspect*twi
lm.h99eastwi1 <- lm(height99~elev*aspect*slope*twi, data=dan_stat)
summary(lm.h99eastwi1)
library(car)
vif(lm.h99eastwi1)
plot(lm.h99eastwi1) %>%
  abline ()

#Height99~ elev*twi
lm.h99etwi1 <- lm(height99~elev*twi, data=dan_stat)
summary(lm.h99etwi1)
library(car)
vif(lm.h99etwi1)
plot(lm.h99etwi1) %>%
  abline ()

#Height99~ elev*slope*aspect
lm.h99eas1 <- lm(height99~elev*aspect*slope, data=dan_stat)
summary(lm.h99eas1)
library(car)
vif(lm.h99eas1)
plot(lm.h99eas1) %>%
  abline ()

#Height99~elev*slope
lm.h99es1 <- lm(height99~elev*slope, data=dan_stat)
summary(lm.h99es1)
vif(lm.h99es1)
plot(lm.h99es1) %>%
  abline ()

#Height99~elev*aspect
lm.h99ea1 <- lm(height99~elev*aspect, data=dan_stat)
summary(lm.h99ea1)
vif(lm.h99ea1)
plot(lm.h99ea1) %>%
  abline ()

#Height99~aspect*slope
lm.h99as1 <- lm(height99~aspect*slope, data=dan_stat)
summary(lm.h99as1)
vif(lm.h99as1)
plot(lm.h99as1) %>%
  abline ()

#Height99~ elev*slope*twi
lm.h99estwi1 <- lm(height99~elev*slope*twi, data=dan_stat)
summary(lm.h99estwi1)
vif(lm.h99estwi1)
plot(lm.h99estwi1) %>%
  abline ()

#Height99~ elev*aspect*twi
lm.h99eatwi1 <- lm(height99~elev*aspect*twi, data=dan_stat)
summary(lm.h99eatwi1)
vif(lm.h99eatwi1)
plot(lm.h99eatwi1) %>%
  abline ()


