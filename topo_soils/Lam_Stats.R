library(MASS)
library(ISLR)

lam_data <- read_csv(here("Desktop","Research","HCRP","Lambir Data", "lam_topo.csv"))

lam_stat$soil <-as.factor(lam_stat$soil)
#Add quadrat level emergent labelling
dbh99 <- quantile99dbh #from heights.r
lam_data$tree_type <- ifelse(lam_data$dbh>=dbh99, "emrgnt", "nonemrgnt")
table(lam_data$tree_type)
lam_label <- lam_data %>% group_by(quadrat,tree_type)  %>%  summarise()
table(lam_label$tree_type)
emergents <- filter(lam_label, tree_type=="emrgnt")
emergentquad <- unique(emergents$quadrat)
table(emergentquad)

lam_data$quad_type <- ifelse(lam_data$quadrat %in% emergentquad, "emrgnt", "nonemrgnt")
table(lam_data$quad_type)

lam_stat <- lam_data %>% group_by(quadrat,quad_type,dbhmean,heightmean,heightmedian,height99,heightmax,HabType,
                                  slope,aspect,tpi,elev,Lambir_TWI,soil)  %>%  summarise()

table(lam_stat$quad_type)
lam_stat$bitype <- ifelse(lam_stat$quad_type=="emrgnt", 1,0)
table(lam_stat$bitype)

mylogit <- glm(bitype~slope, data=lam_stat, family="binomial")
summary(mylogit)
plot(mylogit) %>%
  abline()
#Multiple Logistic Regression

#Random Effect Model
library(nlme)
heightelev <- lme(height99~elev, random=~1|soil, data=lam_stat, method="REML")
summary(heightelev)
plot(heightelev) %>%
  abline()

library(lme4)
heightelev <- lmer(height99~elev+(1|soil), data=lam_stat)
summary(heightelev)
confint(heightelev)

ranef(heightelev)$soil %>% head(4)
library(merTools)
predictInterval(heightelev)
REsim(heightelev)
plotREsim(REsim(heightelev))
#Surrounding Trees Data




dim(lam_stat)
library("GGally")
ggpairs(lam_stat)
install.packages("GGally")
structure(lam_stat)
str(lam_stat)
#Linear Regression------------
#Soils
#Height 99~soil: 
lm.h99soil <- lm(height99~soil, data=lam_stat)
summary(lm.h99soil)
confint(lm.h99soil)
predict(lm.h99soil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99soil) %>%
  abline()
plot(lam_stat$soil,lam_stat$height99)




#Height 99~Elevation: 
lm.h99elev <- lm(height99~elev, data=lam_stat)
summary(lm.h99elev)
names(lm.h99elev)
confint(lm.h99elev)
predict(lm.h99elev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99elev) %>%
  abline()
plot(lam_stat$elev,lam_stat$height99)

#Height Mean~Elevation: 
lm.hmeanelev <- lm(heightmean~elev, data=lam_stat)
summary(lm.hmeanelev)
confint(lm.hmeanelev)
predict(lm.hmeanelev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanelev) %>%
  abline ()

plot(lam_stat$elev,lam_stat$heightmean)

#Height Max~Elevation: 
lm.hmaxelev <- lm(heightmax~elev, data=lam_stat)
summary(lm.hmaxelev)
names(lm.hmaxelev)
confint(lm.hmaxelev)
predict(lm.hmaxelev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxelev) %>%
  abline ()
plot(lam_stat$elev,lam_stat$heightmax)





#Height 99~TPI: 
lm.h99tpi <- lm(height99~tpi, data=lam_stat)
summary(lm.h99tpi)
names(lm.h99tpi)
confint(lm.h99tpi)
predict(lm.h99tpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.h99tpi) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$height99)

#Height Mean~TPI: 
lm.hmeantpi <- lm(heightmean~tpi, data=lam_stat)
summary(lm.hmeantpi)
names(lm.hmeantpi)
confint(lm.hmeantpi)
predict(lm.hmeantpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeantpi) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$heightmean)

#Height Max~tpi: 
lm.hmaxtpi <- lm(heightmax~tpi, data=lam_stat)
summary(lm.hmaxtpi)
names(lm.hmaxtpi)
confint(lm.hmaxtpi)
predict(lm.hmaxtpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxtpi) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$heightmax)



#Height 99~aspect: 
lm.h99aspect <- lm(height99~aspect, data=lam_stat)
summary(lm.h99aspect)
names(lm.h99aspect)
confint(lm.h99aspect)
predict(lm.h99aspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.h99aspect) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$height99)

#Height Mean~aspect: 
lm.hmeanaspect <- lm(heightmean~aspect, data=lam_stat)
summary(lm.hmeanaspect)
names(lm.hmeanaspect)
confint(lm.hmeanaspect)
predict(lm.hmeanaspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanaspect) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$heightmean)

#Height Max~aspect: 
lm.hmaxaspect <- lm(heightmax~tpi, data=lam_stat)
summary(lm.hmaxaspect)
names(lm.hmaxaspect)
confint(lm.hmaxaspect)
predict(lm.hmaxaspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxaspect) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$heightmax)



#Height 99~slope: 
lm.h99slope <- lm(height99~slope, data=lam_stat)
summary(lm.h99slope)
names(lm.h99slope)
confint(lm.h99slope)
predict(lm.h99slope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.h99slope) %>%
  abline ()
plot(lam_stat$slope,lam_stat$height99)

#Height Mean~slope: 
lm.hmeanslope <- lm(heightmean~slope, data=lam_stat)
summary(lm.hmeanslope)
names(lm.hmeanslope)
confint(lm.hmeanslope)
predict(lm.hmeanslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanslope) %>%
  abline ()
plot(lam_stat$slope,lam_stat$heightmean)

#Height Max~slope: 
lm.hmaxslope <- lm(heightmax~slope, data=lam_stat)
summary(lm.hmaxslope)
names(lm.hmaxslope)
confint(lm.hmaxslope)
predict(lm.hmaxslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxslope) %>%
  abline ()
plot(lam_stat$slope,lam_stat$heightmax)



#Height 99~TWI: 
lm.h99twi <- lm(height99~Lambir_TWI, data=lam_stat)
summary(lm.h99twi)
names(lm.h99twi)
confint(lm.h99twi)
predict(lm.h99twi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99twi) %>%
  abline()
plot(lam_stat$Lambir_TWI,lam_stat$height99)

#Height Mean~TWI: 
lm.hmeantwi <- lm(heightmean~Lambir_TWI, data=lam_stat)
summary(lm.hmeantwi)
confint(lm.hmeantwi)
predict(lm.hmeantwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeantwi) %>%
  abline ()

plot(lam_stat$Lambir_TWI,lam_stat$heightmean)

#Height Max~TWI: 
lm.hmaxtwi <- lm(heightmax~Lambir_TWI, data=lam_stat)
summary(lm.hmaxtwi)
names(lm.hmaxtwi)
confint(lm.hmaxtwi)
predict(lm.hmaxtwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxtwi) %>%
  abline ()
plot(lam_stat$Lambir_TWI,lam_stat$heightmax)

#Height 99~Soil: 
lm.h99soil <- lm(height99~soil, data=lam_stat)
summary(lm.h99soil)
names(lm.h99soil)
confint(lm.h99soil)
predict(lm.h99soil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99soil) %>%
  abline()
plot(lam_stat$soil,lam_stat$height99)



#Height 99~HabType: 
lm.h99HabType <- lm(height99~HabType, data=lam_stat)
summary(lm.h99HabType)
names(lm.h99HabType)
confint(lm.h99HabType)
predict(lm.h99HabType, data.frame(HabType=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.h99HabType) %>%
  abline()
plot(lam_stat$HabType,lam_stat$height99)

#Height Mean~HabType: 
lm.hmeanHabType <- lm(heightmean~HabType, data=lam_stat)
summary(lm.hmeanHabType)
confint(lm.hmeanHabType)
predict(lm.hmeanHabType, data.frame(HabType=c(5,10,15)), 
        interval="confidence")
plot(lm.hmeanHabType) %>%
  abline ()

plot(lam_stat$HabType,lam_stat$heightmean)

#Height Max~HabType: 
lm.hmaxHabType <- lm(heightmax~HabType, data=lam_stat)
summary(lm.hmaxHabType)
names(lm.hmaxHabType)
confint(lm.hmaxHabType)
predict(lm.hmaxHabType, data.frame(HabType=c(5,10,15)), 
        interval="confidence")
plot(lm.hmaxHabType) %>%
  abline ()
plot(lam_stat$HabType,lam_stat$heightmax)

#Multiple Linear Regresion------------
#with soil
#Height99~ elev+slope+aspect+tpi+twi+soil
lm.h99eastts <- lm(height99~elev+aspect+slope+tpi+Lambir_TWI+soil, data=lam_stat)
summary(lm.h99eastts)
library(car)
vif(lm.h99eastts)
plot(lm.h99eastts) %>%
  abline ()

#Height99~ elev+slope+aspect+twi+soil
lm.h99easts <- lm(height99~elev+aspect+slope+Lambir_TWI+soil, data=lam_stat)
summary(lm.h99easts)
vif(lm.h99easts)
plot(lm.h99easts) %>%
  abline ()

#Height99~ elev+twi+soil
lm.h99estwi <- lm(height99~elev+Lambir_TWI+soil, data=lam_stat)
summary(lm.h99estwi)
vif(lm.h99estwi)
plot(lm.h99estwi) %>%
  abline ()

#Height99~ elev+soil
lm.h99esoil <- lm(height99~elev+soil, data=lam_stat)
summary(lm.h99esoil)
vif(lm.h99esoil)
plot(lm.h99esoil) %>%
  abline ()

#Height99~ twi+soil
lm.h99stwi <- lm(height99~Lambir_TWI+soil, data=lam_stat)
summary(lm.h99stwi)
vif(lm.h99stwi)
plot(lm.h99stwi) %>%
  abline ()

#Height99~ elev*slope*aspect*tpi*twi*soil
lm.h99eastts1 <- lm(height99~elev*aspect*slope*tpi*Lambir_TWI*soil, data=lam_stat)
summary(lm.h99eastts1)
library(car)
vif(lm.h99eastts1)
plot(lm.h99eastts1) %>%
  abline ()

#Height99~ elev*slope*aspect*twi*soil
lm.h99easts1 <- lm(height99~elev*aspect*slope*Lambir_TWI*soil, data=lam_stat)
summary(lm.h99easts1)
vif(lm.h99easts1)
plot(lm.h99easts1) %>%
  abline ()

#Height99~ elev*twi*soil
lm.h99estwi1 <- lm(height99~elev*Lambir_TWI*soil, data=lam_stat)
summary(lm.h99estwi1)
vif(lm.h99estwi1)
plot(lm.h99estwi1) %>%
  abline ()

#Height99~ elev*soil
lm.h99esoil1 <- lm(height99~elev*soil, data=lam_stat)
summary(lm.h99esoil1)
vif(lm.h99esoil1)
plot(lm.h99esoil1) %>%
  abline ()

#Height99~ twi*soil
lm.h99stwi1 <- lm(height99~Lambir_TWI*soil, data=lam_stat)
summary(lm.h99stwi1)
vif(lm.h99stwi1)
plot(lm.h99stwi1) %>%
  abline ()



#without soil
#Height99~ elev+slope+aspect+tpi+twi
lm.h99eastt <- lm(height99~elev+aspect+slope+tpi+Lambir_TWI, data=lam_stat)
summary(lm.h99eastt)
library(car)
vif(lm.h99eastt)
plot(lm.h99eastt) %>%
  abline ()

#Height99~ elev+slope+aspect+tpi
lm.h99east <- lm(height99~elev+aspect+slope+tpi, data=lam_stat)
summary(lm.h99east)
vif(lm.h99east)
plot(lm.h99east) %>%
  abline ()

#Height99~ elev+slope+aspect+twi
lm.h99eastwi <- lm(height99~elev+aspect+slope+Lambir_TWI, data=lam_stat)
summary(lm.h99eastwi)
vif(lm.h99eastwi)
plot(lm.h99eastwi) %>%
  abline ()

#Height99~ elev+slope+twi
lm.h99estwi <- lm(height99~elev+slope+Lambir_TWI, data=lam_stat)
summary(lm.h99estwi)
vif(lm.h99estwi)
plot(lm.h99estwi) %>%
  abline ()

#Height99~ elev+aspect+twi
lm.h99eatwi <- lm(height99~elev+aspect+Lambir_TWI, data=lam_stat)
summary(lm.h99eatwi)
vif(lm.h99eatwi)
plot(lm.h99eatwi) %>%
  abline ()

#Height99~ elev+slope+aspect
lm.h99eas <- lm(height99~elev+aspect+slope, data=lam_stat)
summary(lm.h99eas)
vif(lm.h99eas)
plot(lm.h99eas) %>%
  abline ()

#Height99~elev+slope
lm.h99es <- lm(height99~elev+slope, data=lam_stat)
summary(lm.h99es)
vif(lm.h99es)
plot(lm.h99es) %>%
  abline ()

#Height99~elev+TWI
lm.h99etwi <- lm(height99~elev+Lambir_TWI, data=lam_stat)
summary(lm.h99etwi)
vif(lm.h99etwi)
plot(lm.h99etwi) %>%
  abline ()

#Height99~elev+aspect
lm.h99ea <- lm(height99~elev+aspect, data=lam_stat)
summary(lm.h99ea)
vif(lm.h99ea)
plot(lm.h99ea) %>%
  abline ()

#Height99~aspect+slope
lm.h99as <- lm(height99~aspect+slope, data=lam_stat)
summary(lm.h99as)
library(car)
vif(lm.h99as)
plot(lm.h99as) %>%
  abline ()


#Height99~ elev*slope*aspect*twi*tpi
lm.h99eastt1 <- lm(height99~elev*aspect*slope*tpi*Lambir_TWI, data=lam_stat)
summary(lm.h99eastt1)
library(car)
vif(lm.h99eastt1)
plot(lm.h99eastt1) %>%
  abline ()

#Height99~ elev*slope*aspect*twi
lm.h99eastwi1 <- lm(height99~elev*aspect*slope*Lambir_TWI, data=lam_stat)
summary(lm.h99eastwi1)
library(car)
vif(lm.h99eastwi1)
plot(lm.h99eastwi1) %>%
  abline ()

#Height99~ elev*twi
lm.h99etwi1 <- lm(height99~elev*Lambir_TWI, data=lam_stat)
summary(lm.h99etwi1)
library(car)
vif(lm.h99etwi1)
plot(lm.h99etwi1) %>%
  abline ()

#Height99~ elev*slope*aspect
lm.h99eas1 <- lm(height99~elev*aspect*slope, data=lam_stat)
summary(lm.h99eas1)
library(car)
vif(lm.h99eas1)
plot(lm.h99eas1) %>%
  abline ()

#Height99~elev*slope
lm.h99es1 <- lm(height99~elev*slope, data=lam_stat)
summary(lm.h99es1)
vif(lm.h99es1)
plot(lm.h99es1) %>%
  abline ()

#Height99~elev*aspect
lm.h99ea1 <- lm(height99~elev*aspect, data=lam_stat)
summary(lm.h99ea1)
vif(lm.h99ea1)
plot(lm.h99ea1) %>%
  abline ()

#Height99~aspect*slope
lm.h99as1 <- lm(height99~aspect*slope, data=lam_stat)
summary(lm.h99as1)
vif(lm.h99as1)
plot(lm.h99as1) %>%
  abline ()

#Height99~ elev*slope*twi
lm.h99estwi1 <- lm(height99~elev*slope*Lambir_TWI, data=lam_stat)
summary(lm.h99estwi1)
vif(lm.h99estwi1)
plot(lm.h99estwi1) %>%
  abline ()

#Height99~ elev*aspect*twi
lm.h99eatwi1 <- lm(height99~elev*aspect*Lambir_TWI, data=lam_stat)
summary(lm.h99eatwi1)
vif(lm.h99eatwi1)
plot(lm.h99eatwi1) %>%
  abline ()


