library(MASS)
library(ISLR)

lam_data <- read_csv(here("Desktop","Research","HCRP","Lambir Data", "lam_topo.csv"))

#Add quadrat level emergent labeling
source("~/Documents/GitHub/tall-trees-research/heights.r")
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

lam_stat <- lam_data

#lam_stat <- lam_data %>% group_by(quadrat,quad_type,dbhmean,heightmean,heightmedian,height99,heightmax,HabType,
                                  #slope,aspect,tpi,elev,Lambir_TWI,soil)  %>%  summarise(quad_x = mean(x),
                                                                                         #quad_y = mean(y))
summary(lam_stat$quad_x)
lam_stat$soil <-as.factor(lam_stat$soil)
table(lam_stat$quad_type)
lam_stat$bitype <- ifelse(lam_stat$quad_type=="emrgnt", 1,0)
table(lam_stat$bitype)

#Multiple Logistic Regression---------
twi3 <- glm(bitype~Lambir_TWI+I(Lambir_TWI^3), data=lam_stat, family="binomial")
summary(twi3)


bielev <- glm(bitype~elev, data=lam_stat, family="binomial")
summary(bielev)
plot(lam_stat$elev, lam_stat$bitype)

bitpi <- glm(bitype~tpi, data=lam_stat, family="binomial")
summary(bitpi)
plot(lam_stat$tpi, lam_stat$bitype)

biaspect <- glm(bitype~aspect, data=lam_stat, family="binomial")
summary(biaspect)
plot(lam_stat$aspect, lam_stat$bitype)

bislope <- glm(bitype~slope, data=lam_stat, family="binomial")
summary(bislope)
plot(lam_stat$slope, lam_stat$bitype)

bitwi <- glm(bitype~Lambir_TWI, data=lam_stat, family="binomial")
summary(bitwi)
plot(lam_stat$Lambir_TWI, lam_stat$bitype)

bisoil <- glm(bitype~soil, data=lam_stat, family="binomial")
summary(bisoil)


#with x and y coords
#bielevxy <- glm(bitype~elev+quad_x+quad_y, data=lam_stat, family="binomial")
bielevxy <- glm(bitype~elev+x+y, data=lam_stat, family="binomial")
summary(bielevxy)
plot(lam_stat$elev, lam_stat$bitype)

#bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=lam_stat, family="binomial")
bitpixy <- glm(bitype~tpi+x+y, data=lam_stat, family="binomial")
summary(bitpixy)
plot(lam_stat$tpi, lam_stat$bitype)

#biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=lam_stat, family="binomial")
biaspectxy <- glm(bitype~aspect+x+y, data=lam_stat, family="binomial")
summary(biaspectxy)
plot(lam_stat$aspect, lam_stat$bitype)

#bislopexy <- glm(bitype~slope+quad_x+quad_y, data=lam_stat, family="binomial")
bislopexy <- glm(bitype~slope+x+y, data=lam_stat, family="binomial")
summary(bislopexy)
plot(lam_stat$slope, lam_stat$bitype)

#bitwixy <- glm(bitype~Lambir_TWI+quad_x+quad_y, data=lam_stat, family="binomial")
bitwixy <- glm(bitype~Lambir_TWI+x+y, data=lam_stat, family="binomial")
summary(bitwixy)
plot(lam_stat$Lambir_TWI, lam_stat$bitype)

#bisoilxy <- glm(bitype~soil+quad_x+quad_y, data=lam_stat, family="binomial")
bisoilxy <- glm(bitype~soil+x+y, data=lam_stat, family="binomial")
summary(bisoilxy)

#Random Effects
#For indiv tree analyses----------
#Height 99~Elevation: 
lmer.h99elev <- lmer(height~elev+(1|soil), data=lam_stat)
summary(lmer.h99elev)
AIC(lmer.h99elev)
names(lmer.h99elev)
confint(lmer.h99elev)

tval <- summary(lmer.h99elev)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)


predict(lmer.h99elev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev)
plot(lam_stat$elev,lam_stat$height)

#Height 99~TPI: 
lmer.h99tpi <- lmer(height~tpi+(1|soil), data=lam_stat)
summary(lmer.h99tpi)
AIC(lmer.h99tpi)
names(summary(lmer.h99tpi))
tval <- summary(lmer.h99tpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-5)

tval <- summary(lmer.h99tpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

predict(lmer.h99tpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi)
plot(lam_stat$tpi,lam_stat$height)

#Height 99~aspect: 
lmer.h99aspect <- lmer(height~aspect+(1|soil), data=lam_stat)
summary(lmer.h99aspect)
AIC(lmer.h99aspect)
confint(lmer.h99aspect)
predict(lmer.h99aspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect)
plot(lam_stat$aspect,lam_stat$height)

tval <- summary(lmer.h99aspect)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~slope: 
lmer.h99slope <- lmer(height~slope+(1|soil), data=lam_stat)
summary(lmer.h99slope)
AIC(lmer.h99slope)
confint(lmer.h99slope)
predict(lmer.h99slope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope)
plot(lam_stat$slope,lam_stat$height)

tval <- summary(lmer.h99slope)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~TWI: 
lmer.h99twi <- lmer(height~Lambir_TWI+(1|soil), data=lam_stat)
summary(lmer.h99twi)
AIC(lmer.h99twi)
confint(lmer.h99twi)
predict(lmer.h99twi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi)
plot(lam_stat$Lambir_TWI,lam_stat$height)

tval <- summary(lmer.h99twi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1169-1)

#Height 99~TWI+TPI: 
lmer.h99twitpi <- lmer(height~Lambir_TWI+tpi+(1|soil), data=lam_stat)
summary(lmer.h99twitpi)
AIC(lmer.h99twitpi)
confint(lmer.h99twitpi)
predict(lmer.h99twitpi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twitpi)
plot(lam_stat$Lambir_TWI,lam_stat$height)

#Height 99~TWI*TPI: 
lmer.h99twitpi1 <- lmer(height~Lambir_TWI*tpi+(1|soil), data=lam_stat)
summary(lmer.h99twitpi1)
AIC(lmer.h99twitpi1)
confint(lmer.h99twitpi1)
predict(lmer.h99twitpi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twitpi1)
plot(lam_stat$Lambir_TWI,lam_stat$height)

#Height 99~slope+aspect: 
lmer.h99slopeaspect <- lmer(height~slope+aspect+(1|soil), data=lam_stat)
summary(lmer.h99slopeaspect)
AIC(lmer.h99slopeaspect)
confint(lmer.h99slopeaspect)
predict(lmer.h99slopeaspect, data.frame(slope=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99slopeaspect)
plot(lam_stat$Lambir_slope,lam_stat$height)

#Ints and Only--------
#Height 99~Elevation: 
lmer.h99elev1 <- lmer(height~elev +(1|soil)+ (0 + elev|soil), data=lam_stat)
summary(lmer.h99elev1)
AIC(lmer.h99elev1)
confint(lmer.h99elev1)
predict(lmer.h99elev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev1)
plot(lam_stat$elev,lam_stat$height)

tval <- summary(lmer.h99elev1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1300-1)

#Height 99~TPI: 
lmer.h99tpi1 <- lmer(height~tpi+(1|soil)+ (0 + tpi|soil), data=lam_stat)
summary(lmer.h99tpi1)
AIC(lmer.h99tpi1)
confint(lmer.h99tpi1)
predict(lmer.h99tpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi1)
plot(lam_stat$tpi,lam_stat$height)

tval <- summary(lmer.h99tpi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~aspect: 
lmer.h99aspect1 <- lmer(height~aspect+(1|soil)+ (0 + aspect|soil), data=lam_stat)
summary(lmer.h99aspect1)
AIC(lmer.h99aspect1)
confint(lmer.h99aspect1)
predict(lmer.h99aspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect1)
plot(lam_stat$aspect,lam_stat$height)

tval <- summary(lmer.h99aspect1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~slope: 
lmer.h99slope1 <- lmer(height~slope+(1|soil)+ (0 + slope|soil), data=lam_stat)
summary(lmer.h99slope1)
AIC(lmer.h99slope1)
confint(lmer.h99slope1)
predict(lmer.h99slope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope1)
plot(lam_stat$slope,lam_stat$height)

tval <- summary(lmer.h99slope1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~TWI: 
lmer.h99twi1 <- lmer(height~Lambir_TWI+(1|soil)+ (0 + Lambir_TWI|soil), data=lam_stat)
summary(lmer.h99twi1)
AIC(lmer.h99twi1)
confint(lmer.h99twi1)
predict(lmer.h99twi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi1)
plot(lam_stat$Lambir_TWI,lam_stat$height)

tval <- summary(lmer.h99twi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Analyses by Quadrat----------
#Just Ints--------
library(lme4)
#Height 99~Elevation: 
lmer.h99elev <- lmer(height99~elev+(1|soil), data=lam_stat)
summary(lmer.h99elev)
AIC(lmer.h99elev)
names(lmer.h99elev)
confint(lmer.h99elev)

tval <- summary(lmer.h99elev)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)


predict(lmer.h99elev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev)
plot(lam_stat$elev,lam_stat$height99)

#Height 99~TPI: 
lmer.h99tpi <- lmer(height99~tpi+(1|soil), data=lam_stat)
summary(lmer.h99tpi)
AIC(lmer.h99tpi)
names(summary(lmer.h99tpi))
tval <- summary(lmer.h99tpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-5)

tval <- summary(lmer.h99tpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

predict(lmer.h99tpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi)
plot(lam_stat$tpi,lam_stat$height99)

#Height 99~aspect: 
lmer.h99aspect <- lmer(height99~aspect+(1|soil), data=lam_stat)
summary(lmer.h99aspect)
AIC(lmer.h99aspect)
confint(lmer.h99aspect)
predict(lmer.h99aspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect)
plot(lam_stat$aspect,lam_stat$height99)

tval <- summary(lmer.h99aspect)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~slope: 
lmer.h99slope <- lmer(height99~slope+(1|soil), data=lam_stat)
summary(lmer.h99slope)
AIC(lmer.h99slope)
confint(lmer.h99slope)
predict(lmer.h99slope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope)
plot(lam_stat$slope,lam_stat$height99)

tval <- summary(lmer.h99slope)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~TWI: 
lmer.h99twi <- lmer(height99~Lambir_TWI+(1|soil), data=lam_stat)
summary(lmer.h99twi)
AIC(lmer.h99twi)
confint(lmer.h99twi)
predict(lmer.h99twi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi)
plot(lam_stat$Lambir_TWI,lam_stat$height99)

tval <- summary(lmer.h99twi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1169-1)

#Height 99~TWI+TPI: 
lmer.h99twitpi <- lmer(height99~Lambir_TWI+tpi+(1|soil), data=lam_stat)
summary(lmer.h99twitpi)
AIC(lmer.h99twitpi)
confint(lmer.h99twitpi)
predict(lmer.h99twitpi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twitpi)
plot(lam_stat$Lambir_TWI,lam_stat$height99)

#Height 99~TWI*TPI: 
lmer.h99twitpi1 <- lmer(height99~Lambir_TWI*tpi+(1|soil), data=lam_stat)
summary(lmer.h99twitpi1)
AIC(lmer.h99twitpi1)
confint(lmer.h99twitpi1)
predict(lmer.h99twitpi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twitpi1)
plot(lam_stat$Lambir_TWI,lam_stat$height99)

#Height 99~slope+aspect: 
lmer.h99slopeaspect <- lmer(height99~slope+aspect+(1|soil), data=lam_stat)
summary(lmer.h99slopeaspect)
AIC(lmer.h99slopeaspect)
confint(lmer.h99slopeaspect)
predict(lmer.h99slopeaspect, data.frame(slope=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99slopeaspect)
plot(lam_stat$Lambir_slope,lam_stat$height99)

#Ints and Only--------
#Height 99~Elevation: 
lmer.h99elev1 <- lmer(height99~elev +(1|soil)+ (0 + elev|soil), data=lam_stat)
summary(lmer.h99elev1)
AIC(lmer.h99elev1)
confint(lmer.h99elev1)
predict(lmer.h99elev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev1)
plot(lam_stat$elev,lam_stat$height99)

tval <- summary(lmer.h99elev1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1300-1)

#Height 99~TPI: 
lmer.h99tpi1 <- lmer(height99~tpi+(1|soil)+ (0 + tpi|soil), data=lam_stat)
summary(lmer.h99tpi1)
AIC(lmer.h99tpi1)
confint(lmer.h99tpi1)
predict(lmer.h99tpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi1)
plot(lam_stat$tpi,lam_stat$height99)

tval <- summary(lmer.h99tpi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~aspect: 
lmer.h99aspect1 <- lmer(height99~aspect+(1|soil)+ (0 + aspect|soil), data=lam_stat)
summary(lmer.h99aspect1)
AIC(lmer.h99aspect1)
confint(lmer.h99aspect1)
predict(lmer.h99aspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect1)
plot(lam_stat$aspect,lam_stat$height99)

tval <- summary(lmer.h99aspect1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~slope: 
lmer.h99slope1 <- lmer(height99~slope+(1|soil)+ (0 + slope|soil), data=lam_stat)
summary(lmer.h99slope1)
AIC(lmer.h99slope1)
confint(lmer.h99slope1)
predict(lmer.h99slope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope1)
plot(lam_stat$slope,lam_stat$height99)

tval <- summary(lmer.h99slope1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Height 99~TWI: 
lmer.h99twi1 <- lmer(height99~Lambir_TWI+(1|soil)+ (0 + Lambir_TWI|soil), data=lam_stat)
summary(lmer.h99twi1)
AIC(lmer.h99twi1)
confint(lmer.h99twi1)
predict(lmer.h99twi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi1)
plot(lam_stat$Lambir_TWI,lam_stat$height99)

tval <- summary(lmer.h99twi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=1250-1)

#Random Effect Model
library(nlme)
heightelev <- lme(height99~elev, random=~1|soil, data=lam_stat, method="REML")
summary(heightelev)
plot(heightelev) %>%
  abline()
hist(lam_data$elev)
library(lme4)
colnames(lam_data)
lam_data$quadrat_group <- as.factor(lam_data$quadrat)
heightelev <- lmer(height99~elev+(1|quadrat_group), data=lam_data)
#if hist(lam_dat) or hist(lamstat) are skewed- log transform them
hist(lam_data$height99)
hist(lam_stat$height99)
summary(heightelev)
str(lam_data$quadrat)
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

#Height~Elevation: 
lm.helev <- lm(height~elev, data=lam_stat)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(lam_stat$elev,lam_stat$height)

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

#Height~TPI: 
lm.htpi <- lm(height~tpi, data=lam_stat)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$height)

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

#Height~aspect: 
lm.haspect <- lm(height~aspect, data=lam_stat)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$height)

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

#Height ~slope: 
lm.hslope <- lm(height~slope, data=lam_stat)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(lam_stat$slope,lam_stat$height)

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

#Height ~TWI: 
lm.htwi <- lm(height~Lambir_TWI, data=lam_stat)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
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

#Height ~Soil: 
lm.hsoil <- lm(height~soil, data=lam_stat)
summary(lm.hsoil)
names(lm.hsoil)
confint(lm.hsoil)
predict(lm.hsoil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoil) %>%
  abline()
plot(lam_stat$soil,lam_stat$height)

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


