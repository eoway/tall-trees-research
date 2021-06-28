library(here)
library(dplyr)
library(lme4)

lam_data <- read_csv("~/Desktop/Research/HCRP/Lambir Data/lam_topo.csv")

source("~/Documents/GitHub/tall-trees-research/heights.r")
dbh99 <- quantile99dbh #from heights.r
lam_data$tree_type <- ifelse(lam_data$dbh>=dbh99, "emrgnt", "nonemrgnt")
lam_data$bitree_type <- ifelse(lam_data$dbh>=dbh99, 1, 0)
table(lam_data$tree_type)
table(lam_data$bitree_type)
lam_label <- lam_data %>% group_by(quadrat,tree_type)  %>%  summarize()
table(lam_label$tree_type)
emergents <- filter(lam_label, tree_type=="emrgnt")
emergentquad <- unique(emergents$quadrat)
table(emergentquad) 

lam_data$quad_type <- ifelse(lam_data$quadrat %in% emergentquad, "emrgnt", "nonemrgnt")
table(lam_data$quad_type)

lam_data$soil <-as.factor(lam_data$soil)
table(lam_data$quad_type)
lam_data$bitype <- ifelse(lam_data$quad_type=="emrgnt", 1,0)
table(lam_data$bitype)

#---------------------------------------------------------------------------------------------#
#--------------------------------Emergent Species List----------------------------------------#
#---------------------------------------------------------------------------------------------#
emerg <- filter(lam_data, tree_type=="emrgnt")
summary(emerg)
lamspecies <- (unique(emerg$species))

lam_data$species_type <- ifelse(lam_data$species %in% lamspecies, "emrgntsp", "nonemrgnt")
table(lam_data$species_type)
lam_ana <- filter(lam_data, species_type == "emrgntsp")
table(lam_ana$species)

lam_em <- filter(lam_ana, tree_type=="emrgnt")
table(lam_em$species)
#---------------------------------------------------------------------------------------------#
#--------------------------------Emergents by Soil Type---------------------------------------#
#---------------------------------------------------------------------------------------------#
table(lam_data$soil)
#clay
clay <- filter(lam_data, soil == "Clay")
table(clay$tree_type)
table(clay$species_type)

#fine loam
fineloam <- filter(lam_data, soil == "Fine_loam")
table(fineloam$tree_type)
table(fineloam$species_type)

#loam
loam <- filter(lam_data, soil == "Loam")
table(loam$tree_type)
table(loam$species_type)

#Sandy Loam
sandyloam <- filter(lam_data, soil == "Sandy_loam")
table(sandyloam$tree_type)
table(sandyloam$species_type)


#---------------------------------------------------------------------------------------------#
#---------------------------------Quadrat Level Analysis--------------------------------------#
#---------------------------------------------------------------------------------------------#
lam_quad <- lam_ana %>% group_by(quadrat,quad_type,dbhmean,heightmean,heightmedian,height99,heightmax,HabType,
                                  slope,aspect,tpi,elev,Lambir_TWI,soil, bitype)  %>%  summarise(quad_x = mean(x),
                                                                                         quad_y = mean(y))

#---------------------------------------------------------------------------------------------#
#-----------------------------Multiple Logistic Regression------------------------------------#
#---------------------------------------------------------------------------------------------#
bielev3 <- glm(bitype~elev, data=lam_quad, family="binomial")
summary(bielev3)
plot(lam_quad$elev, lam_quad$bitype)

bitpi3 <- glm(bitype~tpi, data=lam_quad, family="binomial")
summary(bitpi3)
plot(lam_quad$tpi, lam_quad$bitype)

biaspect3 <- glm(bitype~aspect, data=lam_quad, family="binomial")
summary(biaspect3)
plot(lam_quad$aspect, lam_quad$bitype)

bislope3 <- glm(bitype~slope, data=lam_quad, family="binomial")
summary(bislope3)
plot(lam_quad$slope, lam_quad$bitype)

bitwi3 <- glm(bitype~Lambir_TWI, data=lam_quad, family="binomial")
summary(bitwi3)
plot(lam_quad$Lambir_TWI, lam_quad$bitype, xlab="TWI", ylab="Tree Type")

bisoil <- glm(bitype~soil, data=lam_quad, family="binomial")
summary(bisoil)

#with x and y coords
#bielevxy <- glm(bitype~elev+quad_x+quad_y, data=lam_quad, family="binomial")
bielevxy <- glm(bitype~elev+quad_x+quad_y, data=lam_quad, family="binomial")
summary(bielevxy)
plot(lam_quad$elev, lam_quad$bitype)

#bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=lam_quad, family="binomial")
bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=lam_quad, family="binomial")
summary(bitpixy)
plot(lam_quad$tpi, lam_quad$bitype)

#biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=lam_quad, family="binomial")
biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=lam_quad, family="binomial")
summary(biaspectxy)
plot(lam_quad$aspect, lam_quad$bitype)

#bislopexy <- glm(bitype~slope+quad_x+quad_y, data=lam_quad, family="binomial")
bislopexy <- glm(bitype~slope+quad_x+quad_y, data=lam_quad, family="binomial")
summary(bislopexy)
plot(lam_quad$slope, lam_quad$bitype)

#bitwixy <- glm(bitype~Lambir_TWI+quad_x+quad_y, data=lam_quad, family="binomial")
bitwixy <- glm(bitype~Lambir_TWI+quad_x+quad_y, data=lam_quad, family="binomial")
summary(bitwixy)
plot(lam_quad$Lambir_TWI, lam_quad$bitype)

#bisoilxy <- glm(bitype~soil+quad_x+quad_y, data=lam_quad, family="binomial")
bisoilxy <- glm(bitype~soil+quad_x+quad_y, data=lam_quad, family="binomial")
summary(bisoilxy)

#---------------------------------------------------------------------------------------------#
#-----------------------------------Soil Random Effects---------------------------------------#
#---------------------------------------------------------------------------------------------#
#For indiv tree analyses----------
#height99 99~Elevation: 
lmer.h99elev <- lmer(height99~elev+(1|soil), data=lam_quad)
summary(lmer.h99elev)
AIC(lmer.h99elev)
names(lmer.h99elev)
confint(lmer.h99elev)

tval <- summary(lmer.h99elev)$coefficients[,3]
#pval
2*pt(-abs(tval),df=31411-1)


predict(lmer.h99elev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev)
plot(lam_quad$elev,lam_quad$height99)

#height99 99~TPI: 
lmer.h99tpi <- lmer(height99~tpi+(1|soil), data=lam_quad)
summary(lmer.h99tpi)
AIC(lmer.h99tpi)
names(summary(lmer.h99tpi))
tval <- summary(lmer.h99tpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-5)

tval <- summary(lmer.h99tpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-1)

predict(lmer.h99tpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi)
plot(lam_quad$tpi,lam_quad$height99)

#height99 99~aspect: 
lmer.h99aspect <- lmer(height99~aspect+(1|soil), data=lam_quad)
summary(lmer.h99aspect)
AIC(lmer.h99aspect)
confint(lmer.h99aspect)
predict(lmer.h99aspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect)
plot(lam_quad$aspect,lam_quad$height99)

tval <- summary(lmer.h99aspect)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-1)

#height99 99~slope: 
lmer.h99slope <- lmer(height99~slope+(1|soil), data=lam_quad)
summary(lmer.h99slope)
AIC(lmer.h99slope)
confint(lmer.h99slope)
predict(lmer.h99slope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope)
plot(lam_quad$slope,lam_quad$height99)

tval <- summary(lmer.h99slope)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-1)

#height99 99~TWI: 
lmer.h99twi <- lmer(height99~Lambir_TWI+(1|soil), data=lam_quad)
summary(lmer.h99twi)
AIC(lmer.h99twi)
confint(lmer.h99twi)
predict(lmer.h99twi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi)
plot(lam_quad$Lambir_TWI,lam_quad$height99)

tval <- summary(lmer.h99twi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=28424-1)

#Ints and Only--------
#height99 99~Elevation: 
lmer.h99elev1 <- lmer(height99~elev +(1|soil)+ (0 + elev|soil), data=lam_quad)
summary(lmer.h99elev1)
AIC(lmer.h99elev1)
confint(lmer.h99elev1)
predict(lmer.h99elev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99elev1)
plot(lam_quad$elev,lam_quad$height99)

tval <- summary(lmer.h99elev1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=31411-1)

#height99 99~TPI: 
lmer.h99tpi1 <- lmer(height99~tpi+(1|soil)+ (0 + tpi|soil), data=lam_quad)
summary(lmer.h99tpi1)
AIC(lmer.h99tpi1)
confint(lmer.h99tpi1)
predict(lmer.h99tpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99tpi1)
plot(lam_quad$tpi,lam_quad$height99)

tval <- summary(lmer.h99tpi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-1)

#height99 99~aspect: 
lmer.h99aspect1 <- lmer(height99~aspect+(1|soil)+(0 + aspect|soil), data=lam_quad)
summary(lmer.h99aspect1)
AIC(lmer.h99aspect1)
confint(lmer.h99aspect1)
predict(lmer.h99aspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99aspect1)
plot(lam_quad$aspect,lam_quad$height99)

tval <- summary(lmer.h99aspect1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-1)

#height99 99~slope: 
lmer.h99slope1 <- lmer(height99~slope+(1|soil)+(0 + slope|soil), data=lam_quad)
summary(lmer.h99slope1)
AIC(lmer.h99slope1)
confint(lmer.h99slope1)
predict(lmer.h99slope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.h99slope1)
plot(lam_quad$slope,lam_quad$height99)

tval <- summary(lmer.h99slope1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=30190-1)

#height99 99~TWI: 
lmer.h99twi1 <- lmer(height99~Lambir_TWI+(1|soil)+ (0 + Lambir_TWI|soil), data=lam_quad)
summary(lmer.h99twi1)
AIC(lmer.h99twi1)
confint(lmer.h99twi1)
predict(lmer.h99twi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.h99twi1)
plot(lam_quad$Lambir_TWI,lam_quad$height99)

tval <- summary(lmer.h99twi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=28424-1)

#---------------------------------------------------------------------------------------------#
#------------------------------------Linear Regression----------------------------------------#
#---------------------------------------------------------------------------------------------#
#height99~Elevation: 
lm.helev <- lm(height99~elev, data=lam_quad)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(lam_stat$elev,lam_stat$height99)

#height99~Elevation: 
lm.helevxy <- lm(height99~elev+quad_x+quad_y, data=lam_quad)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(lam_stat$elev,lam_stat$height99)


#height99 99~soil: 
lm.hsoil <- lm(height99~soil, data=lam_quad)
summary(lm.hsoil)
confint(lm.hsoil)
predict(lm.hsoil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoil) %>%
  abline()
plot(lam_stat$soil,lam_stat$height99)

#height99 99~soil: 
lm.hsoilxy <- lm(height99~soil+quad_x+quad_y, data=lam_quad)
summary(lm.hsoilxy)
confint(lm.hsoilxy)
predict(lm.hsoilxy, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoilxy) %>%
  abline()
plot(lam_stat$soilxy,lam_stat$height99)

#height99~TPI: 
lm.htpi <- lm(height99~tpi, data=lam_quad)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$height99)

#height99~TPI: 
lm.htpixy <- lm(height99~tpi+quad_x+quad_y, data=lam_quad)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$height99)

#height99~aspect: 
lm.haspect <- lm(height99~aspect, data=lam_quad)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$height99)

#height99~aspect: 
lm.haspectxy <- lm(height99~aspect+quad_x+quad_y, data=lam_quad)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$height99)

#height99 ~slope: 
lm.hslope <- lm(height99~slope, data=lam_quad)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(lam_stat$slope,lam_stat$height99)

#height99 ~slope: 
lm.hslopexy <- lm(height99~slope+quad_x+quad_y, data=lam_quad)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(lam_stat$slope,lam_stat$height99)

#height99 ~TWI: 
lm.htwi <- lm(height99~Lambir_TWI, data=lam_quad)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(lam_stat$Lambir_TWI,lam_stat$height9999)

#height99 ~TWI: 
lm.htwixy <- lm(height99~Lambir_TWI+quad_x+quad_y, data=lam_quad)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(lam_stat$Lambir_TWI,lam_stat$height99)

#---------------------------------------------------------------------------------------------#
#--------------------------------Individual Level Analysis------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#-----------------------------Multiple Logistic Regression------------------------------------#
#---------------------------------------------------------------------------------------------# 
bitwisoils <- glm(bitree_type~soil+Lambir_TWI+(Lambir_TWI*soil)+x+y, data=lam_ana, family="binomial")
summary(bitwisoils)

bielev3 <- glm(bitree_type~elev, data=lam_ana, family="binomial")
summary(bielev3)
plot(lam_ana$elev, lam_ana$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=lam_ana, family="binomial")
summary(bitpi3)
plot(lam_ana$tpi, lam_ana$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=lam_ana, family="binomial")
summary(biaspect3)
plot(lam_ana$aspect, lam_ana$bitree_type)

bislope3 <- glm(bitree_type~slope, data=lam_ana, family="binomial")
summary(bislope3)
plot(lam_ana$slope, lam_ana$bitree_type)

bitwi3 <- glm(bitree_type~Lambir_TWI, data=lam_ana, family="binomial")
summary(bitwi3)
plot(lam_ana$Lambir_TWI, lam_ana$bitree_type, xlab="TWI", ylab="Tree Type")

bisoil <- glm(bitree_type~soil, data=lam_ana, family="binomial")
summary(bisoil)

#with x and y coords
#bielevxy <- glm(bitype~elev+quad_x+quad_y, data=lam_ana, family="binomial")
bielevxy <- glm(bitree_type~elev+x+y, data=lam_ana, family="binomial")
summary(bielevxy)
plot(lam_ana$elev, lam_ana$bitree_type)

#bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=lam_ana, family="binomial")
bitpixy <- glm(bitree_type~tpi+x+y, data=lam_ana, family="binomial")
summary(bitpixy)
plot(lam_ana$tpi, lam_ana$bitree_type)

#biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=lam_ana, family="binomial")
biaspectxy <- glm(bitree_type~aspect+x+y, data=lam_ana, family="binomial")
summary(biaspectxy)
plot(lam_ana$aspect, lam_ana$bitree_type)

#bislopexy <- glm(bitype~slope+quad_x+quad_y, data=lam_ana, family="binomial")
bislopexy <- glm(bitree_type~slope+x+y, data=lam_ana, family="binomial")
summary(bislopexy)
plot(lam_ana$slope, lam_ana$bitree_type)

#bitwixy <- glm(bitype~Lambir_TWI+quad_x+quad_y, data=lam_ana, family="binomial")
bitwixy <- glm(bitree_type~Lambir_TWI+x+y, data=lam_ana, family="binomial")
summary(bitwixy)
plot(lam_ana$Lambir_TWI, lam_ana$bitree_type)

#bisoilxy <- glm(bitype~soil+quad_x+quad_y, data=lam_ana, family="binomial")
bisoilxy <- glm(bitree_type~soil+x+y, data=lam_ana, family="binomial")
summary(bisoilxy)

#---------------------------------------------------------------------------------------------#
#-----------------------------------Soil Random Effects---------------------------------------#
#---------------------------------------------------------------------------------------------#
#For indiv tree analyses----------
#height99 99~Elevation: 
lmer.helev <- lmer(height99~elev+(1|soil), data=lam_ana)
summary(lmer.helev)
AIC(lmer.helev)
names(lmer.helev)
confint(lmer.helev)

tval <- summary(lmer.helev)$coefficients[,3]
#pval
2*pt(-abs(tval),df=151763-1)


predict(lmer.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.helev)
plot(lam_ana$elev,lam_ana$height99)

#height99 99~TPI: 
lmer.htpi <- lmer(height99~tpi+(1|soil), data=lam_ana)
summary(lmer.htpi)
AIC(lmer.htpi)
names(summary(lmer.htpi))
tval <- summary(lmer.htpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-5)

tval <- summary(lmer.htpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

predict(lmer.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.htpi)
plot(lam_ana$tpi,lam_ana$height99)

#height99 99~aspect: 
lmer.haspect <- lmer(height99~aspect+(1|soil), data=lam_ana)
summary(lmer.haspect)
AIC(lmer.haspect)
confint(lmer.haspect)
predict(lmer.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.haspect)
plot(lam_ana$aspect,lam_ana$height99)

tval <- summary(lmer.haspect)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~slope: 
lmer.hslope <- lmer(height99~slope+(1|soil), data=lam_ana)
summary(lmer.hslope)
AIC(lmer.hslope)
confint(lmer.hslope)
predict(lmer.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.hslope)
plot(lam_ana$slope,lam_ana$height99)

tval <- summary(lmer.hslope)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~TWI: 
lmer.htwi <- lmer(height99~Lambir_TWI+(1|soil), data=lam_ana)
summary(lmer.htwi)
AIC(lmer.htwi)
confint(lmer.htwi)
predict(lmer.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.htwi)
plot(lam_ana$Lambir_TWI,lam_ana$height99)

tval <- summary(lmer.htwi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=139758-1)

#Ints and Only--------
#height99 99~Elevation: 
lmer.helev1 <- lmer(height99~elev +(1|soil)+ (0 + elev|soil), data=lam_ana)
summary(lmer.helev1)
AIC(lmer.helev1)
confint(lmer.helev1)
predict(lmer.helev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.helev1)
plot(lam_ana$elev,lam_ana$height99)

tval <- summary(lmer.helev1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=151763-1)

#height99 99~TPI: 
lmer.htpi1 <- lmer(height99~tpi+(1|soil)+ (0 + tpi|soil), data=lam_ana)
summary(lmer.htpi1)
AIC(lmer.htpi1)
confint(lmer.htpi1)
predict(lmer.htpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.htpi1)
plot(lam_ana$tpi,lam_ana$height99)

tval <- summary(lmer.htpi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~aspect: 
lmer.haspect1 <- lmer(height99~aspect+(1|soil)+ (0 + aspect|soil), data=lam_ana)
summary(lmer.haspect1)
AIC(lmer.haspect1)
confint(lmer.haspect1)
predict(lmer.haspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.haspect1)
plot(lam_ana$aspect,lam_ana$height99)

tval <- summary(lmer.haspect1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~slope: 
lmer.hslope1 <- lmer(height99~slope+(1|soil)+ (0 + slope|soil), data=lam_ana)
summary(lmer.hslope1)
AIC(lmer.hslope1)
confint(lmer.hslope1)
predict(lmer.hslope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.hslope1)
plot(lam_ana$slope,lam_ana$height99)

tval <- summary(lmer.hslope1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~TWI: 
lmer.htwi1 <- lmer(height99~Lambir_TWI+(1|soil)+ (0 + Lambir_TWI|soil), data=lam_ana)
summary(lmer.htwi1)
AIC(lmer.htwi1)
confint(lmer.htwi1)
predict(lmer.htwi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.htwi1)
plot(lam_ana$Lambir_TWI,lam_ana$height99)

tval <- summary(lmer.htwi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=139758-1)

#---------------------------------------------------------------------------------------------#
#------------------------------------Species Random Effects-----------------------------------#
#---------------------------------------------------------------------------------------------#
#For indiv tree analyses----------
#height99 99~Elevation: 
lmer.helev <- lmer(height99~elev+(1|species), data=lam_ana)
summary(lmer.helev)
AIC(lmer.helev)
names(lmer.helev)
confint(lmer.helev)

tval <- summary(lmer.helev)$coefficients[,3]
#pval
2*pt(-abs(tval),df=151763-1)


predict(lmer.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.helev)
plot(lam_ana$elev,lam_ana$height99)

#height99 99~soil: 
lmer.hsoil <- lmer(height99~soil+(1|species), data=lam_ana)
summary(lmer.hsoil)
AIC(lmer.hsoil)
names(lmer.hsoil)
confint(lmer.hsoil)

tval <- summary(lmer.hsoil)$coefficients[,3]
#pval
2*pt(-abs(tval),df=151763-57)

par(mfrow=c(1,1))
plot(lmer.hsoil)
plot(lam_ana$soil,lam_ana$height99)

#height99 99~TPI: 
lmer.htpi <- lmer(height99~tpi+(1|species), data=lam_ana)
summary(lmer.htpi)
AIC(lmer.htpi)
names(summary(lmer.htpi))
tval <- summary(lmer.htpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-57)

tval <- summary(lmer.htpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

predict(lmer.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.htpi)
plot(lam_ana$tpi,lam_ana$height99)

#height99 99~aspect: 
lmer.haspect <- lmer(height99~aspect+(1|species), data=lam_ana)
summary(lmer.haspect)
AIC(lmer.haspect)
confint(lmer.haspect)
predict(lmer.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.haspect)
plot(lam_ana$aspect,lam_ana$height99)

tval <- summary(lmer.haspect)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~slope: 
lmer.hslope <- lmer(height99~slope+(1|species), data=lam_ana)
summary(lmer.hslope)
AIC(lmer.hslope)
confint(lmer.hslope)
predict(lmer.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.hslope)
plot(lam_ana$slope,lam_ana$height99)

tval <- summary(lmer.hslope)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-57)

#height99 99~TWI: 
lmer.htwi <- lmer(height99~Lambir_TWI+(1|species), data=lam_ana)
summary(lmer.htwi)
AIC(lmer.htwi)
confint(lmer.htwi)
predict(lmer.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.htwi)
plot(lam_ana$Lambir_TWI,lam_ana$height99)

tval <- summary(lmer.htwi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=139758-57)

#Ints and Only--------
#height99 99~Elevation: 
lmer.helev1 <- lmer(height99~elev +(1|species)+ (0 + elev|species), data=lam_ana)
summary(lmer.helev1)
AIC(lmer.helev1)
confint(lmer.helev1)
predict(lmer.helev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.helev1)
plot(lam_ana$elev,lam_ana$height99)

tval <- summary(lmer.helev1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=151763-57)

#height99 99~soil: 
lmer.hsoil1 <- lmer(height99~soil+(1|species)+ (0 + soil|species), data=lam_ana)
summary(lmer.hsoil1)
AIC(lmer.hsoil1)
names(lmer.hsoil1)
confint(lmer.hsoil1)

tval <- summary(lmer.hsoil1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=151763-57)

par(mfrow=c(1,1))
plot(lmer.hsoil1)
plot(lam_ana$soil,lam_ana$height99)

#height99 99~TPI: 
lmer.htpi1 <- lmer(height99~tpi+(1|species)+ (0 + tpi|species), data=lam_ana)
summary(lmer.htpi1)
AIC(lmer.htpi1)
confint(lmer.htpi1)
predict(lmer.htpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.htpi1)
plot(lam_ana$tpi,lam_ana$height99)

tval <- summary(lmer.htpi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~aspect: 
lmer.haspect1 <- lmer(height99~aspect+(1|species)+ (0 + aspect|species), data=lam_ana)
summary(lmer.haspect1)
AIC(lmer.haspect1)
confint(lmer.haspect1)
predict(lmer.haspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.haspect1)
plot(lam_ana$aspect,lam_ana$height99)

tval <- summary(lmer.haspect1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~slope: 
lmer.hslope1 <- lmer(height99~slope+(1|species)+ (0 + slope|species), data=lam_ana)
summary(lmer.hslope1)
AIC(lmer.hslope1)
confint(lmer.hslope1)
predict(lmer.hslope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.hslope1)
plot(lam_ana$slope,lam_ana$height99)

tval <- summary(lmer.hslope1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=146299-1)

#height99 99~TWI: 
lmer.htwi1 <- lmer(height99~Lambir_TWI+(1|species)+ (0 + Lambir_TWI|species), data=lam_ana)
summary(lmer.htwi1)
AIC(lmer.htwi1)
confint(lmer.htwi1)
predict(lmer.htwi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.htwi1)
plot(lam_ana$Lambir_TWI,lam_ana$height99)

tval <- summary(lmer.htwi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=139758-1)

#---------------------------------------------------------------------------------------------#
#------------------------------------Linear Regression----------------------------------------#
#---------------------------------------------------------------------------------------------#
#height99~Elevation: 
lm.helev <- lm(height99~elev, data=lam_ana)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(lam_stat$elev,lam_stat$height99)

#height99~Elevation: 
lm.helevxy <- lm(height99~elev+x+y, data=lam_ana)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(lam_stat$elev,lam_stat$height99)


#height99 99~soil: 
lm.hsoil <- lm(height99~soil, data=lam_ana)
summary(lm.hsoil)
confint(lm.hsoil)
predict(lm.hsoil, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoil) %>%
  abline()
plot(lam_stat$soil,lam_stat$height99)

#height99 99~soil: 
lm.hsoilxy <- lm(height99~soil+x+y, data=lam_ana)
summary(lm.hsoilxy)
confint(lm.hsoilxy)
predict(lm.hsoilxy, data.frame(soil=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoilxy) %>%
  abline()
plot(lam_stat$soilxy,lam_stat$height99)

#height99~TPI: 
lm.htpi <- lm(height99~tpi, data=lam_ana)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$height99)

#height99~TPI: 
lm.htpixy <- lm(height99~tpi+x+y, data=lam_ana)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(lam_stat$tpi,lam_stat$height99)

#height99~aspect: 
lm.haspect <- lm(height99~aspect, data=lam_ana)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$height99)

#height99~aspect: 
lm.haspectxy <- lm(height99~aspect+x+y, data=lam_ana)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(lam_stat$aspect,lam_stat$height99)

#height99 ~slope: 
lm.hslope <- lm(height99~slope, data=lam_ana)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(lam_stat$slope,lam_stat$height99)

#height99 ~slope: 
lm.hslopexy <- lm(height99~slope+x+y, data=lam_ana)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(lam_stat$slope,lam_stat$height99)

#height99 ~TWI: 
lm.htwi <- lm(height99~Lambir_TWI, data=lam_ana)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(lam_stat$Lambir_TWI,lam_stat$height9999)

#height99 ~TWI: 
lm.htwixy <- lm(height99~Lambir_TWI+x+y, data=lam_ana)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(lam_stat$Lambir_TWI,lam_stat$height99)

