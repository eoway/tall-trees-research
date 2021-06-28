library(here)
library(dplyr)
library(lme4)

dan_data <- read_csv("~/Desktop/Research/HCRP/dan_dat/dan_topo.csv")

#Add quadrat level emergent labeling
source("~/Documents/GitHub/tall-trees-research/heights.r")
dbh99 <- quantile99dbh #from heights.r
dan_data$tree_type <- ifelse(dan_data$dbh>=dbh99, "emrgnt", "nonemrgnt")
dan_data$bitree_type <- ifelse(dan_data$dbh>=dbh99, 1, 0)
table(dan_data$tree_type)
table(dan_data$bitree_type)
dan_label <- dan_data %>% group_by(tree_type,quadrat)  %>%  summarize()
table(dan_label$tree_type)
emergents <- filter(dan_label, tree_type=="emrgnt")
emergentquad <- unique(emergents$quadrat)
table(emergentquad)

dan_data$quad_type <- ifelse(dan_data$quadrat %in% emergentquad, "emrgnt", "nonemrgnt")
table(dan_data$quad_type)

dan_data$bitype <- ifelse(dan_data$quad_type=="emrgnt", 1,0)
table(dan_data$bitype)

#Create dataset with only emergent species--------------
emerg <- filter(dan_data, tree_type=="emrgnt")
emerg$specgen <- paste(emerg$genus, emerg$species)
table(emerg$specgen)
summary(emerg)
danspecies <- unique(emerg$specgen)

dan_data$specgen <- paste(dan_data$genus, dan_data$species)

dan_data$species_type <- ifelse(dan_data$specgen %in% danspecies, "emrgntsp", "nonemrgnt")
table(dan_data$species_type)
dan_ana <- filter(dan_data, species_type == "emrgntsp")
table(dan_ana$specgen)

dan_em <- filter(dan_ana, tree_type=="emrgnt")
table(dan_em$species)

#------------------------------------------------------------------------------------#
#-----------------------------Quadrat level analysis---------------------------------#
#------------------------------------------------------------------------------------#
dan_quadana <- dan_ana %>% group_by(quadrat,quad_type,soil)  %>%  summarise(quad_x = mean(x_utm),
                                                                          quad_y = mean(y_utm),
                                                                          slope = mean(slope),
                                                                          aspect = mean(aspect),
                                                                          tpi = mean(tpi),
                                                                          elev = mean(elev),
                                                                          twi = mean(twi),
                                                                          height99 = quantile(height, probs = 0.99, na.rm = TRUE))

dan_quadana$bitype <- ifelse(dan_quadana$quad_type=="emrgnt", 1,0)
table(dan_quadana$bitype)

#---------------------------------------------------------------------------------------------#
#-----------------------------Multiple Logistic Regression------------------------------------#
#---------------------------------------------------------------------------------------------#
bielev3 <- glm(bitype~elev, data=dan_quadana, family="binomial")
summary(bielev3)
plot(dan_quadana$elev, dan_quadana$bitype)

bitpi3 <- glm(bitype~tpi, data=dan_quadana, family="binomial")
summary(bitpi3)
plot(dan_quadana$tpi, dan_quadana$bitype)

biaspect3 <- glm(bitype~aspect, data=dan_quadana, family="binomial")
summary(biaspect3)
plot(dan_quadana$aspect, dan_quadana$bitype)

bislope3 <- glm(bitype~slope, data=dan_quadana, family="binomial")
summary(bislope3)
plot(dan_quadana$slope, dan_quadana$bitype)

bitwi3 <- glm(bitype~twi, data=dan_quadana, family="binomial")
summary(bitwi3)
plot(dan_quadana$twi, dan_quadana$bitype, xlab="TWI", ylab="Tree Type")

#with x and y coords
#bielevxy <- glm(bitype~elev+quad_x+quad_y, data=dan_quadana, family="binomial")
bielevxy <- glm(bitype~elev+quad_x+quad_y, data=dan_quadana, family="binomial")
summary(bielevxy)
plot(dan_quadana$elev, dan_quadana$bitype)

#bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=dan_quadana, family="binomial")
bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=dan_quadana, family="binomial")
summary(bitpixy)
plot(dan_quadana$tpi, dan_quadana$bitype)

#biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=dan_quadana, family="binomial")
biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=dan_quadana, family="binomial")
summary(biaspectxy)
plot(dan_quadana$aspect, dan_quadana$bitype)

#bislopexy <- glm(bitype~slope+quad_x+quad_y, data=dan_quadana, family="binomial")
bislopexy <- glm(bitype~slope+quad_x+quad_y, data=dan_quadana, family="binomial")
summary(bislopexy)
plot(dan_quadana$slope, dan_quadana$bitype)

#bitwixy <- glm(bitype~twi+quad_x+quad_y, data=dan_quadana, family="binomial")
bitwixy <- glm(bitype~twi+quad_x+quad_y, data=dan_quadana, family="binomial")
summary(bitwixy)
plot(dan_quadana$twi, dan_quadana$bitype)

#---------------------------------------------------------------------------------------------#
#------------------------------------Linear Regression----------------------------------------#
#---------------------------------------------------------------------------------------------#
#height99~Elevation: 
lm.helev <- lm(height99~elev, data=dan_quadana)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_quadana$elev,dan_quadana$height99)

#height99~Elevation: 
lm.helevxy <- lm(height99~elev+quad_x+quad_y, data=dan_quadana)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_quadana$elev,dan_quadana$height99)

#height99~TPI: 
lm.htpi <- lm(height99~tpi, data=dan_quadana)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_quadana$tpi,dan_quadana$height99)

#height99~TPI: 
lm.htpixy <- lm(height99~tpi+quad_x+quad_y, data=dan_quadana)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_quadana$tpi,dan_quadana$height99)

#height99~aspect: 
lm.haspect <- lm(height99~aspect, data=dan_quadana)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_quadana$aspect,dan_quadana$height99)

#height99~aspect: 
lm.haspectxy <- lm(height99~aspect+quad_x+quad_y, data=dan_quadana)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_quadana$aspect,dan_quadana$height99)

#height99 ~slope: 
lm.hslope <- lm(height99~slope, data=dan_quadana)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_quadana$slope,dan_quadana$height99)

#height99 ~slope: 
lm.hslopexy <- lm(height99~slope+quad_x+quad_y, data=dan_quadana)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_quadana$slope,dan_quadana$height99)

#height99 ~TWI: 
lm.htwi <- lm(height99~twi, data=dan_quadana)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_quadana$twi,dan_quadana$height99)

#height99 ~TWI: 
lm.htwixy <- lm(height99~twi+quad_x+quad_y, data=dan_quadana)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_quadana$twi,dan_quadana$height99)


#---------------------------------------------------------------------------------------------#
#--------------------------------Individual Level Analysis------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#-----------------------------Multiple Logistic Regression------------------------------------#
#---------------------------------------------------------------------------------------------# 
bielev3 <- glm(bitree_type~elev, data=dan_ana, family="binomial")
summary(bielev3)
plot(dan_ana$elev, dan_ana$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dan_ana, family="binomial")
summary(bitpi3)
plot(dan_ana$tpi, dan_ana$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dan_ana, family="binomial")
summary(biaspect3)
plot(dan_ana$aspect, dan_ana$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dan_ana, family="binomial")
summary(bislope3)
plot(dan_ana$slope, dan_ana$bitree_type)

bitwi3 <- glm(bitree_type~twi, data=dan_ana, family="binomial")
summary(bitwi3)
plot(dan_ana$twi, dan_ana$bitree_type, xlab="TWI", ylab="Tree Type")

#with x and y coords
#bielevxy <- glm(bitype~elev+quad_x+quad_y, data=dan_ana, family="binomial")
bielevxy <- glm(bitree_type~elev+plot_x+plot_y, data=dan_ana, family="binomial")
summary(bielevxy)
plot(dan_ana$elev, dan_ana$bitree_type)

#bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=dan_ana, family="binomial")
bitpixy <- glm(bitree_type~tpi+plot_x+plot_y, data=dan_ana, family="binomial")
summary(bitpixy)
plot(dan_ana$tpi, dan_ana$bitree_type)

#biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=dan_ana, family="binomial")
biaspectxy <- glm(bitree_type~aspect+plot_x+plot_y, data=dan_ana, family="binomial")
summary(biaspectxy)
plot(dan_ana$aspect, dan_ana$bitree_type)

#bislopexy <- glm(bitype~slope+quad_x+quad_y, data=dan_ana, family="binomial")
bislopexy <- glm(bitree_type~slope+plot_x+plot_y, data=dan_ana, family="binomial")
summary(bislopexy)
plot(dan_ana$slope, dan_ana$bitree_type)

#bitwixy <- glm(bitype~twi+quad_x+quad_y, data=dan_ana, family="binomial")
bitwixy <- glm(bitree_type~twi+plot_x+plot_y, data=dan_ana, family="binomial")
summary(bitwixy)
plot(dan_ana$twi, dan_ana$bitree_type)

#---------------------------------------------------------------------------------------------#
#------------------------------------Species Random Effects-----------------------------------#
#---------------------------------------------------------------------------------------------#
#For indiv tree analyses----------
library(lme4)
#height 99~Elevation: 
lmer.helev <- lmer(height~elev+(1|species), data=dan_ana)
summary(lmer.helev)
AIC(lmer.helev)
names(lmer.helev)
confint(lmer.helev)

tval <- summary(lmer.helev)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)


predict(lmer.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.helev)
plot(dan_ana$elev,dan_ana$height)

#height 99~TPI: 
lmer.htpi <- lmer(height~tpi+(1|species), data=dan_ana)
summary(lmer.htpi)
AIC(lmer.htpi)
names(summary(lmer.htpi))

tval <- summary(lmer.htpi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

predict(lmer.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.htpi)
plot(dan_ana$tpi,dan_ana$height)

#height 99~aspect: 
lmer.haspect <- lmer(height~aspect+(1|species), data=dan_ana)
summary(lmer.haspect)
AIC(lmer.haspect)
confint(lmer.haspect)
predict(lmer.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.haspect)
plot(dan_ana$aspect,dan_ana$height)

tval <- summary(lmer.haspect)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

#height 99~slope: 
lmer.hslope <- lmer(height~slope+(1|species), data=dan_ana)
summary(lmer.hslope)
AIC(lmer.hslope)
confint(lmer.hslope)
predict(lmer.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.hslope)
plot(dan_ana$slope,dan_ana$height)

tval <- summary(lmer.hslope)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

#height 99~TWI: 
lmer.htwi <- lmer(height~twi+(1|species), data=dan_ana)
summary(lmer.htwi)
AIC(lmer.htwi)
confint(lmer.htwi)
predict(lmer.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.htwi)
plot(dan_ana$twi,dan_ana$height)

tval <- summary(lmer.htwi)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29072-1)

#Ints and Only--------
#height 99~Elevation: 
lmer.helev1 <- lmer(height~elev +(1|species)+ (0 + elev|species), data=dan_ana)
summary(lmer.helev1)
AIC(lmer.helev1)
confint(lmer.helev1)
predict(lmer.helev1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.helev1)
plot(dan_ana$elev,dan_ana$height)

tval <- summary(lmer.helev1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

#height 99~TPI: 
lmer.htpi1 <- lmer(height~tpi+(1|species)+ (0 + tpi|species), data=dan_ana)
summary(lmer.htpi1)
AIC(lmer.htpi1)
confint(lmer.htpi1)
predict(lmer.htpi1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.htpi1)
plot(dan_ana$tpi,dan_ana$height)

tval <- summary(lmer.htpi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

#height 99~aspect: 
lmer.haspect1 <- lmer(height~aspect+(1|species)+ (0 + aspect|species), data=dan_ana)
summary(lmer.haspect1)
AIC(lmer.haspect1)
confint(lmer.haspect1)
predict(lmer.haspect1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.haspect1)
plot(dan_ana$aspect,dan_ana$height)

tval <- summary(lmer.haspect1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

#height 99~slope: 
lmer.hslope1 <- lmer(height~slope+(1|species)+ (0 + slope|species), data=dan_ana)
summary(lmer.hslope1)
AIC(lmer.hslope1)
confint(lmer.hslope1)
predict(lmer.hslope1, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lmer.hslope1)
plot(dan_ana$slope,dan_ana$height)

tval <- summary(lmer.hslope1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29369-1)

#height 99~TWI: 
lmer.htwi1 <- lmer(height~twi+(1|species)+ (0 + twi|species), data=dan_ana)
summary(lmer.htwi1)
AIC(lmer.htwi1)
confint(lmer.htwi1)
predict(lmer.htwi1, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lmer.htwi1)
plot(dan_ana$twi,dan_ana$height)

tval <- summary(lmer.htwi1)$coefficients[,3]
#pval
2*pt(-abs(tval),df=29072-1)

#---------------------------------------------------------------------------------------------#
#------------------------------------Linear Regression----------------------------------------#
#---------------------------------------------------------------------------------------------#
#height99~Elevation: 
lm.helev <- lm(height~elev, data=dan_ana)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_ana$elev,dan_ana$height)

#height~Elevation: 
lm.helevxy <- lm(height~elev+plot_x+plot_y, data=dan_ana)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_ana$elev,dan_ana$height)

#height~TPI: 
lm.htpi <- lm(height~tpi, data=dan_ana)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_ana$tpi,dan_ana$height)

#height~TPI: 
lm.htpixy <- lm(height~tpi+plot_x+plot_y, data=dan_ana)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_ana$tpi,dan_ana$height)

#height~aspect: 
lm.haspect <- lm(height~aspect, data=dan_ana)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_ana$aspect,dan_ana$height)

#height~aspect: 
lm.haspectxy <- lm(height~aspect+plot_x+plot_y, data=dan_ana)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_ana$aspect,dan_ana$height)

#height ~slope: 
lm.hslope <- lm(height~slope, data=dan_ana)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_ana$slope,dan_ana$height)

#height ~slope: 
lm.hslopexy <- lm(height~slope+plot_x+plot_y, data=dan_ana)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_ana$slope,dan_ana$height)

#height ~TWI: 
lm.htwi <- lm(height~twi, data=dan_ana)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_ana$twi,dan_ana$height)

#height ~TWI: 
lm.htwixy <- lm(height~twi+plot_x+plot_y, data=dan_ana)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_ana$twi,dan_ana$height)
