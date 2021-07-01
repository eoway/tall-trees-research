library(MASS)
library(ISLR)
library(here)

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

dan_stat <-dan_data

dan_stat$bitype <- ifelse(dan_stat$quad_type=="emrgnt", 1,0)
table(dan_stat$bitype)

#Species List--------------
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




#Lab Presentation
es <- lm(elev~slope, data=dan_stat)
summary(es)

hesxy <- lm(height~elev+slope+x_utm+y_utm, data=dan_stat)
summary(hesxy)

fit5 <- lm(height~elev+slope+x_utm+y_utm, data=dan_stat)
summary(fit5)
effect_plot(fit5, pred = elev, plot.points = TRUE, colors="red", x.label="Elevation")
effect_plot(fit5, pred = slope, plot.points = TRUE, colors="red", x.label="Slope")

#------------------------------------------------------------------------------------#
#-------------------------Surrounding Tree Analysis----------------------------------#
#------------------------------------------------------------------------------------#

#euclidean()?


#------------------------------------------------------------------------------------#
#-----------------------------Quadrat level analysis---------------------------------#
#------------------------------------------------------------------------------------#

#n_emrgnt is not working properly***************
dan_quad <- dan_data %>% group_by(quadrat,quad_type)  %>%  summarise(quad_x = mean(x_utm),
                                                        n_trees = n(), 
                                                        n_emrgnt = length(unique(tree_type[tree_type == 'emrgnt'])), 
                                                        prop_emrgnt = n_emrgnt/n_trees,            
                                                        quad_y = mean(y_utm),
                                                        slope = mean(slope),
                                                        aspect = mean(aspect),
                                                        tpi = mean(tpi),
                                                        elev = mean(elev),
                                                        twi = mean(twi),
                                                        height99 = quantile(height, probs = 0.99, na.rm = TRUE))

table(dan_quad$n_emrgnt)
dan_quad$bitype <- ifelse(dan_quad$quad_type=="emrgnt", 1,0)
table(dan_quad$bitype)

#Binned plots------
#individual level
summary(dan_stat$elev)
dan_stat$binelev <-  
  cut(dan_stat$elev, breaks=c(seq(215,335,by=10))
  )
summary(dan_stat$binelev)
dan_stat %>%
  ggplot(aes(x=binelev, y=log(height)))+
  geom_boxplot()
dan_stat %>%
  ggplot(aes(x=binelev, y=height))+
  geom_boxplot()

summary(dan_stat$slope)
dan_stat$binslope <-  
  cut(dan_stat$slope, breaks=c(seq(0,50,by=5), 66)
  )
summary(dan_stat$binslope)
dan_stat %>%
  ggplot(aes(x=binslope, y=log(height)))+
  geom_boxplot()
dan_stat %>%
  ggplot(aes(x=binslope, y=height))+
  geom_boxplot()

#quadrat level
summary(dan_quad$elev)
dan_quad$quadbinelev <-  
  cut(dan_quad$elev, breaks=c(215,seq(235,305,by=10), 321)
  )
summary(dan_quad$quadbinelev)
dan_quad %>%
  ggplot(aes(x=quadbinelev, y=log(height99)))+
  geom_boxplot()
dan_quad %>%
  ggplot(aes(x=quadbinelev, y=height99))+
  geom_boxplot()

summary(dan_quad$slope)
dan_quad$quadbinslope <-  
  cut(dan_quad$slope, breaks=c(0,seq(10,30,by=5), 40)
  )
summary(dan_quad$quadbinslope)
dan_quad %>%
  ggplot(aes(x=quadbinslope, y=log(height99)))+
  geom_boxplot()
dan_quad %>%
  ggplot(aes(x=quadbinslope, y=height99))+
  geom_boxplot()

#------------------------------------------------------------------------------------#
#-------------------------Multiple Logistic Regression-------------------------------#
#------------------------------------------------------------------------------------#
bielev3 <- glm(bitype~elev, data=dan_quad, family="binomial")
summary(bielev3)
plot(dan_quad$elev, dan_quad$bitype)

bitpi3 <- glm(bitype~tpi, data=dan_quad, family="binomial")
summary(bitpi3)
plot(dan_quad$tpi, dan_quad$bitype)

biaspect3 <- glm(bitype~aspect, data=dan_quad, family="binomial")
summary(biaspect3)
plot(dan_quad$aspect, dan_quad$bitype)

bislope3 <- glm(bitype~slope, data=dan_quad, family="binomial")
summary(bislope3)
plot(dan_quad$slope, dan_quad$bitype)

bitwi3 <- glm(bitype~twi, data=dan_quad, family="binomial")
summary(bitwi3)
plot(dan_quad$twi, dan_quad$bitype)

#with x_utm and y_utm coords
bielevxy <- glm(bitype~elev+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bielevxy)
plot(dan_quad$elev, dan_quad$bitype)

bitpixy <- glm(bitype~tpi+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bitpixy)
plot(dan_quad$tpi, dan_quad$bitype)

biaspectxy <- glm(bitype~aspect+quad_x+quad_y, data=dan_quad, family="binomial")
summary(biaspectxy)
plot(dan_quad$aspect, dan_quad$bitype)

bislopexy <- glm(bitype~slope+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bislopexy)
plot(dan_quad$slope, dan_quad$bitype)

bitwixy <- glm(bitype~twi+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bitwixy)
plot(dan_quad$twi, dan_quad$bitype)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height99~soil+aspect+elev+x_utm+y_utm, data=dan_quad)
summary(lm.heasxy)
AIC(lm.heasxy)

#height99~Elevation: 
lm.helev <- lm(height99~elev, data=dan_quad)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_quad$elev,dan_quad$height99)

#height99~Elevation: 
lm.helevxy <- lm(height99~elev+quad_x+quad_y, data=dan_quad)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_quad$elev,dan_quad$height99)

#height99~TPI: 
lm.htpi <- lm(height99~tpi, data=dan_quad)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_quad$tpi,dan_quad$height99)

#height99~TPI: 
lm.htpixy <- lm(height99~tpi+quad_x+quad_y, data=dan_quad)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_quad$tpi,dan_quad$height99)

#height99~aspect: 
lm.haspect <- lm(height99~aspect, data=dan_quad)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_quad$aspect,dan_quad$height99)

#height99~aspect: 
lm.haspectxy <- lm(height99~aspect+quad_x+quad_y, data=dan_quad)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_quad$aspect,dan_quad$height99)

#height99 ~slope: 
lm.hslope <- lm(height99~slope, data=dan_quad)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_quad$slope,dan_quad$height99)

#height99 ~slope: 
lm.hslopexy <- lm(height99~slope+quad_x+quad_y, data=dan_quad)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_quad$slope,dan_quad$height99)

#height99~TWI: 
lm.htwi <- lm(height99~twi, data=dan_quad)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_quad$twi,dan_quad$height9999)

#height99~TWI: 
lm.htwixy <- lm(height99~twi+quad_x+quad_y, data=dan_quad)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_quad$twi,dan_quad$height9999)

#------------------------------------------------------------------------------------#
#---------------------------Individual level analysis--------------------------------#
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
#-------------------------Multiple Logistic Regression-------------------------------#
#------------------------------------------------------------------------------------#

#Individual Level
bielev3 <- glm(bitree_type~elev, data=dan_stat, family="binomial")
summary(bielev3)
plot(dan_stat$elev, dan_stat$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dan_stat, family="binomial")
summary(bitpi3)
plot(dan_stat$tpi, dan_stat$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dan_stat, family="binomial")
summary(biaspect3)
plot(dan_stat$aspect, dan_stat$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dan_stat, family="binomial")
summary(bislope3)
plot(dan_stat$slope, dan_stat$bitree_type)

bitwi3 <- glm(bitree_type~twi, data=dan_stat, family="binomial")
summary(bitwi3)
plot(dan_stat$twi, dan_stat$bitree_type)

bisoil <- glm(bitree_type~soil, data=dan_stat, family="binomial")
summary(bisoil)

biastwi <- glm(bitree_type~twi+aspect, data=dan_stat, family="binomial")
summary(biastwi)

biastwixy <- glm(bitree_type~twi+aspect+x_utm+y_utm, data=dan_stat, family="binomial")
summary(biastwixy)
plot(dan_stat$twi, dan_stat$bitree_type )

#with x_utm and y_utm coords
bielevxy <- glm(bitree_type~elev+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bielevxy)
plot(dan_stat$elev, dan_stat$bitree_type)

bitpixy <- glm(bitree_type~tpi+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bitpixy)
plot(dan_stat$tpi, dan_stat$bitree_type)

biaspectxy <- glm(bitree_type~aspect+x_utm+y_utm, data=dan_stat, family="binomial")
summary(biaspectxy)
plot(dan_stat$aspect, dan_stat$bitree_type)

bislopexy <- glm(bitree_type~slope+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bislopexy)
plot(dan_stat$slope, dan_stat$bitree_type)

bitwixy <- glm(bitree_type~twi+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bitwixy)
plot(dan_stat$twi, dan_stat$bitree_type)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height~soil+aspect+elev+x_utm+y_utm, data=dan_stat)
summary(lm.heasxy)
AIC(lm.heasxy)

#Height~Elevation: 
lm.helev <- lm(height~elev, data=dan_stat)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_stat$elev,dan_stat$height)

#Height~Elevation: 
lm.helevxy <- lm(height~elev+x_utm+y_utm, data=dan_stat)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_stat$elev,dan_stat$height)

#Height~TPI: 
lm.htpi <- lm(height~tpi, data=dan_stat)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height)

#Height~TPI: 
lm.htpixy <- lm(height~tpi+x_utm+y_utm, data=dan_stat)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height)

#Height~aspect: 
lm.haspect <- lm(height~aspect, data=dan_stat)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height)

#Height~aspect: 
lm.haspectxy <- lm(height~aspect+x_utm+y_utm, data=dan_stat)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height)

#Height ~slope: 
lm.hslope <- lm(height~slope, data=dan_stat)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height)

#Height ~slope: 
lm.hslopexy <- lm(height~slope+x_utm+y_utm, data=dan_stat)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height)

#Height~TWI: 
lm.htwi <- lm(height~twi, data=dan_stat)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_stat$twi,dan_stat$height99)

#Height~TWI: 
lm.htwixy <- lm(height~twi+x_utm+y_utm, data=dan_stat)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_stat$twi,dan_stat$height99)

#------------------------------------------------------------------------------------#
#----------------------------Individual Level Analysis-------------------------------#
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
#-------------------------Multiple Logistic Regression-------------------------------#
#------------------------------------------------------------------------------------#
bielev3 <- glm(bitree_type~elev, data=dan_stat, family="binomial")
summary(bielev3)
plot(dan_stat$elev, dan_stat$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dan_stat, family="binomial")
summary(bitpi3)
plot(dan_stat$tpi, dan_stat$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dan_stat, family="binomial")
summary(biaspect3)
plot(dan_stat$aspect, dan_stat$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dan_stat, family="binomial")
summary(bislope3)
plot(dan_stat$slope, dan_stat$bitree_type)

bitwi3 <- glm(bitree_type~twi, data=dan_stat, family="binomial")
summary(bitwi3)
plot(dan_stat$twi, dan_stat$bitree_type)

bisoil <- glm(bitree_type~soil, data=dan_stat, family="binomial")
summary(bisoil)

biastwi <- glm(bitree_type~twi+aspect, data=dan_stat, family="binomial")
summary(biastwi)

biastwixy <- glm(bitree_type~twi+aspect+x_utm+y_utm, data=dan_stat, family="binomial")
summary(biastwixy)
plot(dan_stat$twi, dan_stat$bitree_type )

#with x_utm and y_utm coords
bielevxy <- glm(bitree_type~elev+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bielevxy)
plot(dan_stat$elev, dan_stat$bitree_type)

bitpixy <- glm(bitree_type~tpi+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bitpixy)
plot(dan_stat$tpi, dan_stat$bitree_type)

biaspectxy <- glm(bitree_type~aspect+x_utm+y_utm, data=dan_stat, family="binomial")
summary(biaspectxy)
plot(dan_stat$aspect, dan_stat$bitree_type)

bislopexy <- glm(bitree_type~slope+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bislopexy)
plot(dan_stat$slope, dan_stat$bitree_type)

bitwixy <- glm(bitree_type~twi+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bitwixy)
plot(dan_stat$twi, dan_stat$bitree_type)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height99~soil+aspect+elev+x_utm+y_utm, data=dan_stat)
summary(lm.heasxy)
AIC(lm.heasxy)

#height99~Elevation: 
lm.helev <- lm(height99~elev, data=dan_stat)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_stat$elev,dan_stat$height99)

#height99~Elevation: 
lm.helevxy <- lm(height99~elev+x_utm+y_utm, data=dan_stat)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_stat$elev,dan_stat$height99)

#height99~TPI: 
lm.htpi <- lm(height99~tpi, data=dan_stat)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height99)

#height99~TPI: 
lm.htpixy <- lm(height99~tpi+x_utm+y_utm, data=dan_stat)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height99)

#height99~aspect: 
lm.haspect <- lm(height99~aspect, data=dan_stat)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height99)

#height99~aspect: 
lm.haspectxy <- lm(height99~aspect+x_utm+y_utm, data=dan_stat)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height99)

#height99 ~slope: 
lm.hslope <- lm(height99~slope, data=dan_stat)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height99)

#height99 ~slope: 
lm.hslopexy <- lm(height99~slope+x_utm+y_utm, data=dan_stat)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height99)

#height99~TWI: 
lm.htwi <- lm(height99~twi, data=dan_stat)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_stat$twi,dan_stat$height9999)

#height99~TWI: 
lm.htwixy <- lm(height99~twi+x_utm+y_utm, data=dan_stat)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_stat$twi,dan_stat$height9999)

#------------------------------------------------------------------------------------#
#---------------------------Individual level analysis--------------------------------#
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
#-------------------------Multiple Logistic Regression-------------------------------#
#------------------------------------------------------------------------------------#

#Individual Level
bielev3 <- glm(bitree_type~elev, data=dan_stat, family="binomial")
summary(bielev3)
plot(dan_stat$elev, dan_stat$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dan_stat, family="binomial")
summary(bitpi3)
plot(dan_stat$tpi, dan_stat$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dan_stat, family="binomial")
summary(biaspect3)
plot(dan_stat$aspect, dan_stat$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dan_stat, family="binomial")
summary(bislope3)
plot(dan_stat$slope, dan_stat$bitree_type)

bitwi3 <- glm(bitree_type~twi, data=dan_stat, family="binomial")
summary(bitwi3)
plot(dan_stat$twi, dan_stat$bitree_type)

bisoil <- glm(bitree_type~soil, data=dan_stat, family="binomial")
summary(bisoil)

biastwi <- glm(bitree_type~twi+aspect, data=dan_stat, family="binomial")
summary(biastwi)

biastwixy <- glm(bitree_type~twi+aspect+x_utm+y_utm, data=dan_stat, family="binomial")
summary(biastwixy)
plot(dan_stat$twi, dan_stat$bitree_type )

#with x_utm and y_utm coords
bielevxy <- glm(bitree_type~elev+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bielevxy)
plot(dan_stat$elev, dan_stat$bitree_type)

bitpixy <- glm(bitree_type~tpi+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bitpixy)
plot(dan_stat$tpi, dan_stat$bitree_type)

biaspectxy <- glm(bitree_type~aspect+x_utm+y_utm, data=dan_stat, family="binomial")
summary(biaspectxy)
plot(dan_stat$aspect, dan_stat$bitree_type)

bislopexy <- glm(bitree_type~slope+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bislopexy)
plot(dan_stat$slope, dan_stat$bitree_type)

bitwixy <- glm(bitree_type~twi+x_utm+y_utm, data=dan_stat, family="binomial")
summary(bitwixy)
plot(dan_stat$twi, dan_stat$bitree_type)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height~soil+aspect+elev+x_utm+y_utm, data=dan_stat)
summary(lm.heasxy)
AIC(lm.heasxy)

#Height~Elevation: 
lm.helev <- lm(height~elev, data=dan_stat)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_stat$elev,dan_stat$height)

#Height~Elevation: 
lm.helevxy <- lm(height~elev+x_utm+y_utm, data=dan_stat)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_stat$elev,dan_stat$height)

#Height~TPI: 
lm.htpi <- lm(height~tpi, data=dan_stat)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height)

#Height~TPI: 
lm.htpixy <- lm(height~tpi+x_utm+y_utm, data=dan_stat)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_stat$tpi,dan_stat$height)

#Height~aspect: 
lm.haspect <- lm(height~aspect, data=dan_stat)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height)

#Height~aspect: 
lm.haspectxy <- lm(height~aspect+x_utm+y_utm, data=dan_stat)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_stat$aspect,dan_stat$height)

#Height ~slope: 
lm.hslope <- lm(height~slope, data=dan_stat)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height)

#Height ~slope: 
lm.hslopexy <- lm(height~slope+x_utm+y_utm, data=dan_stat)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_stat$slope,dan_stat$height)

#Height~TWI: 
lm.htwi <- lm(height~twi, data=dan_stat)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_stat$twi,dan_stat$height99)

#Height~TWI: 
lm.htwixy <- lm(height~twi+x_utm+y_utm, data=dan_stat)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_stat$twi,dan_stat$height99)
