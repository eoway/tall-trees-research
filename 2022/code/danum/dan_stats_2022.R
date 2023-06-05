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

# Load data
dan_data <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_2022.csv")

# add soils data
clay <- raster("~/Desktop/danum_soils/clay.tif")

cec <- raster("~/Desktop/danum_soils/cec.tif")

nitro <- raster("~/Desktop/danum_soils/nitro.tif")

sand <- raster("~/Desktop/danum_soils/sand.tif")

ph <- raster("~/Desktop/danum_soils/ph.tif")

soc <- raster("~/Desktop/danum_soils/soc.tif")


# convert dan_data into sp, then extract
Danum_TWI <- raster("~/Desktop/Research/HCRP/dan_dat/Danum_TWI.tif")
crs(Danum_TWI)
coords<- dan_data[,c("x_utm","y_utm")]
dan_proj <- crs(Danum_TWI)
spatialdan <- SpatialPointsDataFrame(coords=coords,
                                     data=dan_data,
                                     proj4string=dan_proj)


# extract soils
dan_data$clay <- raster::extract(clay,spatialdan)
dan_data$cec <- raster::extract(cec,spatialdan)
dan_data$nitro <- raster::extract(nitro,spatialdan)
dan_data$sand <- raster::extract(sand,spatialdan)
dan_data$ph <- raster::extract(ph,spatialdan)
dan_data$soc <- raster::extract(soc,spatialdan)

# export df
#write_csv(dan_data, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_2022_soils.csv")
dan_data <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_2022_soils.csv")

# Check DBHs- should all be >10
summary(dan_data)

# Make emergent cutoff
dbh99 <- 95 #from heights.r

# Check alive/dead 
table(dan_data$DFstatus)

# Label trees as emergent/non-emergent
dan_data$tree_type <- ifelse(dan_data$dbh>=dbh99, "emrgnt", "nonemrgnt")
dan_data$bitree_type <- ifelse(dan_data$dbh>=dbh99, 1, 0)

# check labeling- 324 emergents
table(dan_data$tree_type)
table(dan_data$bitree_type)

# Merge quadrat data for labeling
dan_label <- dan_data %>% group_by(tree_type,quadrat)  %>%  dplyr::summarize()

# check tree types
table(dan_label$tree_type)

# Create list of quadrats with emergents
emergents <- filter(dan_label, tree_type=="emrgnt")
emergentquad <- unique(emergents$quadrat)
summary(emergentquad)

# Add labels
dan_data$quad_type <- ifelse(dan_data$quadrat %in% emergentquad, "emrgnt", "nonemrgnt")
dan_data$bitype <- ifelse(dan_data$quad_type=="emrgnt", 1,0)

# check
table(dan_data$bitype)
table(dan_data$quad_type)


#-------------------------------------------------------------------------------#
# ------------------------------------Species List------------------------------
# -----------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
# Create a list of emergent species
emerg <- filter(dan_data, tree_type=="emrgnt")
emerg$specgen <- paste(emerg$genus, emerg$species)
danspecies <- unique(emerg$specgen)
length(danspecies)

# Label species as emergent/nonemergent
dan_data$specgen <- paste(dan_data$genus, dan_data$species)
dan_data$species_type <- ifelse(dan_data$specgen %in% danspecies, "emrgntsp", "nonemrgnt")
table(dan_data$species_type)

# Find total number of species
all_species <- unique(dan_data$specgen)
length(all_species)

# There are 31 species in Danum that have the potential to become emergent out of 467 species

#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#--------------------------------Lab Presentation------------------------------
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
es <- lm(elev~slope, data=dan_data)
summary(es)

hesxy <- lm(height~elev+slope+x_utm+y_utm, data=dan_data)
summary(hesxy)

fit5 <- lm(height~elev+slope+x_utm+y_utm, data=dan_data)
summary(fit5)
effect_plot(fit5, pred = elev, plot.points = TRUE, colors="red", x.label="Elevation")
effect_plot(fit5, pred = slope, plot.points = TRUE, colors="red", x.label="Slope")

#-------------------------------------------------------------------------------#
# --------------------------------------Binned Plots-----------------------------
# ------------------------------------Individual level--------------------------#
#-------------------------------------------------------------------------------#
# Elevation
dan_data$binelev <-  
  cut(dan_data$elev, breaks=c(seq(215,335,by=10))
  )
dan_data %>%
  ggplot(aes(x=binelev, y=log(height)))+
  geom_boxplot()
dan_data %>%
  ggplot(aes(x=binelev, y=height))+
  geom_boxplot()

# Slope
dan_data$binslope <-  
  cut(dan_data$slope, breaks=c(seq(0,50,by=5), 66)
  )
dan_data %>%
  ggplot(aes(x=binslope, y=log(height)))+
  geom_boxplot()
dan_data %>%
  ggplot(aes(x=binslope, y=height))+
  geom_boxplot()

#-------------------------------------------------------------------------------#
# ---------------------------Bivariate Linear Regressions-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height~elev+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)

lm.heasxy1 <- lm(height~elev+x_utm+y_utm, data=dan_data)
summary(lm.heasxy1)
AIC(lm.heasxy1)

lm.heasxy <- lm(height~cec+nitro+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)




lm.heasxy <- lm(height~elev+clay+nitro+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)

lm.heasxy <- lm(height~clay+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)




lm.heasxy <- lm(height99~aspect+elev+x_utm+y_utm, data=dan_data)



lm.check <- lm(aspect~elev, data=dan_data)
summary(lm.check)

lm.check <- lm(twi~elev, data=dan_data)
summary(lm.check)


lm.check <- lm(cec~elev, data=dan_data)
summary(lm.check)

lm.check <- lm(elev~clay, data=dan_data)
summary(lm.check)

lm.check <- lm(elev~soc, data=dan_data)
summary(lm.check)

lm.check <- lm(cec~soc, data=dan_data)
summary(lm.check)

lm.check <- lm(clay~soc, data=dan_data)
summary(lm.check)

lm.check <- lm(nitro~soc, data=dan_data)
summary(lm.check)

lm.check <- lm(nitro~cec, data=dan_data)
summary(lm.check)

lm.check <- lm(nitro~clay, data=dan_data)
summary(lm.check)












#Height~cec: 
lm.hcec <- lm(height~cec, data=dan_data)
summary(lm.hcec)

r2_cec <- summary(lm.hcec)$r.squared
coeff_cec <- lm.hcec$coefficients[2]
p_cec <- summary(lm.hcec)$coefficients[2,4]
aic_cec <- AIC(lm.hcec)

plot(dan_data$cec,dan_data$height)

#Height~sand: 
lm.hsand <- lm(height~sand, data=dan_data)
summary(lm.hsand)

r2_sand <- summary(lm.hsand)$r.squared
coeff_sand <- lm.hsand$coefficients[2]
p_sand <- summary(lm.hsand)$coefficients[2,4]
aic_sand <- AIC(lm.hsand)


plot(dan_data$sand,dan_data$height)

#Height~clay: 
lm.hclay <- lm(height~clay, data=dan_data)
summary(lm.hclay)

r2_clay <- summary(lm.hclay)$r.squared
coeff_clay <- lm.hclay$coefficients[2]
p_clay <- summary(lm.hclay)$coefficients[2,4]
aic_clay <- AIC(lm.hclay)


plot(lm.hclay) %>%
  abline()
plot(dan_data$clay,dan_data$height)

#Height~soc: 
lm.hsoc <- lm(height~soc, data=dan_data)

r2_soc <- summary(lm.hsoc)$r.squared
coeff_soc <- lm.hsoc$coefficients[2]
p_soc <- summary(lm.hsoc)$coefficients[2,4]
aic_soc <- AIC(lm.hsoc)


plot(lm.hsoc) %>%
  abline()
plot(dan_data$soc,dan_data$height)

#Height~nitro: 
lm.hnitro <- lm(height~nitro, data=dan_data)

r2_nitro <- summary(lm.hnitro)$r.squared
coeff_nitro <- lm.hnitro$coefficients[2]
p_nitro <- summary(lm.hnitro)$coefficients[2,4]
aic_nitro <- AIC(lm.hnitro)


plot(lm.hnitro) %>%
  abline()
plot(dan_data$nitro,dan_data$height)


#Height~Elevation: 
lm.helev <- lm(height~elev, data=dan_data)

r2_elev <- summary(lm.helev)$r.squared
coeff_elev <- lm.helev$coefficients[2]
p_elev <- summary(lm.helev)$coefficients[2,4]
aic_elev <- AIC(lm.helev)

plot(lm.helev) %>%
  abline()
plot(dan_data$elev,dan_data$height)


#Height~TPI: 
lm.htpi <- lm(height~tpi, data=dan_data)

r2_tpi <- summary(lm.htpi)$r.squared
coeff_tpi <- lm.htpi$coefficients[2]
p_tpi <- summary(lm.htpi)$coefficients[2,4]
aic_tpi <- AIC(lm.htpi)

plot(lm.htpi) %>%
  abline ()
plot(dan_data$tpi,dan_data$height)

#Height~aspect: 
lm.haspect <- lm(height~aspect, data=dan_data)

r2_aspect <- summary(lm.haspect)$r.squared
coeff_aspect <- lm.haspect$coefficients[2]
p_aspect <- summary(lm.haspect)$coefficients[2,4]
aic_aspect <- AIC(lm.haspect)

plot(lm.haspect) %>%
  abline ()
plot(dan_data$aspect,dan_data$height)

#Height ~slope: 
lm.hslope <- lm(height~slope, data=dan_data)

r2_slope <- summary(lm.hslope)$r.squared
coeff_slope <- lm.hslope$coefficients[2]
p_slope <- summary(lm.hslope)$coefficients[2,4]
aic_slope <- AIC(lm.hslope)

plot(lm.hslope) %>%
  abline ()
plot(dan_data$slope,dan_data$height)

#Height~TWI: 
lm.htwi <- lm(height~twi, data=dan_data)

r2_twi <- summary(lm.htwi)$r.squared
coeff_twi <- lm.htwi$coefficients[2]
p_twi <- summary(lm.htwi)$coefficients[2,4]
aic_twi <- AIC(lm.htwi)

plot(lm.htwi) %>%
  abline()
plot(dan_data$twi,dan_data$height)


#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi","twi","nitro","soc","clay", "sand", "cec")
stats <- c("r2_", "p_", "coeff_", "aic_")



df <- data.frame(matrix(ncol = length(stats), nrow = length(vars)))
colnames(df) <- stats
for (i in 1:length(vars)){
  # make dataframe for stats
  for (k in 1:length(stats)){
    var <- lapply(paste0(stats[k], vars[i]),get)
    df[i,k] <- var
    print(df[i,k])
    }
  df$var <- vars
}

write_csv(df, "Desktop/danum_results/linear_regressions")




#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions with locations-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#
#Height~Elevation: 
lm.helevxy <- lm(height~elev+x_utm+y_utm, data=dan_data)
summary(lm.helevxy)

r2_elev <- summary(lm.helevxy)$r.squared
coeff_elev <- lm.helevxy$coefficients[2]
p_elev <- summary(lm.helevxy)$coefficients[2,4]
aic_elev <- AIC(lm.helevxy)

plot(lm.helevxy) %>%
  abline()
plot(dan_data$elev,dan_data$height)

#Height~TPI: 
lm.htpixy <- lm(height~tpi+x_utm+y_utm, data=dan_data)

r2_tpi <- summary(lm.htpixy)$r.squared
coeff_tpi <- lm.htpixy$coefficients[2]
p_tpi <- summary(lm.htpixy)$coefficients[2,4]
aic_tpi <- AIC(lm.htpixy)

plot(lm.htpixy) %>%
  abline ()
plot(dan_data$tpi,dan_data$height)

#Height~aspect: 
lm.haspectxy <- lm(height~aspect+x_utm+y_utm, data=dan_data)
summary(lm.haspectxy)

r2_aspect <- summary(lm.haspectxy)$r.squared
coeff_aspect <- lm.haspectxy$coefficients[2]
p_aspect <- summary(lm.haspectxy)$coefficients[2,4]
aic_aspect <- AIC(lm.haspectxy)

plot(lm.haspectxy) %>%
  abline ()
plot(dan_data$aspect,dan_data$height)

#Height ~slope: 
lm.hslopexy <- lm(height~slope+x_utm+y_utm, data=dan_data)

r2_slope <- summary(lm.hslopexy)$r.squared
coeff_slope <- lm.hslopexy$coefficients[2]
p_slope <- summary(lm.hslopexy)$coefficients[2,4]
aic_slope <- AIC(lm.hslopexy)

plot(lm.hslopexy) %>%
  abline ()
plot(dan_data$slope,dan_data$height)

#Height~TWI: 
lm.htwixy <- lm(height~twi+x_utm+y_utm, data=dan_data)
summary(lm.htwixy)

r2_twi <- summary(lm.htwixy)$r.squared
coeff_twi <- lm.htwixy$coefficients[2]
p_twi <- summary(lm.htwixy)$coefficients[2,4]
aic_twi <- AIC(lm.htwixy)

plot(lm.htwixy) %>%
  abline()
plot(dan_data$twi,dan_data$height99)


#Height~cec: 
lm.hcecxy <- lm(height~cec+x_utm+y_utm, data=dan_data)
summary(lm.hcec)

r2_cec <- summary(lm.hcecxy)$r.squared
coeff_cec <- lm.hcecxy$coefficients[2]
p_cec <- summary(lm.hcecxy)$coefficients[2,4]
aic_cec <- AIC(lm.hcecxy)

plot(lm.hcec) %>%
  abline()
plot(dan_data$cec,dan_data$height)

#Height~sand: 
lm.hsandxy <- lm(height~sand+x_utm+y_utm, data=dan_data)
summary(lm.hsand)

r2_sand <- summary(lm.hsandxy)$r.squared
coeff_sand <- lm.hsandxy$coefficients[2]
p_sand <- summary(lm.hsandxy)$coefficients[2,4]
aic_sand <- AIC(lm.hsandxy)

plot(lm.hsand) %>%
  abline()
plot(dan_data$sand,dan_data$height)

#Height~clay: 
lm.hclayxy <- lm(height~clay+x_utm+y_utm, data=dan_data)
summary(lm.hclay)

r2_clay <- summary(lm.hclayxy)$r.squared
coeff_clay <- lm.hclayxy$coefficients[2]
p_clay <- summary(lm.hclayxy)$coefficients[2,4]
aic_clay <- AIC(lm.hclayxy)

plot(lm.hclay) %>%
  abline()
plot(dan_data$clay,dan_data$height)

#Height~soc: 
lm.hsocxy <- lm(height~soc+x_utm+y_utm, data=dan_data)
summary(lm.hsoc)

r2_soc <- summary(lm.hsocxy)$r.squared
coeff_soc <- lm.hsocxy$coefficients[2]
p_soc <- summary(lm.hsocxy)$coefficients[2,4]
aic_soc <- AIC(lm.hsocxy)

plot(lm.hsoc) %>%
  abline()
plot(dan_data$soc,dan_data$height)

#Height~nitro: 
lm.hnitroxy <- lm(height~nitro+x_utm+y_utm, data=dan_data)
summary(lm.hnitro)

r2_nitro <- summary(lm.hnitroxy)$r.squared
coeff_nitro <- lm.hnitroxy$coefficients[2]
p_nitro <- summary(lm.hnitroxy)$coefficients[2,4]
aic_nitro <- AIC(lm.hnitroxy)

plot(lm.hnitro) %>%
  abline()
plot(dan_data$nitro,dan_data$height)

#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi","twi","nitro","soc","clay", "sand", "cec")
stats <- c("r2_", "p_", "coeff_", "aic_")

df <- data.frame(matrix(ncol = length(stats), nrow = length(vars)))
colnames(df) <- stats
for (i in 1:length(vars)){
  # make dataframe for stats
  for (k in 1:length(stats)){
    var <- lapply(paste0(stats[k], vars[i]),get)
    df[i,k] <- var
    print(df[i,k])
  }
  df$var <- vars
}

write_csv(df, "Desktop/danum_results/linear_regressions_withxy")



#------------------------------------------------------------------------------------#
#-----------------------------Quadrat level analysis---------------------------------#
#------------------------------------------------------------------------------------#
dan_quad <- dan_data %>% group_by(quadrat,quad_type)  %>%  dplyr::summarise(quad_x = mean(x_utm),
                                                                            n_trees = n(), 
                                                                            n_emrgnt = length(unique(tree_type[tree_type == 'emrgnt'])), 
                                                                            prop_emrgnt = n_emrgnt/n_trees,            
                                                                            quad_y = mean(y_utm),
                                                                            slope = mean(slope),
                                                                            aspect = mean(aspect),
                                                                            tpi = mean(tpi),
                                                                            elev = mean(elev),
                                                                            twi = mean(twi),
                                                                            cec = mean(cec),
                                                                            clay = mean(clay),
                                                                            soc = mean(soc),
                                                                            nitro = mean(nitro),
                                                                            sand = mean(sand),
                                                                            height99 = quantile(height, probs = 0.99, na.rm = TRUE))

table(dan_quad$n_emrgnt)
dan_quad$bitype <- ifelse(dan_quad$quad_type=="emrgnt", 1,0)
table(dan_quad$bitype)

#Binned plots------
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
#bitype~cec: 
glm.hcec <- glm(bitype~cec, data=dan_quad)

coeff_cec <- glm.hcec$coefficients[2]
p_cec <- summary(glm.hcec)$coefficients[2,4]
aic_cec <- AIC(glm.hcec)

plot(dan_quad$cec,dan_quad$bitype)

#bitype~sand: 
glm.hsand <- glm(bitype~sand, data=dan_quad)

coeff_sand <- glm.hsand$coefficients[2]
p_sand <- summary(glm.hsand)$coefficients[2,4]
aic_sand <- AIC(glm.hsand)

plot(dan_quad$sand,dan_quad$bitype)

#bitype~clay: 
glm.hclay <- glm(bitype~clay, data=dan_quad)

coeff_clay <- glm.hclay$coefficients[2]
p_clay <- summary(glm.hclay)$coefficients[2,4]
aic_clay <- AIC(glm.hclay)

plot(dan_quad$clay,dan_quad$bitype)

#bitype~soc: 
glm.hsoc <- glm(bitype~soc, data=dan_quad)

coeff_soc <- glm.hsoc$coefficients[2]
p_soc <- summary(glm.hsoc)$coefficients[2,4]
aic_soc <- AIC(glm.hsoc)

plot(dan_quad$soc,dan_quad$bitype)

#bitype~nitro: 
glm.hnitro <- glm(bitype~nitro, data=dan_quad)

coeff_nitro <- glm.hnitro$coefficients[2]
p_nitro <- summary(glm.hnitro)$coefficients[2,4]
aic_nitro <- AIC(glm.hnitro)

plot(dan_quad$nitro,dan_quad$bitype)



bielev3 <- glm(bitype~elev+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bielev3)
plot(dan_quad$elev, dan_quad$bitype)

bitpi3 <- glm(bitype~tpi+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bitpi3)
plot(dan_quad$tpi, dan_quad$bitype)

biaspect3 <- glm(bitype~aspect+quad_x+quad_y, data=dan_quad, family="binomial")
summary(biaspect3)
plot(dan_quad$aspect, dan_quad$bitype)

bislope3 <- glm(bitype~slope+quad_x+quad_y, data=dan_quad, family="binomial")
summary(bislope3)
plot(dan_quad$slope, dan_quad$bitype)

bitwi3 <- glm(bitype~twi, data=dan_quad, family="binomial")
summary(bitwi3)
plot(dan_quad$twi, dan_quad$bitype)

#with x_utm and y_utm coords

#bitype~cec: 
glm.hcec <- glm(bitype~cec+quad_x+quad_y, data=dan_quad)
summary(glm.hcec)
plot(dan_quad$cec,dan_quad$bitype)

#bitype~sand: 
glm.hsand <- glm(bitype~sand+quad_x+quad_y, data=dan_quad)
summary(glm.hsand)
plot(dan_quad$sand,dan_quad$bitype)

#bitype~clay: 
glm.hclay <- glm(bitype~clay+quad_x+quad_y, data=dan_quad)
summary(glm.hclay)
plot(dan_quad$clay,dan_quad$bitype)

#bitype~soc: 
glm.hsoc <- glm(bitype~soc+quad_x+quad_y, data=dan_quad)
summary(glm.hsoc)
plot(dan_quad$soc,dan_quad$bitype)

#bitype~nitro: 
glm.hnitro <- glm(bitype~nitro+quad_x+quad_y, data=dan_quad)
summary(glm.hnitro)
plot(dan_quad$nitro,dan_quad$bitype)

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
#--------------------------------Quadrat Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height99~aspect+elev+x_utm+y_utm, data=dan_quad)
summary(lm.heasxy)
AIC(lm.heasxy)

#height99~cec: 
lm.hcec <- lm(height99~cec, data=dan_quad)
summary(lm.hcec)
confint(lm.hcec)
predict(lm.hcec, data.frame(cec=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hcec) %>%
  abline()
plot(dan_quad$cec,dan_quad$height99)

#height99~sand: 
lm.hsand <- lm(height99~sand, data=dan_quad)
summary(lm.hsand)
confint(lm.hsand)
predict(lm.hsand, data.frame(sand=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsand) %>%
  abline()
plot(dan_quad$sand,dan_quad$height99)

#height99~clay: 
lm.hclay <- lm(height99~clay, data=dan_quad)
summary(lm.hclay)
confint(lm.hclay)
predict(lm.hclay, data.frame(clay=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hclay) %>%
  abline()
plot(dan_quad$clay,dan_quad$height99)

#height99~soc: 
lm.hsoc <- lm(height99~soc, data=dan_quad)
summary(lm.hsoc)
confint(lm.hsoc)
predict(lm.hsoc, data.frame(soc=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoc) %>%
  abline()
plot(dan_quad$soc,dan_quad$height99)

#height99~nitro: 
lm.hnitro <- lm(height99~nitro, data=dan_quad)
summary(lm.hnitro)
confint(lm.hnitro)
predict(lm.hnitro, data.frame(nitro=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hnitro) %>%
  abline()
plot(dan_quad$nitro,dan_quad$height99)


#height99~cec: 
lm.hcec <- lm(height99~cec+quad_x+quad_y, data=dan_quad)
summary(lm.hcec)
confint(lm.hcec)
predict(lm.hcec, data.frame(cec=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hcec) %>%
  abline()
plot(dan_quad$cec,dan_quad$height99)

#height99~sand: 
lm.hsand <- lm(height99~sand+quad_x+quad_y, data=dan_quad)
summary(lm.hsand)
confint(lm.hsand)
predict(lm.hsand, data.frame(sand=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsand) %>%
  abline()
plot(dan_quad$sand,dan_quad$height99)

#height99~clay: 
lm.hclay <- lm(height99~clay+quad_x+quad_y, data=dan_quad)
summary(lm.hclay)
confint(lm.hclay)
predict(lm.hclay, data.frame(clay=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hclay) %>%
  abline()
plot(dan_quad$clay,dan_quad$height99)

#height99~soc: 
lm.hsoc <- lm(height99~soc+quad_x+quad_y, data=dan_quad)
summary(lm.hsoc)
confint(lm.hsoc)
predict(lm.hsoc, data.frame(soc=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoc) %>%
  abline()
plot(dan_quad$soc,dan_quad$height99)

#height99~nitro: 
lm.hnitro <- lm(height99~nitro+quad_x+quad_y, data=dan_quad)
summary(lm.hnitro)
confint(lm.hnitro)
predict(lm.hnitro, data.frame(nitro=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hnitro) %>%
  abline()
plot(dan_quad$nitro,dan_quad$height99)



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

#bitree_type~cec: 
glm.hcec <- glm(bitree_type~cec, data=dan_data, family="binomial")

coeff_cec <- glm.hcec$coefficients[2]
p_cec <- summary(glm.hcec)$coefficients[2,4]
aic_cec <- AIC(glm.hcec)

plot(dan_data$cec,dan_data$bitree_type)

#bitree_type~sand: 
glm.hsand <- glm(bitree_type~sand, data=dan_data, family="binomial")

coeff_sand <- glm.hsand$coefficients[2]
p_sand <- summary(glm.hsand)$coefficients[2,4]
aic_sand <- AIC(glm.hsand)

plot(dan_data$sand,dan_data$bitree_type)

#bitree_type~clay: 
glm.hclay <- glm(bitree_type~clay, data=dan_data, family="binomial")

coeff_clay <- glm.hclay$coefficients[2]
p_clay <- summary(glm.hclay)$coefficients[2,4]
aic_clay <- AIC(glm.hclay)

plot(dan_data$clay,dan_data$bitree_type)

#bitree_type~soc: 
glm.hsoc <- glm(bitree_type~soc, data=dan_data, family="binomial")

coeff_soc <- glm.hsoc$coefficients[2]
p_soc <- summary(glm.hsoc)$coefficients[2,4]
aic_soc <- AIC(glm.hsoc)

plot(dan_data$soc,dan_data$bitree_type)

#bitree_type~nitro: 
glm.hnitro <- glm(bitree_type~nitro, data=dan_data, family="binomial")

coeff_nitro <- glm.hnitro$coefficients[2]
p_nitro <- summary(glm.hnitro)$coefficients[2,4]
aic_nitro <- AIC(glm.hnitro)

plot(dan_data$nitro,dan_data$bitree_type)



#Individual Level
glm.helev <- glm(bitree_type~elev, data=dan_data, family="binomial")

coeff_elev <- glm.helev$coefficients[2]
p_elev <- summary(glm.helev)$coefficients[2,4]
aic_elev <- AIC(glm.helev)

plot(dan_data$elev, dan_data$bitree_type)

glm.htpi <- glm(bitree_type~tpi, data=dan_data, family="binomial")

coeff_tpi <- glm.htpi$coefficients[2]
p_tpi <- summary(glm.htpi)$coefficients[2,4]
aic_tpi <- AIC(glm.htpi)

plot(dan_data$tpi, dan_data$bitree_type)

glm.haspect <- glm(bitree_type~aspect, data=dan_data, family="binomial")

coeff_aspect <- glm.haspect$coefficients[2]
p_aspect <- summary(glm.haspect)$coefficients[2,4]
aic_aspect <- AIC(glm.haspect)

plot(dan_data$aspect, dan_data$bitree_type)

glm.hslope <- glm(bitree_type~slope, data=dan_data, family="binomial")

coeff_slope <- glm.hslope$coefficients[2]
p_slope <- summary(glm.hslope)$coefficients[2,4]
aic_slope <- AIC(glm.hslope)

plot(dan_data$slope, dan_data$bitree_type)

glm.htwi <- glm(bitree_type~twi, data=dan_data, family="binomial")

coeff_twi <- glm.htwi$coefficients[2]
p_twi <- summary(glm.htwi)$coefficients[2,4]
aic_twi <- AIC(glm.htwi)

plot(dan_data$twi, dan_data$bitree_type)

#-------------------------------------------------------------------------------#
#----------------- Logistic Regressions RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi","twi","nitro","soc","clay", "sand", "cec")
stats <- c("p_", "coeff_", "aic_")



df <- data.frame(matrix(ncol = length(stats), nrow = length(vars)))
colnames(df) <- stats
for (i in 1:length(vars)){
  # make dataframe for stats
  for (k in 1:length(stats)){
    var <- lapply(paste0(stats[k], vars[i]),get)
    df[i,k] <- var
    print(df[i,k])
  }
  df$var <- vars
}

write_csv(df, "Desktop/danum_results/logistic_regressions")





biastwi <- glm(bitree_type~twi+aspect, data=dan_data, family="binomial")
summary(biastwi)

biastwixy <- glm(bitree_type~twi+aspect+x_utm+y_utm, data=dan_data, family="binomial")
summary(biastwixy)
plot(dan_data$twi, dan_data$bitree_type )

#with x_utm and y_utm coords
bielevxy <- glm(bitree_type~elev+x_utm+y_utm, data=dan_data, family="binomial")
summary(bielevxy)
plot(dan_data$elev, dan_data$bitree_type)

bitpixy <- glm(bitree_type~tpi+x_utm+y_utm, data=dan_data, family="binomial")
summary(bitpixy)
plot(dan_data$tpi, dan_data$bitree_type)

biaspectxy <- glm(bitree_type~aspect+x_utm+y_utm, data=dan_data, family="binomial")
summary(biaspectxy)
plot(dan_data$aspect, dan_data$bitree_type)

bislopexy <- glm(bitree_type~slope+x_utm+y_utm, data=dan_data, family="binomial")
summary(bislopexy)
plot(dan_data$slope, dan_data$bitree_type)

bitwixy <- glm(bitree_type~twi+x_utm+y_utm, data=dan_data, family="binomial")
summary(bitwixy)
plot(dan_data$twi, dan_data$bitree_type)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height~soil+aspect+elev+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)

#Height~cec: 
lm.hcec <- lm(height~cec, data=dan_data)
summary(lm.hcec)
confint(lm.hcec)
predict(lm.hcec, data.frame(cec=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hcec) %>%
  abline()
plot(dan_data$cec,dan_data$height)

#Height~sand: 
lm.hsand <- lm(height~sand, data=dan_data)
summary(lm.hsand)
confint(lm.hsand)
predict(lm.hsand, data.frame(sand=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsand) %>%
  abline()
plot(dan_data$sand,dan_data$height)

#Height~clay: 
lm.hclay <- lm(height~clay, data=dan_data)
summary(lm.hclay)
confint(lm.hclay)
predict(lm.hclay, data.frame(clay=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hclay) %>%
  abline()
plot(dan_data$clay,dan_data$height)

#Height~soc: 
lm.hsoc <- lm(height~soc, data=dan_data)
summary(lm.hsoc)
confint(lm.hsoc)
predict(lm.hsoc, data.frame(soc=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hsoc) %>%
  abline()
plot(dan_data$soc,dan_data$height)

#Height~nitro: 
lm.hnitro <- lm(height~nitro, data=dan_data)
summary(lm.hnitro)
confint(lm.hnitro)
predict(lm.hnitro, data.frame(nitro=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.hnitro) %>%
  abline()
plot(dan_data$nitro,dan_data$height)


#Height~Elevation: 
lm.helev <- lm(height~elev, data=dan_data)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_data$elev,dan_data$height)

#Height~Elevation: 
lm.helevxy <- lm(height~elev+x_utm+y_utm, data=dan_data)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_data$elev,dan_data$height)

#Height~TPI: 
lm.htpi <- lm(height~tpi, data=dan_data)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_data$tpi,dan_data$height)

#Height~TPI: 
lm.htpixy <- lm(height~tpi+x_utm+y_utm, data=dan_data)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_data$tpi,dan_data$height)

#Height~aspect: 
lm.haspect <- lm(height~aspect, data=dan_data)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_data$aspect,dan_data$height)

#Height~aspect: 
lm.haspectxy <- lm(height~aspect+x_utm+y_utm, data=dan_data)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_data$aspect,dan_data$height)

#Height ~slope: 
lm.hslope <- lm(height~slope, data=dan_data)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_data$slope,dan_data$height)

#Height ~slope: 
lm.hslopexy <- lm(height~slope+x_utm+y_utm, data=dan_data)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_data$slope,dan_data$height)

#Height~TWI: 
lm.htwi <- lm(height~twi, data=dan_data)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_data$twi,dan_data$height99)

#Height~TWI: 
lm.htwixy <- lm(height~twi+x_utm+y_utm, data=dan_data)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_data$twi,dan_data$height99)

#------------------------------------------------------------------------------------#
#----------------------------Individual Level Analysis-------------------------------#
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
#-------------------------Multiple Logistic Regression-------------------------------#
#------------------------------------------------------------------------------------#
bielev3 <- glm(bitree_type~elev, data=dan_data, family="binomial")
summary(bielev3)
plot(dan_data$elev, dan_data$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dan_data, family="binomial")
summary(bitpi3)
plot(dan_data$tpi, dan_data$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dan_data, family="binomial")
summary(biaspect3)
plot(dan_data$aspect, dan_data$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dan_data, family="binomial")
summary(bislope3)
plot(dan_data$slope, dan_data$bitree_type)

bitwi3 <- glm(bitree_type~twi, data=dan_data, family="binomial")
summary(bitwi3)
plot(dan_data$twi, dan_data$bitree_type)

bisoil <- glm(bitree_type~soil, data=dan_data, family="binomial")
summary(bisoil)

biastwi <- glm(bitree_type~twi+aspect, data=dan_data, family="binomial")
summary(biastwi)

biastwixy <- glm(bitree_type~twi+aspect+x_utm+y_utm, data=dan_data, family="binomial")
summary(biastwixy)
plot(dan_data$twi, dan_data$bitree_type )

#with x_utm and y_utm coords
bielevxy <- glm(bitree_type~elev+x_utm+y_utm, data=dan_data, family="binomial")
summary(bielevxy)
plot(dan_data$elev, dan_data$bitree_type)

bitpixy <- glm(bitree_type~tpi+x_utm+y_utm, data=dan_data, family="binomial")
summary(bitpixy)
plot(dan_data$tpi, dan_data$bitree_type)

biaspectxy <- glm(bitree_type~aspect+x_utm+y_utm, data=dan_data, family="binomial")
summary(biaspectxy)
plot(dan_data$aspect, dan_data$bitree_type)

bislopexy <- glm(bitree_type~slope+x_utm+y_utm, data=dan_data, family="binomial")
summary(bislopexy)
plot(dan_data$slope, dan_data$bitree_type)

bitwixy <- glm(bitree_type~twi+x_utm+y_utm, data=dan_data, family="binomial")
summary(bitwixy)
plot(dan_data$twi, dan_data$bitree_type)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height99~soil+aspect+elev+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)

#height99~Elevation: 
lm.helev <- lm(height99~elev, data=dan_data)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_data$elev,dan_data$height99)

#height99~Elevation: 
lm.helevxy <- lm(height99~elev+x_utm+y_utm, data=dan_data)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_data$elev,dan_data$height99)

#height99~TPI: 
lm.htpi <- lm(height99~tpi, data=dan_data)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_data$tpi,dan_data$height99)

#height99~TPI: 
lm.htpixy <- lm(height99~tpi+x_utm+y_utm, data=dan_data)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_data$tpi,dan_data$height99)

#height99~aspect: 
lm.haspect <- lm(height99~aspect, data=dan_data)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_data$aspect,dan_data$height99)

#height99~aspect: 
lm.haspectxy <- lm(height99~aspect+x_utm+y_utm, data=dan_data)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_data$aspect,dan_data$height99)

#height99 ~slope: 
lm.hslope <- lm(height99~slope, data=dan_data)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_data$slope,dan_data$height99)

#height99 ~slope: 
lm.hslopexy <- lm(height99~slope+x_utm+y_utm, data=dan_data)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_data$slope,dan_data$height99)

#height99~TWI: 
lm.htwi <- lm(height99~twi, data=dan_data)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_data$twi,dan_data$height9999)

#height99~TWI: 
lm.htwixy <- lm(height99~twi+x_utm+y_utm, data=dan_data)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_data$twi,dan_data$height9999)

#------------------------------------------------------------------------------------#
#---------------------------Individual level analysis--------------------------------#
#------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------#
#-------------------------Multiple Logistic Regression-------------------------------#
#------------------------------------------------------------------------------------#

#Individual Level
bielev3 <- glm(bitree_type~elev, data=dan_data, family="binomial")
summary(bielev3)
plot(dan_data$elev, dan_data$bitree_type)

bitpi3 <- glm(bitree_type~tpi, data=dan_data, family="binomial")
summary(bitpi3)
plot(dan_data$tpi, dan_data$bitree_type)

biaspect3 <- glm(bitree_type~aspect, data=dan_data, family="binomial")
summary(biaspect3)
plot(dan_data$aspect, dan_data$bitree_type)

bislope3 <- glm(bitree_type~slope, data=dan_data, family="binomial")
summary(bislope3)
plot(dan_data$slope, dan_data$bitree_type)

bitwi3 <- glm(bitree_type~twi, data=dan_data, family="binomial")
summary(bitwi3)
plot(dan_data$twi, dan_data$bitree_type)

bisoil <- glm(bitree_type~soil, data=dan_data, family="binomial")
summary(bisoil)

biastwi <- glm(bitree_type~twi+aspect, data=dan_data, family="binomial")
summary(biastwi)

biastwixy <- glm(bitree_type~twi+aspect+x_utm+y_utm, data=dan_data, family="binomial")
summary(biastwixy)
plot(dan_data$twi, dan_data$bitree_type )

#with x_utm and y_utm coords
bielevxy <- glm(bitree_type~elev+x_utm+y_utm, data=dan_data, family="binomial")
summary(bielevxy)
plot(dan_data$elev, dan_data$bitree_type)

bitpixy <- glm(bitree_type~tpi+x_utm+y_utm, data=dan_data, family="binomial")
summary(bitpixy)
plot(dan_data$tpi, dan_data$bitree_type)

biaspectxy <- glm(bitree_type~aspect+x_utm+y_utm, data=dan_data, family="binomial")
summary(biaspectxy)
plot(dan_data$aspect, dan_data$bitree_type)

bislopexy <- glm(bitree_type~slope+x_utm+y_utm, data=dan_data, family="binomial")
summary(bislopexy)
plot(dan_data$slope, dan_data$bitree_type)

bitwixy <- glm(bitree_type~twi+x_utm+y_utm, data=dan_data, family="binomial")
summary(bitwixy)
plot(dan_data$twi, dan_data$bitree_type)

#------------------------------------------------------------------------------------#
#--------------------------------Linear Regression-----------------------------------#
#------------------------------------------------------------------------------------#
#Working towards final model
lm.heasxy <- lm(height~soil+aspect+elev+x_utm+y_utm, data=dan_data)
summary(lm.heasxy)
AIC(lm.heasxy)

#Height~Elevation: 
lm.helev <- lm(height~elev, data=dan_data)
summary(lm.helev)
names(lm.helev)
confint(lm.helev)
predict(lm.helev, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helev) %>%
  abline()
plot(dan_data$elev,dan_data$height)

#Height~Elevation: 
lm.helevxy <- lm(height~elev+x_utm+y_utm, data=dan_data)
summary(lm.helevxy)
names(lm.helevxy)
confint(lm.helevxy)
predict(lm.helevxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.helevxy) %>%
  abline()
plot(dan_data$elev,dan_data$height)

#Height~TPI: 
lm.htpi <- lm(height~tpi, data=dan_data)
summary(lm.htpi)
names(lm.htpi)
confint(lm.htpi)
predict(lm.htpi, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpi) %>%
  abline ()
plot(dan_data$tpi,dan_data$height)

#Height~TPI: 
lm.htpixy <- lm(height~tpi+x_utm+y_utm, data=dan_data)
summary(lm.htpixy)
names(lm.htpixy)
confint(lm.htpixy)
predict(lm.htpixy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.htpixy) %>%
  abline ()
plot(dan_data$tpi,dan_data$height)

#Height~aspect: 
lm.haspect <- lm(height~aspect, data=dan_data)
summary(lm.haspect)
names(lm.haspect)
confint(lm.haspect)
predict(lm.haspect, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspect) %>%
  abline ()
plot(dan_data$aspect,dan_data$height)

#Height~aspect: 
lm.haspectxy <- lm(height~aspect+x_utm+y_utm, data=dan_data)
summary(lm.haspectxy)
names(lm.haspectxy)
confint(lm.haspectxy)
predict(lm.haspectxy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.haspectxy) %>%
  abline ()
plot(dan_data$aspect,dan_data$height)

#Height ~slope: 
lm.hslope <- lm(height~slope, data=dan_data)
summary(lm.hslope)
names(lm.hslope)
confint(lm.hslope)
predict(lm.hslope, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslope) %>%
  abline ()
plot(dan_data$slope,dan_data$height)

#Height ~slope: 
lm.hslopexy <- lm(height~slope+x_utm+y_utm, data=dan_data)
summary(lm.hslopexy)
names(lm.hslopexy)
confint(lm.hslopexy)
predict(lm.hslopexy, data.frame(elev=c(5,10,15)), 
        interval="confidence")
plot(lm.hslopexy) %>%
  abline ()
plot(dan_data$slope,dan_data$height)

#Height~TWI: 
lm.htwi <- lm(height~twi, data=dan_data)
summary(lm.htwi)
names(lm.htwi)
confint(lm.htwi)
predict(lm.htwi, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwi) %>%
  abline()
plot(dan_data$twi,dan_data$height99)

#Height~TWI: 
lm.htwixy <- lm(height~twi+x_utm+y_utm, data=dan_data)
summary(lm.htwixy)
names(lm.htwixy)
confint(lm.htwixy)
predict(lm.htwixy, data.frame(twi=c(5,10,15)), 
        interval="confidence")
par(mfrow=c(1,1))
plot(lm.htwixy) %>%
  abline()
plot(dan_data$twi,dan_data$height99)
