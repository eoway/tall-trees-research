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

# census data-------
dat <- read_table("~/Desktop/Africa_forestgeo/rabi_data_cleaned.txt")

# remove rows with no plot coords
dat <- subset(dat, PX <= 500)

# convert DBH to cm
dat$DBH <- dat$DBH/10

#subset to DBH >=10
dat <- subset(dat, DBH >=10)

# calculate heights
dbh2h_01 <- function(dbh,hgt_max,b1Ht,b2Ht){ # exclude hgt_ref here if using first dbh_crit eq.
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  #  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh)),
              exp(b1Ht + b2Ht * log(dbh_crit)))
  return(h)
}
# Central african parameters
b1Ht <- 1.1525
b2Ht <- 0.5547
hgt_max <- 100

dat$height <- dbh2h_01(dat$dbh, hgt_max, b1Ht, b2Ht)




## Load in Korup data and calculate heights for tall tree cutoff
load("Desktop/korup.stem2.rData")
summary(korup.stem2$dbh)

# for now, assume dbh is in mm
korup.stem2$dbh <- korup.stem2$dbh/10

# subset to dbh >= 10cm
korup <- subset(korup.stem2, dbh >=10)
summary(korup$dbh)

# calculate heights
korup$height <- dbh2h_01(korup$dbh, hgt_max, b1Ht, b2Ht)
summary(korup$height)

# change col name of DBH to dbh for bind
colnames(dat)[13] <- c("dbh")

# combine rabi and korup datasets and calculate cuttoff
all_africa <- append(korup$dbh, dat$dbh)

# calculate 99th percentile DBH 
quantile99dbh <- quantile(all_africa, .99) 

# 99th percentile dbh == 80.1
# this is the cutoff for tall trees in central africa!


# load elevation
load("~/Desktop/Africa_forestgeo/rabielev.rdata")
elev <- CTFSElev_rabi[['col']]
elev%>%
  ggplot(mapping=aes(x, y, col = elev))+
  geom_point(aes(x, y, col = elev))

# convert elev and plot x y to relatively random utm
elev$x <- elev$x+597870.96
elev$y <- elev$y+9787247.83

dat$PX <- dat$PX+597870.96
dat$PY <- dat$PY+9787247.83


# create a raster from elev data
elev_rast <- rasterFromXYZ(elev, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")
crs(elev_rast)

# calculate slope, aspect, and tpi from elev
# will need to convert x, y coords of elev to utm
# then project raster to utm
# then calculate
slope_aspect_tpi <- terrain(elev_rast, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
plot(slope_aspect_tpi$slope)

# create spdf
coords<- dat[,c("PX","PY")]
spatialdan <- SpatialPointsDataFrame(coords=coords,
                                     proj4string =crs(elev_rast),
                                     data = dat)
plot(spatialdan)

# extract slope, elevation, aspect, and tpi for each point
dat$slope <- raster::extract(slope_aspect_tpi$slope,spatialdan)
dat$aspect <- raster::extract(slope_aspect_tpi$aspect,spatialdan)
dat$tpi <- raster::extract(slope_aspect_tpi$tpi,spatialdan)
dat$elev <- raster::extract(elev_rast,spatialdan)

summary(dat$elev)

# export dataset
#write_csv(dat, "~/Desktop/Africa_forestgeo/rabi_data_w_enviro.csv")



dat <- read_csv("~/Desktop/Africa_forestgeo/rabi_data_w_enviro.csv")

# convert sp 1, 2, 3, 5, and 8 into unknown
dat$Mnemonic[dat$Mnemonic == "1"] <- "Unknown"
dat$Mnemonic[dat$Mnemonic == "2"] <- "Unknown"
dat$Mnemonic[dat$Mnemonic == "3"] <- "Unknown"
dat$Mnemonic[dat$Mnemonic == "5"] <- "Unknown"
dat$Mnemonic[dat$Mnemonic == "8"] <- "Unknown"

table(dat$Mnemonic)

write_csv(dat, "~/Desktop/Africa_forestgeo/rabi_data_w_enviro_UNKNOWNSP.csv")

# Correlation Checks
check <- lm(aspect~elev, data=dat)
summary(check)

#-------------------------------------------------------------------------------#
#-----------------Working towards Final Model----------------------
# ---------------------------------------------------------#
#-------------------------------------------------------------------------------#











#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# Run some linear regression
lm.helev <- lm(height~elev, data=dat)
r2_elev <- summary(lm.helev)$r.squared
coeff_elev <- lm.helev$coefficients[2]
p_elev <- summary(lm.helev)$coefficients[2,4]
aic_elev <- AIC(lm.helev)
plot(dat$elev, dat$height)

lm.hslope <- lm(height~slope, data=dat)
r2_slope <- summary(lm.hslope)$r.squared
coeff_slope <- lm.hslope$coefficients[2]
p_slope <- summary(lm.hslope)$coefficients[2,4]
aic_slope <- AIC(lm.hslope)
plot(dat$slope, dat$height)

lm.haspect <- lm(height~aspect, data=dat)
summary(lm.haspect)
r2_aspect <- summary(lm.haspect)$r.squared
coeff_aspect <- lm.haspect$coefficients[2]
p_aspect <- summary(lm.haspect)$coefficients[2,4]
aic_aspect <- AIC(lm.haspect)
plot(dat$aspect, dat$height)

lm.htpi <- lm(height~tpi, data=dat)
summary(lm.htpi)
r2_tpi <- summary(lm.htpi)$r.squared
coeff_tpi <- lm.htpi$coefficients[2]
p_tpi <- summary(lm.htpi)$coefficients[2,4]
aic_tpi <- AIC(lm.htpi)
plot(dat$tpi, dat$height)

#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi")
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

write_csv(df, "Desktop/rabi_results/linear_regressions.csv")


#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions with coords----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

lm.helev <- lm(height~elev+PX+PY, data=dat)
r2_elev <- summary(lm.helev)$r.squared
coeff_elev <- lm.helev$coefficients[2]
p_elev <- summary(lm.helev)$coefficients[2,4]

lm.hslope <- lm(height~slope+PX+PY, data=dat)
r2_slope <- summary(lm.hslope)$r.squared
coeff_slope <- lm.hslope$coefficients[2]
p_slope <- summary(lm.hslope)$coefficients[2,4]

lm.haspect <- lm(height~aspect+PX+PY, data=dat)
r2_aspect <- summary(lm.haspect)$r.squared
coeff_aspect <- lm.haspect$coefficients[2]
p_aspect <- summary(lm.haspect)$coefficients[2,4]
summary(lm.haspect)

lm.htpi <- lm(height~tpi+PX+PY, data=dat)
r2_tpi <- summary(lm.htpi)$r.squared
coeff_tpi <- lm.htpi$coefficients[2]
p_tpi <- summary(lm.htpi)$coefficients[2,4]
summary(lm.htpi)

#-------------------------------------------------------------------------------#
#-----------------Bivariate Linear Regressions RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi")
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

write_csv(df, "Desktop/rabi_results/linear_regressions_withxy.csv")


#-------------------------------------------------------------------------------#
#-----------------Bivariate Logistic Regressions-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#
dbh99 <- 80.1
dat$bitree_type <- ifelse(dat$dbh>=dbh99, 1, 0)


glm.helev <- glm(bitree_type~elev, data=dat, family="binomial")
coeff_elev <- glm.helev$coefficients[2]
p_elev <- summary(glm.helev)$coefficients[2,4]
aic_elev <- AIC(glm.helev)

glm.hslope <- glm(bitree_type~slope, data=dat, family="binomial")
coeff_slope <- glm.hslope$coefficients[2]
p_slope <- summary(glm.hslope)$coefficients[2,4]
aic_slope <- AIC(glm.hslope)

glm.haspect <- glm(bitree_type~aspect, data=dat, family="binomial")
summary(glm.haspect)
coeff_aspect <- glm.haspect$coefficients[2]
p_aspect <- summary(glm.haspect)$coefficients[2,4]
aic_aspect <- AIC(glm.haspect)

glm.htpi <- glm(bitree_type~tpi, data=dat, family="binomial")
summary(glm.htpi)
coeff_tpi <- glm.htpi$coefficients[2]
p_tpi <- summary(glm.htpi)$coefficients[2,4]
aic_tpi <- AIC(glm.htpi)




#-------------------------------------------------------------------------------#
#----------------- Logistic Regressions RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi")
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

write_csv(df, "Desktop/rabi_results/logistic_regressions.csv")


#-------------------------------------------------------------------------------#
#----------------- Logistic Regressions with coords-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#
glm.haspect <- glm(bitree_type~aspect+PX+PY, data=dat, family="binomial")
coeff_aspect <- glm.haspect$coefficients[2]
p_aspect <- summary(glm.haspect)$coefficients[2,4]
aic_aspect <- AIC(glm.haspect)

glm.htpi <- glm(bitree_type~tpi+PX+PY, data=dat, family="binomial")
summary(hesxy)
coeff_tpi <- glm.htpi$coefficients[2]
p_tpi <- summary(glm.htpi)$coefficients[2,4]
aic_tpi <- AIC(glm.htpi)

glm.helev <- glm(bitree_type~elev+PX+PY, data=dat, family="binomial")
summary(glm.helev)
coeff_elev <- glm.helev$coefficients[2]
p_elev <- summary(glm.helev)$coefficients[2,4]
aic_elev <- AIC(glm.helev)

glm.hslope <- glm(bitree_type~slope+PX+PY, data=dat, family="binomial")
summary(glm.hslope)
coeff_slope <- glm.hslope$coefficients[2]
p_slope <- summary(glm.hslope)$coefficients[2,4]
aic_slope <- AIC(glm.hslope)

#-------------------------------------------------------------------------------#
#----------------- Logistic Regressions w coords RESULTS TABLE-----------------------
# ------------------------------Individual tree level---------------------------#
#-------------------------------------------------------------------------------#

# make a dataframe with all stats results
vars <- c("elev","slope","aspect","tpi")
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

write_csv(df, "Desktop/rabi_results/logistic_regressions_w_coords.csv")



# check for corr between variables
# r2 < .5
hesxy <- lm(elev~slope, data=dat)
summary(hesxy)

