library(tidyverse)
library(here)
library(dplyr)
library(readxl)
library(raster)
library(rgdal)
library(sp)
library(here)
library(plyr)
library(rgeos)

# GEDI
# Read in shapefile
spdf_dat <- readOGR("/n/holyscratch01/moorcroft_lab/nhegwood/SEA_clipped","SEA_clipped")

# elevation
elev <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SRTM/elevdat.tif")

## Calculate slope, aspect, and tpi
slope_aspect_TPI <- terrain(elev, opt=c('slope', 'aspect', 'TPI'), unit='degrees')

# water
water <- readOGR("/n/holyscratch01/moorcroft_lab/nhegwood/Water/rivers_asia","rivers_asia_37331")

# soil
#clay05 <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/clay_0-5.tif")
#
#nitro05 <-raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/nitro_0-5.tif")
#
#sand05 <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/sand_0-5.tif")
#
#SOC05 <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_soils/SOC_0-5.tif")

# climate precip
jan_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_01")
feb_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
mar_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
apr_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
may_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
jun_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
jul_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
aug_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
sep_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
oct_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
nov_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")
dec_precip <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/precip_av/wc2.1_30s_prec_02")

# climate temp
jan_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_01")
feb_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
mar_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
apr_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
may_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
jun_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
jul_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
aug_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
sep_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
oct_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
nov_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")
dec_temp <- raster("/n/holyscratch01/moorcroft_lab/nhegwood/Climate/temp_av/wc2.1_30s_prec_02")

# create buffer spdf
# calculated degrees that = 11 m on the equator
dat_buff <- gBuffer(spdf_dat, width= 0.000099099, byid = TRUE) #EO: use the gBuffer function in rgeos instead, and set byid=T

# convert spdf to a dataframe to add data to
analysis <- as(spdf_dat, "data.frame")

# elev 
analysis$elev <- raster::extract(elev,dat_buff, method = "bilinear")

# slope 
analysis$slope <- raster::extract(slope_aspect_TPI$slope,dat_buff, method = "bilinear")

# aspect 
analysis$aspect <- raster::extract(slope_aspect_TPI$aspect,dat_buff, method = "bilinear")

# tpi 
analysis$tpi <- raster::extract(slope_aspect_TPI$tpi,dat_buff, method = "bilinear")

# distance to water
analysis$distance <- apply(gDistance(spdf_dat, water,byid=TRUE),2,min)

# precip 
analysis$jan_precip <- raster::extract(jan_precip,dat_buff, method = "bilinear")
analysis$feb_precip <- raster::extract(feb_precip,dat_buff, method = "bilinear")
analysis$mar_precip <- raster::extract(mar_precip,dat_buff, method = "bilinear")
analysis$apr_precip <- raster::extract(apr_precip,dat_buff, method = "bilinear")
analysis$may_precip <- raster::extract(may_precip,dat_buff, method = "bilinear")
analysis$jun_precip <- raster::extract(jun_precip,dat_buff, method = "bilinear")
analysis$jul_precip <- raster::extract(jul_precip,dat_buff, method = "bilinear")
analysis$aug_precip <- raster::extract(aug_precip,dat_buff, method = "bilinear")
analysis$sep_precip <- raster::extract(sep_precip,dat_buff, method = "bilinear")
analysis$oct_precip <- raster::extract(oct_precip,dat_buff, method = "bilinear")
analysis$nov_precip <- raster::extract(nov_precip,dat_buff, method = "bilinear")
analysis$dec_precip <- raster::extract(dec_precip,dat_buff, method = "bilinear")

# temp
analysis$jan_temp <- raster::extract(jan_temp,dat_buff, method = "bilinear")
analysis$feb_temp <- raster::extract(feb_temp,dat_buff, method = "bilinear")
analysis$mar_temp <- raster::extract(mar_temp,dat_buff, method = "bilinear")
analysis$apr_temp <- raster::extract(apr_temp,dat_buff, method = "bilinear")
analysis$may_temp <- raster::extract(may_temp,dat_buff, method = "bilinear")
analysis$jun_temp <- raster::extract(jun_temp,dat_buff, method = "bilinear")
analysis$jul_temp <- raster::extract(jul_temp,dat_buff, method = "bilinear")
analysis$aug_temp <- raster::extract(aug_temp,dat_buff, method = "bilinear")
analysis$sep_temp <- raster::extract(sep_temp,dat_buff, method = "bilinear")
analysis$oct_temp <- raster::extract(oct_temp,dat_buff, method = "bilinear")
analysis$nov_temp <- raster::extract(nov_temp,dat_buff, method = "bilinear")
analysis$dec_temp <- raster::extract(dec_temp,dat_buff, method = "bilinear")

# clay
#analysis$clay <- raster::extract(clay05,dat_buff, method = "bilinear")
#
## nitro05
#analysis$nitro <- raster::extract(nitro05,dat_buff, method = "bilinear")
#
## sand05
#analysis$sand <- raster::extract(sand05,dat_buff, method = "bilinear")
#
## SOC05
#analysis$SOC05 <- raster::extract(SOC05,dat_buff, method = "bilinear")



# export dataframe for analyses
write.csv(analysis, "/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_test.csv")