library(raster)
library(tidyverse)
library(rgdal) # ReadOGR

# GEDI
#dat <- read_csv("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_ALL.csv")
dat <- read_csv("/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_GEDI/SEAsia_trunc_processed/SEAsia00300.csv")

# Subset dat to only high powered shots
dat1 <- subset(dat, beam_type == "full_power")

# Load ecoregion shapefile
water <- readOGR("/n/holyscratch01/moorcroft_lab/nhegwood/ecoregions","ecoregions_biome_1")

# Convert dat into a shapefile to crop
coords<- dat[,c("Longitude","Latitude")]
proj <- crs(water)
spdf_dat <- SpatialPointsDataFrame(coords=coords,
                                   data=dat,
                                   proj4string=proj)

# Crop dat
dat_crop <- crop(spdf_dat, water)

# convert cropped data back to a df
dat_df <- as(spdf_dan, "data.frame")

# export df to csv
write.csv(dat_df, "~/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_cropped.csv")
