library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(sp)
library(raster)
require(XML)
require(rgdal)
require(raster)


setwd("G:/My Drive/Research")
heightdat <- raster("SEA_Regional_Datasets/Forest_height_2019_SASIA_subset.tif"); plot(heightdat)

#convert NAs
NAvalue(heightdat) <- 0
plot(heightdat)

heightdat2 <- heightdat
extent(heightdat2)
heightdat2@ncols
heightdat2@nrows
ext <- extent(99,100, 0, 1)
heightdat3 <- crop(heightdat2, ext)
plot(heightdat3)
extent(heightdat3)

heightshp = rasterToPolygons(heightdat3)
class(heightshp)
names(heightshp)
#plot(heightshp)

heightsamp <- spsample(heightshp, 100, "random")
plot(heightsamp)
shapefile(heightsamp, filename="heightsamp.shp")

heightsample <- readOGR("heightsamp", "heightsamp")
plot(heightsample)

#water data
waterdat <- readOGR("river_data/rivers_asia_37331","rivers_asia_37331")
plot(waterdat)

#calculate distance to nearest river
library(rgeos)
#heightsample$distance <- apply(gDistance(heightsample, waterdat,byid=TRUE),2,min)

#elev data
elevdat <- raster("G:/My Drive/Research/elevdat/elevdat.tif")
NAvalue(elevdat) <- -9999
plot(elevdat)

elevdat2 <- elevdat
extent(elevdat2)
elevdat2@ncols
elevdat2@nrows
ext <- extent(99,100, 0, 1)
elevdat3 <- crop(elevdat2, ext)
plot(elevdat3)
extent(elevdat3)

slope_aspect_TPI <- terrain(elevdat3, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
plot(slope_aspect_TPI)

#add elev, slope, aspect, and tpi to dataset
test <- raster::extract(elevdat3,heightsample)
head(test)
length(test)
dim(heightsample)
analysis <- as(heightsample, "data.frame")
analysis$elev <- raster::extract(elevdat3,heightsample)

temp <- raster::extract(slope_aspect_TPI$slope,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$slope <- raster::extract(slope_aspect_TPI$slope,heightsample)

temp <- raster::extract(slope_aspect_TPI$aspect,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$aspect <- raster::extract(slope_aspect_TPI$aspect,heightsample)

temp <- raster::extract(slope_aspect_TPI$tpi,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$tpi <- raster::extract(slope_aspect_TPI$tpi,heightsample)

#heights
temp <- raster::extract(heightdat3,heightsample)
summary(temp)
head(temp)
length(temp)
dim(heightsample)
analysis$height <- raster::extract(heightdat3,heightsample)

#export file without soil data for now
#issues: soils data not working, pretty sure distance is working incorrectly?
write.csv(analysis, ("G:/My Drive/Research/PRISE_data/analysisdat"))

#soil data
#----------------------------------------------------------------#
#------------------------Shapefile Code---------------------------
#----------------------------------------------------------------#
soilbd <- raster("SEA_Regional_Datasets/SoilGrids_BD_SEAsia.tif")
plot(soilbd)
soilsoc <- raster("SEA_Regional_Datasets/SoilGrids_SOC_SEAsia.tif")
plot(soilsoc)
crs(soilsoc)
soilsand <- raster("SEA_Regional_Datasets/SoilGrids_Sand_SEAsia.tif")
soilph <- raster("SEA_Regional_Datasets/SoilGrids_pH_SEAsia.tif")
soilnit <- raster("SEA_Regional_Datasets/SoilGrids_N_SEAsia.tif")
soilclay <- raster("SEA_Regional_Datasets/SoilGrids_Clay_SEAsia.tif")
soilcec <- raster("SEA_Regional_Datasets/SoilGrids_CEC_SEAsia.tif")

#extract data
#bulk density
temp <- raster::extract(soilbd,heightsample)
head(temp)
summary(temp)
length(temp)
dim(heightsample)
analysis$bulkdensity <- raster::extract(soilbd,heightsample)

#soc
temp <- raster::extract(soilsoc,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$soc <- raster::extract(soilsoc,heightsample)

#sand
temp <- raster::extract(soilsand,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$sand <- raster::extract(soilsand,heightsample)

#ph
temp <- raster::extract(soilph,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$ph <- raster::extract(soilph,heightsample)

#nitrogen
temp <- raster::extract(soilnit,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$nitrogen <- raster::extract(soilnit,heightsample)

#clay
temp <- raster::extract(soilclay,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$clay <- raster::extract(soilclay,heightsample)

#cec
temp <- raster::extract(soilcec,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$cec <- raster::extract(soilcec,heightsample)

#----------------------------------------------------------------#
#---------------------------WCS Code-----------------------------
#----------------------------------------------------------------#
#library(XML)
#library(rgdal)
#library(gdalUtils)
#
#
#voi = "nitrogen" # variable of interest
#depth = "5-15cm"
#quantile = "Q0.5"
#
#voi_layer = paste(voi,depth,quantile, sep="_") # layer of interest 
#
#wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
#wcs_service = "SERVICE=WCS"
#wcs_version = "VERSION=2.0.1" # This works for gdal >=2.3; "VERSION=1.1.1" works with gdal < 2.3.
#
#wcs_request = "DescribeCoverage" 
#
#wcs = paste(wcs_path, wcs_service, wcs_version, wcs_request, sep="&")
#
#l1 <- newXMLNode("WCS_GDAL")
#l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
#l1.l <- newXMLNode("CoverageName", voi_layer, parent=l1)
#
## Save to local disk
#xml.out = "./sg.xml"
#saveXML(l1, file = xml.out)
#
#gdalinfo("./sg.xml")
#
#wcs = paste(wcs_path, wcs_service, wcs_version, wcs_request, sep="&")
#
#bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine)
#igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
#
#wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3
#l1 <- newXMLNode("WCS_GDAL")
#l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
#l1.l <- newXMLNode("CoverageName", "nitrogen_5-15cm_Q0.5", parent=l1)
#
## Save to local disk
#xml.out = "./sg.xml"
#saveXML(l1, file = xml.out)
## Download raster as GeoTIFF (Warning: it can be large!)
#file.out <- './test.tif'
#
#gdal_translate(xml.out, file.out,
#               tr=c(250,250), projwin=bb,
#               projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES","GDAL_HTTP_UNSAFESSL=YES"),
#               verbose=TRUE
#)




