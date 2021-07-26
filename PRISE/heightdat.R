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

analysis <- read_csv("G:/My Drive/Research/PRISE_data/analysisdat")

#soil data
#----------------------------------------------------------------#
#------------------------Shapefile Code---------------------------
#----------------------------------------------------------------#
soilbd <- raster("soils/SoilGrids_BD_SEA-0000000000-0000000000.tif")
soilbd2 <- raster("soils/SoilGrids_BD_SEA-0000000000-0000018944.tif")
soilbd3 <- raster("soils/SoilGrids_BD_SEA-0000018944-0000000000.tif")
soilbd4 <- raster("soils/SoilGrids_BD_SEA-0000018944-0000018944.tif")
plot(soilbd)
plot(soilbd2)
plot(soilbd3)
plot(soilbd4)

soilSOC <- raster("soils/SoilGrids_SOC_SEA-0000000000-0000000000.tif")
soilSOC2 <- raster("soils/SoilGrids_SOC_SEA-0000000000-0000018944.tif")
soilSOC3 <- raster("soils/SoilGrids_SOC_SEA-0000018944-0000000000.tif")
soilSOC4 <- raster("soils/SoilGrids_SOC_SEA-0000018944-0000018944.tif")
plot(soilSOC)
plot(soilSOC2)
plot(soilSOC3)
plot(soilSOC4)

soilSand <- raster("soils/SoilGrids_Sand_SEA-0000000000-0000000000.tif")
soilSand2 <- raster("soils/SoilGrids_Sand_SEA-0000000000-0000018944.tif")
soilSand3 <- raster("soils/SoilGrids_Sand_SEA-0000018944-0000000000.tif")
soilSand4 <- raster("soils/SoilGrids_Sand_SEA-0000018944-0000018944.tif")
plot(soilSand)
plot(soilSand2)
plot(soilSand3)
plot(soilSand4)

soilpH <- raster("soils/SoilGrids_pH_SEA-0000000000-0000000000.tif")
soilpH2 <- raster("soils/SoilGrids_pH_SEA-0000000000-0000018944.tif")
soilpH3 <- raster("soils/SoilGrids_pH_SEA-0000018944-0000000000.tif")
soilpH4 <- raster("soils/SoilGrids_pH_SEA-0000018944-0000018944.tif")
plot(soilpH)
plot(soilpH2)
plot(soilpH3)
plot(soilpH4)

soilnit <- raster("soils/SoilGrids_N_SEA-0000000000-0000000000.tif")
soilnit2 <- raster("soils/SoilGrids_N_SEA-0000000000-0000018944.tif")
soilnit3 <- raster("soils/SoilGrids_N_SEA-0000018944-0000000000.tif")
soilnit4 <- raster("soils/SoilGrids_N_SEA-0000018944-0000018944.tif")
plot(soilnit)
plot(soilnit2)
plot(soilnit3)
plot(soilnit4)

soilclay <- raster("soils/SoilGrids_Clay_SEA-0000000000-0000000000.tif")
soilclay2 <- raster("soils/SoilGrids_Clay_SEA-0000000000-0000018944.tif")
soilclay3 <- raster("soils/SoilGrids_Clay_SEA-0000018944-0000000000.tif")
soilclay4 <- raster("soils/SoilGrids_Clay_SEA-0000018944-0000018944.tif")
plot(soilclay)
plot(soilclay2)
plot(soilclay3)
plot(soilclay4)

soilcec <- raster("soils/SoilGrids_CEC_SEA-0000000000-0000000000.tif")
soilcec2 <- raster("soils/SoilGrids_CEC_SEA-0000000000-0000018944.tif")
soilcec3 <- raster("soils/SoilGrids_CEC_SEA-0000018944-0000000000.tif")
soilcec4 <- raster("soils/SoilGrids_CEC_SEA-0000018944-0000018944.tif")
plot(soilcec)
plot(soilcec2)
plot(soilcec3)
plot(soilcec4)

#extract data
#bulk density
temp <- raster::extract(soilbd,heightsample)
head(temp)
summary(temp)
length(temp)
dim(heightsample)
analysis$bulkdensity <- raster::extract(soilbd,heightsample)

#soc
temp <- raster::extract(soilSOC,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$soc <- raster::extract(soilSOC,heightsample)

#sand
temp <- raster::extract(soilSand,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$sand <- raster::extract(soilSand,heightsample)

#ph
temp <- raster::extract(soilpH,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$ph <- raster::extract(soilpH,heightsample)

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

write.csv(analysis, ("G:/My Drive/Research/PRISE_data/analysisdat"))

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




