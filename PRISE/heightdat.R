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

#heightdat2 <- heightdat
#extent(heightdat2)
#heightdat2@ncols
#heightdat2@nrows
#ext <- extent(99,100, 0, 1)
#heightdat3 <- crop(heightdat2, ext)
#plot(heightdat3)
#extent(heightdat3)

#Sampling from a raster
sample <- sampleRandom(heightdat, 20000, sp=TRUE)
shapefile(sample, filename="sample.shp")
plot(sample)
summary(sample)

#Sample from a shape file
#heightshp = rasterToPolygons(heightdat)
#class(heightshp)
#names(heightshp)
##plot(heightshp)

#heightsamp <- spsample(heightshp, 100, "random")
#plot(heightsamp)
#shapefile(heightsamp, filename="heightsamp.shp")

#heightsample <- readOGR("heightsamp", "heightsamp")
#plot(heightsample)

heightsample <- readOGR("hsample","sample")
plot(heightsample)
#water data
waterdat <- readOGR("river_data/rivers_asia_37331","rivers_asia_37331")
plot(waterdat)
projection(waterdat)
crs(samp)
crs(waterdat)
#calculate distance to nearest river
library(rgeos)
heightsample$distance <- apply(gDistance(heightsample, waterdat,byid=TRUE),2,min)
summary(heightsample$distance)

crs(waterdat)
crs(heightsample)
#elev data
elevdat <- raster("G:/My Drive/Research/elevdat/elevdat.tif")
NAvalue(elevdat) <- -9999
plot(elevdat)
crs(elevdat)
#elevdat2 <- elevdat
#extent(elevdat2)
#elevdat2@ncols
#elevdat2@nrows
#ext <- extent(99,100, 0, 1)
#elevdat3 <- crop(elevdat2, ext)
#plot(elevdat3)
#extent(elevdat3)
#calculate slope, aspect, tpi from cropped data
#slope_aspect_TPI <- terrain(elevdat3, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
#plot(slope_aspect_TPI)

#calculate slope, aspect, tpi for whole dataset
slope_aspect_TPI <- terrain(elevdat, opt=c('slope', 'aspect', 'TPI'), unit='degrees')
writeRaster(slope_aspect_TPI, "slope_aspect_tpi_entire.tif", format="raster")

#Read in slope, aspect, tpi raster
slope_aspect_tpi <- raster("slope_aspect_tpi_entire.tif")
plot(slope_aspect_tpi)

#add elev, slope, aspect, and tpi to dataset
#test <- raster::extract(elevdat3,heightsample)
#head(test)
#length(test)
#dim(heightsample)
#analysis <- as(heightsample, "data.frame")
#analysis$elev <- raster::extract(elevdat3,heightsample)

test <- raster::extract(elevdat,heightsample)
head(test)
length(test)
dim(heightsample)
analysis <- as(heightsample, "data.frame")
analysis$elev <- raster::extract(elevdat,heightsample)


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
temp <- raster::extract(heightdat,heightsample)
summary(temp)
head(temp)
length(temp)
dim(heightsample)
analysis$height <- raster::extract(heightdat,heightsample)

#export file without soil data for now
#issues: soils data not working, pretty sure distance is working incorrectly?
write.csv(analysis, ("G:/My Drive/Research/PRISE_data/fullanalysisdat"))

analysis <- read_csv("G:/My Drive/Research/PRISE_data/fullanalysisdat")

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

soilbd <- projectRaster(soilbd, crs=crs(elevdat))
soilbd2 <- projectRaster(soilbd2, crs=crs(elevdat))
soilbd3 <- projectRaster(soilbd3, crs=crs(elevdat))
soilbd4 <- projectRaster(soilbd4, crs=crs(elevdat))

crs(soilbd)
crs(soilbd2)
crs(soilbd3)
crs(soilbd4)

allsoilbd <- merge(soilbd, soilbd2, soilbd3, soilbd4)

soilSOC <- raster("soils/SoilGrids_SOC_SEA-0000000000-0000000000.tif")
soilSOC2 <- raster("soils/SoilGrids_SOC_SEA-0000000000-0000018944.tif")
soilSOC3 <- raster("soils/SoilGrids_SOC_SEA-0000018944-0000000000.tif")
soilSOC4 <- raster("soils/SoilGrids_SOC_SEA-0000018944-0000018944.tif")

plot(soilSOC)
plot(soilSOC2)
plot(soilSOC3)
plot(soilSOC4)

soilSOC <- projectRaster(soilSOC, crs=crs(elevdat))
soilSOC2 <- projectRaster(soilSOC2, crs=crs(elevdat))
soilSOC3 <- projectRaster(soilSOC3, crs=crs(elevdat))
soilSOC4 <- projectRaster(soilSOC4, crs=crs(elevdat))

crs(soilSOC)
crs(soilSOC2)
crs(soilSOC3)
crs(soilSOC4)


soilSand <- raster("soils/SoilGrids_Sand_SEA-0000000000-0000000000.tif")
soilSand2 <- raster("soils/SoilGrids_Sand_SEA-0000000000-0000018944.tif")
soilSand3 <- raster("soils/SoilGrids_Sand_SEA-0000018944-0000000000.tif")
soilSand4 <- raster("soils/SoilGrids_Sand_SEA-0000018944-0000018944.tif")

plot(soilSand)
plot(soilSand2)
plot(soilSand3)
plot(soilSand4)

soilSand <- projectRaster(soilSand, crs=crs(elevdat))
soilSand2 <- projectRaster(soilSand2, crs=crs(elevdat))
soilSand3 <- projectRaster(soilSand3, crs=crs(elevdat))
soilSand4 <- projectRaster(soilSand4, crs=crs(elevdat))

crs(soilSand)
crs(soilSand2)
crs(soilSand3)
crs(soilSand4)

soilpH <- raster("soils/SoilGrids_pH_SEA-0000000000-0000000000.tif")
soilpH2 <- raster("soils/SoilGrids_pH_SEA-0000000000-0000018944.tif")
soilpH3 <- raster("soils/SoilGrids_pH_SEA-0000018944-0000000000.tif")
soilpH4 <- raster("soils/SoilGrids_pH_SEA-0000018944-0000018944.tif")
plot(soilpH)
plot(soilpH2)
plot(soilpH3)
plot(soilpH4)

soilpH <- projectRaster(soilpH, crs=crs(elevdat))
soilpH2 <- projectRaster(soilpH2, crs=crs(elevdat))
soilpH3 <- projectRaster(soilpH3, crs=crs(elevdat))
soilpH4 <- projectRaster(soilpH4, crs=crs(elevdat))

crs(soilpH)
crs(soilpH2)
crs(soilpH3)
crs(soilpH4)

soilnit <- raster("soils/SoilGrids_N_SEA-0000000000-0000000000.tif")
soilnit2 <- raster("soils/SoilGrids_N_SEA-0000000000-0000018944.tif")
soilnit3 <- raster("soils/SoilGrids_N_SEA-0000018944-0000000000.tif")
soilnit4 <- raster("soils/SoilGrids_N_SEA-0000018944-0000018944.tif")
plot(soilnit)
plot(soilnit2)
plot(soilnit3)
plot(soilnit4)

soilnit <- projectRaster(soilnit, crs=crs(elevdat))
soilnit2 <- projectRaster(soilnit2, crs=crs(elevdat))
soilnit3 <- projectRaster(soilnit3, crs=crs(elevdat))
soilnit4 <- projectRaster(soilnit4, crs=crs(elevdat))

crs(soilnit)
crs(soilnit2)
crs(soilnit3)
crs(soilnit4)


soilclay <- raster("soils/SoilGrids_Clay_SEA-0000000000-0000000000.tif")
soilclay2 <- raster("soils/SoilGrids_Clay_SEA-0000000000-0000018944.tif")
soilclay3 <- raster("soils/SoilGrids_Clay_SEA-0000018944-0000000000.tif")
soilclay4 <- raster("soils/SoilGrids_Clay_SEA-0000018944-0000018944.tif")
plot(soilclay)
plot(soilclay2)
plot(soilclay3)
plot(soilclay4)

soilclay <- projectRaster(soilclay, crs=crs(elevdat))
soilclay2 <- projectRaster(soilclay2, crs=crs(elevdat))
soilclay3 <- projectRaster(soilclay3, crs=crs(elevdat))
soilclay4 <- projectRaster(soilclay4, crs=crs(elevdat))

crs(soilclay)
crs(soilclay2)
crs(soilclay3)
crs(soilclay4)


soilcec <- raster("soils/SoilGrids_CEC_SEA-0000000000-0000000000.tif")
soilcec2 <- raster("soils/SoilGrids_CEC_SEA-0000000000-0000018944.tif")
soilcec3 <- raster("soils/SoilGrids_CEC_SEA-0000018944-0000000000.tif")
soilcec4 <- raster("soils/SoilGrids_CEC_SEA-0000018944-0000018944.tif")
plot(soilcec)
plot(soilcec2)
plot(soilcec3)
plot(soilcec4)

soilcec <- projectRaster(soilcec, crs=crs(elevdat))
soilcec2 <- projectRaster(soilcec2, crs=crs(elevdat))
soilcec3 <- projectRaster(soilcec3, crs=crs(elevdat))
soilcec4 <- projectRaster(soilcec4, crs=crs(elevdat))

crs(soilcec)
crs(soilcec2)
crs(soilcec3)
crs(soilcec4)


#extract data
#bulk density
temp <- raster::extract(soilbd, heightsample)
head(temp)
summary(temp)
length(temp)
dim(heightsample)
analysis$bulkdensity <- raster::extract(soilbd,heightsample)

temp <- raster::extract(soilbd2, heightsample)
head(temp)
summary(temp)
length(temp)
dim(heightsample)
analysis$bulkdensity2 <- raster::extract(soilbd2,heightsample)

temp <- raster::extract(soilbd3, heightsample)
head(temp)
summary(temp)
length(temp)
dim(heightsample)
analysis$bulkdensity3 <- raster::extract(soilbd3,heightsample)

temp <- raster::extract(soilbd4, heightsample)
head(temp)
summary(temp)
length(temp)
dim(heightsample)
analysis$bulkdensity4 <- raster::extract(soilbd4,heightsample)

#soc
temp <- raster::extract(soilSOC,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$soc <- raster::extract(soilSOC,heightsample)

temp <- raster::extract(soilSOC2,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$soc2 <- raster::extract(soilSOC2,heightsample)

temp <- raster::extract(soilSOC3,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$soc3 <- raster::extract(soilSOC3,heightsample)

temp <- raster::extract(soilSOC4,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$soc4 <- raster::extract(soilSOC4,heightsample)

#sand
temp <- raster::extract(soilSand,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$sand <- raster::extract(soilSand,heightsample)

temp <- raster::extract(soilSand2,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$sand2 <- raster::extract(soilSand2,heightsample)

temp <- raster::extract(soilSand3,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$sand3 <- raster::extract(soilSand3,heightsample)

temp <- raster::extract(soilSand4,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$sand4 <- raster::extract(soilSand4,heightsample)

#ph
temp <- raster::extract(soilpH,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$ph <- raster::extract(soilpH,heightsample)

temp <- raster::extract(soilpH2,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$ph2 <- raster::extract(soilpH2,heightsample)

temp <- raster::extract(soilpH3,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$ph3 <- raster::extract(soilpH3,heightsample)

temp <- raster::extract(soilpH4,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$ph4 <- raster::extract(soilpH4,heightsample)

#nitrogen
temp <- raster::extract(soilnit,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$nitrogen <- raster::extract(soilnit,heightsample)

temp <- raster::extract(soilnit2,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$nitrogen2 <- raster::extract(soilnit2,heightsample)

temp <- raster::extract(soilnit3,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$nitrogen3 <- raster::extract(soilnit3,heightsample)

temp <- raster::extract(soilnit4,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$nitrogen4 <- raster::extract(soilnit4,heightsample)

#clay
temp <- raster::extract(soilclay,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$clay <- raster::extract(soilclay,heightsample)

temp <- raster::extract(soilclay2,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$clay2 <- raster::extract(soilclay2,heightsample)

temp <- raster::extract(soilclay3,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$clay3 <- raster::extract(soilclay3,heightsample)

temp <- raster::extract(soilclay4,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$clay4 <- raster::extract(soilclay4,heightsample)

#cec
temp <- raster::extract(soilcec,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$cec <- raster::extract(soilcec,heightsample)

temp <- raster::extract(soilcec2,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$cec2 <- raster::extract(soilcec2,heightsample)

temp <- raster::extract(soilcec3,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$cec3 <- raster::extract(soilcec3,heightsample)

temp <- raster::extract(soilcec4,heightsample)
head(temp)
length(temp)
dim(heightsample)
analysis$cec4 <- raster::extract(soilcec4,heightsample)

is.na(analysis$bulkdensity) -> NULL

analysis$bd <- paste(analysis$bulkdensity, analysis$bulkdensity2, analysis$bulkdensity3, analysis$bulkdensity4, na.rm=TRUE)

write.csv(analysis, ("G:/My Drive/Research/PRISE_data/biganalysisdat"))

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




