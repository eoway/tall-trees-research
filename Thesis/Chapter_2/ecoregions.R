library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(rgdal)
library(sp)
library(sf)
library(here)
library(plyr)
library(rgeos)
library(plotKML)

# read in ecoregions
ecoregions <- readOGR('~/Desktop/Research_2022/Data/Ecoregions',"wwf_terr_ecos")
ecoregions1 <- subset(ecoregions, ecoregions$BIOME == "1")
extent(ecoregions1)
writeOGR(ecoregions1, dsn = "~/Desktop/Research_2022/Data/Ecoregions", layer = "ecoregions_biome_1",
         driver = "ESRI Shapefile" )

# in arcgis tool called dissolve, may not need to do, shouldn't need to do
# crop out madagascar, keep in continental Africa and southeast asia
# Can do: - figure out what extent is and manually list out corners - create a box shapefile from ecoregion
# make one bounding box for africa and one for southeast asia

ecoregions2 <- crop(ecoregions1, extent(-90, 150, -34, 30))
extent(ecoregions1)
plot(ecoregions2)


ecoregions1 %>% 
  dplyr::group_by(OBJECTID) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
