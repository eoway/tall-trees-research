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
plot(ecoregions1)
crs(ecoregions1)

ecoregions2 <- ecoregions1@polygons

ecoregions2 %>% 
  group_by(BIOME) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()
