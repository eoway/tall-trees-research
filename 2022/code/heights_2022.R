library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(ggplot2)

#----------------------------------------------------------------------#
#---------------------------Prepare Data--------------------------------
#----------------------------------------------------------------------#
# Read in Data
hdata <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Elsa_Clean/main_dat.csv")


# Look at DBH Distribution
summary(hdata$dbh)
# Lots of NAs; Min 0; Max 251.8



#Remove trees <10cm
hdata <- filter(hdata, dbh >= 10)
# Check to make sure filtering worked
# Min should now be 10
summary(hdata$dbh)

#----------------------------------------------------------------------#
#-----------------------Latest Censuses----------------------------------
#----------------------------------------------------------------------#
# Subset to only latest censuses
hdata <- filter(hdata, census == "01_census_2016"| census == "02_census_2016"| 
                            census == "03_census_2016"| census == "census_2019" |
                            census == "census_2007_08"| census == "10_census_2014" |
                            census == "30_census_2015"| census == "08_census_2014")
# Check filtering
table(hdata$census)


#----------------------------------------------------------------------#
#------------------------Height Equations-------------------------------
#----------------------------------------------------------------------#
#use the Feldpausch et al 2011 equation with the Southeast Asia regional coefficients
dbh2h_01 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){ # exclude hgt_ref here if using first dbh_crit eq.
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh)),
              exp(b1Ht + b2Ht * log(dbh_crit)))
  return(h)
}



# CASE(3,4) ## Chave et al. 2014 - assuming environmental factor = 0 ## 
dbh2h_34 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh) + (hgt_ref * log(dbh)**2)),
              exp(b1Ht + b2Ht * log(dbh_crit) + (hgt_ref * log(dbh_crit)**2)))
  return(h)
}




## Chave et al. 2014 - WITH environmental factor ##
dbh2h_ChaveE <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht,E){
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  #h <- 0.893 - E + 0.760*log(dbh)-0.0340*(log(dbh)**2)
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht - E + b2Ht * log(dbh) + hgt_ref * log(dbh)**2),
              exp(b1Ht - E + b2Ht * log(dbh_crit) + hgt_ref * log(dbh_crit)**2))
  return(h)
}



#----------------------------------------------------------------------#
#----------------------------Parameters---------------------------------
#----------------------------------------------------------------------#

#Feldspauch - -----
b1Ht_SEA    = 0.5279284 * log(10) # Use for dbh2h_01
# SAME AS: b1Ht_SEA = 1.2156
b2Ht_SEA    = 0.5782 #"coefficient of ln(D)" # Use for dbh2h_01
hgt_ref_SEA = -0.0114
hgt_max_SEA = 100


#Chave------
b1Ht_34    =  0.893 # Use for dbh2h_34 & dbh2h_ChaveE
b2Ht_34    =  0.76 # Use for dbh2h_34 & dbh2h_ChaveE
hgt_ref_34 = -0.034 # Use for dbh2h_34 & dbh2h_ChaveE
hgt_max = 100
#E------library("raster")
library("ncdf4")
source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
# E = (0.178* TS - 0.938 & CWD - 6.61 * PS) * 10^3
# DNM, LHP, SPK, PSO, ALP, CRA
longitude=c(117.78994,114.033333,117.92806,102.3062,-73.4385,-70.2173)
latitude=c(4.955965,4.20161,5.852336,2.973,-3.9603,-12.4402)
coord=cbind(longitude,latitude); coord

E = retrieve_raster("E", coord, format="nc"); E
# Danum "E" value
E_DNM = E[1]; E_DNM #-0.0365256
# Lambir "E" value
E_LHP = E[2]
# Sepilok "E" value
E_SPK = E[3]
# Alpahuayo "E" value
#E_ALP = E[4]; E_ALP #-0.09335928
# Cicra "E" value
#E_CRA = E[5]; E_CRA #-0.04474343




#----------------------------------------------------------------------#
#-------------------------Calculate Heights-----------------------------
#----------------------------------------------------------------------#
#Feld
hdata$heightFeld <- dbh2h_01(hdata$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
summary(hdata$heightFeld)


#Chave
hdata$heightCh <- dbh2h_34(hdata$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34)
summary(hdata$heightCh)


#Chave with E
# need to specify which "E" value to use - from above
hdata$heightE <- dbh2h_ChaveE(hdata$dbh,hgt_max,hgt_ref_34,b1Ht_34,b2Ht_34,E_DNM)
summary(hdata$heightE)


#----------------------------------------------------------------------#
#-----------------------Calculate Quantiles-----------------------------
#----------------------------------------------------------------------#
#Feld Heights Quantiles
quantile90Feld <-quantile(hdata$heightFeld, probs = 0.90, na.rm = TRUE)
quantile95Feld <-quantile(hdata$heightFeld, probs = 0.95, na.rm = TRUE)
quantile99Feld <-quantile(hdata$heightFeld, probs = 0.99, na.rm = TRUE)

#Chave Height Quantiles
quantile90Ch <-quantile(hdata$heightCh, probs = 0.90, na.rm = TRUE)
quantile95Ch <-quantile(hdata$heightCh, probs = 0.95, na.rm = TRUE)
quantile99Ch <-quantile(hdata$heightCh, probs = 0.99, na.rm = TRUE)

#Chave with E Quantiles
quantile90E <-quantile(hdata$heightE, probs = 0.90, na.rm = TRUE)
quantile95E <-quantile(hdata$heightE, probs = 0.95, na.rm = TRUE)
quantile99E <-quantile(hdata$heightE, probs = 0.99, na.rm = TRUE)

#DBH Quantiles-------
quantile90dbh <-quantile(hdata$dbh, probs = 0.90, na.rm = TRUE)
quantile95dbh <-quantile(hdata$dbh, probs = 0.95, na.rm = TRUE)
quantile99dbh <-quantile(hdata$dbh, probs = 0.99, na.rm = TRUE)

#----------------------------------------------------------------------#
#--------------------------Remove Indets--------------------------------
#----------------------------------------------------------------------#
hdata <- filter(hdata, species != "Indet")

#----------------------------------------------------------------------#
#-------Adding Emergent/Non-emergent Labeling by species----------------
#----------------------------------------------------------------------#
#quantile 90-------
#Feld
# Make dataframe with only trees above the percentile threshold
emergent90Feld <- filter(hdata, dbh >= quantile90Feld)
# Check
summary(emergent90Feld$dbh)
quantile90Feld

# filter to only unique species
emergent90Feld <- unique(emergent90Feld$species)
table(emergent90Feld)

# create a column that labels trees of species within emergent90Feld as emergent
# and trees of all other species as non emergent
hdata$tree_type90F <- ifelse(hdata$species %in% c(emergent90Feld), "emrgnt", "non_emrgnt")

# Repeat above for each threshold


#Chave w/o E
emergent90Ch <- filter(hdata, dbh >= quantile90Ch)
summary(emergent90Ch$dbh)

emergent90Ch <- unique(emergent90Ch$species)
summary(emergent90Ch)

hdata$tree_type90Ch <- ifelse(hdata$species %in% c(emergent90Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent90E <- filter(hdata, dbh >= quantile90E)
table(emergent90E$dbh)

emergent90E <- unique(emergent90E$species)
table(emergent90E)

hdata$tree_type90E <- ifelse(hdata$species %in% c(emergent90E), "emrgnt", "non_emrgnt")


#quantile 95-------
#Feld
emergent95Feld <- filter(hdata, dbh >= quantile95Feld)
table(emergent95Feld$dbh)

emergent95Feld <- unique(emergent95Feld$species)
table(emergent95Feld)

hdata$tree_type95F <- ifelse(hdata$species %in% c(emergent95Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent95Ch <- filter(hdata, dbh >= quantile95Ch)
table(emergent95Ch$dbh)

emergent95Ch <- unique(emergent95Ch$species)
table(emergent95Ch)

hdata$tree_type95Ch <- ifelse(hdata$species %in% c(emergent95Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent95E <- filter(hdata, dbh >= quantile95E)
table(emergent95E$dbh)

emergent95E <- unique(emergent95E$species)
table(emergent95E)

hdata$tree_type95E <- ifelse(hdata$species %in% c(emergent95E), "emrgnt", "non_emrgnt")

#quantile 99-------
#Feld
emergent99Feld <- filter(hdata, dbh >= quantile99Feld)
table(emergent99Feld$dbh)

emergent99Feld <- unique(emergent99Feld$species)
table(emergent99Feld)

hdata$tree_type99F <- ifelse(hdata$species %in% c(emergent99Feld), "emrgnt", "non_emrgnt")

#Chave w/o E
emergent99Ch <- filter(hdata, dbh >= quantile99Ch)
table(emergent99Ch$dbh)

emergent99Ch <- unique(emergent99Ch$species)
table(emergent99Ch)

hdata$tree_type99Ch <- ifelse(hdata$species %in% c(emergent99Ch), "emrgnt", "non_emrgnt")

#Chave with E
emergent99E <- filter(hdata, dbh >= quantile99E)
table(emergent99E$dbh)

emergent99E <- unique(emergent99E$species)
table(emergent99E)

hdata$tree_type99E <- ifelse(hdata$species %in% c(emergent99E), "emrgnt", "non_emrgnt")

#------------------DBH Definitions---------------------------
#90th percentile-----
emergent90dbh <- filter(hdata, dbh >= quantile90dbh)
table(emergent90dbh$dbh)

emergent90dbh <- unique(emergent90dbh$species)
table(emergent90dbh)

hdata$tree_type90dbh <- ifelse(hdata$species %in% c(emergent90dbh), "emrgnt", "non_emrgnt")

#95th percentile-----
emergent95dbh <- filter(hdata, dbh >= quantile95dbh)
table(emergent95dbh$dbh)

emergent95dbh <- unique(emergent95dbh$species)
table(emergent95dbh)

hdata$tree_type95dbh <- ifelse(hdata$species %in% c(emergent95dbh), "emrgnt", "non_emrgnt")

#99th percentile-----
emergent99dbh <- filter(hdata, dbh >= quantile99dbh)
table(emergent99dbh$dbh)

emergent99dbh <- unique(emergent99dbh$species)
table(emergent99dbh)

hdata$tree_type99dbh <- ifelse(hdata$species %in% c(emergent99dbh), "emrgnt", "non_emrgnt")



#----------------------------------------------------------------------#
#-Create a column separating emergents, nonemergents, & emergent species-
#----------------------------------------------------------------------#
# Set a size threshold
size_threhold = quantile99dbh
# Create a column that labels trees as either emergent (meaning the tree is actually tall)
# emergent species (meaning the tree is a member of a species that gets tall)
# and non emergent meaning the tree is not tall and is not a member of a species that gets tall
hdata$tree_type99dbhmap <- ifelse(hdata$tree_type99dbh == "emrgnt" & hdata$dbh >= size_threhold, "emrgnt_tree",
                                  ifelse(hdata$tree_type99dbh == "emrgnt" & hdata$dbh < size_threhold, "emrgnt_spp","non_emrgnt"))
# Check
table(hdata$tree_type99dbh)
table(hdata$tree_type99dbhmap)
# if you compare the two tables above, non_emergent should have the same number of observations for both variables
# tree_type99dbhmap splits tree_type99dbh == emrgnt into two variables now



#----------------------------------------------------------------------#
#-----------------------------Boxplots----------------------------------
#----------------------------------------------------------------------#
# Create boxplots showing the dbh distribution of 
# emergents, nonemergents, and emergent species
ggplot(hdata, aes(x=tree_type99dbh, y=dbh, fill=tree_type99dbh)) + 
  geom_boxplot() + 
  theme_classic() + theme(legend.position = c(0.8, 0.8))

ggplot(hdata, aes(x=tree_type99dbhmap, y=dbh, fill=tree_type99dbhmap)) + 
  geom_boxplot() + 
  theme_classic() + theme(legend.position = c(0.8, 0.8))


#----------------------------------------------------------------------#
#---------------------------Dataframes----------------------------------
#----------------------------------------------------------------------#
DNM50=filter(hdata, site == "DNM50")
DNM1=filter(hdata, site == "DNM1")
DNM2=filter(hdata, site == "DNM2")
DNM3=filter(hdata, site == "DNM3")
LHP=filter(hdata, site == "LHP")
SPKA=filter(hdata, site == "SPKA")
SPKH=filter(hdata, site == "SPKH")
SPKS=filter(hdata, site == "SPKS")


# Make data frames for Danum 50 and Lambir
# Danum 50 hectare plot
DNM50=filter(hdata, site == "DNM50")
dan <- filter(DNM50, dbh >= quantile99dbh)
dansp <- filter (dan, species == "excelsa" | species == "johorensis" | 
                   species == "lanceolata" | species == "parvifolia" |
                   species == "pauciflora" | species == "sumatrana" |
                   species == "superba")
table(hdata$site)

# Lambir
LMN <- filter(hdata, site == "LHP")
lam <- filter(LMN, dbh >= quantile99dbh)
lamsp <- filter (lam, species == "excelsa" | species == "johorensis" | 
                   species == "lanceolata" | species == "parvifolia" |
                   species == "pauciflora" | species == "sumatrana" |
                   species == "superba")


#----------------------------------------------------------------------#
#-------------------------------Maps------------------------------------
#----------------------------------------------------------------------#
# filter all using: tree_type99dbhmap instead of tree_type99dbh
#DNM1-----
DNM1 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

#DNM2-----
DNM2 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

#DNM3-----
DNM3 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

#DNM50-----
DNM50 %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

emapdat <- filter(DNM50, dbh >= quantile99dbh)
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=species))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=genus))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()
#DNM50 %>%
#  ggplot(mapping = aes(y=plot_y, x=plot_x))+
#  geom_point(aes(col=tree_type99dbh))+
#  geom_point(emapdat, mapping = aes(y=plot_y, x=plot_x))+
#  theme_classic()

#LHP-----
LHP %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

emapdat <- filter(LHP, dbh >= quantile99dbh)
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=species))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()
emapdat %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=genus))+
  geom_point(size=3, alpha=0.7) + 
  theme_classic()

#SPKA-----
SPKA %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()
#SPKH-----
SPKH %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

#SPKS-----
SPKS %>%
  ggplot(mapping = aes(y=plot_y, x=plot_x, col=tree_type99dbhmap))+
  geom_point(size=3, alpha=0.7) +
  scale_color_manual("tree type", values=c("darkorange","red","grey50")) + 
  theme_classic()

#----------------------------------------------------------------------#
#-------------------Plots for manuscript-------------------------------
#----------------------------------------------------------------------#
#DBH Histogram-------
hdata %>%
  ggplot(hdata, mapping = aes(x=dbh))+
  geom_histogram()+
  geom_vline(xintercept = quantile90dbh) +
  geom_vline(xintercept = quantile95dbh) +
  geom_vline(xintercept = quantile99dbh)

#All three height calculations on one plot
hdata %>%
  ggplot(aes(dbh, heightFeld, colour= "Height Defintion"))+
  geom_point(aes(dbh, heightFeld), color="chartreuse3")+
  geom_point(aes(dbh, heightCh), color="darkorchid3")+
  geom_point(aes(dbh, heightE), color="deepskyblue3")+
  geom_hline(yintercept = quantile90Feld, color="chartreuse4")+
  annotate("text", y= quantile90Feld+2,x=230,label=round(quantile90Feld, digits=3))+
  geom_hline(yintercept = quantile95Feld, color="chartreuse4")+
  annotate("text", y= quantile95Feld+2,x=230,label=round(quantile95Feld, digits=3))+
  geom_hline(size=1, yintercept = quantile99Feld, color="chartreuse4")+
  annotate("text", y= quantile99Feld+2,x=230,label=round(quantile99Feld, digits=3))+
  geom_vline(xintercept = quantile90dbh, color="black")+
  annotate("text", x= quantile90dbh+5,y=5,label=round(quantile90dbh, digits=3))+
  geom_vline(xintercept = quantile95dbh, color="black")+
  annotate("text", x= quantile95dbh+7,y=5,label=round(quantile95dbh, digits=3))+
  geom_vline(size=1, xintercept = quantile99dbh, color="black")+
  annotate("text", x= quantile99dbh+5,y=5,label=round(quantile99dbh, digits=3))+
  xlab("DBH")+
  ylab("Height")

#99th percentile DBH and All Height Calculations Plot with Emergent species colored darker
#----------------------------------------------------------------------#
#--------------------------Elsa Help------------------------------------
#How do I change colors so that each line is different and each set of emergents are different?
#----------------------------------------------------------------------#

# for colors, see ggnewscale (https://github.com/eliocamp/ggnewscale)
# i just came across it and it works beautifully for this
library(ggnewscale)

# also, remember that tree_type99dbhmap is determined based on my arbitrary size threshold of dbh = 100
# once you update it to quantile99dbh, the colors will correspond to the vertical line in this plot

hdata %>%
  ggplot(aes(dbh, heightFeld))+
  geom_point(aes(col=tree_type99dbhmap))+
  scale_color_manual("Feldpausch allom", values=c("green","chartreuse","darkgreen")) + 
  new_scale_color() + # from ggnewscale - everything after this requires a new color scale
  geom_point(aes(col=tree_type99dbhmap, dbh, heightCh))+
  scale_color_manual("Chave allom", values=c("#1d91c0","#41b6c4","#253494")) + 
  new_scale_color() +
  geom_point(aes(col=tree_type99dbhmap, dbh, heightE))+
  scale_color_manual("Chave+E allom", values=c("#7a0177","#ae017e","#810f7c")) + 
  geom_vline(xintercept = quantile99dbh, color="black")+
  xlab("DBH")+
  ylab("Height")+
  theme_classic() 

# hdata %>%
#   ggplot(aes(dbh, heightFeld))+
#   geom_point(aes(col=tree_type99F))+
#   geom_point(aes(col=tree_type99Ch, dbh, heightCh))+
#   geom_point(aes(col=tree_type99E, dbh, heightE))+
#   geom_vline(xintercept = quantile99dbh, color="black")+
#   xlab("DBH")+
#   ylab("Height")+
#   theme_classic()

#Same plot as last one, but with only Feld Height Calculation
hdata %>%
  ggplot(aes(dbh, heightFeld, col=tree_type99F))+
  geom_point()+
  geom_vline(xintercept = quantile99dbh, color="black")+
  xlab("DBH")+
  ylab("Height")

#Stem_BA---
plotdata <- filter(hdata, DFstatus == "A")
table(plotdata$DFstatus)
plotdat <- plotdata %>% group_by(site, tree_type99dbh) %>% dplyr::summarize(n_stems=n(), plot_BA = sum(stem_BA, na.rm=T, 
                                                                                                mean_stem_BA = mean(stem_BA, na.rm=T)))

plotdat$area <- c(1,1,1,1,1,1,50,50,52,52,8,8,12,12,4,4)

plotdat$stemBAha <- plotdat$plot_BA/plotdat$area

plotdat$stemdens <- plotdat$n_stems/plotdat$area
table(LHP$DFstatus)
table(DNM50$DFstatus)
#re
#plotdat$cluster <- factor(plot_dat2$cluster, levels=c("nonemrgnt","emrgnt"),
#labels=c("Nonemergent","Emergent" ))
#----------------------------------------------------------------------#
#--------------------------Elsa Help------------------------------------
#Sepiloks densities are weird
#----------------------------------------------------------------------#
#Stem_BAs plot------
plotdat %>%
  ggplot(plotdat, mapping = aes(fill=tree_type99dbh, y=stemBAha, x=site))+
  geom_col(position="stack")

#Stem density plot------
plotdat %>%
  ggplot(plotdat, mapping = aes(fill=tree_type99dbh, y=stemdens, x=site))+
  geom_col(position="stack")

