library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(fgeo)
library(plyr)

# Read in Lambir Dataframe
dat <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_2022.csv")
# Define emergent cutoff
quantile99dbh = 95
# Label trees as emergent/nonemergent
dat$tree_type <- ifelse(dat$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")
# Subset emergent and nonemergent trees
# Grab random sample from nonemergent trees as a comparison metric
lamdatemerg <- subset(dat, dbh >= quantile99dbh)
lamdatsamp <- subset(dat, dbh < quantile99dbh)
lamdatsamp <- sample_n(lamdatsamp, 317)
# bind emergent trees to random sample of trees
lamdatemerg <- rbind(lamdatsamp, lamdatemerg)

# run the following to create a dataframe of all trees within 5 meters of the tree of interest
# each labeled with the tree of interest ID for summary purposes
quadfocal <- data.frame()
for (i in 1:nrow(lamdatemerg))
{ 
  # define coordinates for tree of interest
  midx <- lamdatemerg$gx[i]
  midy <- lamdatemerg$gy[i]
  # define radius around tree size
  radius = 5
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$gx>=(midx-radius) & dat$gx<=(midx+radius) & dat$gy>=(midy-radius) & dat$gy<=(midy+radius), ]
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatemerg$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatemerg$tree_type[i], length(nrow(temp)))
  # remove tree of interest
  emerg_ID <- lamdatemerg$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  # add data to one dataframe
  quadfocal <- rbind(quadfocal, temp)
}

# Summarize based on middle tree ID
buff_summaries <- quadfocal %>% group_by(midtreeID, midtreetype) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                              dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                              height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                              n_trees = n())
# export dataframe
write.csv(buff_summaries, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_surrounding_tree_data.csv")
