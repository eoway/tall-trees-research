library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(fgeo)
library(plyr)

# Dont forget to give Benton Taylor credit for helping with this

# Read in Danum Dataframe
dat <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_2022.csv")
# Define emergent cutoff
quantile99dbh = 95
# Label trees as emergent/nonemergent
dat$tree_type <- ifelse(dat$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")


#-------------------------------------------------------------------------------#
# -----------------------------1200 tree sample-----------------
#------------------------------------------------------------------------------#

# Subset emergent and nonemergent trees
# Grab random sample from nonemergent trees as a comparison metric
dandatemerg <- subset(dat, dbh >= quantile99dbh)
dandatsamp <- subset(dat, dbh < quantile99dbh)
dandatsamp <- sample_n(dandatsamp, 324)
# bind emergent trees to random sample of trees
dandatemerg <- rbind(dandatsamp, dandatemerg)


#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
# each labeled with the tree of interest ID for summary purposes
quadfocal <- data.frame()
for (i in 1:nrow(dandatemerg))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg$x_utm[i]
  midy <- dandatemerg$y_utm[i]
  # define radius around tree size
  radius = 5
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$x_utm>=(midx-radius) & dat$x_utm<=(midx+radius) & dat$y_utm>=(midy-radius) & dat$y_utm<=(midy+radius), ]
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg$tree_type[i], length(nrow(temp)))
  # remove tree of interest
  emerg_ID <- dandatemerg$treeID
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
write.csv(buff_summaries, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/Danum_5m_600_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------10m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
quadfocal <- data.frame()
for (i in 1:nrow(dandatemerg))
{ 
  midx <- dandatemerg$x_utm[i]
  midy <- dandatemerg$y_utm[i]
  radius = 10
  temp<-dat[dat$x_utm>=(midx-radius) & dat$x_utm<=(midx+radius) & dat$y_utm>=(midy-radius) & dat$y_utm<=(midy+radius), ]
  temp$midtreeID <-rep(dandatemerg$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg$tree_type[i], length(nrow(temp)))
  emerg_ID <- dandatemerg$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  quadfocal <- rbind(quadfocal, temp)
}
buff_summaries <- quadfocal %>% group_by(midtreeID, midtreetype) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                      n_trees = n())
# export dataframe
write.csv(buff_summaries, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/Danum_10m_600_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------10,300 tree sample-----------------
#------------------------------------------------------------------------------#

# Subset emergent and nonemergent trees
# Grab random sample from nonemergent trees as a comparison metric
dandatemerg <- subset(dat, dbh >= quantile99dbh)
dandatsamp <- subset(dat, dbh < quantile99dbh)
dandatsamp <- sample_n(dandatsamp, 10000)
# bind emergent trees to random sample of trees
dandatemerg <- rbind(dandatsamp, dandatemerg)


#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 10,300 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
# each labeled with the tree of interest ID for summary purposes
quadfocal <- data.frame()
for (i in 1:nrow(dandatemerg))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg$x_utm[i]
  midy <- dandatemerg$y_utm[i]
  # define radius around tree size
  radius = 5
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$x_utm>=(midx-radius) & dat$x_utm<=(midx+radius) & dat$y_utm>=(midy-radius) & dat$y_utm<=(midy+radius), ]
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg$tree_type[i], length(nrow(temp)))
  # remove tree of interest
  emerg_ID <- dandatemerg$treeID
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
write.csv(buff_summaries, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/Danum_5m_10000_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------10m buffer, 10,300 tree sample-----------------
#------------------------------------------------------------------------------#
quadfocal <- data.frame()
for (i in 1:nrow(dandatemerg))
{ 
  midx <- dandatemerg$x_utm[i]
  midy <- dandatemerg$y_utm[i]
  radius = 10
  temp<-dat[dat$x_utm>=(midx-radius) & dat$x_utm<=(midx+radius) & dat$y_utm>=(midy-radius) & dat$y_utm<=(midy+radius), ]
  temp$midtreeID <-rep(dandatemerg$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg$tree_type[i], length(nrow(temp)))
  emerg_ID <- dandatemerg$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  quadfocal <- rbind(quadfocal, temp)
}

# Summarize based on middle tree ID
buff_summaries <- quadfocal %>% group_by(midtreeID, midtreetype) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                      n_trees = n())
# export dataframe
write.csv(buff_summaries, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/Danum_10m_10000_trees.csv")
