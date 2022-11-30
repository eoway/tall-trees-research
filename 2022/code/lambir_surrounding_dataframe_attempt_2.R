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


# dat == analyses with alive and dead trees
# dat1 == analyses with only alive trees


# Read in Lambir Dataframe
dat <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_2022.csv")

# Label trees as emergent/nonemergent
dat$tree_type <- ifelse(dat$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")
table(dat$tree_type)
# Subset to only alive trees
dat1 <- subset(dat, DFstatus == "alive")

# Define emergent cutoff
quantile99dbh = 95

# Label trees as emergent/nonemergent
dat1$tree_type <- ifelse(dat1$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")





#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# ------------------------------------5 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
lamdatemerg5 <- subset(dat1, tree_type == "emrgnt")

# Run function to grab information for every tree within 5 meters of an emergent
# Create empty dataframe
surr_emerg5 <- data.frame()
# Define a radius to grab information from
radius = 5
# Loop through every emergent tree
for (i in 1:nrow(lamdatemerg5))
{ 
  # define coordinates for tree of interest
  midx <- lamdatemerg5$gx[i]
  midy <- lamdatemerg5$gy[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$gx>=(midx-radius) & dat1$gx<=(midx+radius) & dat1$gy>=(midy-radius) & dat1$gy<=(midy+radius), ]
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatemerg5$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatemerg5$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatemerg5$species[i], length(nrow(temp)))
  # remove tree of interest
  emerg_ID <- lamdatemerg5$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  # add dat1a to one dat1aframe
  surr_emerg5 <- rbind(surr_emerg5, temp)
}
# check number of emrgnt midtreetype labels - should be 1831
table(surr_emerg5$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg5$speciescomp <- ifelse(surr_emerg5$species == surr_emerg5$midspecies, 1, 0)

# Summarize based on middle tree ID
emerg_summ_5 <- surr_emerg5 %>% group_by(midtreeID, midtreetype) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                              dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                              height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                              n_trees = n(),
                                                                                              n_species = n_distinct(species),
                                                                                              same_species = sum(speciescomp))
# should have 271 rows


#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------10 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
lamdatemerg10 <- subset(dat1, tree_type == "emrgnt")

# Run function to grab information for every tree within 10 meters of an emergent
# Create empty dataframe
surr_emerg10 <- data.frame()

# Define a radius to grab information from
radius = 10

# Loop through every emergent tree
for (i in 1:nrow(lamdatemerg10))
{ 
  # define coordinates for tree of interest
  midx <- lamdatemerg10$gx[i]
  midy <- lamdatemerg10$gy[i]
  
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$gx>=(midx-radius) & dat1$gx<=(midx+radius) & dat1$gy>=(midy-radius) & dat1$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatemerg10$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatemerg10$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatemerg10$species[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatemerg10$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add data to one dataframe
  surr_emerg10 <- rbind(surr_emerg10, temp)
}

# check number of emrgnt midtreetype labels - should be 1831
table(surr_emerg10$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg10$speciescomp <- ifelse(surr_emerg10$species == surr_emerg10$midspecies, 1, 0)

# Summarize based on middle tree ID
emerg_summ_10 <- surr_emerg10 %>% group_by(midtreeID, midtreetype) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                      n_trees = n(),
                                                                                      n_species = n_distinct(species),
                                                                                      same_species = sum(speciescomp))
# should have 272 rows






#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# ------------------------------------5 meter----------------------------------#
# ---------------------------all trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
lamdatemerg5_all <- subset(dat, tree_type == "emrgnt")

# Run function to grab information for every tree within 5 meters of an emergent
# Create empty dataframe
surr_emerg5_all <- data.frame()
# Define a radius to grab information from
radius = 5
# Loop through every emergent tree
for (i in 1:nrow(lamdatemerg5_all))
{ 
  # define coordinates for tree of interest
  midx <- lamdatemerg5_all$gx[i]
  midy <- lamdatemerg5_all$gy[i]
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$gx>=(midx-radius) & dat$gx<=(midx+radius) & dat$gy>=(midy-radius) & dat$gy<=(midy+radius), ]
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatemerg5_all$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatemerg5_all$tree_type[i], length(nrow(temp)))
  temp$midstatus <-rep(lamdatemerg5_all$DFstatus[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatemerg5_all$species[i], length(nrow(temp)))
  # remove tree of interest
  emerg_ID <- lamdatemerg5_all$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  # add data to one dataframe
  surr_emerg5_all <- rbind(surr_emerg5_all, temp)
}
# check number of emrgnt midtreetype labels - should be 1831
table(surr_emerg5_all$midtreetype)

# label dead trees with 1 and alive trees with 0
surr_emerg5_all$status <- ifelse(surr_emerg5_all$DFstatus=="alive", 1, 0)

# label same species with 1 and different trees with 0
surr_emerg5_all$speciescomp <- ifelse(surr_emerg5_all$species == surr_emerg5_all$midspecies, 1, 0)

# Summarize based on middle tree ID
emerg_summ_5_all <- surr_emerg5_all %>% group_by(midtreeID, midtreetype, midstatus, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                      n_trees = n(),
                                                                                      n_species = n_distinct(species),
                                                                                      n_alive = sum(status),
                                                                                      n_dead = n_trees - n_alive,
                                                                                      same_species = sum(speciescomp))
# should have 317 rows





#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------10 meter----------------------------------#
# ---------------------------all trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
lamdatemerg10_all <- subset(dat, tree_type == "emrgnt")

# Run function to grab information for every tree within 10 meters of an emergent
# Create empty dataframe
surr_emerg10_all <- data.frame()

# Define a radius to grab information from
radius = 10

# Loop through every emergent tree
for (i in 1:nrow(lamdatemerg10_all))
{ 
  # define coordinates for tree of interest
  midx <- lamdatemerg10_all$gx[i]
  midy <- lamdatemerg10_all$gy[i]
  
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$gx>=(midx-radius) & dat$gx<=(midx+radius) & dat$gy>=(midy-radius) & dat$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatemerg10_all$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatemerg10_all$tree_type[i], length(nrow(temp)))
  temp$midstatus <-rep(lamdatemerg5_all$DFstatus[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatemerg5_all$species[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatemerg10_all$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add data to one dataframe
  surr_emerg10_all <- rbind(surr_emerg10_all, temp)
}

# check number of emrgnt midtreetype labels - should be 9933
table(surr_emerg10_all$midtreetype)

# label dead trees with 1 and alive trees with 0
surr_emerg10_all$status <- ifelse(surr_emerg10_all$DFstatus=="alive", 1, 0)

# label same species with 1 and different trees with 0
surr_emerg10_all$speciescomp <- ifelse(surr_emerg10_all$species == surr_emerg10_all$midspecies, 1, 0)

# Summarize based on middle tree ID
emerg_summ_10_all <- surr_emerg10_all %>% group_by(midtreeID, midtreetype, midstatus, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                        dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                        height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                        n_trees = n(),
                                                                                        n_species = n_distinct(species),
                                                                                        n_alive = sum(status),
                                                                                        n_dead = n_trees - n_alive,
                                                                                        same_species = sum(speciescomp))
# should have 317 rows













#-------------------------------------------------------------------------------#
# -----------------------------300 nonemergent tree sample-----------------
# -----------------------------------5 meter radius--------------------
# -----------------------------------only alive trees--------------------
#------------------------------------------------------------------------------#
# Grab random sample from nonemergent trees as a comparison metric
nonemrg <- subset(dat1, dbh < quantile99dbh)
lamdatsamp300 <- sample_n(nonemrg, 300)
#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
nonemrg_samp300 <- data.frame()
radius = 5
for (i in 1:nrow(lamdatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp300$gx[i]
  midy <- lamdatsamp300$gy[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$gx>=(midx-radius) & dat1$gx<=(midx+radius) & dat1$gy>=(midy-radius) & dat1$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp300$species[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp300$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add dat1a to one dat1aframe
  nonemrg_samp300 <- rbind(nonemrg_samp300, temp)
}

# label same species with 1 and different trees with 0
nonemrg_samp300$speciescomp <- ifelse(nonemrg_samp300$species == nonemrg_samp300$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info300 <- nonemrg_samp300 %>% group_by(midtreeID, midtreetype, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                              dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                              height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                              n_trees = n(),
                                                                              n_species = n_distinct(species),
                                                                              same_species = sum(speciescomp))
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all5_300_alive <- rbind(surr_info300, emerg_summ_5)

# export dataframe
write.csv(all5_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_600_alive_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------10m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#

nonemrg_samp300_10 <- data.frame()
radius = 10
for (i in 1:nrow(lamdatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp300$gx[i]
  midy <- lamdatsamp300$gy[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$gx>=(midx-radius) & dat1$gx<=(midx+radius) & dat1$gy>=(midy-radius) & dat1$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp300$species[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp300$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add dat1a to one dat1aframe
  nonemrg_samp300_10 <- rbind(nonemrg_samp300_10, temp)
}

# label same species with 1 and different trees with 0
nonemrg_samp300_10$speciescomp <- ifelse(nonemrg_samp300_10$species == nonemrg_samp300_10$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info300_10 <- nonemrg_samp300_10 %>% group_by(midtreeID, midtreetype, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                      n_trees = n(),
                                                                                                      n_species = n_distinct(species),
                                                                                                      same_species = sum(speciescomp))
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all10_300_alive <- rbind(surr_info300_10, emerg_summ_10)

# export dataframe
write.csv(all5_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_600_alive_trees.csv")


#-------------------------------------------------------------------------------#
# -----------------------------300 nonemergent tree sample-----------------
# -----------------------------------5 meter radius--------------------
# -----------------------------------only all trees--------------------
#------------------------------------------------------------------------------#
# Grab random sample from nonemergent trees as a comparison metric
nonemrg_all <- subset(dat, dbh < quantile99dbh)
lamdatsamp300_all <- sample_n(nonemrg_all, 300)
#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
nonemrg_samp300_all <- data.frame()
radius = 5
for (i in 1:nrow(lamdatsamp300_all))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp300_all$gx[i]
  midy <- lamdatsamp300_all$gy[i]
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$gx>=(midx-radius) & dat$gx<=(midx+radius) & dat$gy>=(midy-radius) & dat$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp300_all$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp300_all$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp300_all$species[i], length(nrow(temp)))
  temp$midstatus <-rep(lamdatsamp300_all$DFstatus[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp300_all$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add data to one dataframe
  nonemrg_samp300_all <- rbind(nonemrg_samp300_all, temp)
}
# rows == 2416

# label dead trees with 1 and alive trees with 0
nonemrg_samp300_all$status <- ifelse(nonemrg_samp300_all$DFstatus=="alive", 1, 0)

# label same species with 1 and different trees with 0
nonemrg_samp300_all$speciescomp <- ifelse(nonemrg_samp300_all$species == nonemrg_samp300_all$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info300_all <- nonemrg_samp300_all %>% group_by(midtreeID, midtreetype, midstatus, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                                         n_trees = n(),
                                                                                                                         n_species = n_distinct(species),
                                                                                                                         n_alive = sum(status),
                                                                                                                         n_dead = n_trees - n_alive,
                                                                                                                         same_species = sum(speciescomp))# check number of rows, should = 299


# rows == 300

# combine with emergent dataframe
all5_300_all_all <- rbind(surr_info300_all, emerg_summ_5_all)

# export dataframe
write.csv(all5_300_all_all, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_600_all_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------10000 nonemergent tree sample-----------------
# -----------------------------------5 meter radius--------------------
# -----------------------------------only alive trees--------------------
#------------------------------------------------------------------------------#
# Grab random sample from nonemergent trees as a comparison metric
nonemrg <- subset(dat1, dbh < quantile99dbh)
lamdatsamp10000 <- sample_n(nonemrg, 10000)
#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
nonemrg_samp10000 <- data.frame()
radius = 5
for (i in 1:nrow(lamdatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp10000$gx[i]
  midy <- lamdatsamp10000$gy[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$gx>=(midx-radius) & dat1$gx<=(midx+radius) & dat1$gy>=(midy-radius) & dat1$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp10000$species[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp10000$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add dat1a to one dat1aframe
  nonemrg_samp10000 <- rbind(nonemrg_samp10000, temp)
}

# 48484 observations

# label same species with 1 and different trees with 0
nonemrg_samp10000$speciescomp <- ifelse(nonemrg_samp10000$species == nonemrg_samp10000$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000 %>% group_by(midtreeID, midtreetype, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                      n_trees = n(),
                                                                                                      n_species = n_distinct(species),
                                                                                                      same_species = sum(speciescomp))
# check number of rows, should = 9875
## Figure out a way to include trees that have no trees
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all5_10000_alive <- rbind(surr_info10000, emerg_summ_5)

# export dataframe
write.csv(all5_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_10000_alive_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------10m buffer, 10000 tree sample-----------------
#------------------------------------------------------------------------------#

nonemrg_samp10000_10 <- data.frame()
radius = 10
for (i in 1:nrow(lamdatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp10000$gx[i]
  midy <- lamdatsamp10000$gy[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$gx>=(midx-radius) & dat1$gx<=(midx+radius) & dat1$gy>=(midy-radius) & dat1$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp10000$species[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp10000$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add dat1a to one dat1aframe
  nonemrg_samp10000_10 <- rbind(nonemrg_samp10000_10, temp)
}

# label same species with 1 and different trees with 0
nonemrg_samp10000_10$speciescomp <- ifelse(nonemrg_samp10000_10$species == nonemrg_samp10000_10$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info10000_10 <- nonemrg_samp10000_10 %>% group_by(midtreeID, midtreetype, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                            dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                            height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                            n_trees = n(),
                                                                                                            n_species = n_distinct(species),
                                                                                                            same_species = sum(speciescomp))
# check number of rows, should = 9997
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all10_10000_alive <- rbind(surr_info10000_10, emerg_summ_10)

# export dataframe
write.csv(all5_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_10000_alive_trees.csv")


#-------------------------------------------------------------------------------#
# -----------------------------10000 nonemergent tree sample-----------------
# -----------------------------------5 meter radius--------------------
# -----------------------------------only all trees--------------------
#------------------------------------------------------------------------------#
# Grab random sample from nonemergent trees as a comparison metric
nonemrg_all <- subset(dat, dbh < quantile99dbh)
lamdatsamp10000_all <- sample_n(nonemrg_all, 10000)
#-------------------------------------------------------------------------------#
# -----------------------------5m buffer,   1000 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
nonemrg_samp10000_all <- data.frame()
radius = 5
for (i in 1:nrow(lamdatsamp10000_all))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp10000_all$gx[i]
  midy <- lamdatsamp10000_all$gy[i]
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$gx>=(midx-radius) & dat$gx<=(midx+radius) & dat$gy>=(midy-radius) & dat$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp10000_all$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp10000_all$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp10000_all$species[i], length(nrow(temp)))
  temp$midstatus <-rep(lamdatsamp10000_all$DFstatus[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp10000_all$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add data to one dataframe
  nonemrg_samp10000_all <- rbind(nonemrg_samp10000_all, temp)
}
# rows == 2416

# label dead trees with 1 and alive trees with 0
nonemrg_samp10000_all$status <- ifelse(nonemrg_samp10000_all$DFstatus=="alive", 1, 0)

# label same species with 1 and different trees with 0
nonemrg_samp10000_all$speciescomp <- ifelse(nonemrg_samp10000_all$species == nonemrg_samp10000_all$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info10000_all <- nonemrg_samp10000_all %>% group_by(midtreeID, midtreetype, midstatus, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
## 9970                                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                                         n_trees = n(),
                                                                                                                         n_species = n_distinct(species),
                                                                                                                         n_alive = sum(status),
                                                                                                                         n_dead = n_trees - n_alive,
                                                                                                                         same_species = sum(speciescomp))# check number of rows, should = 299


# rows == 10000

# combine with emergent dataframe
all5_10000_all_all <- rbind(surr_info10000_all, emerg_summ_5_all)

# export dataframe
write.csv(all5_10000_all_all, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_10000_all_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------10m buffer, 10000 tree sample-----------------
#------------------------------------------------------------------------------#
nonemrg_samp10000_all_10 <- data.frame()
radius = 10
for (i in 1:nrow(lamdatsamp10000_all))
{ 
  # define coordinates for tree of interest
  midx <- lamdatsamp10000_all$gx[i]
  midy <- lamdatsamp10000_all$gy[i]
  # crop data to contain only points within the radius of the tree of interest
  temp<-dat[dat$gx>=(midx-radius) & dat$gx<=(midx+radius) & dat$gy>=(midy-radius) & dat$gy<=(midy+radius), ]
  
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(lamdatsamp10000_all$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(lamdatsamp10000_all$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(lamdatsamp10000_all$species[i], length(nrow(temp)))
  temp$midstatus <-rep(lamdatsamp10000_all$DFstatus[i], length(nrow(temp)))
  
  # remove tree of interest
  emerg_ID <- lamdatsamp10000_all$treeID
  temp <- temp[!temp$treeID %in% emerg_ID, ]
  
  # add data to one dataframe
  nonemrg_samp10000_all_10 <- rbind(nonemrg_samp10000_all_10, temp)
}

# label dead trees with 1 and alive trees with 0
nonemrg_samp10000_all_10$status <- ifelse(nonemrg_samp10000_all_10$DFstatus=="alive", 1, 0)

# label same species with 1 and different trees with 0
nonemrg_samp10000_all_10$speciescomp <- ifelse(nonemrg_samp10000_all_10$species == nonemrg_samp10000_all_10$midspecies, 1, 0)

# Summarize based on middle tree ID
surr_info10000_all_10 <- nonemrg_samp10000_all_10 %>% group_by(midtreeID, midtreetype, midspecies) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                                    dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                                    height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                                    n_trees = n(),
                                                                                                                    n_species = n_distinct(species),
                                                                                                                    n_alive = sum(status),
                                                                                                                    n_dead = n_trees - n_alive,
                                                                                                                    same_species = sum(speciescomp))
# 9995
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all10_10000_all_all <- rbind(surr_info10000_all_10, emerg_summ_10_all)

# export dataframe
write.csv(all10_10000_all_all, "~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_10000_all_trees.csv")
