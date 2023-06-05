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

dat <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_2022.csv")

# Define emergent cutoff
quantile99dbh = 95

# Label trees as emergent/nonemergent
dat$tree_type <- ifelse(dat$dbh>=quantile99dbh, "emrgnt", "nonemrgnt")
table(dat$tree_type)
# Subset to only alive trees
dat1 <- subset(dat, DFstatus == "A")

table(dat1$species)

## Create 300 tree sample
## Grab random sample from nonemergent trees as a comparison metric
#nonemrg <- subset(dat1, dbh < quantile99dbh)
#dandatsamp300 <- sample_n(nonemrg, 300)
#dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))
#
#write.csv(dandatsamp300, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_300_sample.csv")
dandatsamp300 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_300_sample.csv")

## Create 10,000 tree sample
## Grab random sample from nonemergent trees as a comparison metric
#dandatsamp10000 <- sample_n(nonemrg, 10000)
#
#write.csv(dandatsamp10000, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_10000_sample.csv")
dandatsamp10000 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_10000_sample.csv")


#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# ------------------------------------5 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg5 <- subset(dat1, tree_type == "emrgnt")
dandatemerg5$NCI<-rep(NA, nrow(dandatemerg5))
# Run function to grab information for every tree within 5 meters of an emergent
# Create empty dataframe
surr_emerg5 <- data.frame()
# Define a radius to grab information from
radius = 5
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg5))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg5$plot_x[i]
  midy <- dandatemerg5$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg5$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
      temp$dis[j]<-sqrt((dandatemerg5$plot_x[i]-temp$plot_x[j])^2+(dandatemerg5$plot_y[i]-temp$plot_y[j])^2)
      NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg5$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 5)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg5$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg5$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg5$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg5$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg5$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg5$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg5 <- rbind(surr_emerg5, temp)
}
# check number of emrgnt midtreetype labels - should be 1245
table(surr_emerg5$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg5$speciescomp <- ifelse(surr_emerg5$species == surr_emerg5$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg5$genuscomp <- ifelse(surr_emerg5$genus == surr_emerg5$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg5$familycomp <- ifelse(surr_emerg5$family == surr_emerg5$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg5$emergnt <- ifelse(surr_emerg5$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_5 <- surr_emerg5 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                      n_trees = n(),
                                                                                      n_species = n_distinct(species),
                                                                                      n_family = n_distinct(family),
                                                                                      n_genus = n_distinct(genus),
                                                                                      same_species = sum(speciescomp),
                                                                                      same_genus = sum(genuscomp),
                                                                                      same_family = sum(familycomp),
                                                                                      n_emrgnt = sum(emergnt))
# should have 271 rows


#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------10 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg10 <- subset(dat1, tree_type == "emrgnt")
dandatemerg10$NCI<-rep(NA, nrow(dandatemerg10))
# Run function to grab information for every tree within 10 meters of an emergent
# Create empty dataframe
surr_emerg10 <- data.frame()
# Define a radius to grab information from
radius = 10
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg10))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg10$plot_x[i]
  midy <- dandatemerg10$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg10$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatemerg10$plot_x[i]-temp$plot_x[j])^2+(dandatemerg10$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg10$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg10$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg10$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg10$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg10$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg10$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg10$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg10 <- rbind(surr_emerg10, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(surr_emerg10$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg10$speciescomp <- ifelse(surr_emerg10$species == surr_emerg10$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg10$genuscomp <- ifelse(surr_emerg10$genus == surr_emerg10$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg10$familycomp <- ifelse(surr_emerg10$family == surr_emerg10$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg10$emergnt <- ifelse(surr_emerg10$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_10 <- surr_emerg10 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                      n_trees = n(),
                                                                                      n_species = n_distinct(species),
                                                                                      n_family = n_distinct(family),
                                                                                      n_genus = n_distinct(genus),
                                                                                      same_species = sum(speciescomp),
                                                                                      same_genus = sum(genuscomp),
                                                                                      same_family = sum(familycomp),
                                                                                      n_emrgnt = sum(emergnt))
# should have 271 rows
#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------30 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg15 <- subset(dat1, tree_type == "emrgnt")
dandatemerg15$NCI<-rep(NA, nrow(dandatemerg15))
# Run function to grab information for every tree within 15 meters of an emergent
# Create empty dataframe
surr_emerg15 <- data.frame()
# Define a radius to grab information from
radius = 15
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg15))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg15$plot_x[i]
  midy <- dandatemerg15$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg15$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatemerg15$plot_x[i]-temp$plot_x[j])^2+(dandatemerg15$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg15$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 15)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg15$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg15$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg15$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg15$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg15$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg15$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg15 <- rbind(surr_emerg15, temp)
}
# check number of emrgnt midtreetype labels - should be 12415
table(surr_emerg15$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg15$speciescomp <- ifelse(surr_emerg15$species == surr_emerg15$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg15$genuscomp <- ifelse(surr_emerg15$genus == surr_emerg15$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg15$familycomp <- ifelse(surr_emerg15$family == surr_emerg15$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg15$emergnt <- ifelse(surr_emerg15$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_15 <- surr_emerg15 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                        dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                        height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                        n_trees = n(),
                                                                                        n_species = n_distinct(species),
                                                                                        n_family = n_distinct(family),
                                                                                        n_genus = n_distinct(genus),
                                                                                        same_species = sum(speciescomp),
                                                                                        same_genus = sum(genuscomp),
                                                                                        same_family = sum(familycomp),
                                                                                        n_emrgnt = sum(emergnt))
# should have 271 rows
#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------40 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg20 <- subset(dat1, tree_type == "emrgnt")
dandatemerg20$NCI<-rep(NA, nrow(dandatemerg20))
# Run function to grab information for every tree within 20 meters of an emergent
# Create empty dataframe
surr_emerg20 <- data.frame()
# Define a radius to grab information from
radius = 20
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg20))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg20$plot_x[i]
  midy <- dandatemerg20$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg20$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatemerg20$plot_x[i]-temp$plot_x[j])^2+(dandatemerg20$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg20$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 20)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg20$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg20$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg20$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg20$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg20$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg20$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg20 <- rbind(surr_emerg20, temp)
}
# check number of emrgnt midtreetype labels - should be 12420
table(surr_emerg20$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg20$speciescomp <- ifelse(surr_emerg20$species == surr_emerg20$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg20$genuscomp <- ifelse(surr_emerg20$genus == surr_emerg20$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg20$familycomp <- ifelse(surr_emerg20$family == surr_emerg20$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg20$emergnt <- ifelse(surr_emerg20$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_20 <- surr_emerg20 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                        dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                        height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                        n_trees = n(),
                                                                                        n_species = n_distinct(species),
                                                                                        n_family = n_distinct(family),
                                                                                        n_genus = n_distinct(genus),
                                                                                        same_species = sum(speciescomp),
                                                                                        same_genus = sum(genuscomp),
                                                                                        same_family = sum(familycomp),
                                                                                        n_emrgnt = sum(emergnt))
# should have 271 rows
#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------50 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg25 <- subset(dat1, tree_type == "emrgnt")
dandatemerg25$NCI<-rep(NA, nrow(dandatemerg25))
# Run function to grab information for every tree within 25 meters of an emergent
# Create empty dataframe
surr_emerg25 <- data.frame()
# Define a radius to grab information from
radius = 25
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg25))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg25$plot_x[i]
  midy <- dandatemerg25$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg25$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatemerg25$plot_x[i]-temp$plot_x[j])^2+(dandatemerg25$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg25$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 25)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg25$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg25$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg25$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg25$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg25$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg25$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg25 <- rbind(surr_emerg25, temp)
}
# check number of emrgnt midtreetype labels - should be 12425
table(surr_emerg25$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg25$speciescomp <- ifelse(surr_emerg25$species == surr_emerg25$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg25$genuscomp <- ifelse(surr_emerg25$genus == surr_emerg25$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg25$familycomp <- ifelse(surr_emerg25$family == surr_emerg25$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg25$emergnt <- ifelse(surr_emerg25$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_25 <- surr_emerg25 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                        dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                        height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                        n_trees = n(),
                                                                                        n_species = n_distinct(species),
                                                                                        n_family = n_distinct(family),
                                                                                        n_genus = n_distinct(genus),
                                                                                        same_species = sum(speciescomp),
                                                                                        same_genus = sum(genuscomp),
                                                                                        same_family = sum(familycomp),
                                                                                        n_emrgnt = sum(emergnt))
# should have 271 rows
#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------60 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg30 <- subset(dat1, tree_type == "emrgnt")
dandatemerg30$NCI<-rep(NA, nrow(dandatemerg30))
# Run function to grab information for every tree within 30 meters of an emergent
# Create empty dataframe
surr_emerg30 <- data.frame()
# Define a radius to grab information from
radius = 30
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg30))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg30$plot_x[i]
  midy <- dandatemerg30$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg30$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatemerg30$plot_x[i]-temp$plot_x[j])^2+(dandatemerg30$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg30$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 30)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg30$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg30$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg30$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg30$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg30$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg30$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg30 <- rbind(surr_emerg30, temp)
}
# check number of emrgnt midtreetype labels - should be 12430
table(surr_emerg30$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg30$speciescomp <- ifelse(surr_emerg30$species == surr_emerg30$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg30$genuscomp <- ifelse(surr_emerg30$genus == surr_emerg30$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg30$familycomp <- ifelse(surr_emerg30$family == surr_emerg30$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg30$emergnt <- ifelse(surr_emerg30$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_30 <- surr_emerg30 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                        dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                        height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                        n_trees = n(),
                                                                                        n_species = n_distinct(species),
                                                                                        n_family = n_distinct(family),
                                                                                        n_genus = n_distinct(genus),
                                                                                        same_species = sum(speciescomp),
                                                                                        same_genus = sum(genuscomp),
                                                                                        same_family = sum(familycomp),
                                                                                        n_emrgnt = sum(emergnt))


#-------------------------------------------------------------------------------#
# ---------------------emergent trees- surrounding dataframe-----------------
# -----------------------------------100 meter----------------------------------#
# ---------------------------only alive trees----------------------------------#
#------------------------------------------------------------------------------#
# Subset to include only emergent trees
dandatemerg50 <- subset(dat1, tree_type == "emrgnt")
dandatemerg50$NCI<-rep(NA, nrow(dandatemerg50))
# Run function to grab information for every tree within 50 meters of an emergent
# Create empty dataframe
surr_emerg50 <- data.frame()
# Define a radius to grab information from
radius = 50
# Loop through every emergent tree
for (i in 1:nrow(dandatemerg50))
{ 
  # define coordinates for tree of interest
  midx <- dandatemerg50$plot_x[i]
  midy <- dandatemerg50$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatemerg50$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatemerg50$plot_x[i]-temp$plot_x[j])^2+(dandatemerg50$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatemerg50$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 50)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatemerg50$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatemerg50$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatemerg50$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatemerg50$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatemerg50$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatemerg50$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  surr_emerg50 <- rbind(surr_emerg50, temp)
}
# check number of emrgnt midtreetype labels - should be 12450
table(surr_emerg50$midtreetype)

# label same species with 1 and different trees with 0
surr_emerg50$speciescomp <- ifelse(surr_emerg50$species == surr_emerg50$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
surr_emerg50$genuscomp <- ifelse(surr_emerg50$genus == surr_emerg50$midgenus, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg50$familycomp <- ifelse(surr_emerg50$family == surr_emerg50$midfamily, 1, 0)

# label same family with 1 and different trees with 0
surr_emerg50$emergnt <- ifelse(surr_emerg50$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
emerg_summ_50 <- surr_emerg50 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                n_trees = n(),
                                                                                                n_species = n_distinct(species),
                                                                                                n_family = n_distinct(family),
                                                                                                n_genus = n_distinct(genus),
                                                                                                same_species = sum(speciescomp),
                                                                                                same_genus = sum(genuscomp),
                                                                                                same_family = sum(familycomp),
                                                                                                n_emrgnt = sum(emergnt))
# should have 271 rows


#-------------------------------------------------------------------------------#
# -----------------------------300 nonemergent tree sample-----------------
# -----------------------------------5 meter radius--------------------
# -----------------------------------only alive trees--------------------
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
# run the following to create a dataframe of all trees within 5 meters of the tree of interest
nonemrg_samp300 <- data.frame()
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))

radius = 5
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 5)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300 <- rbind(nonemrg_samp300, temp)
}
# check number of emrgnt midtreetype labels - should be 1245
table(nonemrg_samp300$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300$speciescomp <- ifelse(nonemrg_samp300$species == nonemrg_samp300$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300$genuscomp <- ifelse(nonemrg_samp300$genus == nonemrg_samp300$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300$familycomp <- ifelse(nonemrg_samp300$family == nonemrg_samp300$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300$emergnt <- ifelse(nonemrg_samp300$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info300 <- nonemrg_samp300 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                        dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                        height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                        n_trees = n(),
                                                                                        n_species = n_distinct(species),
                                                                                        n_family = n_distinct(family),
                                                                                        n_genus = n_distinct(genus),
                                                                                        same_species = sum(speciescomp),
                                                                                        same_genus = sum(genuscomp),
                                                                                        same_family = sum(familycomp),
                                                                                        n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all5_300_alive <- rbind(surr_info300, emerg_summ_5)

# export dataframe
write.csv(all5_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_5m_600_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------10m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))
nonemrg_samp300_10 <- data.frame()

radius = 10
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300_10 <- rbind(nonemrg_samp300_10, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp300_10$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300_10$speciescomp <- ifelse(nonemrg_samp300_10$species == nonemrg_samp300_10$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300_10$genuscomp <- ifelse(nonemrg_samp300_10$genus == nonemrg_samp300_10$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_10$familycomp <- ifelse(nonemrg_samp300_10$family == nonemrg_samp300_10$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_10$emergnt <- ifelse(nonemrg_samp300_10$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info300_10 <- nonemrg_samp300_10 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                          dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                          height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                          n_trees = n(),
                                                                                          n_species = n_distinct(species),
                                                                                          n_family = n_distinct(family),
                                                                                          n_genus = n_distinct(genus),
                                                                                          same_species = sum(speciescomp),
                                                                                          same_genus = sum(genuscomp),
                                                                                          same_family = sum(familycomp),
                                                                                          n_emrgnt = sum(emergnt))
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all10_300_alive <- rbind(surr_info300_10, emerg_summ_10)

# export dataframe
write.csv(all10_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_10m_600_trees.csv")


#-------------------------------------------------------------------------------#
# -----------------------------30m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))

nonemrg_samp300_15 <- data.frame()

radius = 15
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 15)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300_15 <- rbind(nonemrg_samp300_15, temp)
}
# check number of emrgnt midtreetype labels - should be 12415
table(nonemrg_samp300_15$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300_15$speciescomp <- ifelse(nonemrg_samp300_15$species == nonemrg_samp300_15$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300_15$genuscomp <- ifelse(nonemrg_samp300_15$genus == nonemrg_samp300_15$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_15$familycomp <- ifelse(nonemrg_samp300_15$family == nonemrg_samp300_15$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_15$emergnt <- ifelse(nonemrg_samp300_15$tree_type == "emrgnt", 1, 0)

bignci <- subset(nonemrg_samp300_15, midNCI >= 30000)

# Summarize based on middle tree ID
surr_info300_15 <- nonemrg_samp300_15 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                n_trees = n(),
                                                                                                n_species = n_distinct(species),
                                                                                                n_family = n_distinct(family),
                                                                                                n_genus = n_distinct(genus),
                                                                                                same_species = sum(speciescomp),
                                                                                                same_genus = sum(genuscomp),
                                                                                                same_family = sum(familycomp),
                                                                                                n_emrgnt = sum(emergnt))

# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all15_300_alive <- rbind(surr_info300_15, emerg_summ_15)

# export dataframe
write.csv(all15_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_15m_600_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------40m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))

nonemrg_samp300_20 <- data.frame()

radius = 20
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 20)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300_20 <- rbind(nonemrg_samp300_20, temp)
}
# check number of emrgnt midtreetype labels - should be 12420
table(nonemrg_samp300_20$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300_20$speciescomp <- ifelse(nonemrg_samp300_20$species == nonemrg_samp300_20$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300_20$genuscomp <- ifelse(nonemrg_samp300_20$genus == nonemrg_samp300_20$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_20$familycomp <- ifelse(nonemrg_samp300_20$family == nonemrg_samp300_20$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_20$emergnt <- ifelse(nonemrg_samp300_20$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info300_20 <- nonemrg_samp300_20 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                n_trees = n(),
                                                                                                n_species = n_distinct(species),
                                                                                                n_family = n_distinct(family),
                                                                                                n_genus = n_distinct(genus),
                                                                                                same_species = sum(speciescomp),
                                                                                                same_genus = sum(genuscomp),
                                                                                                same_family = sum(familycomp),
                                                                                                n_emrgnt = sum(emergnt))
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all20_300_alive <- rbind(surr_info300_20, emerg_summ_20)

# export dataframe
write.csv(all20_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_20m_600_trees.csv")



#-------------------------------------------------------------------------------#
# -----------------------------50m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))

nonemrg_samp300_25 <- data.frame()

radius = 25
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 25)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300_25 <- rbind(nonemrg_samp300_25, temp)
}
# check number of emrgnt midtreetype labels - should be 12425
table(nonemrg_samp300_25$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300_25$speciescomp <- ifelse(nonemrg_samp300_25$species == nonemrg_samp300_25$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300_25$genuscomp <- ifelse(nonemrg_samp300_25$genus == nonemrg_samp300_25$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_25$familycomp <- ifelse(nonemrg_samp300_25$family == nonemrg_samp300_25$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_25$emergnt <- ifelse(nonemrg_samp300_25$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info300_25 <- nonemrg_samp300_25 %>% group_by(midtreeID, midtreetype,midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                n_trees = n(),
                                                                                                n_species = n_distinct(species),
                                                                                                n_family = n_distinct(family),
                                                                                                n_genus = n_distinct(genus),
                                                                                                same_species = sum(speciescomp),
                                                                                                same_genus = sum(genuscomp),
                                                                                                same_family = sum(familycomp),
                                                                                                n_emrgnt = sum(emergnt))
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all25_300_alive <- rbind(surr_info300_25, emerg_summ_25)

# export dataframe
write.csv(all25_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_25m_600_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------60m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))

nonemrg_samp300_30 <- data.frame()

radius = 30
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 30)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300_30 <- rbind(nonemrg_samp300_30, temp)
}
# check number of emrgnt midtreetype labels - should be 12430
table(nonemrg_samp300_30$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300_30$speciescomp <- ifelse(nonemrg_samp300_30$species == nonemrg_samp300_30$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300_30$genuscomp <- ifelse(nonemrg_samp300_30$genus == nonemrg_samp300_30$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_30$familycomp <- ifelse(nonemrg_samp300_30$family == nonemrg_samp300_30$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_30$emergnt <- ifelse(nonemrg_samp300_30$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info300_30 <- nonemrg_samp300_30 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                n_trees = n(),
                                                                                                n_species = n_distinct(species),
                                                                                                n_family = n_distinct(family),
                                                                                                n_genus = n_distinct(genus),
                                                                                                same_species = sum(speciescomp),
                                                                                                same_genus = sum(genuscomp),
                                                                                                same_family = sum(familycomp),
                                                                                                n_emrgnt = sum(emergnt)
                                                                                                )
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all30_300_alive <- rbind(surr_info300_30, emerg_summ_30)

# export dataframe
write.csv(all30_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_30m_600_trees.csv")


#-------------------------------------------------------------------------------#
# -----------------------------50m buffer, 600 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp300$NCI<-rep(NA, nrow(dandatsamp300))

nonemrg_samp300_50 <- data.frame()

radius = 50
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp300))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp300$plot_x[i]
  midy <- dandatsamp300$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp300$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp300$plot_x[i]-temp$plot_x[j])^2+(dandatsamp300$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp300$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 50)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp300$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp300$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp300$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp300$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp300$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp300$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp300_50 <- rbind(nonemrg_samp300_50, temp)
}
# check number of emrgnt midtreetype labels - should be 12450
table(nonemrg_samp300_50$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp300_50$speciescomp <- ifelse(nonemrg_samp300_50$species == nonemrg_samp300_50$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp300_50$genuscomp <- ifelse(nonemrg_samp300_50$genus == nonemrg_samp300_50$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_50$familycomp <- ifelse(nonemrg_samp300_50$family == nonemrg_samp300_50$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp300_50$emergnt <- ifelse(nonemrg_samp300_50$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info300_50 <- nonemrg_samp300_50 %>% group_by(midtreeID, midtreetype,midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                       dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                       height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                       n_trees = n(),
                                                                                                       n_species = n_distinct(species),
                                                                                                       n_family = n_distinct(family),
                                                                                                       n_genus = n_distinct(genus),
                                                                                                       same_species = sum(speciescomp),
                                                                                                       same_genus = sum(genuscomp),
                                                                                                       same_family = sum(familycomp),
                                                                                                       n_emrgnt = sum(emergnt))
# check number of rows, should = 299
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all50_300_alive <- rbind(surr_info300_50, emerg_summ_50)

# export dataframe
write.csv(all50_300_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_50m_600_trees.csv")






















#-------------------------------------------------------------------------------#
# -----------------------------10000 nonemergent tree sample-----------------
# -----------------------------------5 meter radius--------------------
# -----------------------------------only alive trees--------------------
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# -----------------------------5m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000 <- data.frame()

radius = 5
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 5)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000 <- rbind(nonemrg_samp10000, temp)
}
# check number of emrgnt midtreetype labels - should be 1245
table(nonemrg_samp10000$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000$speciescomp <- ifelse(nonemrg_samp10000$species == nonemrg_samp10000$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000$genuscomp <- ifelse(nonemrg_samp10000$genus == nonemrg_samp10000$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000$familycomp <- ifelse(nonemrg_samp10000$family == nonemrg_samp10000$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000$emergnt <- ifelse(nonemrg_samp10000$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                  dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                  height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                  n_trees = n(),
                                                                                                  n_species = n_distinct(species),
                                                                                                  n_family = n_distinct(family),
                                                                                                  n_genus = n_distinct(genus),
                                                                                                  same_species = sum(speciescomp),
                                                                                                  same_genus = sum(genuscomp),
                                                                                                  same_family = sum(familycomp),
                                                                                                  n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all5_10000_alive <- rbind(surr_info10000, emerg_summ_5)

# export dataframe
write.csv(all5_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_5m_10000_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------20m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000_10 <- data.frame()

radius = 10
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000_10 <- rbind(nonemrg_samp10000_10, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp10000_10$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000_10$speciescomp <- ifelse(nonemrg_samp10000_10$species == nonemrg_samp10000_10$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000_10$genuscomp <- ifelse(nonemrg_samp10000_10$genus == nonemrg_samp10000_10$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_10$familycomp <- ifelse(nonemrg_samp10000_10$family == nonemrg_samp10000_10$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_10$emergnt <- ifelse(nonemrg_samp10000_10$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000_10 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                      dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                      height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                      n_trees = n(),
                                                                                                      n_species = n_distinct(species),
                                                                                                      n_family = n_distinct(family),
                                                                                                      n_genus = n_distinct(genus),
                                                                                                      same_species = sum(speciescomp),
                                                                                                      same_genus = sum(genuscomp),
                                                                                                      same_family = sum(familycomp),
                                                                                                      n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all10_10000_alive <- rbind(surr_info10000, emerg_summ_10)

# export dataframe
write.csv(all10_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_10m_10000_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------30m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000_15 <- data.frame()

radius = 15
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000_15 <- rbind(nonemrg_samp10000_15, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp10000_15$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000_15$speciescomp <- ifelse(nonemrg_samp10000_15$species == nonemrg_samp10000_15$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000_15$genuscomp <- ifelse(nonemrg_samp10000_15$genus == nonemrg_samp10000_15$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_15$familycomp <- ifelse(nonemrg_samp10000_15$family == nonemrg_samp10000_15$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_15$emergnt <- ifelse(nonemrg_samp10000_15$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000_15 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                         n_trees = n(),
                                                                                                         n_species = n_distinct(species),
                                                                                                         n_family = n_distinct(family),
                                                                                                         n_genus = n_distinct(genus),
                                                                                                         same_species = sum(speciescomp),
                                                                                                         same_genus = sum(genuscomp),
                                                                                                         same_family = sum(familycomp),
                                                                                                         n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all15_10000_alive <- rbind(surr_info10000, emerg_summ_15)

# export dataframe
write.csv(all15_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_15m_10000_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------40m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000_20 <- data.frame()

radius = 20
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000_20 <- rbind(nonemrg_samp10000_20, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp10000_20$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000_20$speciescomp <- ifelse(nonemrg_samp10000_20$species == nonemrg_samp10000_20$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000_20$genuscomp <- ifelse(nonemrg_samp10000_20$genus == nonemrg_samp10000_20$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_20$familycomp <- ifelse(nonemrg_samp10000_20$family == nonemrg_samp10000_20$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_20$emergnt <- ifelse(nonemrg_samp10000_20$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000_20 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                         n_trees = n(),
                                                                                                         n_species = n_distinct(species),
                                                                                                         n_family = n_distinct(family),
                                                                                                         n_genus = n_distinct(genus),
                                                                                                         same_species = sum(speciescomp),
                                                                                                         same_genus = sum(genuscomp),
                                                                                                         same_family = sum(familycomp),
                                                                                                         n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all20_10000_alive <- rbind(surr_info10000, emerg_summ_20)

# export dataframe
write.csv(all20_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_20m_10000_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------50m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000_25 <- data.frame()

radius = 25
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000_25 <- rbind(nonemrg_samp10000_25, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp10000_25$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000_25$speciescomp <- ifelse(nonemrg_samp10000_25$species == nonemrg_samp10000_25$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000_25$genuscomp <- ifelse(nonemrg_samp10000_25$genus == nonemrg_samp10000_25$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_25$familycomp <- ifelse(nonemrg_samp10000_25$family == nonemrg_samp10000_25$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_25$emergnt <- ifelse(nonemrg_samp10000_25$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000_25 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                         n_trees = n(),
                                                                                                         n_species = n_distinct(species),
                                                                                                         n_family = n_distinct(family),
                                                                                                         n_genus = n_distinct(genus),
                                                                                                         same_species = sum(speciescomp),
                                                                                                         same_genus = sum(genuscomp),
                                                                                                         same_family = sum(familycomp),
                                                                                                         n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all25_10000_alive <- rbind(surr_info10000, emerg_summ_25)

# export dataframe
write.csv(all25_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_25m_10000_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------60m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000_30 <- data.frame()

radius = 30
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000_30 <- rbind(nonemrg_samp10000_30, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp10000_30$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000_30$speciescomp <- ifelse(nonemrg_samp10000_30$species == nonemrg_samp10000_30$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000_30$genuscomp <- ifelse(nonemrg_samp10000_30$genus == nonemrg_samp10000_30$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_30$familycomp <- ifelse(nonemrg_samp10000_30$family == nonemrg_samp10000_30$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_30$emergnt <- ifelse(nonemrg_samp10000_30$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000_30 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                         n_trees = n(),
                                                                                                         n_species = n_distinct(species),
                                                                                                         n_family = n_distinct(family),
                                                                                                         n_genus = n_distinct(genus),
                                                                                                         same_species = sum(speciescomp),
                                                                                                         same_genus = sum(genuscomp),
                                                                                                         same_family = sum(familycomp),
                                                                                                         n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all30_10000_alive <- rbind(surr_info10000, emerg_summ_30)

# export dataframe
write.csv(all30_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_30m_10000_trees.csv")

#-------------------------------------------------------------------------------#
# -----------------------------100m buffer, 1000 tree sample-----------------
#------------------------------------------------------------------------------#
dandatsamp10000$NCI<-rep(NA, nrow(dandatsamp10000))

nonemrg_samp10000_50 <- data.frame()

radius = 50
# Loop through every emergent tree
for (i in 1:nrow(dandatsamp10000))
{ 
  # define coordinates for tree of interest
  midx <- dandatsamp10000$plot_x[i]
  midy <- dandatsamp10000$plot_y[i]
  # crop dat1a to contain only points within the radius of the tree of interest
  temp<-dat1[dat1$plot_x>=(midx-radius) & dat1$plot_x<=(midx+radius) & dat1$plot_y>=(midy-radius) & dat1$plot_y<=(midy+radius), ]
  # remove tree of interest
  temp <- subset(temp, treeID != dandatsamp10000$treeID[i])
  # calculate NCI and distance from tree of interest
  temp$dis<-rep(NA, nrow(temp))
  NCI = 0
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((dandatsamp10000$plot_x[i]-temp$plot_x[j])^2+(dandatsamp10000$plot_y[i]-temp$plot_y[j])^2)
    NCI<-NCI+sum((temp$dbh[j]^2/temp$dis[j]^2)[temp$dis[j]<=radius & temp$dis[j]>0],na.rm=T)
  }
  dandatsamp10000$NCI[i] <- NCI
  # remove trees outside of buffer
  temp <- subset(temp, dis <= 10)
  # make a column of that contains the tree of interest's ID and treetype
  temp$midtreeID <-rep(dandatsamp10000$treeID[i], length(nrow(temp)))
  temp$midtreetype <-rep(dandatsamp10000$tree_type[i], length(nrow(temp)))
  temp$midspecies <-rep(dandatsamp10000$species[i], length(nrow(temp)))
  temp$midgenus <-rep(dandatsamp10000$genus[i], length(nrow(temp)))
  temp$midfamily <-rep(dandatsamp10000$family[i], length(nrow(temp)))
  temp$midNCI <-rep(dandatsamp10000$NCI[i], length(nrow(temp)))
  # add dat1a to one dat1aframe
  nonemrg_samp10000_50 <- rbind(nonemrg_samp10000_50, temp)
}
# check number of emrgnt midtreetype labels - should be 12410
table(nonemrg_samp10000_50$midtreetype)

# label same species with 1 and different trees with 0
nonemrg_samp10000_50$speciescomp <- ifelse(nonemrg_samp10000_50$species == nonemrg_samp10000_50$midspecies, 1, 0)

# label same genus with 1 and different trees with 0
nonemrg_samp10000_50$genuscomp <- ifelse(nonemrg_samp10000_50$genus == nonemrg_samp10000_50$midgenus, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_50$familycomp <- ifelse(nonemrg_samp10000_50$family == nonemrg_samp10000_50$midfamily, 1, 0)

# label same family with 1 and different trees with 0
nonemrg_samp10000_50$emergnt <- ifelse(nonemrg_samp10000_50$tree_type == "emrgnt", 1, 0)

# Summarize based on middle tree ID
surr_info10000 <- nonemrg_samp10000_50 %>% group_by(midtreeID, midtreetype, midNCI) %>% dplyr::summarize(heightmeansurr=mean(height, na.rm = TRUE),
                                                                                                         dbhmeansurr=mean(dbh, na.rm = TRUE),
                                                                                                         height99surr = quantile(height, probs = 0.99, na.rm = TRUE),
                                                                                                         n_trees = n(),
                                                                                                         n_species = n_distinct(species),
                                                                                                         n_family = n_distinct(family),
                                                                                                         n_genus = n_distinct(genus),
                                                                                                         same_species = sum(speciescomp),
                                                                                                         same_genus = sum(genuscomp),
                                                                                                         same_family = sum(familycomp),
                                                                                                         n_emrgnt = sum(emergnt))
# check number of rows, should = 
# need to find a way to keep in that one has no surrounding trees

# combine with emergent dataframe
all50_10000_alive <- rbind(surr_info10000, emerg_summ_50)

# export dataframe
write.csv(all50_10000_alive, "~/Desktop/Research_2022/Data/Southeast_Asia/Danum_surrounding/danum_50m_10000_trees.csv")
