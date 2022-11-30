library(MASS)
library(ISLR)
library(dplyr)
library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)
library(readxl)
library(raster)
library(fgeo)

#-------------------------------------------------#
# ----------5m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_5_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_5m_600_trees.csv")
# Boxplots
boxplot(dan_surr_5_600$n_trees ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_5_600$height99surr ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_5_600$heightmeansurr ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_5_600$dbhmeansurr ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

#-------------------------------------------------#
# ----------10m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_10_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_10m_600_trees.csv")
# Boxplots
boxplot(dan_surr_10_600$n_trees ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_10_600$height99surr ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_10_600$heightmeansurr ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_10_600$dbhmeansurr ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

#-------------------------------------------------#
# ----------5m buffer, 10,300 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_5_10000 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_5m_10000_trees.csv")
# Boxplots
boxplot(dan_surr_5_10000$n_trees ~ dan_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_5_10000$height99surr ~ dan_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_5_10000$heightmeansurr ~ dan_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_5_10000$dbhmeansurr ~ dan_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))


#-------------------------------------------------#
# ----------10m buffer, 10,300 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_10_10000 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_10m_10000_trees.csv")

# Boxplots
boxplot(dan_surr_10_10000$n_trees ~ dan_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_10_10000$height99surr ~ dan_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_10_10000$heightmeansurr ~ dan_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(dan_surr_10_10000$dbhmeansurr ~ dan_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

