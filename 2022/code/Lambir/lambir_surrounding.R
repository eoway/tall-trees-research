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

#lam_surr_5_600_alive <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_600_alive_trees.csv")
#lam_surr_600_emerg <- subset(lam_surr_5_600, lam_surr_5_600$midtreetype == "emrgnt")
#lam_surr_5_10000 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_10000_trees.csv")
#lam_surr_10000_emerg <- subset(lam_surr_5_10000, lam_surr_5_10000$midtreetype == "emrgnt")

# gut check the sample
# dim, summary, make sure its not doing something funky to emergents, check for sampling with replace

median(lam_surr_600_emerg$dbhmeansurr)
median(lam_surr_10000_emerg$dbhmeansurr)

median(lam_surr_600_emerg$n_trees)
median(lam_surr_10000_emerg$n_trees)

#-------------------------------------------------#
# ----------ALIVE TREES ONLY----------------
#-------------------------------------------------#
#-------------------------------------------------#
# ----------5m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_5_600_alive <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_600_alive_trees.csv")
# Boxplots
boxplot(lam_surr_5_600_alive$n_trees ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$height99surr ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$heightmeansurr ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$dbhmeansurr ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$same_species ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$same_species ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$n_species ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

#-------------------------------------------------#
# ----------10m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_10_600_alive <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_600_alive_trees.csv")
# Boxplots
boxplot(lam_surr_10_600_alive$n_trees ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_alive$height99surr ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_alive$heightmeansurr ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_alive$dbhmeansurr ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))
boxplot(lam_surr_10_600_alive$same_species ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=lam_surr_10_600_alive)

boxplot(lam_surr_10_600_alive$n_species ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=lam_surr_10_600_alive)

#-------------------------------------------------#
# ----------ALL TREES----------------
#-------------------------------------------------#
#-------------------------------------------------#
# ----------5m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_5_600_all <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_600_all_trees.csv")
# Boxplots
boxplot(lam_surr_5_600_all$n_trees ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$height99surr ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$heightmeansurr ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$dbhmeansurr ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$same_species ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$n_species ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$n_alive ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number Alive",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_all$n_dead ~ lam_surr_5_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number Dead",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

#-------------------------------------------------#
# ----------10m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_10_600_all <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_600_all_trees.csv")
# Boxplots
boxplot(lam_surr_10_600_all$n_trees ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$height99surr ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$heightmeansurr ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$dbhmeansurr ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$same_species ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$n_species ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$n_alive ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number Alive",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_all$n_dead ~ lam_surr_10_600_all$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number Dead",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))
#-------------------------------------------------#
# ----------5m buffer, 10,300 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_5_10000 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_10000_trees.csv")
# Boxplots
boxplot(lam_surr_5_10000$n_trees ~ lam_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_10000$height99surr ~ lam_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_10000$heightmeansurr ~ lam_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_10000$dbhmeansurr ~ lam_surr_5_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))


#-------------------------------------------------#
# ----------5m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_5_600_alive <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_5m_600_alive_trees.csv")
# Boxplots
boxplot(lam_surr_5_600_alive$n_trees ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$height99surr ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$heightmeansurr ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_5_600_alive$dbhmeansurr ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))
boxplot(lam_surr_5_600_alive$same_species ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))
boxplot(lam_surr_5_600_alive$n_species ~ lam_surr_5_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

#-------------------------------------------------#
# ----------10m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_10_600_alive <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_600_alive_trees.csv")
# Boxplots
boxplot(lam_surr_10_600_alive$n_trees ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_alive$height99surr ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_alive$heightmeansurr ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_600_alive$dbhmeansurr ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))
boxplot(lam_surr_10_600_alive$same_species ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))
boxplot(lam_surr_10_600_alive$n_species ~ lam_surr_10_600_alive$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))







#-------------------------------------------------#
# ----------10m buffer, 10,300 tree sample-----------------
#-------------------------------------------------#
# Load data
lam_surr_10_10000 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_10m_10000_trees.csv")
# Boxplots
boxplot(lam_surr_10_10000$n_trees ~ lam_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_10000$height99surr ~ lam_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_10000$heightmeansurr ~ lam_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

boxplot(lam_surr_10_10000$dbhmeansurr ~ lam_surr_10_10000$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

