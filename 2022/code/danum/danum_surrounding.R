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
dan_surr_5_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_5m_600_trees_nci_debug.csv")
# Boxplots
boxplot(dan_surr_5_600$n_trees ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$height99surr ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$heightmeansurr ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$n_emrgnt ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_5_600)

table(dan_surr_5_600$n_emrgnt)

boxplot(dan_surr_5_600$dbhmeansurr ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$same_species ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$same_genus ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$same_family ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$n_species ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$n_genus ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$n_family ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_5_600)

boxplot(dan_surr_5_600$midNCI ~ dan_surr_5_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_5_600)


#-------------------------------------------------#
# ----------10m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_10_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_10m_600_trees_nci.csv")
# Boxplots
boxplot(dan_surr_10_600$n_trees ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$height99surr ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$heightmeansurr ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$n_emrgnt ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_10_600)

table(dan_surr_10_600$n_emrgnt)

boxplot(dan_surr_10_600$dbhmeansurr ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$same_species ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$same_genus ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$same_family ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$n_species ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$n_genus ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$n_family ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_10_600)

boxplot(dan_surr_10_600$midNCI ~ dan_surr_10_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_10_600)

#-------------------------------------------------#
# ----------15m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_15_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_15m_600_trees_nci.csv")

# Boxplots
boxplot(dan_surr_15_600$n_trees ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$height99surr ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$heightmeansurr ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$n_emrgnt ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_15_600)

table(dan_surr_15_600$n_emrgnt)

boxplot(dan_surr_15_600$dbhmeansurr ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$same_species ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$same_genus ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$same_family ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$n_species ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$n_genus ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$n_family ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_15_600)

boxplot(dan_surr_15_600$midNCI ~ dan_surr_15_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_15_600)
#-------------------------------------------------#
# ----------20m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_20_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_20m_600_trees_nci.csv")
# Boxplots
boxplot(dan_surr_20_600$n_trees ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$height99surr ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$heightmeansurr ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$n_emrgnt ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_20_600)

table(dan_surr_20_600$n_emrgnt)

boxplot(dan_surr_20_600$dbhmeansurr ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$same_species ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$same_genus ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$same_family ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$n_species ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$n_genus ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$n_family ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_20_600)

boxplot(dan_surr_20_600$midNCI ~ dan_surr_20_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_20_600)

#-------------------------------------------------#
# ----------25m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_25_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_25m_600_trees_nci.csv")
# Boxplots
boxplot(dan_surr_25_600$n_trees ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$height99surr ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$heightmeansurr ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$n_emrgnt ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_25_600)

table(dan_surr_25_600$n_emrgnt)

boxplot(dan_surr_25_600$dbhmeansurr ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$same_species ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$same_genus ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$same_family ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$n_species ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$n_genus ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$n_family ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_25_600)

boxplot(dan_surr_25_600$midNCI ~ dan_surr_25_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "midNCI",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_25_600)

#-------------------------------------------------#
# ----------30m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_30_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_30m_600_trees_nci.csv")
# Boxplots
boxplot(dan_surr_30_600$n_trees ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$height99surr ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$heightmeansurr ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$n_emrgnt ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_30_600)

table(dan_surr_30_600$n_emrgnt)

boxplot(dan_surr_30_600$dbhmeansurr ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$same_species ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$same_genus ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$same_family ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$n_species ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$n_genus ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$n_family ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_30_600)

boxplot(dan_surr_30_600$midNCI ~ dan_surr_30_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_30_600)


#-------------------------------------------------#
# ----------50m buffer, 600 tree sample-----------------
#-------------------------------------------------#
# Load data
dan_surr_50_600 <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Danum/danum_50m_600_trees_nci.csv")
# Boxplots
boxplot(dan_surr_50_600$n_trees ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Trees", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_trees~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$height99surr ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "99th Percentile Height", 
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(height99surr~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$heightmeansurr ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean Height",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(heightmeansurr~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$n_emrgnt ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Emergents",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_emrgnt~midtreetype, data=dan_surr_50_600)

table(dan_surr_50_600$n_emrgnt)

boxplot(dan_surr_50_600$dbhmeansurr ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Mean DBH",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(dbhmeansurr~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$same_species ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_species~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$same_genus ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_genus~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$same_family ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of same families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(same_family~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$n_species ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Species",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_species~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$n_genus ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Genus",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_genus~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$n_family ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(n_family~midtreetype, data=dan_surr_50_600)

boxplot(dan_surr_50_600$midNCI ~ dan_surr_50_600$midtreetype, main = "", 
        xlab = "Tree Type", ylab = "Number of Families",
        col = c("darkolivegreen2", "mediumpurple1"),
        names = c("Emergent", "Nonemergent"))

t.test(midNCI~midtreetype, data=dan_surr_50_600)

