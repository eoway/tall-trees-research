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

# Load data
lam_surr <- read_csv("~/Desktop/Research_2022/Data/Southeast_Asia/Lambir/lambir_surrounding_tree_data.csv")

#-------------------------------------------------#
# -------------Exploration timeeee-----------------
#-------------------------------------------------#

# Boxplots
boxplot(lam_surr$n_trees ~ lam_surr$tree_type, main = "", 
        xlab = "tree type", ylab = "n_trees",
        horizontal = TRUE)

boxplot(lam_surr$height99surr ~ lam_surr$tree_type, main = "", 
        xlab = "tree type", ylab = "n_trees",
        horizontal = TRUE)


boxplot(lam_surr$heightmeansurr ~ lam_surr$tree_type, main = "", 
        xlab = "tree type", ylab = "n_trees",
        horizontal = TRUE)

boxplot(lam_surr$dbhmeansurr ~ lam_surr$tree_type, main = "", 
        xlab = "tree type", ylab = "n_trees",
        horizontal = TRUE)
