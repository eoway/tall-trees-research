library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)

data <- read_csv(here("Desktop", "Research", "R", "Data", "data_clean.csv"))

#Calculate Heights------
#use the Feldpuasch et al 2011 equation with the Southeast Asia regional coefficients
dbh2h_01 <- function(dbh,hgt_max,hgt_ref,b1Ht,b2Ht){ # exclude hgt_ref here if using first dbh_crit eq.
  #  dbh_crit <- exp((log(hgt_max)-b1Ht)/b2Ht)
  dbh_crit <- exp(-0.5 / hgt_ref * (b2Ht - sqrt(b2Ht**2 - 4 * hgt_ref * (b1Ht - log(hgt_max)))))
  h <- ifelse(dbh <= dbh_crit,
              exp(b1Ht + b2Ht * log(dbh)),
              exp(b1Ht + b2Ht * log(dbh_crit)))
  return(h)
}

# Southeast Asia regional parameters
b1Ht_SEA    = 0.5279284 * log(10) # Use for dbh2h_01
# SAME AS: b1Ht_SEA = 1.2156
b2Ht_SEA    = 0.5782 #"coefficient of ln(D)" # Use for dbh2h_01
hgt_ref_SEA = -0.0114
hgt_max_SEA = 100

data$height <- dbh2h_01(data$dbh, hgt_max_SEA, hgt_ref_SEA, b1Ht_SEA, b2Ht_SEA)
table(data$height)

#variables------
data90h <- data
table(data90h$species)
data90h <- filter(data90h, species != "Indet")
data95h <- data90h
data99h <- data90h
quantile90h <-quantile(data90h$height, probs = 0.90, na.rm = TRUE)
quantile95h <-quantile(data95h$height, probs = 0.95, na.rm = TRUE)
quantile99h <-quantile(data99h$height, probs = 0.99, na.rm = TRUE)

#quantile 90-------
table(quantile90h)

emergent90h <- filter(data90h, dbh >= quantile90h)
table(emergent90h$dbh)

emergentspecies90h <- unique(emergent90h$species)
table(emergentspecies90h)

data90h$tree_type <- ifelse(data90h$species %in% c(emergentspecies90h), "emrgnt", "non_emrgnt")


#quantile 95-------
table(quantile95h)

emergent95h <- filter(data95h, dbh >= quantile95h)
table(emergent95h$dbh)

emergentspecies95h <- unique(emergent95h$species)
table(emergentspecies95h)

data95h$tree_type <- ifelse(data95h$species %in% c(emergentspecies95h), "emrgnt", "non_emrgnt")


#quantile 99-------
table(quantile99h)

emergent99h <- filter(data99h, dbh >= quantile99h)
table(emergent99h$dbh)

emergentspecies99h <- unique(emergent99h$species)
table(emergentspecies99h)

data99h$tree_type <- ifelse(data99h$species %in% c(emergentspecies99h), "emrgnt", "non_emrgnt")

