library(tidyverse)
library(here)
library(skimr)
library(dplyr)
library(stringr)

data90 <- read_csv(here("Desktop", "Research", "R", "Data", "data_clean.csv"))
table(data90$species)
data90 <- filter(data90, species != "Indet")
data95 <- data90
data99 <- data90

quantile90 <-quantile(data$dbh, probs = 0.90, na.rm = TRUE)
quantile95 <-quantile(data$dbh, probs = 0.95, na.rm = TRUE)
quantile99 <-quantile(data$dbh, probs = 0.99, na.rm = TRUE)

#quantile 90-------
table(quantile90)

emergent90 <- filter(data90, dbh >= quantile90)
table(emergent90$dbh)

emergentspecies90 <- unique(emergent90$species)
table(emergentspecies90)

data90$tree_type <- ifelse(data90$species %in% c(emergentspecies90), "emrgnt", "non_emrgnt")
    
#quantile 95---------
table(quantile95)

emergent95 <- filter(data95, dbh >= quantile95)
table(emergent95$dbh)

emergentspecies95 <- unique(emergent95$species)
table(emergentspecies95)

data95$tree_type <- ifelse(data95$species %in% c(emergentspecies95), "emrgnt", "non_emrgnt")

#quantile 99--------
table(quantile99)

emergent99 <- filter(data, dbh >= quantile99)
table(emergent99$dbh)

emergentspecies99 <- unique(emergent99$species)
table(emergentspecies99)

data99$tree_type <- ifelse(data99$species %in% c(emergentspecies99), "emrgnt", "non_emrgnt")

