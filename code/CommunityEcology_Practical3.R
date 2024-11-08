# Community Ecology - Practical 3 - Biodiversity Under Pressure             8/11/2024


############################## Package preparation ###################################

my_packages <- c('tidyverse', 'vegan', 'betapart', 'gam', 'scam','gdm','car', 'zetadiv')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

library(tidyverse)
library(vegan)
library(betapart)
library(gam)
library(scam)
library(gdm)
library(car)
library(zetadiv)

#######################################################################################

#In this practical, we will use null models based on permutation algorithms, along with regression models, to explore and interpret alpha and beta diversity values for established alien species in French Polynesia (Society archipielago) and in Hawai'i, and explore what drives species richness and turnover in these two archipielagos.

#First, we load our already-prepared data frames for the analyses:

load("data/Practical3_data/islands_Soc_Haw_null_models_practical.RData")

beta.soc <- beta.pair(dat.soc.pa) # list with Sorensen, Simpson and nestedness beta diversity values stored in each element of the list as a distance matrix format (one value for each pair of islands)
# now, we calculate the average beta diversity across islands for each index
mean(beta.soc$beta.sim)
mean(beta.soc$beta.sne)
mean(beta.soc$beta.sor)
