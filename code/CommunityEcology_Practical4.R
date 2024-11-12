# Community Ecology - Practical 4 - Biodiversity Under Pressure             8/11/2024



############################## Package preparation ###################################

my_packages <- c('tidyverse', 'vegan', 'betapart', 'fields')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

library(tidyverse)
library(vegan)
library(betapart)
library(fields)

#######################################################################################

# In this practical we will un a metacommunity model with different values of dispersal ability and niche width and analyse its results to then explore the process-pattern relationship.
# Instead of coding it ourselves, we will use MCSim R package uploaded in GitHub

