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

