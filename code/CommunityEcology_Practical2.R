# Community Ecology - Practical 2 - Biodiversity Under Pressure           1/11/2024


## Designing a network of protected areas ##

# We will look into community patterns of bird communities and compare them between different special protected areas in Great Britain.

# First, we need to load packages that we will be using:

my_packages <- c('tidyverse', 'maps', 'sf', 'vegan', 'betapart')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

library(tidyverse)
library(maps)
library(sf)
library(vegan)
library(betapart)

# Now, we load the data we will be working with

bird.dat.tot <- read.csv("data/atlas_open_data_files/distributions.csv")
coords.dat <- read.csv("data/atlas_open_data_files/grid_square_coordinates_lookup.csv")
