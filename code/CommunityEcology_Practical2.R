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

bird.dat.tot <- bird.dat.tot[which(bird.dat.tot$resolution==10),]
coords.dat <- coords.dat[which(coords.dat$resolution==10),]

map(database="world",regions="UK") # here we visualise the UK oputline
points(coords.dat$long,coords.dat$lat) # here we superimpose the coordinates in coords.dat to this outline

UK <- map(database="world", regions="UK", fill=T)
UK.sf <- st_as_sf(UK) # here we're storing the poligon of the outline of UK into a "Spatial Feature" (sf), which is useful to work with GIS on R
UK.sf <- st_cast(UK.sf, "POLYGON") # once saved as a sf, we change the geometry into a "polygon" (or rather, we make sure that its geometry is identified as a polygon)
plot(st_geometry(UK.sf)) # we use st_geometry() to make sure that we're plotting only the outline and not a filling. In this particular case, this makes it more efficient and we need less computer power to get the same thing. 
points(coords.dat)

coords.sf <- st_as_sf(coords.dat[,1:2],coords = c("long", "lat"), crs = st_crs(UK.sf)) # we're transforming the data in coords.dat into sf

coords.dat.island.ind <- st_intersects(UK.sf,coords.sf)[[15]] # we extract all the points that intersect with the 15th polygon: main island.
coords.dat.island <- coords.dat[coords.dat.island.ind,] # we select the rows in coords.dat that correspond to those points

# And we visualise the output:

plot(st_geometry(UK.sf))
points(coords.dat.island)


