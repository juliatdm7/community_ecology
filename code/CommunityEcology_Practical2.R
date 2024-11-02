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


# Now we'll select data for the most recent period, and format it into a site-by-species matrix

# We will select the most recent time frame to extract the data from and we will create two dataframes: one for winter season and another one for summer season

##select bird data for the desired period
bird.dat <- bird.dat.tot[which(bird.dat.tot$period=="2008-11" | bird.dat.tot$period=="2007/08-10/11"),c("period","speccode","grid")] # we phrase it like this as the data gathered between 2008 and 2011 is stored with these two phrasings
dim(coords.dat.island)
coords.dat.island <- coords.dat.island[which(coords.dat.island$grid %in% bird.dat$grid),] # this allows us to check which grids actually have bird data: the intersection between all the grids and the grids associated to our data
dim(coords.dat.island)
bird.dat <- bird.dat[which(bird.dat$grid %in% coords.dat.island$grid),]

bird.summer.dat <- bird.dat[which(bird.dat$period=="2008-11"),c("speccode","grid")]
bird.winter.dat <- bird.dat[which(bird.dat$period=="2007/08-10/11"),c("speccode","grid")]

bird.summer.dat$presence <- 1 ##add a column with the valuesto populate the site-by-species data frame
bird.summer.dat.pa <- bird.summer.dat %>%
  pivot_wider(names_from=speccode,values_from=c(presence))
##site-by-species data frames with NAs
list0 <- as.list(rep(0,ncol(bird.summer.dat.pa))) ##values to replace the NAs
names(list0) <- names(bird.summer.dat.pa)
bird.summer.dat.pa <- as.data.frame(bird.summer.dat.pa %>% replace_na(list0)) ##replace the NAs by 0’s
row.names(bird.summer.dat.pa) <- bird.summer.dat.pa$grid
##change row names
bird.summer.dat.pa <- bird.summer.dat.pa[,-1] ##remove the first column with site names
bird.summer.dat.pa <- bird.summer.dat.pa[order(row.names(bird.summer.dat.pa)),]
##sort by grid cell names

bird.winter.dat$presence <- 1
bird.winter.dat.pa <- bird.winter.dat %>% pivot_wider(names_from=speccode,values_from=c(presence))
list0 <- as.list(rep(0,ncol(bird.winter.dat.pa)))
names(list0) <- names(bird.winter.dat.pa)
bird.winter.dat.pa <- as.data.frame(bird.winter.dat.pa %>% replace_na(list0))
row.names(bird.winter.dat.pa) <- bird.winter.dat.pa$grid
bird.winter.dat.pa <- bird.winter.dat.pa[,-1]
bird.winter.dat.pa <- bird.winter.dat.pa[order(row.names(bird.winter.dat.pa)),]

#Now, we need to conform that there are no empty cells, nor species occurring in no cell

which(colSums(bird.summer.dat.pa)==0)
which(rowSums(bird.summer.dat.pa)==0)
which(colSums(bird.winter.dat.pa)==0)
which(rowSums(bird.winter.dat.pa)==0)



## Compute general patterns ##

##1) Gamma diversity
gamma.summer <- ncol(bird.summer.dat.pa)
gamma.winter <- ncol(bird.winter.dat.pa) # gamma diversity is greater in winter than in summer in the main island.

##2) Alfa diversity
alfa.summer <- mean(rowSums(bird.summer.dat.pa)) # the average alfa diversity (diversity per grid) is 83.51
alfa.winter <- mean(rowSums(bird.winter.dat.pa)) # the average alfa diversity (diversity per grid) is 92.224 (also greater)

hist(rowSums(bird.summer.dat.pa), ylim=c(0,800), xlim=c(0,300))
hist(rowSums(bird.winter.dat.pa), ylim=c(0,800), xlim=c(0,300))

##3) Species accumulation curves

par(mfrow=c(1,2))
sac.bird.summer <- vegan::specaccum(bird.summer.dat.pa)
plot(sac.bird.summer$richness, col ="blue", main = "SAC for Summer", ylim=c(0,400))

sac.bird.winter <- vegan::specaccum(bird.winter.dat.pa)
plot(sac.bird.winter$richness, col ="blue", main = "SAC for Winter")

    # From the SAC, it seems that saturation is higher for Winter (close to 400) than for summer (around 260 maybe) season. Also, it looks like summer season reaches saturation faster than winter curve, which means than during the summer you would need to sample less grids to account for most bird diversity, unlike in the winter.

sac.chao.summer <- poolaccum(bird.summer.dat.pa)
plot(sac.bird.summer$richness, col ="blue", ylim=c(0, max(rowMeans(sac.chao.summer$chao))), type="l", main = "SAC for Summer")
points(3:nrow(bird.summer.dat.pa), rowMeans(sac.chao.summer$chao), col="red", type="l")

sac.chao.winter <- poolaccum(bird.winter.dat.pa)
plot(sac.bird.winter$richness, col ="blue", ylim=c(0, max(rowMeans(sac.chao.winter$chao))), main = "SAC for Winter", type="l")
points(3:nrow(bird.winter.dat.pa), rowMeans(sac.chao.winter$chao), col="red", type="l")

# However, it's important to note that in this particular case the chao estimate is not working properly because of the scale we're working on.
# The grid size is too big, which means that one grid could include sampling sites in which species have not been detected (true 0s) and sampling sites where species have been detected (true 1s). However, chao estimator will interpret this as not having encountered all species that could potentially be there, and as a result is over estimating the actual diversity in the island.

beta.summer <- betapart::beta.pair(bird.summer.dat.pa)
