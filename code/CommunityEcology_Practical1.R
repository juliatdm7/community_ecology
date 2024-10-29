# Comunity Ecology - Practical 1 - Biodiversity Under Pressure        29/10/2024


install.packages("tidyverse")
install.packages("vegan")
install.packages("betapart")

library(tidyverse)
library(vegan)
library(betapart)

#We will examine patterns for the Society archipelago in French Polynesia (14 islands), the Marquesas archipelago (10 islands), also in French Polynesia, but more remote and closer to the equator, the Hawaiian archipelago (13 islands), with bigger islands, the Fiji archipelago (49 islands), with many more islands, and the Samoa archipelago (12 islands, two big ones and 10 much smaller around). 
#These archipelagos therefore have different levels of remoteness, spatial scales, population size, and are in different hemispheres
#How do these different factors affect plant invasion?:
## Smaller islands will most likely sustain smaller populations, which are more susceptible to detrimental effects by invasive plants, f.e.

dat.all <- read.csv("data/PaciFlora_data/Species_list_full_2905.csv", sep = ";")
View(dat.all) # this data frame is in a “tidy” format, i.e. each row represents a unique combination of site and species, with additional information about these two elements

# we need to remove rows with NAs for species or islands:

speciesNA <- c(which(is.na(dat.all$species))) ##which species have NA’s
islandsNA <- c(which(is.na(dat.all$island))) ##which islands have NA’s
new.dat.all <- dat.all[-speciesNA,]
new.dat.all <- new.dat.all[-islandsNA,]
View(new.dat.all)

# now, ideally we would clean the data to make sure that there are no synonym species; we'll skip that for now

# we create 6 new data frames from new.dat.all, each of which will include all entries corresponding to the archipielago of interest:

dat.soc <- new.dat.all[which(new.dat.all$islandgroup=="Society"),]
dat.mar <- new.dat.all[which(new.dat.all$islandgroup=="Marquesas"),]
dat.haw <- new.dat.all[which(new.dat.all$islandgroup=="Hawaiian"),]
dat.sam <- new.dat.all[which(new.dat.all$islandgroup=="Samoa"),]
dat.fij <- new.dat.all[which(new.dat.all$islandgroup=="Fiji"),]

View(dat.soc)
View(dat.mar)
View(dat.haw)
View(dat.sam)
View(dat.fij)

# now, to compute community patterns, we need site-by-species dataframes for each archipielago, which we can achieve using the following bit of code:

# for Society archipielago:

dat.soc.red <- dat.soc[,c("species","island")] # we create a dataframe including only species and island data
dat.soc.red$presence <- 1  # we create a new column in this dataframe in which we'll indicate presence/absence of each of this species 
dat.soc.pa <- dat.soc.red %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=species,values_from=c(presence)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.soc.pa))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.soc.pa) # we assign the species names to this list of 0s
dat.soc.pa <- as.data.frame(dat.soc.pa %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.soc.pa) <- dat.soc.pa$island # here we give to each row the name of its corresponding island
dat.soc.pa <- dat.soc.pa[,-1] # we remove the island column because we don't need it anymore, as we have given island names to all rows

# we can now repeat this for all archipielagos

# Marquesas archipielago #
dat.mar.red <- dat.mar[,c("species","island")] 
dat.mar.red$presence <- 1 
dat.mar.pa <- dat.mar.red %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.mar.pa)))
names(list0) <- names(dat.mar.pa)
dat.mar.pa <- as.data.frame(dat.mar.pa %>% replace_na(list0))
row.names(dat.mar.pa) <- dat.mar.pa$island
dat.mar.pa <- dat.mar.pa[,-1]

# Hawaii archipielago #
dat.haw.red <- dat.haw[,c("species","island")]
dat.haw.red$presence <- 1 
dat.haw.pa <- dat.haw.red %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.haw.pa)))
names(list0) <- names(dat.haw.pa)
dat.haw.pa <- dat.haw.pa %>% replace_na(list(island = "0")) # I had to add this line because the following error poped up: "Error in `vec_assign()`: ! Can't convert `replace$island` <double> to match type of `data$island` <character>."
dat.haw.pa <- as.data.frame(dat.haw.pa %>% replace_na(list0))
row.names(dat.haw.pa) <- dat.haw.pa$island
dat.haw.pa <- dat.haw.pa[,-1]

# Samoa archipielago #
dat.sam.red <- dat.sam[,c("species","island")] 
dat.sam.red$presence <- 1 
dat.sam.pa <- dat.sam.red %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.sam.pa)))
names(list0) <- names(dat.sam.pa)
dat.sam.pa <- dat.sam.pa %>% replace_na(list(island = "0"))
dat.sam.pa <- as.data.frame(dat.sam.pa %>% replace_na(list0))
row.names(dat.sam.pa) <- dat.sam.pa$island
dat.sam.pa <- dat.sam.pa[,-1]

# Fiji archipielago #
dat.fij.red <- dat.mar[,c("species","island")] 
dat.fij.red$presence <- 1 
dat.fij.pa <- dat.mar.red %>% 
  pivot_wider(names_from=species,values_from=c(presence))
list0 <- as.list(rep(0,ncol(dat.fij.pa)))
names(list0) <- names(dat.fij.pa)
dat.fij.pa <- as.data.frame(dat.fij.pa %>% replace_na(list0))
row.names(dat.fij.pa) <- dat.fij.pa$island
dat.fij.pa <- dat.fij.pa[,-1]


## Richness patterns
# let's calculate alfa and gamma diversity for each archipielago
alfa.div.soc <- mean(rowSums(dat.soc.pa)) # this is the average number of species per island in the Society archipielago: 150.3571
gamma.div.soc <- ncol(dat.soc.pa) # number of species present in the Society archipielago:685  

alfa.div.mar <- mean(rowSums(dat.mar.pa)) # this is the average number of species per island in the Marquesas archipielago:166.3
gamma.div.mar <- ncol(dat.mar.pa) # number of species present in the Marquesas archipielago:475  

alfa.div.haw <- mean(rowSums(dat.haw.pa)) # this is the average number of species per island in the Hawaiian archipielago:417.1538
gamma.div.haw <- ncol(dat.haw.pa) # number of species present in the Hawaiian archipielago:1771  

alfa.div.sam <- mean(rowSums(dat.sam.pa)) # this is the average number of species per island in the Samoan archipielago:110.1667
gamma.div.sam <- ncol(dat.sam.pa) # number of species present in the Samoan archipielago:413

alfa.div.fij <- mean(rowSums(dat.fij.pa)) # this is the average number of species per island in the Fiji archipielago:164.5
gamma.div.fij <- ncol(dat.fij.pa) # number of species present in the Fiji archipielago:473

alfa <- c(alfa.div.soc, alfa.div.mar, alfa.div.haw, alfa.div.sam, alfa.diversity.fij)
gamma <- c(gamma.div.soc, gamma.div.mar, gamma.div.haw, gamma.div.sam, gamma.diversity.fij)

indices <- c("alfa", "gamma")
islands <- c("Society", "Marquesas", "Hawaii", "Samoa", "Fiji")

div_indices <- as.data.frame(matrix(c(alfa,  gamma), nrow=5, ncol=2, dimnames = list(islands,indices)))


## Species Accumulation Curves (SAC)

sac.soc <- vegan::specaccum(dat.soc.pa)
plot(sac.soc, col ="blue", ci.type = "polygon", ci.col = "lightblue", ci.lty=2, main = "SAC for Society archipielago")

sac.mar <- vegan::specaccum(dat.mar.pa)
plot(sac.mar, col ="blue", ci.type = "polygon", ci.col = "lightblue", ci.lty=2, main = "SAC for Marquesas archipielago")

sac.haw <- vegan::specaccum(dat.haw.pa)
plot(sac.soc, col ="blue", ci.type = "polygon", ci.col = "lightblue", ci.lty=2, main = "SAC for Hawaii archipielago")

sac.sam <- vegan::specaccum(dat.sam.pa)
plot(sac.sam, col ="blue", ci.type = "polygon", ci.col = "lightblue", ci.lty=2, main = "SAC for Samoa archipielago")

sac.fij <- vegan::specaccum(dat.fij.pa)
plot(sac.fij, col ="blue", ci.type = "polygon", ci.col = "lightblue", ci.lty=2, main = "SAC for Fiji archipielago")

# none of these curves seems to be saturating just yet. Yes, the slope is decreasing as we increase the number of sites (islands) sampled, but it doesn't feel close enough to saturation.



