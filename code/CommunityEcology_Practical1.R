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

