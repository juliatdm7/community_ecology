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

# are these values different from what we would expect by chance alone? 
# to answer this question we will use a permutation algorithm to generate beta diversity values under randomness.
# we will do this using permatfull() function in vegan package.
permatfull(dat.soc.pa, times = 99, mtype = "prab") # we're using dat.soc.pa as our community data matrix, setting the number of permutations to 99 (although it would be better to use 999 permutations, because then we would have 999 simulation plus our actual observation and it would make it easier to determine significancy) and we tell the function that our data type is presence/absence instead of count with the "mtype" argument
# we can also decide which constraints to impose to the presence/absence matrix:
## when fixing row sums we're saying that for each island we want to keep the number of species but we don't really mind about which species to keep nor on how many islands we can find each species: we're fixing alpha diversity.
## when fixing column sums we're saying that, for the whole archipielago, we want to keep the number of islands in which they appear, but we don't really care about the specific islands in which they are present or how many species each island has: we're fixing species occupancy.

#We can compute permutations for all four options and store them in lists:
dat.soc.pa.perm.none <- permatfull(dat.soc.pa, times = 99, mtype = "prab", fixedmar = "none")
dat.soc.pa.perm.row <- permatfull(dat.soc.pa, times = 99, mtype = "prab", fixedmar = "rows")
dat.soc.pa.perm.col <- permatfull(dat.soc.pa, times = 99, mtype = "prab", fixedmar = "columns")
dat.soc.pa.perm.both <- permatfull(dat.soc.pa, times = 99, mtype = "prab", fixedmar = "both")

beta.mean <- function(dat){ ##this function computes the average for Sorensen and Simpson beta diversity for a given site-by-species matrix dat
  beta.dat <- beta.pair(dat)
  return(c(mean(beta.dat$beta.sor),mean(beta.dat$beta.sim)))
}

beta.rand.soc.none <- data.frame(matrix(unlist(lapply(dat.soc.pa.perm.none$perm,beta.mean)),99,2,byrow = TRUE)) # here we're calculating beta diversity (Sorensen and Simpson diversity indices) for each element of the list (each randomisation). Then, we're converting these into a dataframe by turning them into numerical data (unlist() function) and using the function matrix(). 
names(beta.rand.soc.none) <- c("Sorensen","Simpson")

beta.rand.soc.row <- data.frame(matrix(unlist(lapply(dat.soc.pa.perm.row$perm,beta.mean)),99,2,byrow = TRUE))
names(beta.rand.soc.row) <- c("Sorensen","Simpson")

beta.rand.soc.col <- data.frame(matrix(unlist(lapply(dat.soc.pa.perm.col$perm,beta.mean)),99,2,byrow = TRUE))
names(beta.rand.soc.col) <- c("Sorensen","Simpson")

beta.rand.soc.both <- data.frame(matrix(unlist(lapply(dat.soc.pa.perm.both$perm,beta.mean)),99,2,byrow = TRUE))
names(beta.rand.soc.both) <- c("Sorensen","Simpson")

beta.soc.obs <- c(mean(beta.soc$beta.sor),mean(beta.soc$beta.sim))

par(mfrow=c(2,4))
hist(beta.rand.soc.none$Sorensen,breaks=seq(0,1,0.025),main="None fixed",xlab="Sorensen")
abline(v=quantile(beta.rand.soc.none$Sorensen,0.025),col="blue")
abline(v=quantile(beta.rand.soc.none$Sorensen,0.975),col="blue")
abline(v=beta.soc.obs[1],col="red")

hist(beta.rand.soc.row$Sorensen,breaks=seq(0,1,0.025),main="Rows fixed",xlab="Sorensen")
abline(v=quantile(beta.rand.soc.row$Sorensen,0.025),col="blue")
abline(v=quantile(beta.rand.soc.row$Sorensen,0.975),col="blue")
abline(v=beta.soc.obs[1],col="red")

hist(beta.rand.soc.col$Sorensen,breaks=seq(0,1,0.025),main="Columns fixed",xlab="Sorensen")
abline(v=quantile(beta.rand.soc.col$Sorensen,0.025),col="blue")
abline(v=quantile(beta.rand.soc.col$Sorensen,0.975),col="blue")
abline(v=beta.soc.obs[1],col="red")

hist(beta.rand.soc.both$Sorensen,breaks=seq(0,1,0.025),main="Both columns and rows fixed",xlab="Sorensen")
abline(v=quantile(beta.rand.soc.both$Sorensen,0.025),col="blue")
abline(v=quantile(beta.rand.soc.both$Sorensen,0.975),col="blue")
abline(v=beta.soc.obs[1],col="red")

hist(beta.rand.soc.none$Simpson,breaks=seq(0,1,0.025),main="None fixed",xlab="Simpson")
abline(v=quantile(beta.rand.soc.none$Simpson,0.025),col="blue")
abline(v=quantile(beta.rand.soc.none$Simpson,0.975),col="blue")
abline(v=beta.soc.obs[2],col="red")

hist(beta.rand.soc.row$Simpson,breaks=seq(0,1,0.025),main="Rows fixed",xlab="Simpson")
abline(v=quantile(beta.rand.soc.row$Simpson,0.025),col="blue")
abline(v=quantile(beta.rand.soc.row$Simpson,0.975),col="blue")
abline(v=beta.soc.obs[2],col="red")

hist(beta.rand.soc.col$Simpson,breaks=seq(0,1,0.025),main="Columns fixed",xlab="Simpson")
abline(v=quantile(beta.rand.soc.col$Simpson,0.025),col="blue")
abline(v=quantile(beta.rand.soc.col$Simpson,0.975),col="blue")
abline(v=beta.soc.obs[2],col="red")

hist(beta.rand.soc.both$Simpson,breaks=seq(0,1,0.025),main="Both columns and rows fixed",xlab="Simpson")
abline(v=quantile(beta.rand.soc.both$Simpson,0.025),col="blue")
abline(v=quantile(beta.rand.soc.both$Simpson,0.975),col="blue")
abline(v=beta.soc.obs[2],col="red")



