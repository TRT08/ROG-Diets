##Edited 7/16/14

setwd("G:/R/Goby diets vs avail/")

library(compositions)
library(adehabitat)
library(lme4)

diets <- read.csv("diets.csv")
rownames(diets) <- diets$Goby
diets <- subset(diets, select = -1)

avail <- read.csv("avail.csv")
rownames(avail) <- avail$Avail
avail <- subset(avail, select = -1)


#Compute the centered log ratio transform of a (dataset of) composition(s) and its inverse.
select<- clr(diets)-clr(avail)
select<-data.frame(select)

#Selectivity boxplot
boxplot(select, use.cols = TRUE, main="What do gobies prefer?", 
        xlab="Diet Items", ylab="Selectivity rating")

#Compositional Analysis of Habitat Use
results <- compana(diets, avail, test="randomisation", nrep=5000, rnv=0.0001, alpha=0.05)
names(results)
results$rm

#Linear Mixed Models
#Relate Selectivity index of each critter to: biomass of all critters (fixed) + season (fixed) + habitat (random?)+ gear (fixed)

biomass <- read.csv("biomass.csv")
rownames(biomass) <- biomass$Avail
biomass<- subset(biomass, select = -1)

select.model = lmer(SWorms ~ WormsB + IsopodsB + HumansB+ MusselsB +Season + (1|Habitat), data=biomass) 

