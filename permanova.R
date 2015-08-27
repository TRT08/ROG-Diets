
library(ggplot2)
library(scales)
library(compositions)
library(adehabitat)
library(lme4)
library(paleoMAS)
library(doBy)
library(reshape)
library(reshape2)
library(scales)
library(lubridate)
library(extrafont)
library(plyr)
library(outliers)
library(forams)
library(vegan)
library(FSA)
library(compare)

#  RUN THESE FIRST TO GET THE NECESSARY FILES FOR THIS SCRIPT TO RUN ########
source("F:/DATA/SLBE/R scripts/Diet biomass and frequency/dietbiomassfreqscript.R",echo=TRUE, max.deparse.length=10000)
NA.omit.biomass.diets<- NA.omit.biomass
rm(NA.omit.biomass)
setwd("F:/DATA/SLBE/R scripts/Round Goby Diet Analysis/")#set directory


####################   PERMANOVA    ####################

#Better than ANOSIM http://www.esajournals.org/doi/abs/10.1890/12-2010.1
###RUN FIRST####
names(CompiledFishLogandDiets)
d <- CompiledFishLogandDiets
d <- subset(CompiledFishLogandDiets, Cheese.=="N") #remove cheesiness
d <-d[!(d$Diet.Item %in% c("Fish", "Fish Egg", "Empty", "Fish Spp (whole or mostly)", "Fish scales" ,"???")),]
d <-d[!(d$Order %in% c("Fish", "???", "Empty")),]
d$Month <- month(d$Date)
d <-subset(d, Year>="2012")
d<-d[!is.na(d$Site.Condition.For.Event),]
d<-d[!is.na(d$SizeClass),]

effort.time<-subset(FinalFish[,c("YearSER", "Effort..hrs.")])

d<- join(d,effort.time,by=c("YearSER"),match="all")

#Get your species data
freq <- cast(d, YearSERUnique ~ Family, value='Total.Organisms.sum', sum)

#Figure out which environmental vars you want
names(d)
d$poop<-1
environ<-summaryBy(poop~YearSERUnique+ Year+SizeClass+ Site.Condition.For.Event+Month+Year+ SizeClass +Effort..hrs.+DieoffYear, data=d, FUN=(mean))
names(environ)
environ$"poop.(mean)"<-NULL
#environSERs <- environ$YearSER[is.na(environ$All.SiteCondition)]
#environ<- environ[!is.na(environ$All.SiteCondition),]
#freq <- freq[!freq$YearSER %in% environSERs, ]

row.names(environ)<- environ$YearSERUnique
environ$YearSERUnique<-NULL
mode(environ$DieoffYear) <- "integer" #makes TRUE/FALSE statement into 1/0

#Make site condition into a dummy
dummy <- as.data.frame(model.matrix( ~ Site.Condition.For.Event - 1, data=environ))
environ<- cbind(dummy,environ)
environ$Site.Condition.For.Event <-NULL

row.names(freq)<- freq$YearSERUnique
freq$YearSERUnique<-NULL

#environ$NumMussels <- freq$Dreissenidae
#freq <- subset(freq, select = -c(Dreissenidae) )
environ<-data.frame(lapply(environ,as.numeric))

###adonis(freq ~ DayNum*Depth.m.standard*Year*Month*Latitude*Longitude*DieoffYear*Clad.*NumMussels, data=environ, method="bray",permutations=999)
###lat/long may need to be removed bc it makes the permanova blow up

